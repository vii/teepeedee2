
(in-package :cont)

(export '(expr-sequence->cps call/cc let/cc lambda/cc))

;;; CALL/CC and friends
(defun call/cc (cc)
  "Implements delimited continuations."
  (declare (ignore cc))
  (error "Please ensure CALL/CC is caled from within WITH-CALL/CC
  macro."))

(defmacro let/cc (k &body body)
  "A shortcut for CALL/CC."
  `(call/cc (lambda (,k)
	      ,@body)))

(defcpstransformer call/cc (cons k-expr env)
  (declare (ignore env))
  "Implements call/cc operator."
  (declare (special *ctx*))
  (assert (= (length cons) 2)
	  nil
	  "Please ensure that CALL/CC has one argument")
  `(funcall ,(cadr cons)
	    ,k-expr))

;;; QUOTE
(defcpstransformer quote (cons k-expr env)
  (declare (ignore env))
  "Converts a quoted datum to CPS style."
  `(funcall ,k-expr ,cons))

;;; PROGN
(defun extract-declarations (expr-list)
  "Returns a list of declarations extracted from the EXPR-LIST."
  (loop for i in expr-list
       while (and (consp i)
		  (eq (car i) 'declare))
       collect i))

(defun remove-declarations (expr-list)
  "Removes declarations from the EXPR-LIST."
  (let ((c 0))
    (loop for i in expr-list
       while (and (consp i)
		  (eq (car i) 'declare))
       do (incf c))
    (nthcdr c expr-list)))

(defun expr-sequence->cps (expr-list k-expr env)
  "Converts a list of expressions to be evaluated in order to CPS
  style."
  (expr->cps (car expr-list)
	     (if (null (cdr expr-list))
		 k-expr
		 (let ((r (gensym))
		       (rest-args (gensym)))
		   `(lambda (&optional ,r &rest ,rest-args)
		      (declare (ignore ,r ,rest-args))
		      ,(expr-sequence->cps (cdr expr-list) k-expr env))))
	     env))

(defcpstransformer progn (cons k-expr env)
  "Converts a PROGN form to CPS style."
  (expr-sequence->cps (cdr cons) k-expr env))

;;; BLOCK
(defcpstransformer block (cons k-expr env)
  "Transforms BLOCK to CPS style."
  (declare (special *ctx*))
  (assert (symbolp (cadr cons))
	  nil
	  "Please ensure the name of the BLOCK is a symbol.")
  (let ((k (gensym)))
    (unwind-protect
	 (progn
	   (push k (gethash (cadr cons) (ctx-block-tags *ctx*)))
	   `(let ((,k ,k-expr))
	      (declare (ignorable ,k))
	      ,(expr-sequence->cps (cddr cons) k env)))
      (pop (gethash (cadr cons) (ctx-block-tags *ctx*))))))

;;; RETURN-FROM
(defcpstransformer return-from (cons k-expr env)
  "Transforms RETURN-FROM to CPS style."
  (declare (ignore k-expr))
  (declare (special *ctx*))
  (expr->cps (caddr cons) (or (car (gethash (cadr cons) (ctx-block-tags *ctx*)))
			      (error "There is no block named ~A" (cadr cons)))
	     env))

;;; TAGBODY
(defun group-tagbody-forms (expr-list)
  "Groups TAGBODY forms into a list where each item is a list with CAR
being the go-tag name (or nil for default first tag), and CDR is a
list of expressions for the tag."
  (let (res)
    (loop for expr in expr-list
       initially (when (not (atom (car expr-list)))
		   (push (list nil) res))
       when (and (atom expr)
		 (not (or (symbolp expr) (integerp expr))))
       do (error "Atom is not a go-tag.") else 
       when (and (atom expr)
		 (or (symbolp expr) (integerp expr)))
       do (push (list expr) res) else
       do (push expr (car res)))
    (reverse (mapcar #'reverse res))))

(defun named-tb-forms>cps (forms k-expr env)
  "Takes a list of named TAGBODY forms and converts CAR form to CPS."
  (let* ((form (car forms))
	 (tag-fn-name (car form))
	 (tb-form (cddr form))
	 (next-form (cadr forms))
	 (next-tag-fn-name (car next-form))
	 (args1 (gensym))
	 (args2 (gensym)))
    `(,tag-fn-name (&rest ,args1)
		   (declare (ignore ,args1))
		   ,(expr-sequence->cps tb-form
					(if next-tag-fn-name
					    `(function ,next-tag-fn-name)
					    `(lambda (&rest ,args2)
					       (declare (ignore ,args2))
					       (funcall ,k-expr nil)))
					env))))

(defun duplicates-p (sequence &key (key #'identity) (test #'eql))
  "Returns true if there are duplicate elements in the sequence, false
  otherwise."
  (eq (length sequence) (length (remove-duplicates sequence :key key :test test))))

(defcpstransformer tagbody (cons k-expr env)
  "Transforms TAGBODY to CPS style."
  (declare (special *ctx*))
  (let* ((tb-forms (group-tagbody-forms (cdr cons)))
	 (named-tb-forms (mapcar (lambda (form)
				   (cons (gensym) form))
				 tb-forms)))
    (assert (duplicates-p tb-forms :key #'car)
	    nil
	    "Make sure there are no duplicate tags")
    (unwind-protect
	 (progn
	   (mapc (lambda (form)
		   (push (car form) (gethash (cadr form) (ctx-go-tags *ctx*))))
		 (if (caar tb-forms)
		     named-tb-forms
		     (cdr named-tb-forms)))
	   `(labels (,@(maplist (lambda (forms)
				  (named-tb-forms>cps forms k-expr env))
				named-tb-forms))
	      ,(if (null tb-forms)
		   `(funcall ,k-expr nil)
		   `(,(caar named-tb-forms)))))
      (mapc (lambda (form)
	      (pop (gethash (cadr form) (ctx-go-tags *ctx*))))
	    (if (caar tb-forms)
		named-tb-forms
		(cdr named-tb-forms))))))

;;; GO
(defcpstransformer go (cons k-expr env)
  "Transforms GO to CPS style."
  (declare (ignore k-expr env))
  (declare (special *ctx*))
  `(,(or (car (gethash (cadr cons) (ctx-go-tags *ctx*)))
	 (error "There is no go-tag named ~A" (cadr cons)))))

;;; IF
(defcpstransformer if (cons k-expr env)
  "Converts an IF form to CPS style."
  (let ((pred-expr (cadr cons))
	(pred-true-expr (caddr cons))
	(pred-false-expr (cadddr cons)))
    (expr->cps pred-expr
	       (let ((pred (gensym))
		     (rest-args (gensym)))
		 `(lambda (,pred &rest ,rest-args)
		    (declare (ignore ,rest-args))
		    (if ,pred
			,(expr->cps pred-true-expr k-expr env)
			,(expr->cps pred-false-expr k-expr env))))
	       env)))

;;; FUNCTION
#+allegro (eval-when (:compile-toplevel :load-toplevel :execute)
	    (unless (c2mop:class-finalized-p (find-class 'c2mop:funcallable-standard-object))
	      (c2mop:finalize-inheritance (find-class 'c2mop:funcallable-standard-object))))

(defclass funcallable/cc (#+allegro c2mop:funcallable-standard-object)
  ((function :accessor f/cc-function
	     :initarg :function
	     :documentation "A function object that accepts a
	     continuation argument."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A structure that represents a funcallable object"))

(defun make-funcallable (function)
  "Creates an instance of FUNCALLABLE/CC."
  (let ((inst (make-instance 'funcallable/cc :function function)))
    (c2mop:set-funcallable-instance-function
     inst
     #-cmu (lambda (&rest args)
	     (apply (f/cc-function inst) #'values args))
     #+cmu (compile nil
		    `(lambda (&rest args)
		       (apply (f/cc-function ,inst) #'values args))))
    inst))

(defun funcall/cc (fdesignator k &rest args)
  "Implements FUNCALL for a CPS converter."
  (typecase fdesignator
    (funcallable/cc (apply (f/cc-function fdesignator) k args))
    (t (funcall k (apply fdesignator args)))))

(defun apply/cc (fdesignator k &rest args)
  "Implements FUNCALL for a CPS converter."
  (typecase fdesignator
    (funcallable/cc (apply #'apply (f/cc-function fdesignator) k args))
    (t (funcall k (apply #'apply fdesignator args)))))

(defun lambda-expr->cps (cons k-expr env)
  "Converts a LAMBDA expression to CPS style."
  (declare (ignore k-expr))
  (let ((arg-list (cadr cons))
	(body (cddr cons)))
    (let ((k (gensym)))
      `(make-funcallable (lambda (,k ,@arg-list)
			   (declare (ignorable ,k))
			   ,@(extract-declarations body)
			   ,(expr-sequence->cps (remove-declarations body) k env))))))

(defcpstransformer function (cons k-expr env)
  (declare (special *ctx*))
  (let ((fdesignator (cadr cons)))
    (etypecase fdesignator
      (symbol (if (find fdesignator (ctx-local-functions *ctx*))
		  `(funcall ,k-expr (make-funcallable ,cons))
		  `(funcall ,k-expr ,cons)))
      (list (ecase (car fdesignator)
	      (setf `(funcall ,k-expr ,cons))
	      (lambda `(funcall ,k-expr ,(lambda-expr->cps fdesignator k-expr env))))))))

(defmacro lambda/cc (args-list &body body)
  "A helper macro that wraps LAMBDA definitions with WITH-CALL/CC."
  `(with-call/cc
     (lambda ,args-list
       ,@body)))

;;; LET*
(defun let*-varlist->cps (varlist let-body k-expr env)
  "Converts a list of variables from a LET* expression to CPS style."
  (let* ((var (car varlist))
	 (var-name (if (consp var) (car var) var))
	 (var-value (when (consp var) (cadr var))))
    (if var
	(expr->cps var-value
		   (let ((rest-args (gensym)))
		     `(lambda (,var-name &rest ,rest-args)
			(declare (ignore ,rest-args))
			,@(unless (cdr varlist)
				  (extract-declarations let-body))
			,(let*-varlist->cps (cdr varlist) let-body k-expr env)))
		   env)
	(expr-sequence->cps (remove-declarations let-body) k-expr env))))

(defcpstransformer let* (cons k-expr env)
  "Converts a LET* expression to CPS style."
  (let ((varlist (cadr cons))
	(forms (cddr cons)))
    (let*-varlist->cps varlist forms k-expr env)))

;;; LET
(defun let-varlist->cons-pairs (varlist)
  "Converts a list of LET vars into name/value cons pairs."
  (mapcar (lambda (var)
	    (cons (if (consp var) (car var) var)
		  (when (consp var) (cadr var))))
	  varlist))

(defcpstransformer let (cons k-expr env)
  "Converts a LET expression to CPS style."
  (let ((varlist (let-varlist->cons-pairs (cadr cons)))
	(forms (cddr cons)))
    (expr->cps `((lambda ,(mapcar #'car varlist)
		   ,@(extract-declarations forms)
		   ,@(remove-declarations forms))
		 ,@(mapcar #'cdr varlist))
	       k-expr
	       env)))

;;; SETQ
(defun setq->cps (setq-pairs k-expr env)
  "Converts a list of SETQ pairs to CPS style."
  (when setq-pairs
    (expr->cps (cdar setq-pairs)
	       (let ((i (gensym))
		     (rest-args (gensym)))
		 (assert (symbolp (caar setq-pairs))
			 nil
			 "The value ~A is not a symbol" (caar setq-pairs))
		 `(lambda (,i &rest ,rest-args)
		    (declare (ignore ,rest-args))
		    ,@(if (cdr setq-pairs)
			  `((setq ,(caar setq-pairs) ,i)
			    ,(setq->cps (cdr setq-pairs) k-expr env))
			  `((funcall ,k-expr (setq ,(caar setq-pairs) ,i))))))
	       env)))

(defcpstransformer setq (cons k-expr env)
  "Converts a SETQ expression to CPS style."
  (assert (evenp (length (cdr cons)))
	  nil
	  "Odd number of argumenuts to SETQ: ~A" cons)
  (setq->cps
   (loop for i in (cdr cons) by #'cddr
      for j in (cddr cons) by #'cddr
      collect (cons i j))
   k-expr
   env))

;;; THE
(defcpstransformer the (cons k-expr env)
  "Converts a THE expression to CPS style."
  (expr->cps (caddr cons) k-expr env))

;;; FLET/LABELS
(defmacro transform-forms-in-env (forms k-expr transf-env &environment env)
  "Macroexpands code within a form that sets up a lexical environment
and runs a CPS code walker on it."
  (let ((*ctx* transf-env))
    (declare (special *ctx*))
    (expr-sequence->cps forms k-expr env)))

(defun transform-local-function (fn env in-env-p)
  "Transforms a local (FLET or LABELS) function to CPS style."
  (declare (special *ctx*))
  (let ((fn-name (car fn))
	(fn-args (cadr fn))
	(fn-forms (cddr fn))
	(k (gensym)))
    (assert (and fn-name (symbolp fn-name))
	    nil
	    "Function name must be a non-NIL symbol")
    (assert (>= (length fn) 2)
	    nil
	    "Function arguments not specified")
    `(,fn-name (,k ,@fn-args)
	       (declare (ignorable ,k))
	       ,@(extract-declarations fn-forms)
	       ,(if in-env-p
		    `(transform-forms-in-env ,(remove-declarations fn-forms) ,k
					     ,(copy-transformation-contect *ctx*))
		    (expr-sequence->cps (remove-declarations fn-forms) k env)))))

(defun declare-function-names-local (names)
  "Declares function names local in a transformation environment."
  (declare (special *ctx*))
  (dolist (fn names)
    (push fn (ctx-local-functions *ctx*))))

(defun undeclare-function-names-local (names)
  "Undeclares function names local in a transformation environment."
  (declare (special *ctx*))
  (dolist (fn names)
    (setf (ctx-local-functions *ctx*)
	  (remove fn (ctx-local-functions *ctx*) :count 1))))

(defmacro with-local-function-names (names &body body)
  "Runs BODY within a transformation environment in which functions
  named by NAMES are declared as local."
  (let ((fn-list (gensym)))
    `(let ((,fn-list ,names))
       (unwind-protect
	    (progn
	      (declare-function-names-local ,fn-list)
	      ,@body)
	 (undeclare-function-names-local ,fn-list)))))

(defcpstransformer flet (cons k-expr env)
  "Converts an FLET expression to CPS style."
  (declare (special *ctx*))
  (let ((fn-list (cadr cons))
	(forms (cddr cons)))
    (assert (>= (length cons) 2)
	    nil
	    "Too few parameters to FLET")
    `(flet ,(mapcar (lambda (fn)
		      (transform-local-function fn env nil))
		    fn-list)
       ,@(extract-declarations forms)
       ,(with-local-function-names (mapcar #'car fn-list)
          `(transform-forms-in-env ,(remove-declarations forms) ,k-expr
				   ,(copy-transformation-contect *ctx*))))))

(defcpstransformer labels (cons k-expr env)
  "Converts a LABELS expression to CPS style."
  (declare (special *ctx*))
  (let ((fn-list (cadr cons))
	(forms (cddr cons)))
    (assert (>= (length cons) 2)
	    nil
	    "Too few parameters to LABELS")
    (with-local-function-names (mapcar #'car fn-list)
      `(labels ,(mapcar (lambda (fn)
			  (transform-local-function fn env t))
			fn-list)
	 ,@(extract-declarations forms)
	 (transform-forms-in-env ,(remove-declarations forms) ,k-expr
				 ,(copy-transformation-contect *ctx*))))))

;;; MACROLET
(defcpstransformer macrolet (cons k-expr env)
  "Converts a MACROLET expression to CPS style."
  (declare (ignore env))
  (declare (special *ctx*))
  `(macrolet ,(cadr cons)
     ,@(extract-declarations (cddr cons))
     (transform-forms-in-env ,(remove-declarations (cddr cons)) ,k-expr
			     ,(copy-transformation-contect *ctx*))))

;;; SYMBOL-MACROLET
(defcpstransformer symbol-macrolet (cons k-expr env)
  "Converts a SYMBOL-MACROLET expression to CPS style."
  (declare (ignore env))
  (declare (special *ctx*))
  `(symbol-macrolet ,(cadr cons)
     ,@(extract-declarations (cddr cons))
     (transform-forms-in-env ,(remove-declarations (cddr cons)) ,k-expr
			     ,(copy-transformation-contect *ctx*))))

;;; LOCALLY
(defcpstransformer locally (cons k-expr env)
  "Converts a LOCALLY expression to CPS style."
  `(locally
     ,@(extract-declarations (cdr cons))
     ,(expr-sequence->cps (remove-declarations (cdr cons)) k-expr env)))

;;; CATCH/THROW
(defcpstransformer catch (cons k-expr env)
  (declare (ignore env))
  "Transforms a CATCH expression to CPS style."
  `(funcall ,k-expr ,cons))

;;; VALUES
(defcpstransformer values (cons k-expr env)
  "Transforms a VALUES expression to CPS style."
  (cond ((not (cdr cons))
	 `(funcall ,k-expr))
	(t
	 (expr->cps `(list ,@(cdr cons))
		    (let ((args (gensym))
			  (rest (gensym)))
		      `(lambda (&optional ,args &rest ,rest)
			(declare (ignore ,rest))
			(apply ,k-expr ,args)))
		    env))))

;;; VALUES-LIST
(defcpstransformer values-list (cons k-expr env)
  "Transforms a VALUES-LIST expression to CPS style."
  (expr->cps (cadr cons)
	     (let ((args (gensym))
		   (rest (gensym)))
	       `(lambda (,args &rest ,rest)
		  (declare (ignore ,rest))
		  (apply ,k-expr ,args)))
	     env))

;;; MULTIPLE-VALUE-CALL
(defun accum-mc-sequence->cps (expr-list fn k-expr args env)
  "Converts a list of expressions to be evaluated in order to CPS
  style and collects their results (including multiple values)."
  (if (null expr-list)
      (expr->cps fn
		 (let ((r (gensym)))
		   `(lambda (,r)
		      (apply/cc ,r ,k-expr (append ,@(reverse args)))))
		 env)
      (expr->cps (car expr-list)
		 (let ((rs (gensym)))
		   `(lambda (&rest ,rs)
		      ,(accum-mc-sequence->cps (cdr expr-list) fn k-expr (cons rs args) env)))
		 env)))

(defcpstransformer multiple-value-call (cons k-expr env)
  "Transforms a MULTIPLE-VALUE-CALL expression to CPS style."
  (let ((fn (cadr cons))
	(forms (cddr cons)))
    (accum-mc-sequence->cps forms fn k-expr nil env)))

;;; MULTIPLE-VALUE-PROG1
(defcpstransformer multiple-value-prog1 (cons k-expr env)
  "Transforms MULTIPLE-VALUE-PROG1 expression to CPS style."
  (let ((args (gensym))
	(res (gensym)))
    (expr->cps `(let (,args)
		  (multiple-value-call (lambda (&rest ,res)
					 (setf ,args ,res))
		    ,(cadr cons))
		  ,@(cddr cons)
		  (values-list ,args))
	       k-expr
	       env)))

;;; LOAD-TIME-VALUE
(defcpstransformer load-time-value (cons k-expr env)
  "Transforms LOAD-TIME-VALUE expression to CPS style."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

;;; EVAL-WHEN
(defcpstransformer eval-when (cons k-expr env)
  "Transforms EVAL-WHEN expression to CPS style."
  `(eval-when ,(cadr cons)
     ,(expr-sequence->cps (cddr cons) k-expr env)))

;;; PROGV
(defcpstransformer progv (cons k-expr env)
  "Basic support for now."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

;;; UNWIND-PROTECT
(defcpstransformer unwind-protect (cons k-expr env)
  "Basic support for UNWIND-PROTECT."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

