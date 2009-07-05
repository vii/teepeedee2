(in-package :cont)

(export '(with-call/cc expr->cps))

(defmacro with-call/cc (&body body &environment env)
  "Gives access to CALL/CC by transforming BODY to continuation
passing style."
  (let ((*ctx* (make-call/cc-context)))
    (declare (special *ctx*))
    (expr-sequence->cps body '#'values env)))

(defun expr->cps (expr k-expr env)
  "Transforms expression to CPS style."
  (if (atom expr)
      (atom->cps expr k-expr)
      (cons->cps expr k-expr env)))

(defun atom->cps (atom k-expr)
  "Transforms an atom to CPS style."
  `(funcall ,k-expr ,atom))

(defun cons->cps (cons k-expr env)
  "Transforms a cons to CPS style."
  (let ((transformer (gethash (car cons) *special-form-transformers*)))
    (if transformer
	(funcall transformer cons k-expr env)
	(multiple-value-bind (expansion expanded-p)
	    (macroexpand-1 cons env)
	  (if expanded-p
	      (expr->cps expansion k-expr env)
	      (funcall->cps (cons `(function ,(car cons))
				  (cdr cons))
			    k-expr nil
			    env))))))

(defun non-call/cc-form (form env function-position-p)
  (declare (special *ctx*))
  (cond 
    ((and function-position-p (cpstransformer form)) nil)
    ((constantp form env) t)
    ((and (listp form) (eq (first form) 'function) 
	  (symbolp (second form))
	  (or (not function-position-p) 
	      (and (not (cpstransformer (second form)))
		   (not (find form (ctx-local-functions *ctx*)))
		   (not (find (second form) (ctx-local-functions *ctx*)))
		   (fboundp (second form)) (not (typep (second form) 'funcallable/cc)))))
     t)))

(defun tidy-up-multiple-value-call (multiple-value-call func &rest args)
  (assert (eq multiple-value-call 'multiple-value-call))
  (cond ((and (member func (list '(function values) (function values) 'values) :test 'equalp) 
	      (not (rest args)))
	 (first args))
	(t
	 `(,multiple-value-call ,func ,@args))))

(defun tidy-up-application (app-sym func k-expr &rest args)
  (declare (special *ctx*))
  (cond ((and (listp func) (eq 'function (first func)) (not (cddr func)) (symbolp (second func))
	      (not (find func (ctx-local-functions *ctx*)))
	      (not (find (second func) (ctx-local-functions *ctx*)))
	      (fboundp (second func)) (not (typep (second func) 'funcallable/cc)))
	 (apply 'tidy-up-multiple-value-call (ecase app-sym
				       (funcall/cc `(multiple-value-call ,k-expr (,(second func) ,@args)))
				       (apply/cc `(multiple-value-call ,k-expr (apply ,func ,@args))))))
	(t
	 `(,app-sym ,func ,k-expr ,@args))))

(defun gensym-name-for (form)
  (let ((name (or (ignore-errors (remove #\Newline (format nil "~A" form))) "unknown")))
    (subseq name 0 (min (length name) 20))))

(defun application->cps (app-sym cons k-expr args env)
  "Transforms a function application to CPS style."
  (cond
    ((and cons (non-call/cc-form (car cons) env (not args)))
     (application->cps app-sym (cdr cons) k-expr (cons (car cons) args) env))
    (cons
     (expr->cps (car cons)
		(let ((i (gensym (gensym-name-for (car cons))))
		      (rest-args (gensym "rest")))
		  `(lambda (&optional ,i &rest ,rest-args)
		     (declare (ignorable ,i))
		     (declare (ignore ,rest-args))
		     ,(application->cps app-sym (cdr cons)
					k-expr
					(cons i args)
					env)))
		env))
    (t
     (let ((r-args (reverse args)))
       (apply 'tidy-up-application 
	      `(,app-sym ,(car r-args) ,k-expr ,@(cdr r-args)))))))

(defun funcall->cps (cons k-expr args env)
  "Transforms FUNCALL to CPS style."
  (application->cps 'funcall/cc cons k-expr args env))

(defun apply->cps (cons k-expr args env)
  "Transforms APPLY to CPS style."
  (application->cps 'apply/cc cons k-expr args env))

