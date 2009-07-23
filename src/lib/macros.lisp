(in-package #:tpd2.lib)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro check-symbols (&rest names)
  `(progn
     ,@(loop for n in names collect 
             `(check-type ,n symbol))))


(defmacro with-package (package &body body)
  (let ((*package* (find-package package)))
    (labels ((substitute-symbols-into-package (form)
	     (etypecase form
	       (cons (mapcar #'substitute-symbols-into-package form))
	       (symbol (intern (symbol-name form)))
	       (atom form))))
    `(let ((*package* (find-package ',package)))
       ,@(substitute-symbols-into-package body)))))


(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro acond (&rest clauses)
  (when clauses
    (destructuring-bind ((test &rest body) &rest left-over)
	clauses
      `(aif ,test (progn ,@(or body `(it)))
	    (acond ,@left-over))))) 

(defmacro awhen (test &body body)
  `(aif ,test
	(progn ,@body)))

(defmacro awhile (test &body body)
  `(loop for it = ,test
      while it
      do (locally ,@body)))

(defmacro adolist (list &body body)
  `(dolist (it ,list)
     ,@body))

(define-modify-macro appendf (&rest lists) append)

(define-modify-macro reversed-deletef
    (item &rest misc-args)
  (lambda (place value &rest misc-args)
    (apply #'delete value place misc-args)))

(defmacro deletef (item place &rest misc-args)
  `(reversed-deletef ,place ,item ,@misc-args))

(defmacro dohash ((key value table &optional (result-form nil)) &body body)
  `(progn
     (maphash (lambda (,key ,value) ,@body) ,table)
     ,result-form))

(defun generate-case-key (keyform &key test (transform 'identity) clauses)
  (with-unique-names (xkeyform)
    (flet ((apply-transform (form)
	     `(,transform ,form)))
      `(let ((,xkeyform ,(apply-transform keyform)))
	 (cond ,@(mapcar 
		  (lambda(clause) 
		    (list* (typecase (first clause)
			     ((member t otherwise) t)
			     (list `(member ,xkeyform (list ,@(mapcar #'apply-transform (first clause))) :test (function ,test)))
			     (t `(funcall (function ,test) ,xkeyform ,(apply-transform (first clause)))))
			   (rest clause))) clauses))))))

(defmacro case-func (keyform func &rest clauses)
  (generate-case-key keyform :test func :clauses clauses))

(defmacro case-equalp (keyform &rest clauses)
  `(case-func ,keyform equalp ,@clauses))

(defmacro case-= (keyform &rest clauses)
  `(case-func ,keyform = ,@clauses))

(defmacro def-if-unbound (def name args &body body)
  (check-symbols name)
  (unless (fboundp name)
    `(,def ,name ,args
       ,@body)))

(defmacro ignorable-let (let-name bindings &body body)
  (let ((names (mapcar 'force-first bindings)))
    `(,let-name ,bindings
		(declare (ignorable ,@names))
		,@body)))

(defun filter-until-full (fn list max-num)
  (remove-if-not fn list :count max-num))

(defun filter (fn list)
  (remove-if-not fn list))

(defun mv-filter (fn list)
  (let ((ret-t) (ret-nil))
    (dolist (var list)
      (if (funcall fn var)
	  (push var ret-t)
	  (push var ret-nil)))
    
    (values (nreverse ret-t)
	    (nreverse ret-nil))))

(defun filter-non-nil (list)
  (filter #'identity list))

(defun merge-constant-arguments (args &key (process-one 'identity) join env)
  (let ((joined))
    (labels (
	     (out (list)
		(cond 
		  ((or (every 'constantp list)
		       (loop for x in list for exp = (macroexpand x env) 
			     always (constantp exp) 
			     collect exp into e
			     finally (setf list e)))
		   (eval `(,join ,@list)))
		  ((rest list)
		   `(read-only-load-time-value (,join ,@list)))
		  (t
		   `(read-only-load-time-value ,(first list)))))
	     (constants ()
	     (when joined
	       (prog1 
		   (out joined)
		 (setf joined nil))))
	   (process-one (arg)
	     (if (eq 'identity process-one)
		 arg
		 `(,process-one ,arg))))
      (filter-non-nil 
       (append
	(loop for arg in args
	   if (load-time-constantp arg env)
	   do (appendf joined (list (process-one arg)))
	   else append (append (list (constants)) (list (process-one arg)))
	   and do (setf joined nil))
	(when joined (list (constants))))))))

(defun separate-declarations (declarations-and-body)
  (loop for form in declarations-and-body
     when (and (listp form) (eq (first form) 'declare))
     collect form into declarations
     else collect form into body
     finally (return (values declarations body))))

(defun separate-keywords (arglist)
  (let ((keywords) (non-keywords))
    (loop for remaining = arglist then (if (keywordp (first remaining))
					   (progn
					     (push (first remaining) keywords)
					     (push (second remaining) keywords)
					     (cddr remaining))
					   (progn
					     (push (first remaining) non-keywords)
					     (cdr remaining)))
       while remaining)
    (values (nreverse keywords) (nreverse non-keywords))))

(defmacro signal-protect (protected &body cleanup)
  (with-unique-names (c)
    `(handler-bind 
	 ((t (lambda(,c) (declare (ignore ,c)) ,@cleanup)))
       ,protected)))


(defmacro let-current-values (vars &body body)
  `(let ,(loop for v in vars collect `(,v ,v))
     ,@body))


(defmacro with-preserve-specials (specials &body body)
  (let ((tmps (mapcar (lambda(x)(gensym (symbol-name x))) specials)))
    `(let ,(loop for s in specials
		 for m in tmps
		 collect `(,m (when (boundp ',s),s)))
       (macrolet ((with-specials-restored (&body body)
		  `(let ,',(loop for s in specials
				 for m in tmps
				 collect `(,s ,m))
		     ,@body)))
	 ,@body))))
