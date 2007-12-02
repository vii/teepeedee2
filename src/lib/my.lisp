(in-package #:tpd2.lib)

(defgeneric my-auto-prefices (instance))

(defmethod my-auto-prefices (instance)
  (my-auto-prefices (class-of instance)))

(defmethod my-auto-prefices ((symbol (eql nil)))
  nil)

(defmethod my-auto-prefices ((symbol symbol))
  (aif (find-class symbol nil)
       (my-auto-prefices it)
       (list symbol)))

(defmethod my-auto-prefices ((class class))
  (list (class-name class)))

(defun generate-defmyclass (&key name superclasses slots conc-name predicate-sym)
  `(eval-always
     (prog1
	 (defclass ,name (,@superclasses)
	   ,(mapcar (lambda(slot-spec)
		      (let ((slot-name (first slot-spec)))
			`(,slot-name
			  :initarg ,(intern (symbol-name slot-name) :keyword)
			  ,@(awhen (rest slot-spec)
				   (when (not (keywordp (second slot-spec)))
				     `(:initform ,(second slot-spec))))
			  :accessor ,(intern (strcat conc-name slot-name))))) slots))
       (defun ,(intern (strcat 'make- name)) (&rest args)
	 (apply #'make-instance ',name args))
       (defmethod assign ((original ,name) (copy ,name))
	 ,@(mapcar (lambda(slot) 
		     (let ((slot-name (first slot)))
		       `(setf (slot-value copy ',slot-name) (copy (slot-value original ',slot-name))))) slots)
	 ,@(when superclasses
		 `((call-next-method)))
	 copy)
       (defmethod my-auto-prefices ((class (eql (find-class ',name))))
	 (cons ',name (mapcan 'my-auto-prefices ',superclasses)))
       (defgeneric ,predicate-sym (var))
       (defmethod ,predicate-sym (var)
	 (declare (ignore var))
	 nil)
       (defmethod ,predicate-sym ((var ,name))
	 (declare (ignore var))
	 t))))

(defmacro defmyclass (name-and-options &rest slots)
  (let ((name (if (atom name-and-options)
		  name-and-options
		  (first name-and-options)))
	(options (rest (force-list name-and-options))))
    (check-type name symbol)
    (generate-defmyclass 
     :name name 
     :superclasses (mapcan (lambda(x) (when (eq (first x) :include) (rest x))) options)
     :slots (mapcar 'force-list slots)
     :conc-name (strcat name "-")
     :predicate-sym (intern (strcat name '-p )))))

(defun my-function (func prefices)
  (let ((possibilities (mapcar (lambda(prefix) (intern (strcat prefix '- func) (symbol-package prefix))) prefices)))
    (or (find-if 'fboundp possibilities) (first possibilities))))

(defmacro its (func instance &rest args)
  (check-type func symbol)
  (once-only (instance)
    `(funcall (my-function ',func (my-auto-prefices ,instance)) ,instance ,@args)))

(defun set-its (new-value func instance &rest args)
  (check-type func symbol)
  (eval `(setf (,(my-function func (my-auto-prefices instance)) ,instance ,@args) ',new-value)))

(define-setf-expander its (func instance &rest args) ; cannot use defsetf because need to control evaluation of func argument
  (check-type func symbol)
  (let ((new-value (gensym)))
    (values 
     nil
     nil
     (list new-value)
     `(set-its ,new-value ',func ,instance ,@args)
     `(its ',func ,instance ,@args))))

(defun my-func-name-to-symbol (class func)
  (etypecase func
    (symbol (my-function func (my-auto-prefices class)))
    (list (unquote-quoted-symbol func))))

(defmacro with-shorthand-accessor ((accessor class &optional (instance class)) &body body)
  (check-type class symbol)
  (once-only ((instance ignorable `(type ,class)))
    `(macrolet ((,accessor (func &rest args)
		  `(,(my-func-name-to-symbol ',class func)
		     ,',instance ,@args)))
       ,@body)))

(defun structure-classp (class)
  (eq (find-class 'structure-class) (class-of (force-class class))))

(defmacro my-defun (class func lambda-list &body body)
  (flet ((my-make-def (class func args)
	   (multiple-value-bind (def my-arg)
	       (if (and (structure-classp class)
			(not (and (listp func) (eq (first func) 'quote) (fboundp (second func)) (subtypep (type-of (fdefinition (second func))) 'generic-function))))
		   (values 'defun class)
		   (values 'defmethod `(,class ,class)))
	     (if (and (listp func) (eq (first func) 'setf))
		 (values def (list 'setf (my-func-name-to-symbol class (second func)))
			 (list* (first args) my-arg (rest args)))
		 (values def (my-func-name-to-symbol class func) (list* my-arg args))))))

    (check-type class symbol)
    (multiple-value-bind (combination-type args declarations-and-body)
	(if (keywordp lambda-list)
	    (values lambda-list (first body) (rest body))
	    (values nil lambda-list body))
      (multiple-value-bind (declarations body)
	  (separate-declarations declarations-and-body)
	(multiple-value-bind 
	      (def name lambda-list)
	    (my-make-def class func args)
	  `(,def ,name ,@(force-list combination-type) ,lambda-list
		 ,@declarations
		 (labels ((my-call ()
			  (let ((me ,class))
			    (with-shorthand-accessor (my ,class me)
			      ,@body))))
		   (my-call))))))))

(defun my-call ()
  "Inside a my-defun, #'my-call is the function call again"
  (error "not inside a my-defun"))