(in-package #:tpd2.lib)

(defgeneric copy (original))
(defgeneric assign (original copy))

(defmethod assign ((original array) (copy array))
  (loop for i from 0 upto (array-total-size original)
       do (setf (row-major-aref copy i) (copy (row-major-aref original i)))))

(defmethod copy ((original number))
  original)
(defmethod copy ((original symbol))
  original)
(defmethod copy ((original cons))
  (cons (copy (car original)) (copy (cdr original))))
;(defmethod copy ((original structure-object)) XXX disabled as copy-structure is shallow
;  (copy-structure original))
(defmethod copy ((original array))
  (let ((new
	 (apply #'make-array
		(list* (array-dimensions original)
		       :element-type (array-element-type original)
		       :adjustable (adjustable-array-p original)
		       :fill-pointer (when (array-has-fill-pointer-p original)
				       (fill-pointer original))))))
    (assign original new)
    new))
(defmethod copy ((original standard-object))
  (let ((new (make-instance (class-of original))))
    (assign original new)
    new))

(defgeneric my-auto-prefices (instance))

(defmethod my-auto-prefices ((seq list))
  (loop for x in seq appending (my-auto-prefices x)))

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

(defun parse-defstruct (name-and-options)
  (values
   (force-first name-and-options)
   (loop for x in (force-rest name-and-options) 
	 if (and (listp x) (eq (first x) :include))
	 collect (second x))))

(defun generate-defmyclass-defstruct (&key name superclasses slots conc-name predicate-sym)
  `(eval-always
     (defclass ,name (,@superclasses)
       ,(mapcar (lambda(slot-spec)
		  (let ((slot-name (force-first slot-spec)))
		    `(,slot-name
		      :initarg ,(intern (symbol-name slot-name) :keyword)
		      :initform ,(when (and (force-rest slot-spec) (not (keywordp (second slot-spec))))
				  (second slot-spec))
		      :accessor ,(concat-sym conc-name slot-name)))) slots))
     (defun ,(concat-sym-from-sym-package name 'make- name) (&rest args)
       (apply #'make-instance ',name args))
     (defgeneric ,predicate-sym (var))
     (defmethod ,predicate-sym (var)
       (declare (ignore var))
       nil)
     (defmethod ,predicate-sym ((var ,name))
       (declare (ignore var))
       t)))

(defmacro defmyclass-defstruct (name-and-options &rest slots)
  (multiple-value-bind (name superclasses)
      (parse-defstruct name-and-options)
    (generate-defmyclass-defstruct
     :name name 
     :superclasses superclasses
     :slots slots
     :conc-name (concat-sym name '-)
     :predicate-sym (concat-sym name '-p))))

(defun generate-defstruct (&key defstruct name-and-options slots)
  (multiple-value-bind (name superclasses)
      (parse-defstruct name-and-options)
    `(eval-always
	(progn
	  (,defstruct ,name-and-options ,@slots)
	  (defmethod assign ((original ,name) (copy ,name))
	    ,@(mapcar (lambda(slot) 
		       (let ((slot-name (force-first slot)))
			 `(setf (slot-value copy ',slot-name) (copy (slot-value original ',slot-name))))) slots)
	    ,@(when superclasses
		    `((call-next-method)))
	   copy)
	  (defmethod my-auto-prefices ((class (eql (find-class ',name))))
	    (cons ',name (my-auto-prefices ',superclasses)))
	  (find-class ',(force-first name-and-options))))))

(defmacro defmystruct (name-and-options &rest slots)
  (generate-defstruct 
   :defstruct 'defstruct 
   :name-and-options name-and-options 
   :slots slots))

(defmacro defmyclass (name-and-options &rest slots)
  (generate-defstruct 
   :defstruct 'defmyclass-defstruct
   :name-and-options name-and-options 
   :slots slots))
    
(defun my-function (func prefices)
  (let ((possibilities (mapcar (lambda(prefix) (concat-sym prefix '- func)) prefices)))
    (or (find-if 'fboundp possibilities) (first possibilities))))

(defun its-type-sym (instance)
  (when (symbolp instance)
    (concat-sym-from-sym-package instance '%tpd2-its-known-type- instance)))

(defmacro with-its-type ((instance type) &body body)
  (check-type instance symbol)
  (check-type type symbol)
  `(symbol-macrolet ((,(its-type-sym instance) ,type))
     ,@body))

(defun its-known-type (instance env)
  (let ((sym (its-type-sym instance)))
    (when sym
      (multiple-value-bind
	   (type valid)
	  (macroexpand-1 sym env) 
	(when valid type)))))

(defmacro its (func instance &rest args &environment env)
  (check-type func symbol)
  (let ((its-known-type (its-known-type instance env)))
    (cond (its-known-type
	   `(,(my-function func (my-auto-prefices its-known-type)) ,instance ,@args))
	  (t
	   (once-only (instance)
	     `(funcall (my-function ',func (my-auto-prefices ,instance)) ,instance ,@args))))))

(defmacro set-its (new-value func instance &rest args &environment env)
  (let ((its-known-type (its-known-type instance env)))
    (cond (its-known-type
	   `(setf (,(my-function func (my-auto-prefices its-known-type)) ,instance ,@args) ,new-value))
	  (t
	   `(set-its-dynamic ,new-value ',func ,instance ,@args)))))

(defun set-its-dynamic (new-value func instance &rest args)
  (check-type func symbol)
  (eval `(setf (,(my-function func (my-auto-prefices instance)) ,instance ,@args) ',new-value)))

(define-setf-expander its (func instance &rest args) 
					; cannot use defsetf because need to control evaluation of func argument
  ; XXX maybe evaluates thing too many times . . .
  (check-type func symbol)
  (with-unique-names (new-value)
   (values 
    nil
    nil
    (list new-value)
    `(set-its ,new-value ,func ,instance ,@args)
    `(its ,func ,instance ,@args))))

(defun my-func-name-to-symbol (class func)
  (etypecase func
    (symbol (my-function func (my-auto-prefices class)))
    (list 
     (ecase (first func)
       (quote
	(unquote-quoted-symbol func))
       (setf
	(list 'setf (my-func-name-to-symbol class (second func))))))))

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
	       (let ((func-sym (my-func-name-to-symbol class func)))
		 (if (and (fboundp func-sym) (subtypep (type-of (fdefinition func-sym)) 'generic-function))
		     (values 'defmethod `(,class ,class))
		     (values 'defun class)))
	     (if (and (listp func) (eq (first func) 'setf))
		 (values def (list 'setf (my-func-name-to-symbol class (second func)))
			 (list* (first args) my-arg (rest args)))
		 (values def (my-func-name-to-symbol class func) (list* my-arg args))))))
    (check-type class symbol)
    (multiple-value-bind (combination-type args declarations-and-body)
	(if (keywordp lambda-list)
	    (values lambda-list (first body) (rest body))
	    (values nil lambda-list body))
      (multiple-value-bind (declarations-and-body inline)
	  (if (equalp '(my-declare-fast-inline) (first declarations-and-body))
	      (values (cons '(declare (optimize speed)) (rest declarations-and-body)) t)
	      (values declarations-and-body nil))
	(multiple-value-bind (declarations body)
	    (separate-declarations declarations-and-body)
	  (multiple-value-bind 
		(def name lambda-list)
	      (my-make-def class func args)
	    `(progn 
	       ,(when inline `(declaim (inline ,name)))
	       (,def ,name ,@(force-list combination-type) ,lambda-list
			  ,@declarations
			  (labels ((my-call ()
				     (let ((me ,class))
				       (with-shorthand-accessor (my ,class me)
					 ,@body))))
			    (my-call))))))))))

(defun my-call ()
  "Inside a my-defun, #'my-call is the function call again"
  (error "not inside a my-defun"))