(in-package #:tpd2.lib)

(defun unquote-quoted-symbol (func)
  (assert (eq (first func) 'quote)) 
  (check-type (second func) symbol)
  (second func))

(declaim (ftype (function (t) list) force-list))
(defun-speedy force-list (val)
  (if (listp val)
      val
      (list val)))

(defun-speedy force-first (form)
  (typecase form
    (list (first form))
    (t form)))

(defun-speedy force-rest (form)
  (when (listp form) (rest form)))


(defun force-class (class)
  (if (symbolp class)
      (find-class class)
      class))

(declaim (inline make-displaced-vector))
(defun make-displaced-vector (vector &key (start 0) (end (length vector)))
  (multiple-value-bind
	(orig offset)
      (array-displacement vector)
    (when orig
      (setf vector orig)
      (incf end offset)
      (incf start offset))
    (make-array (- end start)
		:element-type (array-element-type vector)
		:displaced-to vector
		:displaced-index-offset start)))

(defmacro without-call/cc (&body body)
  `(locally ,@body))

(defun debug-assert-report (test-form place-values)
  (format *error-output* "~&Debug assertion failed: ~S evaluated to nil.~%" test-form)
  (loop for (place value) in place-values do
	(format *error-output* "~&~S = ~S of type ~A: ~A~&" place value (type-of value)
		(with-output-to-string (*standard-output*) (describe value)))))

(defun query-new-value ()
  (format *query-io* "~&Enter a new value:~%")
  (list (eval (read *query-io*))))

(defmacro debug-assert (test-form &optional places datum &rest arguments)
  `(progn
     ,test-form
     (values))

  #+tpd2-debug-assert
  (with-unique-names (block val)
    (let ((gensyms (loop for place in places collect (gensym (force-string place)))))
      `(without-call/cc
	 (unless ,test-form
	   (loop do
		 (locally
		     (declare (optimize debug safety (speed 0)))
		   (flet ((,block () ;; use flet to get better debug on SBCL
			    (let ,(loop for p in places for g in gensyms collect `(,g ,p))
			      (restart-case (error ,(or datum (format nil "The debug assertion ~S failed." test-form)) ,@arguments)
			       (continue ()
				 :report "Print a description of the debug assertion and continue."
				 (debug-assert-report
				  ',test-form
				  (list
				   ,@(loop for place in places 
					   for g in gensyms collect
					   `(list ',place ,g))))
				 (return 'debug-assert-skip))
			       ,@(loop for place in places 
				       for g in gensyms collect
				       `(store-value (,val)
						     :interactive query-new-value
						     :report (lambda (stream) (format stream "The current value of ~S is ~S; supply a new value for it." ',place ,g))
						     (setf ,place ,val)))
			       (debug-assert-retry ()
				 :report "Retry the assertion."
				 (return-from ,block))
			       (debug-assert-skip ()
				 :report "Accept that the assertion will fail this time and continue without printing anything."
				 (return 'debug-assert-skip))))))
		    (,block)))
		 until ,test-form))
	 (values)))))

(defmacro debug-unreachable ()
  `(debug-assert (not 'reached-here)))


(defmacro defconstant-string (name value &optional documentation)
  `(define-constant ,name ,value
     :test 'string=
     ,@(when documentation `((:documentation ,documentation)))))

(defmacro defconstant-bv  (name value &optional documentation)
  `(define-constant ,name (force-byte-vector ,value)
     :test 'equalp
     ,@(when documentation `((:documentation ,documentation)))))
