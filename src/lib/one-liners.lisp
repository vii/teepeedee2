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



(declaim (inline cdr-assoc))
(defun cdr-assoc (alist key &key (test 'eq))
  (cdr (assoc key alist :test test)))


(define-setf-expander cdr-assoc (place key &key (test ''eq) &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (with-unique-names (store key-val test-val)
      (values 
       (append dummies (list key-val test-val))
       (append vals (list key test))
       `(,store ,@newval)
       (with-unique-names (tmp cur)
	 `(let (,@(loop for d in dummies 
			for v in vals
			collect `(,d ,v))
		(,cur ,getter))
	    (let ((,tmp (assoc ,key-val ,cur :test ,test-val)))
	      (cond (,tmp 
		     (rplacd ,tmp ,store))
		    (t
		     (setf ,(first newval) (acons ,key ,store ,cur))
		     ,setter))
	      ,store)))
       `(cdr-assoc ,getter)))))

(defmacro without-call/cc (&body body)
  `(locally ,@body))

(defmacro debug-assert (&rest args)
  `(progn
     ,(first args)
     (values))

  #+tpd2-debug-assert
  (with-unique-names (block)
    `(without-call/cc
       (block ,block
	 (restart-case (assert ,@args)
	   (debug-assert-skip ()
	     :report "Accept that the assertion will fail this time and continue"
	     (return-from ,block 'debug-assert-skip)))
	 (values)))))

(defmacro debug-unreachable ()
  `(debug-assert (not 'reached-here)))


