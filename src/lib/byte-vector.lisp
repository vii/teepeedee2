(in-package #:tpd2.lib)

(eval-always
  (defun make-byte-vector (len)
    (declare (optimize speed))
    (declare (type (unsigned-byte *) len))
    (make-array len :element-type '(unsigned-byte 8))))

(declaim (inline make-byte-vector))

(deftype byte-vector (&optional (len '*))
  `(vector (unsigned-byte 8) ,len))
(deftype simple-byte-vector (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))

(declaim (ftype (function ((unsigned-byte *)) simple-byte-vector) make-byte-vector)) 

(defmacro with-pointer-to-vector-data ((ptr lisp-vector) &body body)
  (check-symbols ptr)
  (once-only (lisp-vector)
    (with-unique-names (tmp real-vector offset cffi-ptr)
      `(let ((,tmp))
	 (multiple-value-bind
	       (,real-vector ,offset)
	     (array-displacement ,lisp-vector)
	   (when ,real-vector
	     (setf ,lisp-vector ,real-vector))
	   (cffi:with-pointer-to-vector-data (,cffi-ptr ,lisp-vector)
	     (let ((,ptr (cffi:inc-pointer ,cffi-ptr ,offset)))
	       (setf ,tmp (multiple-value-list (locally ,@body)))))
	   (values-list ,tmp))))))

(defun concatenate-simple-byte-vectors (args)
  (declare (optimize speed (safety 0)))
  (let ((len 0))
    (declare (type fixnum len))
    (loop for x in args do 
	  (incf len (length (the simple-byte-vector x))))
    (let ((ret (make-byte-vector len)) (i 0))
      (declare (type fixnum i))
      (loop for x in args do 
	    (loop for c across (the simple-byte-vector x) do
		  (setf (aref ret i) c)
		  (incf i)))
      ret)))
(declaim (inline concatenate-simple-byte-vectors))


(defconstant +byte-to-digit-table+
  (make-array 256 :element-type '(integer -1 36) 
	      :initial-contents (loop for i from 0 below 256 
				      collect 
				      (labels ((c (x) (char-code x))
					     (in-range (a b x offset)
					       (let ((l (min (c a) (c b)))
						     (m (max (c a) (c b))))
						 (when 
						     (and (>= x l)
							  (>= m x))
						   (+ (- x l) offset)))))
					(or (in-range #\a #\z i 10)
					    (in-range #\A #\Z i 10)
					    (in-range #\0 #\9 i 0)
					    -1)))))
(defun-consistent byte-to-digit (byte)
  (declare (type (unsigned-byte 8) byte))
  (aref +byte-to-digit-table+ byte))

(declaim (ftype (function ( (unsigned-byte 8)) (integer -1 36)) byte-to-digit-consistent-internal))


(defun byte-vector-parse-integer (string &optional (base 10))
  (declare (optimize speed))
  (declare (type byte-vector string))
  (let ((i 0) (val 0) (sign 1))
    (flet ((cur ()
	     (aref string i))
	   (eat ()
	     (incf i)))
      (declare (ftype (function () (unsigned-byte 8)) cur))
      (when (= (char-code #\-) (cur))
	(setf sign -1)
	(eat))
      (loop while (> (length string) i) do
	    (setf val (+ (byte-to-digit (cur)) (* val base)))
	    (eat))
      (* sign val))))


(defun byte-to-ascii-upper (x)
  (declare (optimize speed (safety 0)))
  (declare (type (unsigned-byte 8) x))
  (if (and (>= x (char-code #\a)) (<= x (char-code #\z)))
      (+ (- (char-code #\A) (char-code #\a)) x)
      x))
(declaim (inline byte-to-ascii-upper))
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) byte-to-ascii-upper))

(defun eql-fold-ascii-case (a b)
  (declare (optimize speed (safety 0)))
  (= (byte-to-ascii-upper a) (byte-to-ascii-upper b)))
(declaim (inline eql-fold-ascii-case))

(defun byte-vector=-fold-ascii-case (a b)
  (declare (optimize speed (safety 0)))
  (declare (type byte-vector a b))
  (and (= (length a) (length b))
       (loop for i from 0 below (length a)
	     always (eql-fold-ascii-case (aref a i) (aref b i)))))
(declaim (inline byte-vector=-fold-ascii-case))
