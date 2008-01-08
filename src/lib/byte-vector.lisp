(in-package #:tpd2.lib)

(def-if-unbound defun-consistent utf8-encode (str) ; XXX not implemented
  (assert (every (lambda(x) (> 128 (char-int x))) str))
  (map '(vector (unsigned-byte 8)) 'char-code str))

(defun-consistent utf8-encode (str) ; XXX not implemented
  (declare (type string str))
  (declare (optimize speed))
  (block encode
    (let ((vec (make-byte-vector (length str))))
      (loop for i fixnum from 0 for s across str do
	    (let ((c (char-code s)))
	      #+sbcl (when (> c 127)
		       (return-from encode (babel:string-to-octets str :encoding :utf-8)))
	      (setf (aref vec i) (char-code s))))
	    vec)))


(def-if-unbound defun-consistent byte-vector-to-string (vec)
  (map 'string 'code-char vec))

(defun-consistent force-byte-vector (val)
  (declare (optimize speed (safety 0)))
  (typecase val
    (null #.(make-byte-vector 0))
    (string (utf8-encode val))
    (character (utf8-encode (string val)))
    (byte-vector val)
    (sequence (map 'byte-vector 'identity val))
    (t (utf8-encode (force-string val)))))

(declaim (ftype (function (t) byte-vector) force-byte-vector-consistent-internal))

(defmacro with-pointer-to-vector-data ((ptr lisp-vector) &body body)
  (check-symbols ptr)
  (once-only (lisp-vector)
    (with-unique-names (tmp real-vector offset)
      `(let ((,tmp))
	 (multiple-value-bind
	       (,real-vector ,offset)
	     (array-displacement ,lisp-vector)
	   
	   (when ,real-vector
	     (setf ,lisp-vector ,real-vector))
	   (cffi:with-pointer-to-vector-data (,ptr ,lisp-vector)
	     (cffi:incf-pointer ,ptr ,offset)
	     (setf ,tmp (locally ,@body)))
	   ,tmp)))))

(defun byte-vector-cat (&rest args)
  (declare (optimize speed))
  (let ((vecs (mapcar (lambda(x)(force-byte-vector x)) args)))
    (let ((len (reduce '+ (mapcar 'length vecs))))
      (let ((ret (make-byte-vector len)) (i 0))
	(loop for v in vecs do
	      (replace ret v :start1 i)
	      (incf i (length v)))
	ret))))
(declaim (inline byte-vector-cat))

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
  (if (and (>= x (char-code #\a)) (<= x (char-code #\z)))
      (+ (- (char-code #\A) (char-code #\a)) x)
      x))
