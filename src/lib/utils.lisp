(in-package #:tpd2.lib)

(eval-always
  (defun make-byte-vector (len)
    (make-array len :element-type '(unsigned-byte 8))))

(declaim (inline make-byte-vector))

(deftype byte-vector (&optional (len '*))
  `(vector (unsigned-byte 8) ,len))
(deftype simple-byte-vector (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))

#+sbcl
(defun-consistent byte-vector-to-string (vec)
  (babel:octets-to-string vec :encoding :utf-8 :errorp nil))

(defun-consistent force-string (val)
  (declare (optimize speed))
  (let ((str
	 (the string
	   (typecase val
	     (null "")
	     (symbol (symbol-name val))
	     (byte-vector (byte-vector-to-string val))
	     (string val)
	     (t  (let ((*print-pretty* nil)) (princ-to-string val)))))))
    (etypecase str
      (simple-string str)
      (string (replace (make-string (length str)) (the (and string (not simple-string)) str))))))

(declaim (ftype (function (t) simple-string) force-string-consistent-internal))

(defun random-shuffle (sequence)
  (awhen (not (zerop (length sequence)))
    (let ((i (random (length sequence))))
      (cons (elt sequence i) (random-shuffle (remove-if (lambda(x) (declare (ignore x)) t) sequence :start i :count 1))))))
(defun random-elt (sequence)
  (when sequence
    (elt sequence (random (length sequence)))))
(defun read-safely (&rest args)
  (let ((*read-eval* nil))
    (apply 'read args)))
(defun read-safely-from-string (string)
  (with-input-from-string (*standard-input* (force-string string)) (read-safely)))
