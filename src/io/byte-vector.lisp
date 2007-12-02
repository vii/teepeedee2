(in-package #:tpd2.io)

(eval-always
  (defun make-byte-vector (len)
    (cffi-sys::make-shareable-byte-vector len)))

(deftype byte-vector (&optional (len '*))
  `(vector (unsigned-byte 8) ,len))

#+sbcl
(defun-consistent utf8-encode (str)
  (babel:string-to-octets str :encoding :utf-8))

(def-if-unbound defun-consistent utf8-encode (str) ; XXX not implemented
  (assert (every (lambda(x) (> 128 (char-int x))) str))
  (map '(vector (unsigned-byte 8)) #'char-code str))

(defun-consistent force-byte-vector (val)
  (typecase val
    (null #.(make-byte-vector 0))
    (string (utf8-encode val))
    (integer (utf8-encode (string (code-char val))))
    (character (utf8-encode (string val)))
    (sequence (map 'byte-vector 'identity val))
    (t val)))

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
	     (when ,offset
	       (cffi:incf-pointer ,ptr ,offset))
	     (setf ,tmp (locally ,@body)))
	   ,tmp)))))