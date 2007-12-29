(in-package #:tpd2.lib)

(eval-always
  (defun make-byte-vector (len)
    (cffi-sys::make-shareable-byte-vector len)))

(deftype byte-vector (&optional (len '*))
  `(vector (unsigned-byte 8) ,len))

#+sbcl
(defun-consistent byte-vector-to-string (vec)
  (babel:octets-to-string vec :encoding :utf-8 :errorp nil))

(defun-consistent force-string (val)
  (declare (optimize speed))
  (the string
    (typecase val
      (null "")
      (symbol (symbol-name val))
      (byte-vector (byte-vector-to-string val))
      (string val)
      (t  (let ((*print-pretty* nil)) (princ-to-string val))))))

(declaim (ftype (function (t) string) force-string-consistent-internal))

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