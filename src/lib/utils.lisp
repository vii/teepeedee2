(in-package #:tpd2.lib)

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
