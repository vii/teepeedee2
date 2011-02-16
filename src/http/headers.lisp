(in-package #:tpd2.http)

(defun match-int (value)
  (declare (type simple-byte-vector value))
  (match-bind ((len (integer))) value
              len))
(defun match-each-word (value func)
  (declare (type function func))
  (declare (type simple-byte-vector value))
  (match-bind (
                (+ word (or (+ (space)) (last))
                   '(funcall func word)))
              value))

(defprotocol process-headers (con process-header-func)
  (declare (type function process-header-func))
  (let (last-header-name)
    (loop for line = (io 'recvline con)
          until (zerop (length line))
          do (without-call/cc
                 (if-match-bind ((space) value)
                                line
                                (funcall process-header-func last-header-name value)
                                (match-bind
                                    (header-name ":" (* (space)) value)
                                    line
                                  (funcall process-header-func header-name value)
                                  (setf last-header-name header-name)))))
  (values)))
