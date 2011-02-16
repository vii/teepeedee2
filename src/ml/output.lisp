(in-package #:tpd2.ml)

(defmystruct (raw-ml-sendbuf (:include sendbuf)))

(declaim (ftype (function (t) simple-byte-vector) really-escape-string))
(defun really-escape-string (value)
  (let ((value (force-simple-byte-vector value)))
    (declare (optimize speed))
    (match-replace-all value
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (#\' "&#39;"))))             ; &apos; is *not* HTML but only XML

(declaim (ftype (function (t) (or raw-ml-sendbuf simple-byte-vector)) escape-data-consistent-internal ml-output-form-consistent-internal))
(defun-consistent escape-data (value)
  (typecase value
    (nil #.(force-byte-vector nil))
    (raw-ml-sendbuf
     value)
    (t
     (values (really-escape-string value)))))

(defun-consistent ml-output-form (value)
  (typecase value
    (null #.(force-byte-vector nil))
    (raw-ml-sendbuf
     value)
    ((or standard-object structure-object list)
     (object-to-ml value))
    (t
     (values (really-escape-string value)))))

(defmacro output-escaped-ml (&rest args)
  `(with-ml-output
       ,@args))

(defmacro output-raw-ml (&rest body)
  `(with-ml-output
     (output-raw-ml
       ,@body)))

(defmacro output-ml-comment (&rest body)
  `(with-ml-output
     (output-raw-ml "<!--" ,@body "-->")))

(defmacro without-ml-output (&body body)
  `(locally ,@body (values)))

(defmacro escape-attribute-value (value)
  `(escape-data ,value))

(defun ml-output-form-to-list (form env)
  (labels ((r (form)
             (typecase form
               (null nil)
               (list
                (case (first form)
                  (with-ml-output (loop for x in (rest form) appending (r x)))
                  (output-raw-ml (copy-list (rest form)))
                  ((without-ml-output escape-data) (list form))
                  (t
                    (multiple-value-bind (new changed)
                        (macroexpand-1 form env)
                      (if changed
                          (r new)
                          (list `(ml-output-form ,form)))))))
               (t (list `(ml-output-form ,form))))))
    (r form)))

(defmacro with-ml-output-start (&body body)
  `(macrolet
       ((with-ml-output (&body body &environment env)
                        `(with-sendbuf-continue (ml-sendbuf)
                           ,@(loop for x in body appending (ml-output-form-to-list x env)))))
     (let ((ml-sendbuf (make-raw-ml-sendbuf)))
       (with-ml-output ,@body)
       ml-sendbuf)))

(defmacro with-ml-output (&body body)
  `(with-ml-output-start ,@body))

(defmacro with-ml-to-string (&body body)
  `(force-string (with-ml-output-start ,@body)))

(my-defun raw-ml-sendbuf 'make-load-form (&optional env)
  (declare (ignore env))
  `(with-ml-output-start
     (output-raw-ml ,(my to-byte-vector))))
