(in-package #:tpd2.ml)

(defstruct (raw-ml-sendbuf (:include sendbuf)))

(defun-consistent escape-data (value)
  (typecase value
    (raw-ml-sendbuf
     value)
    (t
     (macrolet ((f (x) `(force-simple-byte-vector ,x)))
       (match-replace-all (f value)
			  (#\< (f "&lt;"))
			  (#\> (f "&gt;"))
			  (#\& (f "&amp;"))
			(#\' (f "&#39;")) ; &apos; is *not* HTML but only XML
			)))))

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

(defun-consistent escape-attribute-value (value)
  (escape-data value))

(defun with-ml-output-form-to-list (form)
  (typecase form
    (null nil)
    (list
     (case (first form) 
       (with-ml-output (mapcan 'with-ml-output-form-to-list (rest form)))
       (output-raw-ml (copy-list (rest form)))
       (without-ml-output (list form))
       (t (list `(escape-data ,form)))))
    (t (list `(escape-data ,form)))))

(defmacro with-ml-output-start (&body body)
  `(macrolet	      
       ((with-ml-output (&body body)
			`(with-sendbuf-continue (ml-sendbuf)
			   ,@(mapcan 'with-ml-output-form-to-list body))))
     (let ((ml-sendbuf (make-raw-ml-sendbuf)))
       (with-ml-output ,@body)
       ml-sendbuf)))

(defmacro with-ml-output (&body body)
  `(with-ml-output-start ,@body))

(defmacro with-ml-to-string (&body body)
  `(force-string (with-ml-output-start ,@body)))

