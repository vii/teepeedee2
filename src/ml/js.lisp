(in-package #:tpd2.ml)

(defun-consistent js-to-string-func (form)
  (ps:ps* form))

(defmacro js-to-string (&body body)
  `(js-to-string-func (superquote (progn ,@body))))

(defmacro js-html-script (&body body)
  `(tpd2.ml.html:<script :type "text/javascript" 			  
			 (output-ml-comment
			  #\Newline
			  (js-to-string ,@body)
			  "//"
			  )))

(defmacro js-to-bv (&body body)
  `(force-byte-vector (js-to-string ,@body)))
(defmacro js-html-script-as-bv (&body body)
  `(force-byte-vector 
    (js-html-script ,@body)))

(defmacro js-attrib (&body body)
  `(sendbuf-to-byte-vector 
    (with-sendbuf ()
      "javascript:"
      (js-to-string ,@body))))



