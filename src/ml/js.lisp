(in-package #:tpd2.ml)

(defun-consistent js-to-string (form)
  (ps:ps* form))

(defmacro js-html-script (&body body)
  `(tpd2.ml.html:<script :type "text/javascript" 			  
			 (output-ml-comment
			  #\Newline
			  (js-to-string (superquote (progn ,@body)))
			  "//"
			  )))

(defmacro js-attrib (&body body)
  `(sendbuf-to-byte-vector 
    (with-sendbuf ()
      "javascript:"
      (js-to-string (superquote (progn ,@body))))))

