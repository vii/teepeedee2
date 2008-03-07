(in-package #:tpd2.webapp)

(defun webapp-page-body-start (title)
  (<div :class "header"	
	(output-object-to-ml (webapp-frame))
	(<h1 (output-raw-ml title))))

(defun webapp-page-footer ()
  (with-ml-output
    (output-raw-ml
     (js-library-footer))))

(defun webapp-page-head (title)
  (<head
    (<title (output-raw-ml title))
    (output-raw-ml (webapp-page-head-css)
		   (js-library))))

(defun webapp-page-head-css ())

(defmacro title-once (title)
  `(sendbuf-to-byte-vector (with-ml-output-start ,@(force-list title))))

(defmacro webapp-ml (title &body body)
  (with-unique-names (title-ml)
    `(let ((,title-ml
	    (title-once ,title)))
       (with-ml-output-start 
	 (output-raw-ml "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"" 
			" \"http://www.w3.org/TR/html4/loose.dtd\">")
	 (<html
	   (output-raw-ml
	    (webapp-page-head ,title-ml))
	   (<body
	     (output-raw-ml (webapp-page-body-start ,title-ml))
	     ,@body
	     (output-raw-ml (webapp-page-footer))))))))



(defmacro webapp-lambda (title &body body)
  (with-unique-names (l)
  `(let ((,l))
     (setf ,l (lambda()
		(setf (frame-current-page (webapp-frame)) ,l)
		(webapp-ml ,title ,@body)))
     ,l)))

(defmacro webapp (title &body body)
  `(funcall (webapp-lambda ,title ,@body)))

(defmacro link-to-webapp (title &body body)
  (with-unique-names (title-ml)
    `(let ((,title-ml (title-once ,title)))
       (html-replace-link (output-raw-ml ,title-ml) 
	 (webapp ((output-raw-ml ,title-ml)) ,@body)))))

(defmacro webapp-section (title &body body)
  `(<div :class "webapp-section"
	 (<h3 ,@(force-list title))
	 ,@body))

(defmacro webapp-select-one (title list-generation-form &key action replace display)
  (with-unique-names (i)
    `(webapp-section ,title
		     (<ul
		       (loop for ,i in ,list-generation-form
			     do (let-current-values (,i)
				  ,(cond
				    (action
				     `(<li (html-action-link (funcall ,display ,i) (funcall ,action ,i))))
				    (replace
				     `(<li (html-replace-link (funcall ,display ,i) (funcall ,replace ,i))))
				    (t (error "Please specify an action or a replacement")))))))))


(defmacro webapp-display (object)
  `(output-object-to-ml ,object))
