(in-package #:tpd2.webapp)

(declaim (inline webapp-default-page-footer webapp-default-page-head-contents))

(defun webapp-default-page-footer ()
  (with-ml-output
    (output-raw-ml
     (js-library-footer))))

(defun webapp-default-page-head-contents ()
  (with-ml-output
    (output-raw-ml (js-library))
    (js-html-script
      (setf *channels-url* 
	    (+ (unquote +channel-page-name+) 
	       (unquote-splice
		(when (webapp-frame-available-p)
		  (list 
		   (strcat "?" +webapp-frame-id-param+ "=")
		   (force-string (frame-id (webapp-frame)))))))))))

(defmacro ml-to-byte-vector (ml)
  `(sendbuf-to-byte-vector (with-ml-output-start ,ml)))

(defmacro webapp-ml (title-and-options &body body)
  (with-unique-names (title-ml)
    (destructuring-bind (title &key head-contents)
	(typecase title-and-options
	  (null (list nil))
	  (list title-and-options)
	  (t (list title-and-options)))
      `(let ((,title-ml
	      (ml-to-byte-vector ,title)))
	 (when (webapp-frame-available-p)
	   (setf (webapp-frame-var 'actions) nil))
	 (values
	  (with-frame-site
	      (with-ml-output-start 
		  (output-raw-ml "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"" 
				 " \"http://www.w3.org/TR/html4/loose.dtd\">")
		(<html
		 (<head
		  (current-site-call page-head ,title-ml)
		  ,head-contents)
		 (<body
		  (current-site-call page-body-start ,title-ml)
		  ,@body
		  (current-site-call page-body-footer ,title-ml)))))
	  +http-header-html-content-type+)))))

(defmacro webapp (title-and-options &body body)
  (with-unique-names (l)
    `(labels ((,l ()
		(when (webapp-frame-available-p)
		  (setf (frame-current-page (webapp-frame)) 
			#',l))
		(webapp-ml ,title-and-options ,@body)))
	   (,l))))

(defmacro link-to-webapp (title &body body)
  (with-unique-names (title-ml)
    `(let ((,title-ml (ml-to-byte-vector ,title)))
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
				     `(<li (html-action-link (,display ,i) (,action ,i))))
				    (replace
				     `(<li (html-replace-link (,display ,i) (,replace ,i))))
				    (t (error "Please specify an action or a replacement")))))))))

(defmacro webapp-display (object)
  `(output-object-to-ml ,object))
