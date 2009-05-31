(in-package #:tpd2.webapp)

(defstruct action
  (id (random-web-sparse-key 10))
  (func nil))

(defun register-action-id (function)
  (let ((action (make-action :func function)))
    (push action
	  (webapp-frame-var 'actions))
    (action-id action)))

(defmacro page-action-lambda (&body body)
  `(lambda(all-http-params!)
     (declare (ignorable all-http-params!))
     ,@body))

(defmacro page-action-link (&body body)
  `(page-link +action-page-name+ :.id. (register-action-id (page-action-lambda ,@body))))

(defmacro html-replace-link (text &body body)
  `(<A :class +replace-link-class+ 
       :href (page-action-link (setf (frame-current-page (webapp-frame)) (lambda() ,@body))) ,text))

(defmacro html-action-link (text &body body)
  `(<A :class +action-link-class+ 
       :href (page-action-link ,@body) ,text))

(defmacro html-collapser (toggle &body body)
  `(with-ml-output 
       (<div :onclick (js-attrib (toggle-hiding this.next-sibling))
	     ,toggle)
       (<div :class +html-class-collapsed+
	     ,@body)))

(defmacro html-action-form-collapsed (title lambda-list &body body)
  `(html-collapser (<p ,(force-first title))
		   (html-action-form (nil ,@(force-rest title) :after-submit-js ((toggle-hiding this.parent-node))) ,lambda-list ,@body)))

(defmacro html-action-form (title-and-options lambda-list &body body)
  (destructuring-bind (title 
		       &key (action-link      
			     `(page-action-link 
			       (let ,(loop for p in lambda-list collect
					   `(,(force-first p) (or (alist-get all-http-params! ,(force-byte-vector (force-first p)) 
									     :test 'byte-vector=-fold-ascii-case)
								  ,(second (force-list p)))))
				 ,@body)))
		       (after-submit-js))
      (force-list title-and-options)
    (let ((body-ml
	   (loop for nv in lambda-list collect
		 (destructuring-bind (name &optional value &key (type '<input) reset)
		     (force-list nv)
		    (let ((name (force-byte-vector name)))
		      (when reset
			(appendf after-submit-js `((setf (slot-value (this.elements.named-item ,(force-string name)) 'value) ,(if (eq reset t) nil reset)))))
		      (ecase type
			(<input
			 `(<input :type :text :name ,name
				  ,@(when value `(:value ,value))))
			(<textarea
			 `(<textarea :name ,name ,value))
			(:hidden
			 `(<input :type :text :name ,name :value ,value :style (css-attrib :display "none")))))))))
       `(<form 
	 :onsubmit (js-attrib (return (let ((async-submit-success (async-submit-form this))) ,@after-submit-js async-submit-success)))
	 :method :post 
	 :action ,action-link     
	 (<p
	  ,title
	  ,@body-ml
	  (<input :class "plain-submit" :type :submit :value "↵"))))))

(defun find-action (id)
  (and id (find id (webapp-frame-var 'actions) :key 'action-id :test 'equalp)))

(defun action-respond-body (&key .id. .javascript. all-http-params!)
  (with-frame-site 
    (awhen (find-action .id.)
	   (funcall (action-func it) all-http-params!))
    (if .javascript.
	(webapp-respond-ajax all-http-params!)
	(funcall (frame-current-page (webapp-frame))))))

(defun webapp-respond-ajax-body (all-http-params!)
  (let ((channels (channel-string-to-states 
		   (alist-get all-http-params! (force-byte-vector '.channels.) 
			      :test 'byte-vector=-fold-ascii-case))))
   (channel-respond-body channels :always-body t)))
  
(defun register-action-page ()
  (defpage-lambda +action-page-name+ #'action-respond-body :defaulting-lambda-list (.id. .javascript. all-http-params!)))


(my-defun frame 'simple-channel-body-ml ()
  (<div :class "frame"
	(<div :class "change-name" 
	      (html-action-form "Your name " ((new-name (my username)))
		(my change-username new-name)
		(values)))
	(output-object-to-ml (my messages))))

