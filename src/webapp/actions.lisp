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
  `(lambda(all-http-params)
     (declare (ignorable all-http-params))
     ,@body))

(defmacro page-action-link (&body body)
  `(page-link +action-page-name+ :.id. (register-action-id (page-action-lambda ,@body))))

(defmacro html-replace-link (text &body body)
  `(<A :class +replace-link-class+ 
       :href (page-action-link (setf (frame-current-page (webapp-frame)) (lambda() ,@body))) ,text))

(defmacro html-action-link (text &body body)
  `(<A :class +action-link-class+ 
       :href (page-action-link ,@body) ,text))

(defmacro html-action-form (title lambda-list &body body)
  `(<form 
     :onsubmit (js-attrib (return (async-submit-form this))) 
     :method :post 
     :action 
     (page-action-link 
      (let ,(loop for p in lambda-list collect
		  `(,(force-first p) (or (cdr-assoc all-http-params ,(force-byte-vector (force-first p)) 
						    :test 'byte-vector=-fold-ascii-case)
					 ,(second (force-list p)))))
	  ,@body))
     (<p
       ,title
       ,@(loop for nv in lambda-list collect
	       (destructuring-bind (name &optional value &key (type '<input))
		   (force-list nv)
		 (let ((name (force-byte-vector name)))
		   (ecase type
		     (<input
		       `(<input :type :text :name ,name
				,@(when value `(:value ,value))))
		     (<textarea
		       `(<textarea :name ,name ,value))
		     (:hidden
		      `(<input :type :text :name ,name :value ,value :style (css-attrib :display "none")))))))
       (<input :class "plain-submit" :type :submit :value "↵"))))

(defun find-action (id)
  (and id (find id (webapp-frame-var 'actions) :key 'action-id :test 'equalp)))

(defun action-respond-body (&key .id. .channels. .javascript. all-http-params)
  (awhen (find-action .id.)
    (funcall (action-func it) all-http-params))
  (with-sendbuf
      ()
    (if .javascript.
	(channel-respond-body (channel-string-to-states .channels.))
	(funcall (frame-current-page (webapp-frame))))))

(defun register-action-page ()
  (defpage-lambda +action-page-name+ #'action-respond-body (.id. .channels. .javascript. all-http-params)))


(my-defun frame 'simple-channel-body-ml ()
  (<div :class "frame"
	(<div :class "change-name" 
	      (html-action-form "Your name " ((new-name (my username)))
		(setf (my username) new-name)
		(my notify)
		(values)))
	(output-object-to-ml (my messages))))

