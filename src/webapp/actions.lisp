(in-package #:tpd2.webapp)

(defconstant +action-page-name+ "/*action*")

(defconstant +action-form-class+ "*action-form*")
(defconstant +action-link-class+ "*action-link*")

(defconstant +html-id-async-status+ "*async-status*")

(defun register-action-id (function)
  (let ((id (random-web-sparse-key 10)))
    (push (cons id function) (webapp-session-var 'actions))
    id))

(defmacro html-action-link (text &body body)
  (with-unique-names (params)
    `(<A :class +action-link-class+ :href (page-link +action-page-name+ 
						     :id (register-action-id (lambda(,params)(declare (ignore ,params)) ,@body))) ,text)))

(defmacro html-action-form (title lambda-list &body body)
  `(<form :onsubmit (js-attrib (return (async-submit-form this))) :method :post :action 
	  (page-link +action-page-name+ :id 
		     (register-action-id 
		      (lambda(params)
			(let ,(loop for p in lambda-list collect
				    `(,(force-first p) (or (cdr-assoc params ,(force-byte-vector (force-first p)) 
								      :test 'byte-vector=-fold-ascii-case)
							   ,(second (force-list p)))))
			  ,@body))))
	  (<p
	    ,title
	    ,@(loop for nv in lambda-list collect
		    (destructuring-bind (name &optional value)
			(force-list nv)
		      `(<input :type :text :name ,(force-byte-vector name) 
			       ,@(when value `(:value ,value)))))
	    (<input :type :submit :value "Send"))))

(defun run-action (id all-http-params)
  (awhen (and id (assoc id (webapp-session-var 'actions) :test #'equalp))
    (prog1 (with-ml-output (output-raw-ml (funcall (cdr it) all-http-params)))
      (deletef it (webapp-session-var 'actions)))))

(defmacro defactionpage ((ml-var) &body body)
  (check-symbols ml-var)
  `(defpage ,+action-page-name+ (id all-http-params)
     (let ((,ml-var
	    (run-action id all-http-params)))
       ,@body)))


(defun action-script-helper ()
  (js-html-script 
    (defun find-element (element-id)
      (return (document.get-element-by-id element-id)))

    (defun reset-element (element content)
      (setf element.inner-h-t-m-l content)
      (dolist (script (element.get-elements-by-tag-name "script"))
	(eval script.inner-h-t-m-l)))

    (defun reset-element-id (element-id content)
      (reset-element (find-element element-id)))

    (defun set-status-from-ready-state (ready-state)
      (case ready-state
	(2 (set-status "waiting"))
	(3 (set-status "loading"))
	(4 (set-status nil))))

    (defun set-status (status)
      (return)
      (reset-element-id (unquote +html-id-async-status+)
		    (if status (+ ". . . " status) "")))
    (defun set-status-failed (initial-status)
      (set-status #.(force-string (<span :class "error" "failed . . . retrying")))
      (let ((msg (+ initial-status " failed")))
	(setf (aref msg 0) (.to-upper-case (aref msg 0) ))
	;(error-message msg)
	))

    (defun async-request (url initial-status)
      (ps:try
       (let ((req  (make-xml-http-request)))
	 (set-status initial-status)

	 (setf req.onreadystatechange 
	       (lambda ()
		 (set-status-from-ready-state req.ready-state)
		 (case req.ready-state
		   (4 
		    (let ((success nil))
		      (ignore-errors
			(setf success (and (eql 200 req.status) req.response-text)))
		      (if success
			  (reset-element document.body req.response-text)
			  (ps:do-set-timeout (1000)
			    (async-request url initial-status))))))))
	 (req.open "GET" url t)
	 (req.send ""))
       (:catch (e)
	 (set-status-failed initial-status e)
	 (return -1)))
      (return 0))

    (defun async-submit-form (form)
      (let ((inputs form.elements)
	    (params (make-array)))
	(params.push "javascript=true")
	(dolist (input inputs)
	  (when (and input.name input.value)
	    (params.push (+ (encode-U-R-I-component input.name) "=" (encode-U-R-I-component input.value)))))
	(return (async-submit-link (+ form.action
				      (if (!= -1 (form.action.search "\\?")) "&" "?") (params.join "&"))))))

    (defun async-submit-link (link)
      (if (async-request link "sending")
	  (return true) ; error occurred so actually submit the form normally
	  (return false)))
    (defun async-submit-link-href (link)
      (when (async-submit-link link)
	(setf window.location link)))

    (dolist (element (document.get-elements-by-tag-name "a"))
      (when (== element.class-name (unquote +action-link-class+))
	(setf element.href (+ "javascript:asyncSubmitLinkHref(\'" element.href "\')"))))
    (dolist (element (document.get-elements-by-tag-name "div"))
      (when (== element.class-name "scroll-to-bottom")
	(setf element.scroll-top element.scroll-height)))))

