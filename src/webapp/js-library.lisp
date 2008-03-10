(in-package #:tpd2.webapp)

(ps:defpsmacro debug-log (&rest args)
  (declare (ignorable args))
  #+use-ps-console.log `(console.log ,@args))

(ps:defpsmacro ignore-errors (&body body)
  `(ps:try (progn 
	  ,@body) 
	(:catch (e) 		    
	  (debug-log "IGNORING ERROR" e))))

(defun js-library ()
  (js-html-script
    (defun find-element (element-id)
      (return (document.get-element-by-id element-id)))

    (defun handle-special-elements (parent)
      (dolist (element (parent.get-elements-by-tag-name "a"))
	(when (== element.class-name (unquote +action-link-class+))
	  (setf element.href (+ "javascript:asyncSubmitLinkHref(\'" element.href "\')"))))
      (dolist (element (parent.get-elements-by-tag-name "div"))
	(when (== element.class-name (unquote +html-class-scroll-to-bottom+))
	  (setf element.scroll-top element.scroll-height))))

    (defun reset-element (element content)
      (setf element.inner-h-t-m-l content)
      (handle-special-elements element)
      (dolist (script (element.get-elements-by-tag-name "script"))
	(eval script.inner-h-t-m-l)))

    (defun reset-element-id (element-id content)
      (reset-element (find-element element-id) content))

   (defun append-element-id (element-id content)
     (let ((element (find-element element-id)))
       (+= element.inner-h-t-m-l content)))


    (defun make-xml-http-request ()
      (if (slot-value window '*X-M-L-Http-Request )
	  (return (new *X-M-L-Http-Request))
	  (return (new (*Active-X-Object "Microsoft.XMLHTTP")))))
    (defvar *active-request*)

    (defun async-request-done (req url)
      (debug-log "async request received" req)
      (unless (=== req *active-request*)
	(return))

      (let ((success nil))
	(ignore-errors
	  (setf success (and (eql 200 req.status) req.response-text)))
	(if success
	    (progn
	      (setf *active-request* nil)
	      (ignore-errors
		(eval req.response-text)
		(debug-log "async request completed okay" req)))
	    (progn
	      (debug-log "async request unsuccessful" req)
	      (ps:do-set-timeout (100)
		(when (=== req *active-request*)
		  (async-request url "Retrying")))))))
    
    (defun async-request (url initial-status)
      (ps:try
       (progn
	 (let ((tmp *active-request*))
	   (setf *active-request* nil)
	   (when tmp
	     (ignore-errors
	       (tmp.abort))))
	 
	 
	 (let ((req (make-xml-http-request)))
	   (setf *active-request* req)
	   
	   (setf req.onreadystatechange 
		 (lambda ()
		   (case req.ready-state
		     (4 
		      (async-request-done req url)))))

	   (req.open "GET" url t)
	   (req.send "")))
       (:catch (e)
	 (debug-log "async request was not started" url initial-status e)
	 (return -1)))
      (return 0))

    (defun channels-get-param ()
      (let ((lines (make-array)))
	(ps:doeach (channel *channels*)
		   (lines.push (+ (encode-U-R-I-component channel) "|" (aref *channels* channel))))
	(return (+ ".channels.=" (lines.join ";")))))

    (defun async-submit-form (form)
      (let ((inputs form.elements)
	    (params (make-array)))
	(dolist (input inputs)
	  (when (and input.name input.value)
	    (params.push (+ (encode-U-R-I-component input.name) "=" (encode-U-R-I-component input.value)))))
	(return (async-submit-link (+ form.action
				      (if (!= -1 (form.action.search "\\?")) "&" "?") (params.join "&"))))))

    (defun async-submit-link (link)
      (if (async-request (+ link (if (!= -1 (link.search "\\?")) "&" "?") ".javascript.=t&"
			    (channels-get-param)) "sending")
	  (return true) ; error occurred so actually submit the form normally
	  (return false)))
    (defun async-submit-link-href (link)
      (when (async-submit-link link)
	(setf window.location link)))

    (defvar *channels*)
    (unless *channels* (setf *channels* (ps:new *object)))
   
   (defun channel (name counter)
     (setf (aref *channels* name) (max (if (aref *channels* name) (aref *channels* name) 0) counter)))))

(defun js-library-footer ()
  (js-html-script
    (handle-special-elements document.body)
   
   (defun fetch-channels ()
       (async-request (+ (unquote (force-string (page-link +channel-page-name+))) "&" (channels-get-param))
		      nil))
   (defun trigger-fetch-channels ()
     (ps:do-set-timeout (50) 
       (fetch-channels)))
    (trigger-fetch-channels)))

