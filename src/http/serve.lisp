(in-package #:tpd2.http)

(defprotocol http-serve (con)
  (match-bind (method :whitespace url :whitespace?
		      (:? "HTTP/" (version-major :integer 1) "." (version-minor :integer 0) :whitespace?) 
		      :$)
      (io 'recvline con)
    (let ((request-content-length 0)
	  host
	  (connection-close (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor))))))
      (flet ((process-header (name value)
	       (when (length value)
		 (case-match-fold-ascii-case name
		  ("content-length" 
		   (match-bind ((len :integer)) value
		     (setf request-content-length len)))
		  ("host"
		   (setf host value))
		  ("connection"
		   (match-bind (:*
				   '(case-match-fold-ascii-case (:word)
				     ("close" (setf connection-close t))
				     ("keep-alive" (setf connection-close nil))) 
				   :whitespace?)
		       value))))))
	(let ((last-header-name))
	  (loop for line = (io 'recvline con)
		until (zerop (length line))
		do (if-match ('(:s) line)
			     (process-header last-header-name line)
			     (match-bind 
				 ((header-name (:until-and-eat :whitespace? ":" :whitespace?))
				  (value (:until :whitespace? :$)))
				 line
			       (process-header header-name value)
			       (setf last-header-name header-name))))))
      
      (let ((request-body
	     (unless (zerop request-content-length)
	       (io 'recv con request-content-length))))
	(io 'http-serve-response con url host request-body)
	(if connection-close
	    (hangup con)
	    (io 'http-serve con))))))

(defprotocol http-serve-response (con url host request-body)
  (let (params)
    (flet ((parse-params (str)
	     (match-bind (:* (name (:until-and-eat "=")) (value (:until-and-eat (:or :$ "&")))
			     '(push (cons (url-encoding-decode name) (url-encoding-decode value)) params))
		 str)))
      (match-bind ((path (:until-and-eat (:or :$ "?"))) (q (:rest)))
	  url
	  (parse-params q)
	  (parse-params request-body)
	  (io 'send con 
	      (build-http-response host path params))))))


(defun build-http-response (host path params)
  (let ((body (with-sendbuf
		"<h1>Thanks for your interest</h1>" +newline+
		"<p>You asked for: " host path " with " 
		'(loop for (name . value) in params do
		       (sendbuf-merge sendbuf (with-sendbuf
						name "=" value " ")))
		"</p>" +newline+)))
    (with-sendbuf
      "HTTP/1.1 200 OK" +newline+
      "Content-Length: " (force-string (sendbuf-len body)) +newline+
      "Connection: close" +newline+
      +newline+
      (sendbuf-merge body))))

