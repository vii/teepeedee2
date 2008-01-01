(in-package #:tpd2.http)

(defun http-serve-timeout ()
  60)

(defprotocol http-serve (con)
  (con-reset-timeout con (http-serve-timeout))
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
	(io 'send con (http-parse-and-generate-response url :host host :request-body request-body))
	(if connection-close
	    (hangup con)
	    (io 'http-serve con))))))

(defun http-parse-and-generate-response (url &key host request-body)
  (let (params)
    (flet ((parse-params (str)
	     (when str
	       (match-bind (:* (name (:until-and-eat "=")) (value (:until-and-eat (:or :$ "&")))
			       '(push (cons (url-encoding-decode name) (url-encoding-decode value)) params))
		   str))))
      (match-bind ((path (:until-and-eat (:or :$ ("?" (q (:rest)))))))
	  url
	(parse-params q)
	(parse-params request-body)
	(generate-http-response host path params)))))

