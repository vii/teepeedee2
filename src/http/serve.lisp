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
	(io 'process-headers con #'process-header))
      
      (let ((request-body
	     (unless (zerop request-content-length)
	       (io 'recv con request-content-length))))
	(io 'parse-and-dispatch con url :request-body request-body :host host))
      (if connection-close
	  (hangup con)
	  (io 'http-serve con)))))

(defun test-http-request (url &optional request-body)
  (match-bind ((:? (protocol (:until-and-eat "://")) (hostname (:until "/"))) path-and-args)
      url
    (let ((*break-on-signals* t))
      (with-output-to-string (stream)
	(multiple-value-bind (path params)
	    (http-parse path-and-args :request-body request-body)
	  (dispatch (tpd2.io::make-con :socket stream) (constantly nil) path :host hostname :params params))))))

(defun parse-and-dispatch (con done path-and-args &key request-body host)
  (let (params)
    (flet ((parse-params (str)
	     (when str
	       (match-bind (:* (name (:until-and-eat "=")) (value (:until-and-eat (:or :$ "&")))
			       '(push (cons (url-encoding-decode name) (url-encoding-decode value)) params))
		   str))))
      (match-bind ((path (:until-and-eat (:or :$ ("?" (q (:rest)))))))
	  path-and-args
	(parse-params q)
	(parse-params request-body)
	(dispatch con done path :params params :host host)))))

