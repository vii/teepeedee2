(in-package #:tpd2.http)

(defun http-serve-timeout ()
  60)

(defun http-serve-wait-timeout ()
  120)

(defprotocol http-serve (con)
  (reset-timeout con (http-serve-wait-timeout))
  (match-bind (method (+ (space)) url (or (last) (+ (space)))
		      (:? "HTTP/" (version-major (integer) 1) "." (version-minor (integer) 0)))
      (io 'recvline con)
    (reset-timeout con (http-serve-timeout))
    (let ((request-content-length 0)
	  host
	  (connection-close (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor))))))
      (flet ((process-header (name value)
	       (when (length value)
		 (case-match-fold-ascii-case name
		  ("content-length" 
		   (match-bind ((len (integer))) value
		     (setf request-content-length len)))
		  ("host"
		   (setf host value))
		  ("connection"
		   (match-bind ( (+ word (or (+ (space)) (last))
				    '(case-match-fold-ascii-case word
				      ("close" (setf connection-close t))
				      ("keep-alive" (setf connection-close nil))) ))
		       value))))))
	(io 'process-headers con #'process-header))
      (let ((request-body
	     (unless (zerop request-content-length)
	       (io 'recv con request-content-length))))
	(io 'parse-and-dispatch con url :request-body request-body :host host))
      (if connection-close
	  (hangup con)
	  (io 'http-serve con)))))

(defprotocol parse-and-dispatch (con path-and-args &key request-body host)
  (let (params tmp)
    (without-call/cc
      (flet ((parse-params (str)
	       (when str
		 (match-bind ( (*  name "=" value (or (last) "&")
				   '(push (cons (url-encoding-decode name) (url-encoding-decode value)) params)))
		     str))
	       (values)))
	(match-bind (path (or (last) (progn "?" q)))
	    path-and-args
	  (parse-params q)
	  (parse-params request-body)
	  (setf tmp path))))
    (io 'dispatch con tmp :params params :host host)))

