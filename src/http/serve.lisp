(in-package #:tpd2.http)

(defun http-serve-timeout ()
  60)

(defun http-serve-wait-timeout ()
  120)

(defconstant-bv +http-param-origin+ (force-byte-vector 'http-peer-info!))

(defun match-x-forwarded-for (value)
  (match-bind
   (+ (and (char) (progn host (or (progn "," (* (space))) (last)))))
   value
   host))

(defprotocol http-serve (con)
  (without-call/cc
      (reset-timeout con (http-serve-wait-timeout)))
  (match-bind (method (+ (space)) url (or (last) (+ (space)))
		      (:? "HTTP/" (version-major (unsigned-byte :max-len 3) 1) "." (version-minor (unsigned-byte :max-len 3) 0)))
      (io 'recvline con)
      (without-call/cc
	  (reset-timeout con (http-serve-timeout)))
      (let ((request-content-length 0)
	    host
	    (request-origin (con-peer-info con))
	    (connection-close       
	     (without-call/cc (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor)))))))
	(io 'process-headers con 
	    (without-call/cc 
		(lambda(name value)
		  (unless (zerop (length value))
		    (case-match-fold-ascii-case name
						("content-length" 
						 (setf request-content-length (match-int value)))
						("host"
						 (setf host value))
						("connection"
						 (match-each-word value
								  (lambda(word)
								    (case-match-fold-ascii-case word
												("close" (setf connection-close t))
												("keep-alive" (setf connection-close nil))) )))
						("x-forwarded-for" 
						 (setf request-origin
						       (match-x-forwarded-for value))))))))
      (let ((request-body
	     (unless (zerop request-content-length)
	       (io 'recv con request-content-length))))
	(io 'parse-and-dispatch con url :request-body request-body :host host :origin request-origin))
      (cond 
	(connection-close	 
	 (io 'recv-discard-and-close con))
	(t (io 'http-serve con))))))

(defprotocol parse-and-dispatch (con path-and-args &key request-body host origin)
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
	  (setf tmp path)))
      (push (cons +http-param-origin+ origin) params)) ; makes sure it's first so it can't be overridden by the user
    (io 'dispatch con tmp :params params :host host)))



(defun http-start-server (port)
  (let ((socket (tpd2.io:make-con-listen :port port)))
    (tpd2.io:socket-only-accept-if-data-ready socket)
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http:http-serve)
    socket))
