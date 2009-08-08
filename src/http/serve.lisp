(in-package #:tpd2.http)

(defun http-serve-timeout ()
  60)

(defconstant-bv +http-param-origin+ (force-byte-vector 'http-peer-info!))

(defun match-x-forwarded-for (value)
  (match-bind
   (+ (and (char) (progn host (or (progn "," (* (space))) (last)))))
   value
   host))

(defconstant-bv +header-end+ (concatenate 'simple-byte-vector +newline+ +newline+))

(defprotocol http-serve (con)
  (without-call/cc
      (reset-timeout con (http-serve-timeout)))

  (io 'http-serve-process-headers con (io 'recvline con +header-end+)))

(defprotocol http-serve-process-headers (con headers)
   (let ((request-content-length 0)
	host
	url
	method
	(request-origin (con-peer-info con))
	connection-close)
    (without-call/cc
      (flet ((handle-header (name value)
	       (unless (zerop (length value))
		 (case-match-fold-ascii-case name
					     ("content-length" 
					      (setf request-content-length (match-int value)))
					     ("host"
					      (setf host value))
					     ("connection"
					      (match-bind (  
							   (+ word (or (+ (space)) (last))
							      '(case-match-fold-ascii-case word
								("close" (setf connection-close t))
								("keep-alive" (setf connection-close nil)))))
						  value))
					     ("x-forwarded-for" 
					      (setf request-origin
						    (match-x-forwarded-for value)))))))
       (match-bind (macrolet ((lws () `(or #\Space #\Tab))) 
		     (progn
		       method-tmp (+ (lws))
		       url-tmp 
		       (or (progn (+ (lws)) (:? "HTTP/" (version-major (unsigned-byte :max-len 3) 1) "." (version-minor (unsigned-byte :max-len 3) 0) (* (lws))) +newline+)
			   +newline+)
		       '(setf connection-close (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor))))
			 url url-tmp
			 method method-tmp)
		       (+ 
			header-name ":" (* (lws)) value (or +newline+ (last))
				     '(handle-header header-name value)
				     (* (+ (lws)) extra-value (or +newline+ (last))
					'(handle-header header-name extra-value))
				     )
		       (last)))
	   headers)))
    
    
    (io 'parse-and-dispatch con url 
	:request-body 
	(unless (zerop request-content-length)
	  (io 'recv con request-content-length)) 
	:host host 
	:origin request-origin)
    (cond 
      (connection-close	 

       ;;; In the case where the client did not legitimately expect a
       ;;; connexion close, they could pipeline more requests. Closing the
       ;;; socket might cause them to get ECONNRESET, which
       ;;; could make it unclear how many requests were really
       ;;; proccessed.

       ;;; The solution is to shutdown the write half of the socket,
       ;;; drain the read half of the socket and then close it.

       ;;; (io 'recv-discard-and-close con)

       ;;; However, this is slow, and as we never close the socket
       ;;; unless the client actually itself requested the socket to
       ;;; be closed, we can safely hangup.
       (hangup con)
       )
      (t (io 'http-serve con)))))

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

(defun http-serve-wait-timeout ()
  60)

(defun http-start-server (port)
  (let ((socket (tpd2.io:make-con-listen :port port)))
    (tpd2.io:socket-only-accept-if-data-ready socket (http-serve-wait-timeout))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http:http-serve)
    socket))
