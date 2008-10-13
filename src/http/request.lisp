(in-package #:tpd2.http)

(defprotocol http-read-chunked (con)
  (let ((body
	 (loop 
	       for len = (match-bind ((len (integer 16)) (* (space)) (last))  
		   (io 'recvline con) 
		 len)
	       until (zerop len)
	       collect (copy-byte-vector (io 'recv con len))
	       do
	       (match-bind ((last)) (io 'recvline con)))))
    
    (loop for line = (io 'recvline con)
	  until (zerop (length line)))

    (without-call/cc (apply-byte-vector-cat body))))

(defvar *connection-cache* (make-hash-table :test #'equalp))

(defprotocol http-request (con request done &key connection-cache)
  (io 'send con request)
  (let ((content-length)
	(chunked)
	(gzip)
	(connection-close))
      (match-bind ("HTTP/" (version-major (integer)) "." (version-minor (integer)) (+ (space)) (code (integer)) (+ (space)) banner)
	  (io 'recvline con)
	(flet ((decode (bytes)
		 (cond (gzip (error "Sorry; haven't implemented GZIP decompression yet"))
		       (t (funcall done bytes :response-code code)))))

	  (when (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor))))
	    (setf connection-close t))

	  (io 'process-headers con 
	      (without-call/cc (lambda(name value)
				 (unless (zerop (length value))
				   (case-match-fold-ascii-case name
							       ("content-length"
								(setf content-length (match-int value)))
							       ("connection"
								(match-each-word value
										 (lambda(word)
										   (case-match-fold-ascii-case word
													       ("close" (setf connection-close t))
													       ("keep-alive" (setf connection-close nil))) )))
							       ("transfer-encoding"
								(match-each-word value
										 (lambda(word)
										   (case-match-fold-ascii-case word
													       ("chunked" (setf chunked t))
													       ("gzip" (setf gzip t)))))))))))
	  (decode
	   (cond 
	     (chunked
	      (io 'http-read-chunked con))
	     (content-length
	      (io 'recv con content-length))
	     (t 
	      (setf connection-close t)
	      (io 'recv-until-close con))))
	  
	  (cond ((or connection-close (not connection-cache))
		 (hangup con))
		(t
		 (add-to-connection-cache con connection-cache)))))))


(defun http-connection-cache-timeout ()
  25)

(defun add-to-connection-cache (con key)
  (con-clear-failure-callbacks con)
  (unless (con-dead? con)
    (con-when-ready-to-read con (lambda() (con-fail con))) 
    (con-add-failure-callback con 
			      (lambda(&rest args)
				(declare (ignore args))
				(debug-assert (member con (gethash key *connection-cache*)))
				(deletef con (gethash key *connection-cache*))))
    (reset-timeout con (http-connection-cache-timeout))
    (push con (gethash key *connection-cache*))))

#+tpd2-http-no-connection-cache
(defun add-to-connection-cache (con key)
  (declare (ignore key))
  (con-clear-failure-callbacks con)
  (hangup con))

(defun get-http-request-con (address port)
  (let ((con (pop (gethash (list address port) *connection-cache*))))
    (cond (con
	   (con-clear-failure-callbacks con)
	   (reset-timeout con)
	   (debug-assert (not (con-dead? con)))
	   (cond ((con-connected? con)
		  con)
		 (t
		  (hangup con)
		  (get-http-request-con address port))))
	  (t
	   (make-con-connect :address address :port port)))))

(defun launch-http-request (&key (port 80) address body 
			    (path (force-byte-vector "/")) 
			    extra-header-lines
			    hostname
			    timeout
			    failure
			    done
			    (method (force-byte-vector "GET")))
  (unless address
    (setf address (lookup-hostname hostname)))
  (unless address
    (error "Please specify an address"))
  (let ((con (get-http-request-con address port)) succeeded)
    (when failure
      (con-add-failure-callback con (lambda(e) (unless succeeded (funcall failure e)))))
    (when timeout
      (reset-timeout con timeout))
    (unless hostname
      (setf hostname 
	    (with-sendbuf (s)
	      address 
	      (unless (eql port 80)
		(with-sendbuf-continue (s) ":" port)))))
  
    (launch-io 'http-request con 
	       (with-sendbuf (s)
		 method " " path " HTTP/1.1" +newline+
		 (unless (zerop (length body))
		   (with-sendbuf-continue (s)
		     "Content-Length: " (length body) +newline+))
		 "User-Agent: tpd2/0" +newline+
		 "Host: " hostname +newline+
		 extra-header-lines
		 +newline+
		 body)
	       (lambda(&rest args)(setf succeeded t) (apply done args))
	       :connection-cache (list address port))))
