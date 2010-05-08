(in-package #:tpd2.http)

;;;;; FIXME Section 4.2 of RFC 2616: multiple headers equivalent to a
;;;;; comma-delimited sequence.

(defun http-serve-timeout ()
  60)

(defun match-x-forwarded-for (value)
  (match-bind
   (+ (and (char) (progn host (or (progn "," (* (space))) (last)))))
   value
   host))

(defun-speedy match-request-url (url)
  (match-bind (path (or (last) (progn "?" q)))
      url
    (setf (servestate-path*) path
	  (servestate-query-string*) q)))

(defconstant-bv +header-end+ (concatenate 'simple-byte-vector +newline+ +newline+))

(defprotocol http-serve (con)
  (without-call/cc
      (reset-timeout con (http-serve-timeout)))

  (io 'http-serve-parse-headers con (io 'recvline-shared-buf con +header-end+)))

#- (and)
(defun http-serve-parse-headers-clean (con done headers)
  (declare (optimize speed))
  (let (
	(request-content-length 0)
	host
	(request-origin (con-peer-info con))
	connection-close)
    (flet ((handle-header (name value)
	     (declare (type simple-byte-vector name value))
	     (unless (zerop (length value))
	        (case-match-fold-ascii-case name
					   ("content-length" 
					    (setf request-content-length (match-int value)))
					   ("host"
					    (setf host value))
					   ("connection"
					    (match-bind (  
							 (+ word (or (+ (space)) (last))
							    '(case-match-fold-ascii-case (the simple-byte-vector word)
							      ("close" (setf connection-close t))
							      ("keep-alive" (setf connection-close nil)))))
						value))
					   ("x-forwarded-for" 
					    (setf request-origin
						  (match-x-forwarded-for value)))))))
      (declare (dynamic-extent #'handle-header))
      (match-bind 
	  (macrolet ((lws () `(or #\Space #\Tab))) 
	    (progn
	      method (+ (lws))
	      url
	      (or (progn (+ (lws)) (:? "HTTP/" (version-major (unsigned-byte :max-length 3) 1) "." 
				       (version-minor (unsigned-byte :max-length 3) 0) (* (lws))) +newline+)
		  +newline+)
	      '(setf connection-close (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor)))))
	      (* 
	       header-name ":" (* (lws)) value +newline+
	       '(handle-header header-name value)
	       (* (+ (lws)) extra-value +newline+
		  '(handle-header header-name extra-value))
	       )
	      +newline+))
	  headers

	(http-serve-process-body con done 
				 url
				 :request-content-length request-content-length 
				 :host host 
				 :origin request-origin
				 :connection-close connection-close)))))

(defun http-serve-parse-headers (con done headers)
  (declare (optimize speed))
  (let ((*servestate* (make-servestate :origin (con-peer-info con))))
    (flet ((handle-header (name value)
	     (declare (type simple-byte-vector name value))
	     (unless (zerop (length value))
	        #.`(case-match-fold-ascii-case name
					       ("content-length" 
						(setf (servestate-content-length*) (match-int value)))
					       ("connection"
						(match-bind (  
							     (+ word (or (+ (space)) (last))
								'(case-match-fold-ascii-case (the simple-byte-vector word)
								  ("close" (setf (servestate-connection-close*) t))
								  ("keep-alive" (setf (servestate-connection-close*) nil)))))
						value))
					       ("x-forwarded-for" 
						(setf (servestate-origin*)
						      (match-x-forwarded-for value)))
					       ,@(loop for f in *stored-servestate-header-fields*
						       collect
						       `(,(force-string f)
							  (push value  (,(concat-sym 'servestate- f '*)))))))))
      (declare (dynamic-extent #'handle-header))
      (let ((pos 0))
	(declare (type (integer 0 100000) pos)
		 (type simple-byte-vector headers))
	(macrolet (
		   (e (&optional (d 0))
		     `(locally (declare (optimize (safety 0))) (aref headers (+ pos ,d))))
		   (u (&rest chars)
		     (with-unique-names (s)
		       `(let ((,s pos))
			  (loop until (m ,@chars) do (incf pos))
			  (subseq headers ,s pos))))
		   (m (&rest chars)
		     `(let ((e (e))) (declare (type (unsigned-byte 8) e)) (or ,@(loop for c in chars collect `(= e ,(char-code c))))))
		   (w (&rest chars)
		     `(loop while (m ,@chars) do (incf pos)))
		   (s (string)
		     `(progn
			,@(loop for c across string 
			       for i from 0 collect
			       `(assert (= (e ,i) ,(char-code c))))
			(incf pos ,(length string))))
		   (i () ;; XXX hack
		     `(multiple-value-prog1 
			 (- (e) (char-code #\0))
			(incf pos)))
		   (ulws ()
		     `(multiple-value-prog1 (u #\Space #\Tab) (lws)))
		   
		   (lws () `(w #\Space #\Tab))
		   (assert-eol ()
		     `(progn (incf pos) (assert (= ,(char-code #\Newline) (e))) (incf pos)))
		   (line ()
		     `(multiple-value-prog1 (u #\Return) (assert-eol))))

	  (let ((version-major 0) (version-minor 9))
	    (setf (servestate-method*) (ulws))

	    (match-request-url (ulws))

	    (cond ((= (e) (char-code #\Return)))
		  (t
		   (s "HTTP/")
		   (setf version-major (i))
		   (s ".")
		   (setf version-minor (i))
		   (lws)
		   (assert-eol)))
	    (setf (servestate-connection-close*) 
		  (not (or (< 1 version-major) (and (= 1 version-major) (< 0 version-minor)))))
	    (loop until (= (e) (char-code #\Return))
		  do (cond ((m #\Space #\Tab))
			   (t
			    (let ((header-name (u #\:)))
			      (incf pos)
			      (lws)
			      (handle-header header-name (line))))))
	    (assert-eol)

	    (http-serve-process-body con done *servestate*)))))))

(defprotocol http-serve-process-body (con servestate)
  (unless (zerop (servestate-content-length servestate))
    (setf (servestate-post-parameters servestate)
	  (force-simple-byte-vector
	   (io 'recv con (servestate-content-length servestate)))))

  (io 'dispatch-servestate con servestate)

  (cond 
       ((servestate-connection-close servestate)	 

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
	(hangup con))
       (t (io 'http-serve con))))

(defun http-serve-wait-timeout ()
  60)

(defun http-start-server (port)
  (let ((socket (tpd2.io:make-con-listen :port port)))
    (tpd2.io:socket-only-accept-if-data-ready socket (http-serve-wait-timeout))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http:http-serve)
    socket))
