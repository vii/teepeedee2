(in-package #:tpd2.test)

(def-suite http :in :tpd2)
(in-suite http)

(defsite *test-site* :dispatcher "*..*")

(test http-request-and-serve
  (let* ((port 18283)
	 (req-count 100)
	 (req-count-to-go req-count)
	 (address "127.0.0.1")
	 (*client-http-connection-cache* (make-hash-table :test 'equalp))
	 (disp (find-or-make-dispatcher (force-byte-vector "*..*")))
	 *default-site*)
    (with-site (*test-site*)
      (labels ((build-body (n)
		 (with-sendbuf ()
		   (let ((v (make-byte-vector n)))
		     (loop for a below (length v) do (setf (aref v a) (random #x100)))
		     v)))
	       (random-alphnum (n)
		 (let ((v (make-byte-vector n)))
		   (loop for a below (length v) do (setf (aref v a) (+ (char-code #\A) (random 26))))
		   v))
	       (build-headers (n)
		 (let ((s (with-sendbuf ())))
		   (loop for i below n do
			 (with-sendbuf-continue (s)
			   "X-"
			   (random-alphnum (1+ (random 20)))
			   ": "
			   (random-alphnum (1+ (random 100)))
			   +newline+
			   ))
		   s)))
	(defpage "/" (header body attempt)
	  (values
	   (build-body (byte-vector-parse-integer body))
	   (build-headers (byte-vector-parse-integer header))))

	(with-independent-event-loop ()
	    (let ((socket (tpd2.io:make-con-listen :address address :port port)))
	      (is-true socket)
	      (block event-loop
		(labels ((req-finished ()
			   (when (zerop (decf req-count-to-go))
			     (return-from event-loop))))
		  (unwind-protect
		       (progn
			 (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve)
			 (loop for attempt below req-count do
			       (let ((b (* (random 16) (random (* 16 1024))))
				     (h (* (random 16) (random 16)))
				     (attempt attempt))
				 (tpd2.http:launch-http-request
				  :timeout 60
				  :port port :address address
				  :hostname (tpd2.http:dispatcher-canonical-name disp)
				  :path (byte-vector-cat "/?BODY=" b "&HEADER=" h "&ATTEMPT=" attempt)
				  :extra-header-lines (build-headers (min 20 attempt))
				  :done
				  (lambda (response &key response-code)
				    (is (= 200 response-code)) (is (= (length response) b))
				    (req-finished))
				  :failure
				  (lambda (&rest e)
				    (fail (format nil "attempt ~A had error ~A; headers ~A body ~A" attempt e h b))
				    (req-finished)))))
			 (event-loop))
		    (progn
		      (tpd2.io:hangup socket)))))))))))
