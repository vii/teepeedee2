(in-package #:tpd2.http)

(defprotocol http-request (con request)
  (io 'send con request)
  (let ((content-length)
	(chunked)
	(gzip))
    (flet ((decode (bytes)
	     (cond (gzip (error "Sorry; haven't implemented GZIP decompression yet"))
		   (t bytes))))
      (match-bind (code :whitespace (banner (:rest)))
		  (io 'recvline con)
	
	(io 'process-headers con (lambda(name value)
					 (when (length value)
					   (case-match-fold-ascii-case name
								       ("content-length"
									(match-bind ((len :integer)) value
									  (setf content-length len)))
								       ("transfer-encoding"
									(match-bind (:*
										     '(case-match-fold-ascii-case (:word)
										       ("chunked" (setf chunked t))
										       ("gzip" (setf gzip t)))
										     :whitespace?)
									    value)))))))
      (decode
       (cond 
	 (chunked
	  (let ((body
		 (loop for len = (byte-vector-parse-integer (io 'recvline con))
		       while (not (zerop len))
		       collect (io 'recv con len)
		       do
		       (let ((line (io 'recvline con)))
			     (match-bind (:$) line)))))
	    (loop for line = (io 'recvline con)
		  until (zerop (length line)))
	    (apply 'byte-vector-cat body)))
	 (content-length
	  (io 'recv con content-length))
	 (t (error "Sorry; haven't implemented HTTP streaming")))))))
