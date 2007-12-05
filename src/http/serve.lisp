(in-package #:tpd2.http)

#|

(define-condition serve-http-invalid-method (protocol-error)
  ())



(defprotocol http-request
    (let (postdata skip-body keep-alive uri (version-major 1) (version-minor 0) 
                   host referer
                   content-length)
      (rexparse (readline)
                (or :get 
                    (:post (act (setf postdata t)))
                    (:head (act (setf skip-body t))))
                (whitespace)
                (non-whitespace uri)
                (whitespace)
                (? :http/
                   (int version-major)
                   "."
                   (int version-minor)))
      (when (or (> 1 version-major)
                (and (= 1 version-major)
                     (>= 1 version-minor)))
        (setf keep-alive t))

      (loop for line = (readline)
	    while (not (zerop (length line)))
	    do
	    (rexparse line
		      (rexplet ((sep (? (whitespace)) ":" (? (whitespace))))
			       (or (:content-length (sep) (int content-length))
				   (:host (sep) (non-whitespace host))
				   (:referer (sep) (non-whitespace referer))
				   (:connection (sep) (word :close) (act (setf keep-alive nil)))
				   ((when (trust-last-x-forwarded-for)) 
				    :x-forwarded-for (sep) 
				    (non-whitespace (my peer)) 
				    (* (? (whitespace)) "," (? (whitespace)) 
				       (non-whitespace (my peer))))
				   t))))

      (when content-length
	(setf post-data (read content-length)))
      
      (
      |#