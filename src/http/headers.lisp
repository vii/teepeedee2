(in-package #:tpd2.http)

(defprotocol process-headers (con process-header-func)
  (let ((last-header-name))
    (loop for line = (io 'recvline con)
	  until (zerop (length line))
	  do (without-call/cc 
	       (if-match-bind ((+ (space) ) value (progn (* (space)) (last)))
			      line
			      (funcall process-header-func last-header-name value)
			      (match-bind 
				  (header-name (progn (* (space)) ":") (* (space)) value (progn (* (space)) (last)))
				  line
			      (funcall process-header-func header-name value)
			      (setf last-header-name header-name)))))
  (values)))
