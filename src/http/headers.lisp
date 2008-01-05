(in-package #:tpd2.http)

(defprotocol process-headers (con process-header-func)
  (let ((last-header-name))
    (loop for line = (io 'recvline con)
	  until (zerop (length line))
	  do (if-match ('(:s) line)
		       (funcall process-header-func last-header-name line)
		       (match-bind 
			((header-name (:until-and-eat :whitespace? ":" :whitespace?))
			 (value (:until :whitespace? :$)))
			line
			(funcall process-header-func header-name value)
			(setf last-header-name header-name)))))
  (values))
