(in-package #:tpd2.webapp)

(defvar *channels* (make-hash-table :test 'equalp))

(defmyclass channel
  (id (random-web-sparse-key 10))
  (state 0)
  (subscribers nil))

(my-defun channel 'initialize-instance :after (&key)
  (setf (gethash (my id) *channels*) me))

(my-defun channel notify ()
  (let ((subscribers (my subscribers)))
    (setf (my subscribers) nil)
    (incf (my state))
    (loop for s in subscribers do 
	  (with-ignored-errors (tpd2.io:report-unless-normal-connection-error) 
	    (funcall s))))
  (values))

(my-defun channel subscribe (f)
  (push f (my subscribers)))
(my-defun channel unsubscribe (f)
  (deletef f (my subscribers)))

(defun find-channel (id)
  (check-type id byte-vector)
  (gethash id *channels*))

(my-defun channel destroy ()
  (when (eq me (gethash (my id) *channels*))
    (remhash (my id) *channels*)))

(defgeneric channel-update (channel subscriber-state))

(defun channel-respond-page (dispatcher con done)
  (declare (ignore dispatcher))
  (apply-page-call (:con con :function 'channel-respond :create-frame nil) con done (.channels.)))

(defun channel-string-to-states (channels)
  (let ((channel-states))
    (match-bind ( (* channel "|" (state (integer)) (or ";" (last)) 
		     '(awhen (find-channel channel) (push (cons it state) channel-states))))
	channels)
    channel-states))

(defun channel-respond-body (channel-states &key always-body)
  (let (at-least-one)
    (let ((sendbuf
	   (with-ml-output
	     (loop for (channel . state) in channel-states do
		   (unless (equalp state (channel-state channel))
		     (setf at-least-one t)
		     (output-raw-ml 
		      (js-to-string 
		       (channel 
			(unquote (force-string (channel-id channel))) 
			(unquote (channel-state channel)))))
		     (output-raw-ml (channel-update channel state))))
	     (output-raw-ml (js-to-string "OK")))))
      (when (or at-least-one always-body)
	sendbuf))))

(defun channel-respond (con done &key .channels.)
  (let ((channel-states (channel-string-to-states .channels.)))
    (with-preserve-specials (*webapp-frame* *servestate*) 
      (flet ((finished () 
	       (or (con-dead? con)
		   (with-specials-restored
		       (with-frame-site 
			   (awhen (channel-respond-body channel-states)
			     (start-http-response)
			     (send-http-response con done it)
			     t))))))
      (unless (finished)
	(let (func 
	      (original-timeout-handler 
	       (timeout-func (tpd2.io:con-timeout con)))
	      (original-hangup-handler
	       (tpd2.io:con-hangup-hook con)))
	  (flet ((unsubscribe ()
		   (setf (timeout-func (tpd2.io:con-timeout con)) original-timeout-handler
			 (tpd2.io:con-hangup-hook con) original-hangup-handler)
		   (loop for (channel ) in channel-states do (channel-unsubscribe channel func))))
	    (setf func
		  (lambda() (when (finished) (unsubscribe))))
	    (loop for (channel ) in channel-states do (channel-subscribe channel func))

	    (setf (timeout-func (tpd2.io:con-timeout con))
		  (lambda ()
		    (unsubscribe)
		    (unless (con-dead? con)
		      (with-specials-restored
			  (with-ignored-errors (tpd2.io:report-unless-normal-connection-error)
			    (start-http-response 
			     :banner (force-byte-vector "504 Timeout")
			     :content-type #.(byte-vector-cat "Retry-after: 0" +newline+))
			    (send-http-response con done 
						(with-sendbuf () (js-to-string "TIMEOUT"))))))))
	    (setf (tpd2.io:con-hangup-hook con)
		  (lambda (&rest args)
		    (unsubscribe)
		    (when original-hangup-handler
		      (apply original-hangup-handler args)))))))))))

(defun register-channel-page ()
  (dispatcher-register-path (site-dispatcher (current-site)) +channel-page-name+ #'channel-respond-page))

(my-defun channel 'object-to-ml ()
  (js-html-script (channel (unquote (force-string (my id))) (unquote (my state)))))

