(in-package #:tpd2.webapp)

(defvar *channels* (trivial-garbage:make-weak-hash-table :weakness :value :test 'equalp))

(defstruct (channel (:constructor %make-channel))
  (id (random-web-sparse-key 10))
  (state 0)
  (subscribers nil))

(defun make-channel (&rest args)
  (let ((channel (apply '%make-channel args)))
    (channel-init channel)
    channel))

(my-defun channel init ()
  (setf (gethash (my id) *channels*) me))

(my-defun channel notify ()
  (incf (my state)))

(my-defun channel subscribe (f)
  (push f (my subscribers)))
(my-defun channel unsubscribe (f)
  (deletef f (my subscribers)))

(defgeneric channel-update (channel subscriber-state))

(defconstant +channel-page-name+ "/*channel*")

(defun channel-respond-page (dispatcher con done path all-http-params)
  (declare (ignore dispatcher path))
  (apply-page-call 'channel-respond con done (channels)))

(defun channel-respond (con done &key channels)
  (let ((channel-states))
    (match-bind (:* (channel (:until-and-eat "|")) (state (:until-and-eat ";")) 
		    '(push (cons channel (byte-vector-parse-integer state)) channel-states))
	channels)

    (flet ((finished ()
	     (let ((sendbuf
		    (with-ml-output
		      (loop for (channel . state) in channel-states do
			    (unless (eql state (channel-state channel))
			       (output-raw-ml (channel-update channel state)))))))
	       (when (not (sendbuf-empty sendbuf))
		 (respond-http con done :body sendbuf)
		 t))))
    (let (func)
      (unless (finished)
	(flet ((unsubscribe ()
		 (loop for (channel . state) in channel-states do (channel-unsubscribe channel func))))
	  (setf func
		(lambda() (when (finished) (unsubscribe))))
	  (loop for (channel . state) in channel-states do (channel-subscribe channel func))))))))

(defun register-channel-page ()
  (dispatcher-register-path *default-dispatcher*  +channel-page-name+ #'channel-respond-page))