(in-package #:tpd2.webapp)

(defmyclass (simple-channel (:include channel)))

(defgeneric simple-channel-body-ml (simple-channel))

(my-defun simple-channel update (subscriber-state)
  (declare (ignore subscriber-state))
  (with-ml-output
    (output-raw-ml
     (js-to-string
       (reset-element-id (unquote (force-string (my id)))
			 (unquote (force-string 
				   (simple-channel-body-ml me))))))))

(my-defun simple-channel 'object-to-ml ()
  (<div :id (my id) 
	(simple-channel-body-ml me)
	(call-next-method)))
