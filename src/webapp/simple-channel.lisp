(in-package #:tpd2.webapp)

(defvar *horrible-simple-channel-object-to-ml-hack* nil)

(defmyclass (simple-channel (:include channel)))

(my-defun simple-channel update (subscriber-state)
  (with-ml-output
    (output-raw-ml
     (js-to-string
       (reset-element-id (unquote (force-string (my id)))
			 (unquote (force-string 
				   (let ((*horrible-simple-channel-object-to-ml-hack* t))
				     (object-to-ml me)))))))))

(my-defun simple-channel 'object-to-ml :around ()
	  (if *horrible-simple-channel-object-to-ml-hack*
	      (call-next-method)
	      (<div :id (my id) (call-next-method))))