(in-package #:tpd2.webapp)

(defmyclass (list-channel (:include simple-channel))
    (list nil))

(my-defun list-channel add (message)
  (push message (my list))
  (my notify))

(my-defun list-channel del (message)
  (deletef message (my list)))

(my-defun list-channel 'object-to-ml ()
  (<div :class "list-channel"
	(loop for x in (reverse (my list)) do
	      (output-object-to-ml x))
	(output-raw-ml (call-next-method))))
