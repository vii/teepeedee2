(in-package #:tpd2.webapp)

(defmyclass (list-channel (:include simple-channel))
    (list nil))

(my-defun list-channel add (message)
  (push message (my list))
  (my notify))

(my-defun list-channel del (message)
  (deletef message (my list)))

(my-defun list-channel 'simple-channel-body-ml ()
  (<div :class "list-channel"
	(loop for x in (reverse (my list)) do
	      (output-object-to-ml x))))

