(in-package #:tpd2.webapp)

(defmyclass (simple-channel (:include channel)))

(my-defun simple-channel update (subscriber-state)
  (with-ml-output
    (output-raw-ml
     (js-to-string
       (reset-element-id (unquote (force-string (my id)))
			 (unquote (force-string (object-to-ml me))))))))
