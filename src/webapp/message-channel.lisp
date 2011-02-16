(in-package #:tpd2.webapp)

(defmyclass (message-channel (:include channel))
    (messages nil))

(my-defun message-channel broadcast (message)
  (push (force-byte-vector message) (my messages))
  (my notify))


(my-defun message-channel update (subscriber-state)
  (with-ml-output
    (output-raw-ml
     (js-to-string
       (append-element-id (unquote (force-string (my id))) (unquote
                                                            (force-string
                                                             (let ((sendbuf (with-sendbuf())))
                                                               (loop for x in (subseq (reverse (my messages)) subscriber-state)
                                                                     do (sendbuf-add sendbuf x))
                                                               sendbuf))))
       (channel (unquote (force-string (my id))) (unquote (my state)))))))

(my-defun message-channel 'object-to-ml ()
  (<div :id (my id) :class +html-class-scroll-to-bottom+
        (loop for x in (reverse (my messages)) do
              (output-raw-ml x))
        (output-raw-ml (call-next-method))))

