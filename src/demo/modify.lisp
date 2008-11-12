(in-package #:tpd2.demo)

(defun find-forum (name)
  (find name *fora* :test 'equalp :key 'forum-name))

(my-defun message 'object-to-ml ()
  (<div :class "message"
	(<p (my text) (<span :class "message-attribution" " by " (my author) " at " (time-string (my time))))
	(when (equalp (frame-username (webapp-frame)) (my author))
	  (<p (html-action-link "Delete"
				(datastore-delete me)
				(its notify (find-forum (my forum-name))))))))
