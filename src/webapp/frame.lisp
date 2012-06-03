(in-package #:tpd2.webapp)

(defmyclass (frame (:include simple-channel))
    current-page
  (site (current-site))
  variables
  (username (random-name))
  timeout
  trace-info
  destroy-hooks)

(defvar *frames* (make-hash-table :test #'equalp))

(my-defun frame exit ()
  (loop for hook in (my destroy-hooks)
        do (funcall hook me))
  (my 'channel-destroy)
  (remhash (my id) *frames*))

(my-defun frame reset-timeout ()
  (timeout-set (my timeout) (* 5 60)))

(my-defun frame 'initialize-instance :after (&key)
  (setf (gethash (my id) *frames*) me)
  (setf (my timeout) (make-timeout :func (lambda()(my exit))))
  (my reset-timeout))

(defun find-frame (id)
  (gethash id *frames*))

(defun-speedy webapp-frame-available-p ()
  (and (boundp '*webapp-frame*) *webapp-frame*))

(defun webapp-frame (&rest args-for-make-frame)
  (unless *webapp-frame*
    (setf *webapp-frame* (apply 'make-frame args-for-make-frame)))
  *webapp-frame*)

(my-defun frame var (id &optional default)
  (getf (my variables) id default))

(my-defun frame (setf var) (val id)
  (setf (getf (my variables) id) val))

(defun webapp-frame-var (id &optional default)
  (frame-var (webapp-frame) id default))

(defun (setf webapp-frame-var) (val id &optional default)
  (declare (ignore default))
  (setf (frame-var (webapp-frame) id) val))

(defun list-all-frames ()
  (let (ret)
    (maphash (lambda(k v)(declare (ignore k))(push v ret)) *frames*)
    ret))

(defun frame-id (frame)
  (channel-id frame))

(my-defun frame change-username (new-name)
  (setf (my username) new-name)
  (my notify)
  new-name)

(my-defun frame 'simple-channel-body-ml ())