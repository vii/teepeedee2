(in-package #:tpd2.webapp)

(defmyclass (frame (:include simple-channel))
  current-page
  variables
  (username (random-name))
  (messages (make-list-channel))
  timeout)

(defvar *frames* (make-hash-table :test #'equalp))

(my-defun frame exit ()
  (remhash (my id) *frames*))

(my-defun frame reset-timeout ()
  (timeout-set (my timeout) (* 5 60)))

(my-defun frame 'initialize-instance :after (&key)
  (setf (gethash (my id) *frames*) me)
  (setf (my timeout) (make-timeout :func (lambda()(my exit))))
  (my reset-timeout))

(defun find-frame (id)
  (awhen (gethash id *frames*)
    it))

(defun webapp-frame ()
  (unless *webapp-frame*
    (setf *webapp-frame* (make-frame)))
  *webapp-frame*)

(my-defun frame var (id)
  (getf (my variables) id))

(my-defun frame (setf var) (val id)
  (setf (getf (my variables) id) val))

(defun webapp-frame-var (id)
  (frame-var (webapp-frame) id))

(defun (setf webapp-frame-var) (val id)
  (setf (frame-var (webapp-frame) id) val))

(defun list-all-frames ()
  (let (ret)
    (maphash (lambda(k v)(declare (ignore k))(push v ret)) *frames*)
    ret))

(defun frame-id (frame)
  (channel-id frame))
