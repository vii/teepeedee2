(in-package #:tpd2.webapp)

(defstruct (session (:constructor %make-session))
  (id (random-web-sparse-key 10))
  variables
  (username (random-name))
  timeout)

(defvar *sessions* (make-hash-table :test #'equalp))

(defun make-session (&rest args)
  (let ((session (apply '%make-session args)))
    (session-init session)
    session))

(my-defun session exit ()
  (remhash (my id) *sessions*))

(my-defun session init ()
  (setf (gethash (my id) *sessions*) me)
  (setf (my timeout) (make-timeout :func (lambda()(my exit))))
  (my reset-timeout))

(my-defun session reset-timeout ()
  (timeout-set (my timeout) (* 10 60)))

(defun find-session (id)
  (awhen (gethash id *sessions*)
    (session-reset-timeout it)
    it))

(defvar *webapp-session* nil)

(defun webapp-session ()
  (unless *webapp-session*
    (setf *webapp-session* (make-session)))
  *webapp-session*)

(my-defun session var (id)
  (getf (my variables) id))

(my-defun session (setf var) (val id)
  (setf (getf (my variables) id) val))

(defun webapp-session-var (id)
  (session-var (webapp-session) id))

(defun (setf webapp-session-var) (val id)
  (setf (session-var (webapp-session) id) val))

(defun list-all-sessions ()
  (let (ret)
    (maphash (lambda(k v)(declare (ignore k))(push v ret)) *sessions*)
    ret))
