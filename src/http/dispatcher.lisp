(in-package #:tpd2.http)

(defstruct dispatcher
  canonical-name
  (paths (make-hash-table :test 'equalp))
  (error-responder 'default-http-error-page))

(defun dispatch (con done path &key params host)
  (dispatcher-respond (find-dispatcher host) con done path params))

(defun build-http-response (&key code banner headers body)
  (with-sendbuf (response)
    "HTTP/1.1 " code " " banner +newline+
    "Content-Length: " (sendbuf-len body) +newline+
    "Content-Type: text/html;charset=utf-8" +newline+
    headers
    +newline+
    body))
(declaim (inline build-http-response))

(defun respond-http (con done &key (code (force-byte-vector 200)) (banner (force-byte-vector "OK"))
		     headers body)
  (send con done (build-http-response :code code :banner banner :headers headers :body body)))
(declaim (inline respond-http))

(my-defun dispatcher respond (con done path params)
  (let ((f (gethash path (my paths))))
    (handler-case 
	(if f
	    (funcall f me con done path params)
	    (respond-http con done :code  404 :banner  "Not found"
		:body (funcall (my error-responder) me path params)))
      (error ()
	(respond-http con done
	 :body (with-sendbuf () "<h1>I made a mistake. Sorry</h1>")
	 :code 500
	 :banner "Internal error")))))

(my-defun dispatcher register-path (path func)
  (setf (gethash (force-byte-vector path) (my paths)) func))

(my-defun dispatcher 'default-http-error-page (path params)
  (declare (ignore params path))
  (with-sendbuf () 
    "<h1>Where? What? Ask for something else.</h1>"))


(defvar *default-dispatcher* (make-dispatcher))

(defvar *dispatchers* nil)

(defun find-dispatcher (host)
  (or (cdr-assoc *dispatchers* host :test 'equalp) *default-dispatcher*))




