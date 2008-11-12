(in-package #:tpd2.http)

(defstruct dispatcher
  canonical-name
  (paths (make-hash-table :test 'equalp))
  (error-responder 'default-http-error-page))

(defun dispatch (con done path &key params host)
  (dispatcher-respond (find-dispatcher host) con done path params))

(defun build-http-response (&key code banner headers body)
  (declare (type sendbuf body))
  (declare (dynamic-extent body))
  (with-sendbuf (response)
    "HTTP/1.1 " code " " banner +newline+
    "Content-Length: " (sendbuf-len body) +newline+
    "Content-Type: text/html;charset=utf-8" +newline+
    headers
    +newline+
    body))


(defun-speedy respond-http (con done &key (code (force-byte-vector 200)) (banner (force-byte-vector "OK"))
		     headers body)
  (declare (type sendbuf body))
  (declare (dynamic-extent body))
  (send con done (build-http-response :code code :banner banner :headers headers :body body)))


(my-defun dispatcher respond (con done path params)
  (let ((f (gethash path (my paths))))
    (handler-case 
	(cond  
	  (f
	   (funcall f me con done path params))
	  (t
	   ;(format *error-output* "LOST ~A~&" (strcat (my canonical-name) "/" path))
	   (respond-http con done :code  404 :banner  "Not found"
			 :body (funcall (my error-responder) me path params))))
      (error (e)
	(format *error-output* "ERROR ~A~&--- ~A~&" (strcat (my canonical-name) "/" path) (backtrace-description e))
	(respond-http con done
		      :body (with-sendbuf () "<h1>I programmed this thoughtlessly. Sorry for the inconvenience.</h1>")
		      :code 500
		      :banner "Internal error")))))

(my-defun dispatcher register-path (path func)
  (setf (gethash (force-byte-vector path) (my paths)) func))

(my-defun dispatcher 'default-http-error-page (path params)
  (declare (ignore params path))
  (with-sendbuf () 
    "<h1>I made a mistake. Sorry for the inconvenience.</h1>"))

(defvar *default-dispatcher* (make-dispatcher))

(defvar *dispatchers* nil)

(defun do-find-dispatcher (host)
  (cdr-assoc *dispatchers* host :test 'equalp))

(defun find-dispatcher (host)
  (or (do-find-dispatcher host) *default-dispatcher*))

(defun find-or-make-dispatcher (host)
  (let ((host (force-byte-vector host)))
    (or (do-find-dispatcher host)
	(let ((it (make-dispatcher :canonical-name host)))
	  (push (cons host it) *dispatchers*)
	  it))))