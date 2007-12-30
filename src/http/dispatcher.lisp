(in-package #:tpd2.http)

(defstruct dispatcher
  canonical-name
  (paths (make-hash-table :test 'equalp))
  (error-responder 'default-http-error-page))

(my-defun dispatcher build-http-response (&key (code 200) (banner "OK") headers body)
  (my-declare-fast-inline)
  (with-sendbuf (response)
    "HTTP/1.1 " code " " banner +newline+
    "Content-Length: " (sendbuf-len body) +newline+
    headers
    +newline+
    body))

(my-defun dispatcher respond (path params)
  (let ((f (gethash path (my paths))))
    (handler-case 
	(if f
	    (my build-http-response :body
				 (funcall f me path params))
	    (my build-http-response :code 404 :banner "Not found" :body
				 (funcall (my error-responder) me path params)))
      (error ()
	(my build-http-response 
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

(defun generate-http-response (host path params)
  (let ((dispatcher (find-dispatcher host)))
    (dispatcher-respond dispatcher path params)))

