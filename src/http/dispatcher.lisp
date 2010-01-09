(in-package #:tpd2.http)

(defstruct dispatcher
  canonical-name
  (paths (make-hash-table :test 'equalp))
  (error-responder 'default-http-error-page))

(defun dispatch (con done path &key params host)
  (dispatcher-respond (find-dispatcher host) con done path params))

(defun-speedy build-http-response (&key banner headers body)
  (declare (type sendbuf body))
  (declare (dynamic-extent body))
  (with-sendbuf ()
    "HTTP/1.1 " banner +newline+
    "Content-Length: " (sendbuf-len body) +newline+
    headers
    +newline+
    body))

(defun-speedy respond-http (con done &key (banner (force-byte-vector "200 OK"))
		     (headers #.(byte-vector-cat "Content-Type: text/html;charset=utf-8" tpd2.io:+newline+)) body)
  (declare (type sendbuf body))
  (declare (dynamic-extent body))
  (send con done (build-http-response :banner banner :headers headers :body body)))

(my-defun dispatcher respond (con done path params)
  (let ((f (gethash path (my paths))))
    (handler-case 
	(cond  
	  (f
	   (locally (declare (optimize speed) (type function f))
	     (funcall f me con done path params)
	     (values)))
	  (t
	   ;(format *error-output* "LOST ~A~&" (strcat (my canonical-name) "/" path))
	   (respond-http con done :banner (force-byte-vector "404 Not found")
			 :body (funcall (my error-responder) me path params))))
      (error (e)
	(format *error-output* "~&PAGE ERROR ~A~&--- ~A~&-AGAIN PAGE ERROR ~A~&" (strcat (my canonical-name) path) 
		(backtrace-description e)
		e)
	(respond-http con done
		      :body (with-sendbuf () "<h1>I programmed this thoughtlessly. Sorry for the inconvenience.</h1>")
		      :banner (force-byte-vector "500 Internal error"))))))

(my-defun dispatcher register-path (path func)
  (setf (gethash (force-byte-vector path) (my paths)) (alexandria:ensure-function func)))

(my-defun dispatcher 'default-http-error-page (path params)
  (declare (ignore params path))
  (with-sendbuf () 
    "<h1>I made a mistake. Sorry for the inconvenience.</h1>"))

(defvar *default-dispatcher* (make-dispatcher))

(defvar *dispatchers* nil)

(defun find-dispatcher-go (host)
  (alist-get *dispatchers* host :test #'equalp))

(defun find-dispatcher (host)
  (or (find-dispatcher-go host) *default-dispatcher*))

(defun find-or-make-dispatcher (host)
  (let ((host (force-byte-vector host)))
    (or (find-dispatcher-go host)
	(let ((it (make-dispatcher :canonical-name host)))
	  (push (cons host it) *dispatchers*)
	  it))))

(defun dispatcher-add-alias (dispatcher alias)
  (check-type dispatcher dispatcher)
  (setf (alist-get *dispatchers* (force-byte-vector alias)) dispatcher))

(my-defun dispatcher 'print-object (stream)
  (print-unreadable-object (me stream :type t :identity t)
    (format stream "~S" (force-string (my canonical-name)))
    (loop for p being the hash-keys of (my paths)
	  do (format stream " ~A" (force-string p)))))

(defun describe-dispatchers (&optional (*standard-output* *standard-output*))
  ;; TODO print a list of hostnames for each dispatcher
  (loop for (path . dispatcher) in *dispatchers*
	do (format t "~&~S -> ~A~&" (force-string path) dispatcher))
  (format t "~&DEFAULT -> ~A~&" *default-dispatcher*))