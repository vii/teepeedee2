(in-package #:tpd2.http)

(defstruct dispatcher
  canonical-name
  (paths (make-hash-table :test 'equalp))
  (error-responder 'default-http-error-page))

(defun dispatch-servestate (con done *servestate*)
  (dispatcher-respond (find-dispatcher (servestate-host*)) 
		      con done))

(defun-speedy start-http-response (&key (banner (force-byte-vector "200 OK"))
					(content-type #.(byte-vector-cat "Content-Type: text/html;charset=utf-8" tpd2.io:+newline+)))
  (setf (servestate-response*)
	(with-sendbuf ()
	  "HTTP/1.1 " banner +newline+
	  content-type)))

(defun-speedy map-http-params (func)
  (declare (dynamic-extent func) (type (function (simple-byte-vector simple-byte-vector) t) func))
  (labels (
	   (f (name value)
	     (funcall func (url-encoding-decode name) (url-encoding-decode value)))
	   (parse-params (str)
	   (when str
	     (match-bind ( (*  name "=" value (or (last) "&")
			       '(f name value)))
		 str)))
	 (parse-cookie-params (str)
	   (when str
	     (match-bind ( (*  name "=" value (or (last) "," ";")
			       '(f name value)))
		 str))))
    (declare (inline parse-cookie-params parse-params f)
	     (dynamic-extent #'parse-params #'parse-cookie-params #'f))
    (parse-params (servestate-query-string*))
    (parse-params (servestate-post-parameters*))
    (parse-cookie-params (servestate-cookie*))))

(defmacro with-http-params (bindings &body body)
  (with-unique-names (f pname pvalue)
    `(let ,(loop for b in bindings for (n default) = (force-list b)
		 collect `(,n ,default))
       (flet ((,f (,pname ,pvalue)
		(declare (type simple-byte-vector ,pname ,pvalue))
		(case-match-fold-ascii-case ,pname
					    ,@(loop for b in bindings
						    collect 
						    (destructuring-bind
							  (var &optional default &key conv (name (force-byte-vector var)))
							(force-list b)
						      (declare (ignore default))
						      `(,(force-byte-vector name)
							 (setf ,var ,(if conv
									 `(,conv ,pvalue)
									 pvalue)))
						      )))))
	 (declare (inline ,f) (dynamic-extent #',f))
	 (map-http-params #',f)
	 (locally ,@body)))))

(defmacro with-http-headers (() &body body)
  `(with-sendbuf-continue ((servestate-response-as-sendbuf*))
     ,@body))

(defun-speedy send-http-response (con done body)
  (declare (type sendbuf body))
  (with-http-headers ()
     "Content-Length: " (sendbuf-len body) +newline+
     +newline+
     body)
  (send
   con done
   (servestate-response-as-sendbuf*)))

(defun-speedy respond-http (con done &key banner body)
  (start-http-response :banner banner)
  (send-http-response con done body))

(my-defun dispatcher respond (con done)
  (let ((f (gethash (servestate-path*) (my paths))))
    (handler-case 
	(cond  
	  (f
	   (locally (declare (optimize speed) (type function f))
	     (funcall f me con done)
	     (values)))
	  (t
	   ;(format *error-output* "LOST ~A~&" (strcat (my canonical-name) "/" path))
	   (respond-http con done :banner (force-byte-vector "404 Not found")
			 :body (funcall (my error-responder) me))))
      (error (e)
	(format *error-output* "~&PAGE ERROR ~A~&--- ~A~&-AGAIN PAGE ERROR ~A~&" (strcat (my canonical-name) (servestate-path*)) 
		(backtrace-description e)
		e)
	(respond-http con done
		      :body (with-sendbuf () "<h1>I programmed this thoughtlessly. Sorry for the inconvenience.</h1>")
		      :banner (force-byte-vector "500 Internal error"))))))

(my-defun dispatcher register-path (path func)
  (setf (gethash (force-byte-vector path) (my paths)) (alexandria:ensure-function func)))

(my-defun dispatcher 'default-http-error-page ()
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