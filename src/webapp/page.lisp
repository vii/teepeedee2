(in-package #:tpd2.webapp)

(defconstant +webapp-session-id-param+ (force-byte-vector "*webapp-session*"))

(defmacro defpage (path defaulting-lambda-list &body body)
  (let ((normal-func-name (intern (strcat 'page- path)))
	(arg-names (mapcar 'force-first defaulting-lambda-list))
	(arg-values (mapcar (lambda(x)(second (force-list x))) defaulting-lambda-list)))
    `(progn
       (defun ,normal-func-name (&key ,@defaulting-lambda-list)
	 ,@body)
       (dispatcher-register-path *default-dispatcher* ,path
				 (lambda(dispatcher path params)
				   (declare (ignore dispatcher path))
				   (let ((*webapp-session*
					  (awhen (cdr-assoc params +webapp-session-id-param+ :test 'byte-vector=-fold-ascii-case)
					    (find-session it))))
				     (,normal-func-name 
				      ,@(loop for name in arg-names
					      for value in arg-values
					      collect (intern (force-string name) :keyword)
					      if (eq name 'all-http-params)
					      collect 'params
					      else
					      collect `(or (cdr-assoc params ,(force-byte-vector name) 
								      :test 'byte-vector=-fold-ascii-case)
							   ,value))))))
	 ',normal-func-name)))

(defun page-link (page &rest args)
  (sendbuf-to-byte-vector
   (with-sendbuf (sendbuf)
     page
     "?"
     +webapp-session-id-param+
     "="
     (awhen *webapp-session*
       (session-id it))
     (loop for (param val) on args by #'cddr
	   do (with-sendbuf-continue (sendbuf)
		"&"
		(symbol-name param)
		"="
		(percent-hexpair-encode val))))))
   