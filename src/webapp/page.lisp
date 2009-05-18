(in-package #:tpd2.webapp)

(defvar *webapp-frame*)
(define-constant +webapp-frame-id-param+ (force-byte-vector ".webapp-frame.")
  :test 'equalp)

(define-constant +web-safe-chars+ 
  (force-byte-vector 
   (append (loop for c from (char-code #\A) to (char-code #\Z) collect c)
	   (loop for c from (char-code #\a) to (char-code #\z) collect c)
	   (loop for c from (char-code #\0) to (char-code #\9) collect c)
	   (mapcar 'char-code '(#\- #\_))))
  :test 'equalp)

(defun generate-args-for-defpage-from-params (params-var defaulting-lambda-list)
  (let ((arg-names (mapcar 'force-first defaulting-lambda-list))
	(arg-values (mapcar (lambda(x)(second (force-list x))) defaulting-lambda-list)))
    (loop for name in arg-names
	  for value in arg-values
	  collect (intern (force-string name) :keyword)
	  if (eq name 'all-http-params)
	  collect params-var
	  else
	  collect `(or (alist-get ,params-var ,(force-byte-vector name) 
				  :test 'byte-vector=-fold-ascii-case)
		       ,value))))

(defmacro with-webapp-frame ((con params) &body body)
  (check-symbols params con)
  `(let ((*webapp-frame*
	  (awhen (alist-get ,params +webapp-frame-id-param+ :test 'byte-vector=-fold-ascii-case)
	    (find-frame it))))
     (setf (frame-trace-info (webapp-frame :site (current-site))) (con-peer-info con))
     (frame-reset-timeout (webapp-frame))
     (locally
	 ,@body)))

(defmacro apply-page-call (con function &rest args)
  (let* ((defaulting-lambda-list (car (last args)))
	 (normal-args (butlast args)))
    `(with-webapp-frame (,con all-http-params)
       (funcall ,function ,@normal-args ,@(generate-args-for-defpage-from-params 'all-http-params defaulting-lambda-list)))))


(defmacro defpage-lambda (path function &key defaulting-lambda-list)
  (multiple-value-bind (function defaulting-lambda-list)
      (cond ((and (not defaulting-lambda-list)
		  (listp function)
		  (eq (first function) 'lambda))
	     (values `(lambda (&key ,@(second function))
			,@(cddr function))
		     (second function)))
	    (t
	     (values function defaulting-lambda-list)))
    `(dispatcher-register-path (site-dispatcher (current-site)) ,path
			     (lambda(dispatcher con done path all-http-params)
			       (declare (ignore dispatcher path))
			       (multiple-value-bind (body headers)
				   (apply-page-call con ,function ,defaulting-lambda-list)
				 (respond-http con done :body body :headers headers))))))

(defmacro defpage (path defaulting-lambda-list &body body)
  (let ((normal-func-name (intern (strcat 'page- 
					  (typecase path
					    ((or string byte-vector) path)
					    (t ()))))))
    `(progn
       (defun ,normal-func-name (&key ,@defaulting-lambda-list)
	 ,@body)
       (defpage-lambda 
	   ,path ',normal-func-name :defaulting-lambda-list ,defaulting-lambda-list)
       ',normal-func-name)))

(defmacro page-link (&optional (page '+action-page-name+) &rest args)
  `(sendbuf-to-byte-vector
    (with-sendbuf (sendbuf)
      ,page
      "?.unique.="
      (random-web-sparse-key 4)
      "&"
      +webapp-frame-id-param+
      "="
      (awhen *webapp-frame*
	(frame-id it))
      ,@(loop for (param val) on args by #'cddr
	      collect "&"
	      collect (symbol-name param)
	      collect "="
	      collect `(percent-hexpair-encode ,val)))))

(defun-speedy random-web-safe-char ()
  (aref +web-safe-chars+ (random (length +web-safe-chars+))))


(defun random-web-sparse-key (length)
  (let ((bv (make-byte-vector length)))
    (loop for i from 0 below length
	  do (setf (aref bv i) (random-web-safe-char)))
    bv))
