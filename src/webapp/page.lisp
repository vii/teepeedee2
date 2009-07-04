(in-package #:tpd2.webapp)

(defvar *webapp-frame*)
(defconstant-bv +webapp-frame-id-param+ ".webapp-frame.")

(defun-speedy get-http-param (params name)
  (alist-get params name :test #'byte-vector=-fold-ascii-case))

(define-constant +web-safe-chars+ 
  (force-byte-vector 
   (append (loop for c from (char-code #\A) to (char-code #\Z) collect c)
	   (loop for c from (char-code #\a) to (char-code #\z) collect c)
	   (loop for c from (char-code #\0) to (char-code #\9) collect c)
	   (mapcar 'char-code '(#\- #\_))))
  :test 'equalp)

(defun generate-args-for-defpage-from-params (&key params-var con-var defaulting-lambda-list)
  (declare (ignore con-var))
  (let ((arg-names (mapcar 'force-first defaulting-lambda-list))
	(arg-values (mapcar (lambda(x)(second (force-list x))) defaulting-lambda-list)))
    (flet ((xlate (name)
	     (case name 
	       (all-http-params! params-var))))
     (loop for name in arg-names
	   for value in arg-values
	   for xlated = (xlate name)
	   collect (intern (force-string name) :keyword)
	   if xlated
	   collect xlated 
	   else
	   collect (let ((val-form `(get-http-param ,params-var ,(force-byte-vector name))))
		     (if value
			 `(or ,val-form
			      ,value)
			 val-form))))))

(defmacro with-webapp-frame ((params &key (create-frame t)) &body body)
  (check-symbols params)
  `(let ((*webapp-frame*
	  (awhen (get-http-param ,params +webapp-frame-id-param+)
		 (find-frame it))))
     (when ,(if create-frame t `(webapp-frame-available-p))
       (setf (frame-trace-info (webapp-frame :site (current-site))) 
	     (get-http-param
	      ,params tpd2.http:+http-param-origin+))
       (frame-reset-timeout (webapp-frame)))
     (locally
	 ,@body)))

(defmacro apply-page-call-without-frame (con function &rest args)
  (let* ((defaulting-lambda-list (car (last args)))
	 (normal-args (butlast args)))
    `(funcall ,function ,@normal-args ,@(generate-args-for-defpage-from-params 
					 :con-var con
					 :params-var 'all-http-params! 
					 :defaulting-lambda-list defaulting-lambda-list))))

(defmacro apply-page-call ((&key con function create-frame) &rest args)
  `(with-webapp-frame (all-http-params! :create-frame ,create-frame)
     (apply-page-call-without-frame ,con ,function ,@args)))
 
(defmacro defpage-lambda (path function &key defaulting-lambda-list (create-frame t))
  (multiple-value-bind (function defaulting-lambda-list)
      (cond ((and 
	      (not defaulting-lambda-list)
	      (listp function)
	      (eq (first function) 'lambda))
	     (let ((args (second function)))
	      (values `(lambda (&key ,@args)
			 ,@(cddr function))
		      args)))
	    (t
	     (values function defaulting-lambda-list)))
    `(dispatcher-register-path (site-dispatcher (current-site)) ,path
			     (lambda(dispatcher con done path all-http-params!)
			       (declare (ignore dispatcher path))
			       (declare (ignorable all-http-params!))
			       (multiple-value-bind (body headers)
				   (apply-page-call
				    (:con con :function ,function :create-frame ,create-frame)  ,defaulting-lambda-list)
				 (respond-http con done :body body :headers headers))))))

(defmacro defpage (path defaulting-lambda-list &body body)
  (let ((normal-func-name (intern (strcat 'page- 
					  (typecase path
					    ((or string byte-vector) path)
					    (t ()))))))
    `(progn
       ;; for some reason inlining this |PAGE-...| thing makes it very slow on SBCL
       (defun ,normal-func-name (&key ,@defaulting-lambda-list)
	 ,@body)
       (defpage-lambda 
	   ,path #',normal-func-name :defaulting-lambda-list ,defaulting-lambda-list ,@(loop for (key value) on body by #'cddr while (keywordp key) collect key collect value))
       ',normal-func-name)))

(defmacro page-link (&optional (page '+action-page-name+) &rest args)
  `(sendbuf-to-byte-vector
    (with-sendbuf (sendbuf)
      ,page
      "?.unique.="
      (random-web-sparse-key 4)
      (when (webapp-frame-available-p)
	     (with-sendbuf-continue (sendbuf)
	       "&"
	       +webapp-frame-id-param+
	       "="
	      (frame-id (webapp-frame))))
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
