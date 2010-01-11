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

(defun generate-args-for-defpage-from-params (&key defaulting-lambda-list 
					      basic-call create-frame)
  `(with-http-params (,@defaulting-lambda-list (*webapp-frame* nil :name ,+webapp-frame-id-param+ :conv find-frame))
     (when ,(if create-frame t `*webapp-frame*)
       (setf (frame-trace-info (webapp-frame :site (current-site))) 
	     (servestate-origin*))
       (frame-reset-timeout (webapp-frame)))
     
     (,@basic-call ,@(loop for p in defaulting-lambda-list for n = (force-first p)
			   collect (intern (force-string n) :keyword)
			   collect n))))

(defmacro apply-page-call ((&key con function create-frame) &rest args)
  (declare (ignore con))
  (let* ((defaulting-lambda-list (car (last args)))
	 (normal-args (butlast args)))
    (generate-args-for-defpage-from-params 
     :defaulting-lambda-list defaulting-lambda-list
     :basic-call `(funcall ,function ,@normal-args)
     :create-frame create-frame)))
 
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
			     (lambda(dispatcher con done)
			       (declare (ignore dispatcher))
			       (start-http-response)
			       (send-http-response con done
				(apply-page-call
				 (:con con :function ,function :create-frame ,create-frame)  ,defaulting-lambda-list))))))

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
      "?.=" ; sometimes a unique string to stop overeager caches
      (when (webapp-frame-available-p)
	     (with-sendbuf-continue (sendbuf)
	       (random-web-sparse-key 4)
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
