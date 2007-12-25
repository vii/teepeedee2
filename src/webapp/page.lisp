(in-package #:tpd2.webapp)

(defmacro defpage (path defaulting-lambda-list &body body)
  (let ((normal-func-name (intern (strcat 'page- path)))
	(arg-names (mapcar 'force-first defaulting-lambda-list))
	(arg-values (mapcar (lambda(x)(second (force-list x))) defaulting-lambda-list)))
  `(prog1
     (defun ,normal-func-name (&key ,@defaulting-lambda-list)
       ,@body)
     (dispatcher-register-path *default-dispatcher* ,path
			       (lambda(dispatcher path params)
				 (declare (ignore dispatcher path))
				 (,normal-func-name 
				  ,@(loop for name in arg-names
					  for value in arg-values
					  collect (intern (force-string name) :keyword)
					  collect `(or (cdr-assoc params ,(force-byte-vector name) :test 'byte-vector=-fold-ascii-case)
						       ,value))))))))
				 