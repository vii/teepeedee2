(in-package #:tpd2.blog)

(defun wipe-comments ()
  (datastore-close)
  (tpd2.datastore::datastore-delete-all 'comment))

(defun recover-comment-forms (dir)
  (loop for file in (cl-fad:list-directory dir)
	do 
	(wipe-comments)
	(format *trace-output* "recovering comments from ~A~&" file)
	(ignore-errors (time (datastore-use-file file))
		       (format *trace-output* "success recovering comments from ~A~&" file))
	append
	(loop for comment in (datastore-retrieve-all 'comment)
	      collect (datastore-record-constructor-form comment))))

(defun load-comment-forms (forms)
  (wipe-comments)
  (eval `(progn ,@forms)))

(defun comment-similar-p (a b)
  (macrolet ((cmp-fields (&rest fields)
	       `(and
		 ,@(loop for field in fields
			 for accessor = (concat-sym 'comment- field)
			 collect `(equalp (,accessor a) (,accessor b))))))
    (cmp-fields
     author
     text
     entry-index-name)))

(defun clean-duplicate-comments ()
  (let ((comments (copy-list (datastore-retrieve-all 'comment))))
    (let ((table (make-hash-table :test 'equalp)))
      (loop for comment in comments do
	    (macrolet ((m (comment)
			 `(gethash (comment-entry-index-name ,comment) table)
			 ))
	      (if (some (lambda (c) (comment-similar-p c comment)) (m comment))
		  (datastore-delete comment)
		  (push comment (m comment))))))))

(my-defun comment anonymous-p ()
  (equalp (force-byte-vector "Anonymous") (my author)))

(defun save-comments-to-file (filename)
  (with-open-file 
      (*standard-output* filename :direction :output :if-exists :error :if-does-not-exist :create) 
    (with-standard-io-syntax 
      (let ((*package* #.*package*)) 
	(mapcar (lambda (x) (format t "~S~%" x)) 
		(mapcar 'datastore-record-constructor-form (sort (copy-list (datastore-retrieve-all 'comment)) #'>  :key 'comment-time)))))))