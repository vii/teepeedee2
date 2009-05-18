(in-package #:tpd2.blog)

(defstruct blog
  name
  admin-password-file
  dir
  entries
  (site (current-site))
  (link-base "/")
  comment-index-prefix
  static-base-url)

(my-defun blog admin-password ()
	  (awhen (my admin-password-file)
	    (with-open-file (stream it)
	      (read-line stream))))

(my-defun blog read-in ()
  (with-site ((my site)) 
    (setf (my entries) 
	  (sort
	   (iter:iter (iter:for path in (cl-fad:list-directory (my dir)))
		      (let ((filename (force-string path)))
			(unless (or (find #\# filename) (find #\~ filename))
			  (let ((entry (read-in-entry me (file-namestring filename))))
			    (iter:collect entry)))))
		      #'> :key #'entry-time))
    (my set-page))
  me)

(my-defun blog ready-entries (&key (start 0))
	  (subseq (remove-if-not 'entry-ready (my entries)) start))

(my-defun blog feed-url ()
	  (byte-vector-cat (my link-base) "feed.atom"))
(my-defun blog admin-url ()
	  (byte-vector-cat (my link-base) "blog-admin"))

(my-defun blog set-page ()
  (with-site ((my site))
    (defpage-lambda (my feed-url)
	(lambda ()
	  (my feed)))

    (defpage-lambda (my admin-url) 
	(lambda (password entry-name)
	  (webapp "Blog administration"
		  (when (and password (equal (force-string password) (force-string (my admin-password))))
		    (let ((comments 
			   (if entry-name
			       (datastore-retrieve-indexed 'comment 'entry-index-name entry-name)
			       (remove-if-not (lambda (comment)
						(and (typecase (comment-entry-index-name comment)
						       ((or string byte-vector) t))
						     (if-match-bind ((= (my comment-index-prefix)))
								    (comment-entry-index-name comment)))) 
					      (datastore-retrieve-all 'comment)))))
		      (loop for c in comments
			    do (<div :class "comment-admin"
				     (output-object-to-ml c)
				     (let ((c c))
				       (html-action-form "Edit comment"
							 ((text (comment-text c)  :type <textarea)
							  (author (comment-author c)))
							 (setf (comment-text c) text
							       (comment-author c) author)
							 (webapp "Changed"))
				       (html-replace-link "Delete"
							  (webapp "Deleting comment"
								  (output-object-to-ml c)
								  (datastore-delete c)))))))))))

    (defpage-lambda (my link-base) 
	(lambda ((n (force-byte-vector 0)))
	  (webapp ((my name) :head-contents (<link :rel "alternate" :type "application/atom+xml" :href (my feed-url)))
	    (let ((n (byte-vector-parse-integer n)))
	      (let ((entries (my ready-entries :start n)) (count 10))
		(<div :class "blog"
		      (loop while entries
			    repeat count
			    do 
			    (let ((entry (pop entries)))
			      (<h2 (<a :href (entry-url-path entry) (entry-title entry)))
			      (output-object-to-ml entry)))
		      (when entries
			(<p :class "next-entries" (<a :href (page-link (my link-base) :n (force-byte-vector (+ n count))) "More entries")))))))))))
    
(my-defun blog last-updated ()
	  (loop for e in (my entries)
		when (entry-ready e)
		maximizing (entry-time e)))