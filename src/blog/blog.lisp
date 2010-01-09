(in-package #:tpd2.blog)

(defconstant +max-comment-length+ 8000)

(defstruct blog
  name
  admin-password-file
  dir
  entries
  entries-table
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
    (setf 
     (my entries-table) (make-hash-table :test 'equalp)
     (my entries) 
     (sort
      (iter:iter (iter:for path in (cl-fad:list-directory (my dir)))
		 (let ((filename (force-string path)))
		   (unless (or (find #\# filename) (find #\~ filename))
		     (let ((entry (read-in-entry me (file-namestring filename))))
		       (iter:collect entry)))))
      #'> :key #'entry-time))
    (loop for entry in (my entries)
	  do (setf (gethash (entry-index-name entry) (my entries-table)) entry))
    (my set-page))
  me)

(defun split-into-list-by-comma (str)
  (match-split (progn (* (space)) "," (* (space)))
	       str))

(my-defun blog ready-entries (&key (age (get-universal-time)) tags)
  (loop for e in (my entries)
	when (and (entry-front-page-p e tags) (<= (entry-time e) age))
	collect e))

(my-defun blog atom-feed-url ()
  (byte-vector-cat (my link-base) "feed.atom"))
(my-defun blog rss-feed-url ()
  (byte-vector-cat (my link-base) "feed.rss"))

(my-defun blog post-comment-url ()
  (byte-vector-cat (my link-base) "comment.form"))

(my-defun blog admin-url ()
  (byte-vector-cat (my link-base) "blog-admin"))

(defmacro defpage-lambda-blog (path function &rest args)
  `(defpage-lambda ,path ,function :create-frame nil ,@args))

(my-defun blog set-page ()
  (with-site ((my site))
    (defpage-lambda-blog (my atom-feed-url)
	(lambda (tags)
	  (my atom-feed :tags (split-into-list-by-comma tags))))
    (defpage-lambda-blog (my rss-feed-url)
	(lambda ()
	  (my rss-feed)))

    (defpage-lambda (my admin-url) 
	(lambda (password entry-name)
	  (webapp "Blog administration"
	    (<form :method :post
		   :action (my admin-url)
		   (<p "Password "
		       (<input :type :text :name "password" )
		       (<input :class "plain-submit" :type :submit :value "↵")))
	    (when (and password (equal (force-string password) (force-string (my admin-password))))
	      (let ((comments 
		     (if entry-name
			 (datastore-retrieve-indexed 'comment 'entry-index-name entry-name)
			 (remove-if-not (lambda (comment)
					  (and (typecase (comment-entry-index-name comment)
						 ((or string byte-vector) t))
					       (if-match-bind ((= (my comment-index-prefix)) ":")
							      (comment-entry-index-name comment)))) 
					(datastore-retrieve-all 'comment)))))
		(loop for c in (sort (copy-seq comments) #'> :key #'comment-time)
		      do (<div :class "comment-admin"
			       (output-object-to-ml c)
			       (let ((c c))
				 (html-action-form "Edit comment"
				     ((text (comment-text c)  :type <textarea)
				      (author (comment-author c)))
				   (setf (comment-text c) text
					 (comment-author c) author))
				 (html-action-link "Delete"
				   (datastore-delete c))))))))))

    (defpage-lambda-blog (my post-comment-url)
	(lambda (text author entry-name keep-this-empty .javascript. http-peer-info! all-http-params!)
	  (let ((entry-name (force-string entry-name)))
	    (let ((success 		   
		   (when (and 
			  (zerop (length keep-this-empty))
			  text
			  (not (zerop (length text)))
			  (< (length text) +max-comment-length+)
			  (not (if-match-bind (t (or "[url=" "[URL=")) text))
			  (not (equalp 
				text 
				(ignore-errors (comment-text (first (datastore-retrieve-indexed 'comment 'entry-index-name entry-name)))))))
		     (let ((entry (gethash entry-name (my entries-table))))
		       (when entry
			 (make-comment 
			  :author author
			  :text text
			  :trace-details http-peer-info!
			  :entry-index-name entry-name)
			 (channel-notify entry))
		       t))))
	      (cond 
		(.javascript.
		 (if success
		     (webapp-respond-ajax-body all-http-params!)
		     (tpd2.io:with-sendbuf ()
		       (js-to-bv (alert "Comment rejected.")))))
		(success
		 (webapp "Comment accepted" (<p "Thank you.")))
		(t
		 (webapp "Comment rejected by spam protection"
		   (<p "Sorry for the inconvenience. Please contact the blog owner with a description of the problem."))))))))


    (defpage-lambda-blog (my link-base) 
	(lambda ((age (force-byte-vector (get-universal-time))) (tags))
	  (webapp ((my name) 
		   :head-contents 
		   (with-ml-output
		     (<link :rel "alternate" :type "application/atom+xml" :href (my atom-feed-url))

					; disable the RSS feed as RSS wants to have absolute URLs
					;		     (<link :rel "alternate" :type "application/rss+xml" :href (my rss-feed-url)))

		     ))
	    (let ((age (byte-vector-parse-integer age)))
	      (let ((entries (my ready-entries :age age :tags (split-into-list-by-comma tags))) (count 10))
		(<div :class "blog"
		      (loop while entries
			    repeat count
			    do 
			    (let ((entry (pop entries)))
			      (<h2 (<a :href (entry-url-path entry) (entry-title entry)))
			      (output-object-to-ml entry)))
		      (when entries
			(<h3 :class "next-entries" (<a :href (page-link (my link-base) :age (force-byte-vector (entry-time (first entries))) :tags (force-byte-vector tags)) "More entries")))))))))))

(my-defun blog last-updated ()
  (loop for e in (my entries)
	when (entry-front-page-p e)
	maximizing (entry-time e)))
