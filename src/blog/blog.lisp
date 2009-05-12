(in-package #:tpd2.blog)

(defstruct blog
  name
  dir
  entries
  (site (current-site))
  (link-base "/")
  comment-index-prefix
  static-base-url)

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

(my-defun blog set-page ()
  (with-site ((my site))
    (defpage-lambda (my feed-url)
	(lambda ()
	  (my feed)))

    (defpage-lambda (my link-base) 
	(lambda(&key n)
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
			(<p :class "next-entries" (<a :href (page-link (my link-base) :n (force-byte-vector (+ n count))) "More entries"))))))))
      ((n (force-byte-vector 0))))))
    
(my-defun blog last-updated ()
	  (loop for e in (my entries)
		when (entry-ready e)
		maximizing (entry-time e)))