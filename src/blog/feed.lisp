(in-package #:tpd2.blog)

(my-defun blog feed (&key (count 10))
	  (values
	   (with-ml-output-start 
	       (output-raw-ml "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
	     (tpd2.ml.atom:<feed 
	      (tpd2.ml.atom:<title
	       (my name))
	      (tpd2.ml.atom:<link
	       :href (my link-base))
	      (tpd2.ml.atom:<updated 
	       (w3c-timestring (my last-updated)))
	      (loop repeat count
		    for entry in (my ready-entries) do
		    (tpd2.ml.atom:<entry 
		     (tpd2.ml.atom:<title (entry-title entry))
		     (tpd2.ml.atom:<updated (w3c-timestring (entry-time entry)))
		     (tpd2.ml.atom:<id (entry-url-path entry))
		     (tpd2.ml.atom:<link (entry-url-path entry))
		     (tpd2.ml.atom:<content :type "html" 
					    (tpd2.io:sendbuf-to-byte-vector (entry-story-ml entry)))))))
	   (byte-vector-cat "Content-Type: application/atom+xml" tpd2.io:+newline+)))