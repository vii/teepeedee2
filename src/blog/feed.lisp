(in-package #:tpd2.blog)

(my-defun blog atom-feed (&key (count 10) tags)
  (values
   (with-ml-output-start
     (output-raw-ml "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
     (tpd2.ml.atom:<feed :xmlns "http://www.w3.org/2005/Atom"
                         (tpd2.ml.atom:<title
                           (my name))
                         (tpd2.ml.atom:<link
                           :href (my link-base))
                         (tpd2.ml.atom:<updated
                           (w3c-timestring (my last-updated)))
                         (loop repeat count
                               for entry in (my ready-entries :tags tags) do
                               (tpd2.ml.atom:<entry
                                 (tpd2.ml.atom:<title (entry-title entry))
                                 (tpd2.ml.atom:<updated (w3c-timestring (entry-time entry)))
                                 (tpd2.ml.atom:<id (entry-url-path entry))
                                 (tpd2.ml.atom:<link :href (entry-url-path entry))
                                 (tpd2.ml.atom:<content :type "html"
                                                        (tpd2.io:sendbuf-to-byte-vector (entry-story-ml entry)))))))
   (byte-vector-cat "Content-Type: application/atom+xml" tpd2.io:+newline+)))


(my-defun blog rss-feed (&key (count 10) tags)
  (values
   (with-ml-output-start
     (output-raw-ml "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
     (tpd2.ml.rss:<rss :version "2.0"
                       (tpd2.ml.rss:<channel
                         (tpd2.ml.rss:<description
                           (my name))
                         (tpd2.ml.rss:<title
                           (my name))
                         (tpd2.ml.rss:<link
                           (my link-base))
                         (loop repeat count
                               for entry in (my ready-entries :tags tags) do
                               (tpd2.ml.rss:<item
                                 (tpd2.ml.rss:<title (entry-title entry))
                                 (tpd2.ml.rss:<link (entry-url-path entry))
                                 (tpd2.ml.rss:<description
                                   (tpd2.io:sendbuf-to-byte-vector (entry-story-ml entry))))))))
   (byte-vector-cat "Content-Type: application/rss+xml" tpd2.io:+newline+)))
