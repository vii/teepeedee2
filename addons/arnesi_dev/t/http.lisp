;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.http :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.http)

(test escape-uri
  (for-all ((uri (gen-string :elements (gen-character :code-limit #16rffff))))
    (is (string= uri (unescape-as-uri (escape-as-uri uri)))))

  (is (string= (unescape-as-uri "a+b+c")
               "a b c")))

(defmacro help-test-bad-uri (uri expected-error)
  `(progn
     (signals ,expected-error
       (unescape-as-uri ,uri))
     (finishes
       (unescape-as-uri-non-strict ,uri))
     (let ((returned (unescape-as-uri-non-strict ,uri)))
       (is (> (length returned) (* 0.5 (length ,uri)))) ; a big chunk should be returned
       (is (string= (subseq returned 0 8) ; that is looking like a proper url
                    (subseq ,uri 0 8))))))

(test unescape-uri/iso8859-1-instead-of-utf8
  (help-test-bad-uri "http://router.advertising.se/?&CHANNEL_ID=1&SITE_KEY=Webbhotell%20f%F6r%20att%20placera%20en%20Tower%20server?&SITE_ALT_KEY=&SITE_URL=http%3A%2F%2Fwww.webmasternetwork.se%2Ff13t11622.html&REF=http%3A%2F%2Fwww.webmasternetwork.se%2Ff13.html"
                     error))

(test unescape-uri/wrong-percentage-quoting
  (help-test-bad-uri "http://ad.doubleclick.net/adi/N763.business_week_online/B1803870.12;sz=468x60;ord=%%REALREAND%%?"
                     expected-digit-uri-parse-error))

(test unescape-uri/percentage-at-end
  (help-test-bad-uri "http://groups.google.com/groups/adfetch?adid=zMKqMREAAAAwVvp0Nmmxmm2KqccSr5KzFSRgCP-avRN4YT0eROC0jw&hl=en&sabc=%23eeeeee&sabcg=239&siphc=%23999999&siphfc=%23ffffff&w=100%"
                     uri-parse-error))
