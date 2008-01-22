(in-package #:tpd2.test)

(def-suite regex :in :tpd2)
(in-suite regex)

(defmacro test-match-bind-enters-body (bindings string &body body)
  (with-unique-names (did-it)
    `(let (,did-it)
       (match-bind ,bindings
	   ,string
	   (setf ,did-it t)
	 ,@body)
       (is (not (not ,did-it))))))

(test value 
  (test-match-bind-enters-body ("this") "this")
  
  (signals match-failed
    (match-bind ("this") "that")))

(test whitespace
  (test-match-bind-enters-body (:s) " x")

  (signals match-failed
    (match-bind (:s) "x ")))

(test word
  (test-match-bind-enters-body (w) "x "
    (is (string= (force-string w) "x")))

  (test-match-bind-enters-body (w) "one word"
    (is (string= (force-string w) "one"))))

(test integer
  (test-match-bind-enters-body ((w :integer)) "666"
    (is (= 666 w)))
  (test-match-bind-enters-body ((i (:integer)) "." (f (:integer)) " ") "3.14159 "
    (is (= 3 i))
    (is (= 14159 f))))
  
(test http-request-line
  (dolist (request-line '("GET / HTTP/1.0" "GET   /index.html  "))
    (test-match-bind-enters-body (method :whitespace url :whitespace?
					 (:? "HTTP/" (version-major (:integer) 1) "." (version-minor (:integer) 0) :whitespace?) 
					 :$)
			       request-line
			       (is (string= (force-string method) "GET"))
			       (is (= 1 version-major))
			       (is (= 0 version-minor))
			       ))

  (test-match-bind-enters-body (method :whitespace url :whitespace?
				       (:? "HTTP/" (version-major (:integer) 1) "." (version-minor (:integer) 0) :whitespace?) 
				       :$)
      "POST    /index.php?   HTTP/901.121  "
    (is (string= (force-string method) "POST"))
    (is (string= (force-string url) "/index.php?"))
    (is (= 901 version-major))
    (is (= 121 version-minor))))


(test until-match
  (test-match-bind-enters-body
      ((left (:until-and-eat :whitespace? ":" :whitespace?))
       (right (:until :whitespace? :$)))
      "X-Header: x y "))

(test http-header
  (loop for (name . value) in '(
				  ("User-Agent" . "curl/7.16.4 (i486-pc-linux-gnu) libcurl/7.16.4 OpenSSL/0.9.8e zlib/1.2.3.3 libidn/1.0")
				  ("Host" . "localhost:9999")
				  ("Accept" . "*/*"))
	do
	(loop for ws in '("" "  " "                                         ") do 
	      (test-match-bind-enters-body
		  ((rname (:until-and-eat :whitespace? ":" :whitespace?))
		   (rvalue (:until :whitespace? :$)))
		  (strcat name ws ":" ws value ws)
		  (is (string= (force-string rname) name))
		  (is (string= (force-string rvalue) value))))))
		  
	    
(test match-or
  (test-match-bind-enters-body
      ("c" (w (:or (:integer) "a" "b")) "b")
      "cab"
    (is (string= (force-string w) "a")))

  (signals match-failed
    (match-bind
	("c" (w (:or (:integer) "a" "b")) "b")
	"cub"))

  (test-match-bind-enters-body
      ("c" (w (:or? (:integer) "a" "b")) "ub")
      "cub"))


(test match-replace-all
  (flet ((r (value)
	   (match-replace-all value
			      #\< "&lt;"
			      #\> "&gt;"
			      #\& "&amp;"
			      #\' "&#39;")))
    (is (string= "&amp;" (force-string (r "&"))))

    (is (string= "&lt;h1&gt;The Good, the &#39;Bad&#39; &amp; the ugly&lt;/h1&gt;"
		 (force-string (r "<h1>The Good, the 'Bad' & the ugly</h1>"))))))
