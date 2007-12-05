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
  (test-match-bind-enters-body ((w (integer))) "666"
    (is (= 666 w)))
  (test-match-bind-enters-body ((i (integer)) "." (f (integer)) " ") "3.14159 "
    (is (= 3 i))
    (is (= 14159 f))))
  
(test http-request-line
  (dolist (request-line '("GET / HTTP/1.0" "GET   /index.html  "))
    (test-match-bind-enters-body (method :whitespace url :whitespace?
					 (:? "HTTP/" (version-major (integer) 1) "." (version-minor (integer) 0) :whitespace?) 
					 :$)
			       request-line
			       (is (string= (force-string method) "GET"))
			       (is (= 1 version-major))
			       (is (= 0 version-minor))
			       ))

  (test-match-bind-enters-body (method :whitespace url :whitespace?
				       (:? "HTTP/" (version-major (integer) 1) "." (version-minor (integer) 0) :whitespace?) 
				       :$)
      "POST    /index.php?   HTTP/901.121  "
    (is (string= (force-string method) "POST"))
    (is (string= (force-string url) "/index.php?"))
    (is (= 901 version-major))
    (is (= 121 version-minor))))