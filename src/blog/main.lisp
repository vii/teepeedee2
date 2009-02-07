(in-package #:tpd2.blog)

(defvar *root-dir* "/home/john/Junk/mopoko/")
(defvar *blog-dir* (strcat *root-dir* "/Blog/"))

(datastore-use-file (strcat *root-dir* "tpd2-datastore.log.lisp"))

(make-blog 

(defun css ()
  (let ((unimportant-color "#888888"))
    (css-html-style
      ((".inherit" <input <a)
       :text-decoration "inherit" :color "inherit" :background-color "inherit" :font-size "inherit" :font-weight "inherit"
       :font-family "inherit" 
       :border "none" :padding "0 0 0 0" :margin "0 0 0 0")
      (<body :font-family "verdana, sans-serif" :word-spacing "0.075em" :letter-spacing "0.010em" :color "black" :background-color "white")
      ("p + p" :text-indent "1em")
      ("p" :line-height "1.4em" :letter-spacing "0.007em" :word-spacing "0.0025em")
      ((<h1 <h2 <h3 <h4 <h5 <h6) :letter-spacing "0.05em" :font-weight "normal" :margin "0 0 0 0" :padding "0 0 0 0")
      ((<span <div <h1 <h2 <h3 <h4 <h5 <h6 <p <a <input) :direction "ltr" :unicode-bidi "bidi-override")
      ("h2.estaircase" :text-align "right" :border-bottom "thin solid black" :margin-bottom "0.5em" :color unimportant-color)
      (<h1 :font-size "3em")
      (<h2 :font-size "2em")
      (".blog-entry"
       :margin-left "5%" :margin-right "5%"
       :font-family "georgia, serif"
       :x-column-width "20em"
       :x-column-gap "2em")
      ("input[type=text]" 
       :display "inline"
       :border-bottom "thin dashed black" 
       :font-style "italic" )
      (".robot" :font-style "italic")
      ("[onclick],a,input[type=submit]" 
       :cursor "pointer"
       :color "blue")
      ("p.time" 
       :color unimportant-color))))


(defrecord location
    (time :initform (get-universal-time))
  description 
  latitude
  longitude)

(defrecord phone-contact
    string)

(my-defun location coordinates ()
  (tpd2.io:with-sendbuf ()
    (abs (my longitude))
    (if (> 0 (my longitude)) "S"
	"N")
    (abs (my latitude))
    (if (> 0 (my latitude)) "E"
	"W")))

(my-defun location 'object-to-ml ()
  (<p :class "location"
      (<a :href 
	  (tpd2.io:sendbuf-to-byte-vector 
	   (tpd2.io:with-sendbuf () "http://maps.google.com/?q="
			 (tpd2.http:percent-hexpair-encode
			  (if (my latitude)
			      (tpd2.io:sendbuf-to-byte-vector (my coordinates))
			      (my description)))))
	  (my description)
      
	  (when (my latitude)
	    (<span :class "coordinates"
		   (my coordinates))))
      " as of " (time-string (my time)) "."))

(my-defun phone-contact 'object-to-ml ()
  (<p :class "phone-number" (my string)))


(with-site (:dispatcher "127.0.0.1:8888"
			:page-body-start 
			(lambda(title)
			  `(<div :class "header"
				      (<h2 :class "estaircase" 
					   (<A :href (page-link "/") 
					       :class "inherit" "E Staircase"))
				      (<h1 :class "title" ,title)))
			:page-head (lambda(title)
				     `(<head
					(<title ,title)
					(css)
					(webapp-default-page-head-contents))))

  (defpage "/" ()
    (webapp "The E Staircase Blog"
      (<p (<A :href (page-link "FAQ") "What is this?") " " 
	  (<A :href (page-link "Location") "Where am I?") " " 
	  (<a :href "http://john.fremlin.org" "Who am I?"))
      (loop for i below 10
	    for b in *blog-entries*
	    do
	    (output-object-to-ml b))))

  (defpage "/FAQ" ()
    (webapp "About the blog"
      (<p "This blog is firstly to keep my family and friends updated about my travels, "
	  "and secondly to disseminate information and ideas that deserve to be better known.")
      (<p "It is named for E Staircase and the people there.")))

  (defpage "/Location" ()
    (webapp "Where I am"
      (loop for l in (datastore-retrieve-all 'location 10) do
	    (output-object-to-ml l))
      
      (loop for l in (datastore-retrieve-all 'phone-contact 1) do
	    (<h2 "What telephone number to call to talk to me")
	    (output-object-to-ml l)))))
