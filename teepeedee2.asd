(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(cl:in-package #:teepeedee2.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-fad))

(loop for addon in (remove-if-not 'cl-fad:directory-pathname-p (cl-fad:list-directory "addons"))
      do
      (pushnew addon asdf:*central-registry* :test #'equal))

#-tpd2-debug
(declaim (optimize speed))

#+tpd2-debug
(progn
  (pushnew :tpd2-debug *features*)
  (declaim (optimize debug safety (speed 1)))
  (pushnew :tpd2-debug-assert *features*))

(asdf:defsystem :teepeedee2
  :name "teepeedee2"
  :author "John Fremlin <john@fremlin.org>"
  :version "prerelease"
  :description "Multiprotocol fast networking framework"

  :components (
	       (:module :src
			:components (
				     (:file "packages")
				     (:module :lib
					      :depends-on ("packages")
					      :components (
							    (:file "macros" :depends-on ("once-only" "one-liners")) 
							    (:file "once-only")
							    (:file "one-liners")
							    (:file "utils" :depends-on ("macros" "byte-vector"))
							    (:file "superquote" :depends-on ("utils"))
							    (:file "strcat" :depends-on ("macros" "utils"))
							    (:file "my" :depends-on ("macros" "once-only" "strcat" "one-liners"))
							    (:file "byte-vector" :depends-on ("macros"))
							    (:file "callcc" :depends-on ("macros"))
							    (:file "quick-queue" :depends-on ("utils" "my"))
							    (:file "timeout" :depends-on ("quick-queue"))))
				     
				     (:module :io
					       :depends-on (:lib)
					       :components (
							    (:file "socket")
							    (:file "recvbuf" :depends-on ("socket"))
							    (:file "sendbuf" :depends-on ("socket" "syscalls"))
							    (:file "posix-socket" :depends-on ("syscalls" "socket" "con"))
							    (:file "con" :depends-on ("sendbuf" "recvbuf"))
							    (:file "mux" :depends-on ("con"))
							    (:file "epoll" :depends-on ("syscalls" "mux"))
							    (:file "syscalls")
							    (:file "protocol" :depends-on ("socket" "con"))
							    (:file "repeater" :depends-on ("con" "protocol"))))

				     (:module :http
					      :depends-on (:lib :io)
					      :components (
							   (:file "encoding")
							   (:file "headers")
							   (:file "dispatcher" :depends-on ("servestate"))
							   (:file "servestate")
							   (:file "serve" :depends-on ("encoding" "headers" "dispatcher"))
							   (:file "request" :depends-on ("headers"))))
				     (:module :ml
					      :depends-on (:lib :io)
					      :components (
							   (:file "output")
							   (:file "object-to-ml" :depends-on ("output"))
							   (:file "define-dtd" :depends-on ("object-to-ml"))
							   (:file "css" :depends-on ("html"))
							   (:file "js" :depends-on ("html"))
							   (:file "html" :depends-on ("define-dtd"))
							   (:file "atom" :depends-on ("define-dtd"))
							   (:file "rss" :depends-on ("define-dtd"))))
				     (:module :datastore
					      :depends-on (:lib)
					      :components ((:file "datastore")))
				     (:module :webapp
					      :depends-on (:http :ml)
					      :components ((:file "page" :depends-on ("site"))
							   (:file "list-channel" :depends-on ("simple-channel"))
							   (:file "simple-channel" :depends-on ("channel"))
							   (:file "frame" :depends-on ("names" "list-channel" "simple-channel"))
							   (:file "names")
							   (:file "html-constants")
							   (:file "site")
							   (:file "default-site" :depends-on ("site" "webapp" "js-library"))
							   (:file "js-library" :depends-on ("html-constants" "page"))
							   (:file "actions" :depends-on ("page" "html-constants" "frame"))
							   (:file "channel" :depends-on ("page" "html-constants"))
							   (:file "webapp" :depends-on ("actions"))
							   (:file "message-channel" :depends-on ("channel"))))
				     (:module :game
					      :depends-on (:webapp :ml )
					      :components (
							   (:file "generic")
							   (:file "framework" :depends-on ("generic")) 
							   (:file "controllers" :depends-on ("framework"))
							   (:file "card")
							   (:file "coins" :depends-on ("framework"))
							   (:file "unassigned-controller" :depends-on ("controllers"))
							   (:file "web" :depends-on ("card" "controllers" "unassigned-controller"))
							   ))
				     (:module :small-games
					      :depends-on (:game)
					      :components (
							   (:file "nash-bargain") 
							   (:file "prisoners-dilemma") 
							   (:file "ultimatum")
							   (:file "roshambo")))
				     (:module :blog
					      :depends-on (:webapp :ml :datastore)
					      :components ((:file "entry")
							   (:file "feed" :depends-on ("blog"))
							   (:file "blog" :depends-on ("entry"))))
				     (:module :truc
					      :depends-on (:game)
					      :components ( (:file "truc") (:file "web" :depends-on ("truc"))
							    (:file "robots" :depends-on ("truc")))))))
  :depends-on (
	       :trivial-garbage
	       :cl-cont
	       :cffi
	       :iterate
	       :alexandria
	       :cl-irregsexp
	       :trivial-backtrace
	       :parenscript))
