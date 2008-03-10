(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(in-package #:teepeedee2.system)

(loop for addon in (directory "addons/*/") do
      (pushnew addon asdf:*central-registry* :test #'equal))

(proclaim '(optimize debug))

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
							    (:file "utils" :depends-on ("utf8"))
							    (:file "utf8" :depends-on ("macros" "byte-vector"))
							    (:file "superquote" :depends-on ("utils"))
							    (:file "strcat" :depends-on ("macros" "utils"))
							    (:file "my" :depends-on ("macros" "once-only" "strcat" "one-liners"))
							    (:file "byte-vector" :depends-on ("macros"))
							    (:file "regex" :depends-on ("byte-vector"  "callcc"))
							    (:file "callcc")
							    (:file "quick-queue" :depends-on ("utils" "my"))
							    (:file "timeout" :depends-on ("quick-queue"))))
				     
				     (:module :io
					       :depends-on (:lib)
					       :components (
							    (:file "peer-info")
							    (:file "socket")
							    (:file "recvbuf" :depends-on ("socket"))
							    (:file "sendbuf" :depends-on ("socket" "syscalls"))
							    (:file "posix-socket" :depends-on ("syscalls" "socket" "con"))
							    (:file "con" :depends-on ("peer-info" "sendbuf" "recvbuf"))
							    (:file "mux" :depends-on ("con"))
							    (:file "epoll" :depends-on ("syscalls" "mux"))
							    (:file "syscalls")
							    (:file "protocol" :depends-on ("socket"))
							    (:file "repeater" :depends-on ("con" "protocol"))))

				     (:module :http
					      :depends-on (:lib :io)
					      :components (
							   (:file "encoding")
							   (:file "headers")
							   (:file "dispatcher")
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
							   (:file "html" :depends-on ("define-dtd"))))
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
							   (:file "js-library" :depends-on ("html-constants" "page"))
							   (:file "actions" :depends-on ("page" "html-constants" "frame"))
							   (:file "channel" :depends-on ("page" "html-constants"))
							   (:file "webapp" :depends-on ("actions"))
							   (:file "message-channel" :depends-on ("channel"))))
				     (:module :game
					      :depends-on (:webapp :ml )
					      :components ((:file "framework") 
							   (:file "controllers" :depends-on ("framework"))
							   (:file "card")
							   (:file "unassigned-controller" :depends-on ("controllers"))
							   (:file "web" :depends-on ("controllers" "unassigned-controller"))))
				     (:module :truc
					      :depends-on (:game)
					      :components ( (:file "truc") (:file "web" :depends-on ("truc"))
							    (:file "robots" :depends-on ("truc"))))))

	       (:module :t 
			:depends-on (:src)
			:components (
				     (:file "suite")
				     (:file "io" :depends-on ("suite"))
				     (:file "regex" :depends-on ("suite")))))
  :depends-on (
	       :trivial-garbage
	       :cl-cont
	       :cffi
	       :iterate
	       :fiveam
	       :cl-utilities
	       :parenscript))
