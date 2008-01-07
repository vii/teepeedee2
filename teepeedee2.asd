(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(in-package #:teepeedee2.system)

(loop for addon in (directory "addons/*/") do
      (pushnew addon asdf:*central-registry* :test #'equal))

(proclaim '(optimize speed))

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
							    (:file "utils" :depends-on ("macros" "once-only"))
							    (:file "strcat" :depends-on ("macros" "utils"))
							    (:file "my" :depends-on ("macros" "once-only" "strcat" "one-liners"))
							    (:file "byte-vector" :depends-on ("macros" "utils"))
							    (:file "regex" :depends-on ("byte-vector"  "callcc"))
							    (:file "callcc")
							    (:file "quick-queue" :depends-on ("utils" "my"))
							    (:file "timeout" :depends-on ("quick-queue"))))
				     
				     (:module :io
					       :depends-on (:lib)
					       :components (
							    (:file "peer-info")
							    (:file "socket")
							    (:file "recvbuf" :depends-on ("posix-socket"))
							    (:file "sendbuf" :depends-on ("posix-socket"))
							    (:file "posix-socket" :depends-on ("syscalls" "socket"))
							    (:file "con" :depends-on ("peer-info" "sendbuf" "recvbuf"))
							    (:file "mux" :depends-on ("con"))
							    (:file "epoll" :depends-on ("syscalls" "mux"))
							    (:file "syscalls")
							    (:file "protocol" :depends-on ("socket"))))

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
							   (:file "define-dtd")
							   (:file "css" :depends-on ("html"))
							   (:file "html" :depends-on ("define-dtd"))))
				     (:module :webapp
					      :depends-on (:http :ml)
					      :components ((:file "page" :depends-on ("session"))
							   (:file "session" :depends-on ("names"))
							   (:file "names")))
				     (:module :game
					      :depends-on (:webapp :ml )
					      :components ((:file "framework") 
							   (:file "controllers" :depends-on ("framework"))
							   (:file "card")
							   (:file "web" :depends-on ("controllers"))))
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
	       #+sbcl :babel
		      :trivial-garbage
		      :cl-cont
		      :cffi
		      :iterate
		      :fiveam
		      :cl-utilities))
