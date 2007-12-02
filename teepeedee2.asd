(loop for addon in (directory "addons/*/") do
      (pushnew addon asdf:*central-registry* :test #'equal))

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
							    (:file "my" :depends-on ("macros" "once-only" "strcat" "one-liners"))))
				     (:module :io
					       :depends-on (:lib "packages")
					       :components (
							    (:file "byte-vector")
							    (:file "peer-info")
							    (:file "recvbuf" :depends-on ("posix-socket"))
							    (:file "sendbuf" :depends-on ("posix-socket"))
							    (:file "posix-socket" :depends-on ("byte-vector" "syscalls"))
							    (:file "con" :depends-on ("byte-vector" "peer-info" "sendbuf" "recvbuf"))
							    (:file "mux" :depends-on ("con"))
							    (:file "epoll" :depends-on ("syscalls" "mux"))
							    (:file "syscalls")
							    (:file "protocol")
							    (:file "echo-line" :depends-on ("con" "protocol"))
							    )))))
  :depends-on (
	       #+sbcl :babel
		      :trivial-garbage
		      :arnesi
		      :cffi
		      :fiveam
		      :parenscript
		      :cl-utilities))
