(asdf:defsystem :teepeedee2-test
  :name "teepeedee2 tests"
  :author "John Fremlin <john@fremlin.org>"
  :version "prerelease"
  :description "Tests for teepeedee2"
  :components ((:module :t 
	   
			:components (
				     (:file "suite")
				     (:file "io" :depends-on ("suite"))
				     (:file "http" :depends-on ("suite"))
				     )))
  :depends-on (
	       :fiveam
	       :teepeedee2))