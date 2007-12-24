;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cont-test-asd
  (:use :cl :asdf))

(in-package :cl-cont-test-asd)

(defsystem cl-cont-test
  :name "cl-cont-test"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A test harness for cl-cont library."
  :depends-on (:cl-cont :rt)
  :components ((:module
		test
		:components
		((:file "cont-test")
		 (:file "cases"
			:depends-on ("cont-test"))
		 (:file "helper-cases"
			:depends-on ("cont-test"))))))


