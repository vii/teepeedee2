;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cont-asd
  (:use :cl :asdf))

(in-package :cl-cont-asd)

(defsystem cl-cont
  :name "cl-cont"
  :version "0.3.7"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A library that implements continuations by
  transforming Common Lisp code to continuation passing style."
  :depends-on (:closer-mop)
  :components ((:module
		src
		:components
		((:file "cont")
		 (:file "special-transformers"
			:depends-on ("cont"))
		 (:file "helper-transformers"
			:depends-on ("cont" "special-transformers"))
		 (:file "walker"
			:depends-on ("cont" "special-transformers"))))))

