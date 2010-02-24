;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cont-asd
  (:use :cl :asdf))

(in-package :cl-cont-asd)

(defsystem cl-cont
  :name "cl-cont"
  :version "0.3.8"
  :maintainer "Leslie P. Polzer <polzer@gnu.org>"
  :author "Slava Akhmechet, Stephen Compall <s11@member.fsf.org>, John Fremlin, Leslie P. Polzer <polzer@gnu.org>"
  :licence "LLGPL"
  :description "A library that implements continuations by
  transforming Common Lisp code to continuation passing style."
  :depends-on (:closer-mop :alexandria)
  :in-order-to ((asdf:test-op (load-op :cl-cont-test)))
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

(defmethod perform ((o asdf:test-op) (c (eql (find-system :cl-cont))))
  (funcall (intern "TEST-CONT" :cl-cont-test)))

