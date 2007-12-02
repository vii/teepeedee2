;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.arnesi.system)
    (defpackage :it.bese.arnesi.system
      (:documentation "ASDF System package for ARNESI.")
      (:use :common-lisp :asdf))))

(in-package :it.bese.arnesi.system)

(defsystem :arnesi
  :components ((:static-file "arnesi.asd")
               (:module :src
                :components ((:file "accumulation" :depends-on ("packages" "one-liners"))
                             (:file "asdf" :depends-on ("packages" "io"))
                             (:file "csv" :depends-on ("packages" "string"))
                             (:file "compat" :depends-on ("packages"))
                             (:module :call-cc
                              :components ((:file "interpreter")
                                           (:file "handlers")
                                           (:file "apply")
                                           (:file "generic-functions")
                                           (:file "common-lisp-cc"))
                              :serial t
                              :depends-on ("packages" "walk" "flow-control" "lambda-list" "list" "string" "defclass-struct"))
			     (:file "debug" :depends-on ("accumulation"))
                             (:file "decimal-arithmetic" :depends-on ("packages"))
                             (:file "defclass-struct" :depends-on ("packages" "list"))
                             (:file "flow-control" :depends-on ("packages" "one-liners"))
                             (:file "hash" :depends-on ("packages" "list" "one-liners" "string"))
                             (:file "http" :depends-on ("packages" "vector" "string"))
                             (:file "io" :depends-on ("packages" "flow-control" "string"))
                             (:file "lambda" :depends-on ("packages"))
			     (:file "lambda-list" :depends-on ("packages" "walk"))
			     (:file "lisp1" :depends-on ("packages" "lambda-list" "one-liners" "walk" "unwalk"))
                             (:file "lexenv" :depends-on ("packages" "one-liners"))
                             (:file "list" :depends-on ("packages" "one-liners" "accumulation" "flow-control"))
                             (:file "log" :depends-on ("packages" "numbers" "hash" "io"))
                             (:file "matcher" :depends-on ("packages" "hash" "list" "flow-control" "one-liners"))
                             (:file "mop" :depends-on ("packages" "mopp"))
			     (:file "mopp" :depends-on ("packages" "list" "flow-control"))
                             (:file "numbers" :depends-on ("packages"))
                             (:file "one-liners" :depends-on ("packages"))
                             (:file "packages")
			     (:file "pf-reader" :depends-on ("packages"))
			     (:file "posixenv" :depends-on ("packages"))
                             (:file "queue" :depends-on ("packages"))
                             (:file "sequence" :depends-on ("packages"))
                             (:file "bracket-reader" :depends-on ("list"))
                             (:file "sharpl-reader" :depends-on ("packages" "flow-control" "mopp"))
                             (:file "specials" :depends-on ("packages" "hash"))
                             (:file "string" :depends-on ("packages" "list"))
                             (:file "time" :depends-on ("packages"))
			     (:file "unwalk" :depends-on ("packages" "walk"))
                             (:file "vector" :depends-on ("packages" "flow-control"))
                             (:file "walk" :depends-on ("packages" "list" "mopp" "lexenv" "one-liners")))))
  :properties ((:features "v1.4.0" "v1.4.1" "v1.4.2" "cc-interpreter"
                          "join-strings-return-value" "getenv"))
  :depends-on (:swank))

(defsystem :arnesi.test
  :components ((:module :t
		:components ((:file "accumulation" :depends-on ("suite"))
                             (:file "call-cc" :depends-on ("suite"))
                             (:file "http" :depends-on ("suite"))
                             (:file "log" :depends-on ("suite"))
                             (:file "matcher" :depends-on ("suite"))
                             (:file "numbers" :depends-on ("suite"))
                             (:file "queue" :depends-on ("suite"))
                             (:file "read-macros" :depends-on ("suite"))
                             (:file "string"  :depends-on ("suite"))
                             (:file "sequence" :depends-on ("suite"))
			     (:file "sharpl" :depends-on ("suite"))
                             (:file "flow-control" :depends-on ("suite"))
			     (:file "walk" :depends-on ("suite"))
			     (:file "csv" :depends-on ("suite"))
                             (:file "suite"))))
  :depends-on (:arnesi :FiveAM)
  :in-order-to ((compile-op (load-op :arnesi))))

(defsystem :arnesi.cl-ppcre-extras
  :components ((:module :src
                :components ((:file "cl-ppcre-extras"))))
  :depends-on (:cl-ppcre :arnesi))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :arnesi))))
  (asdf:oos 'asdf:load-op :arnesi.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :it.bese.arnesi))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :arnesi))))
  nil)

;;;; * Introduction

;;;; A collection of various common lisp utilites.

;;;;@include "src/packages.lisp"


;; Copyright (c) 2002-2006 Edward Marco Baringer
;; Copyright (c) 2006 Luca Capello http://luca.pca.it <luca@pca.it>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, Luca Capello, nor
;;    BESE, nor the names of its contributors may be used to endorse
;;    or promote products derived from this software without specific
;;    prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
