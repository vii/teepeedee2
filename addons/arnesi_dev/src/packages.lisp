;; -*- lisp -*-

(in-package :common-lisp-user)

;;;; * Introduction

;;;; It is a collection of lots of small bits and pieces which have
;;;; proven themselves usefull in various applications. They are all
;;;; tested, some even have a test suite and a few are even
;;;; documentated.

(defpackage :it.bese.arnesi
  (:documentation "The arnesi utility suite.")
  (:nicknames :arnesi)
  (:use :common-lisp)
  (:export

   #:clean-op
   #:collect-timing

   #:make-reducer
   #:make-pusher
   #:make-collector
   #:with-reducer
   #:with-collector
   #:with-collectors

   #:form
   #:walk-form
   #:make-walk-env
   #:*walk-handlers*
   #:*warn-undefined*
   #:undefined-reference
   #:undefined-variable-reference
   #:undefined-function-reference
   #:return-from-unknown-block
   #:defwalker-handler
   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:declaration-form
   #:constant-form
   #:variable-reference
   #:local-variable-reference
   #:local-lexical-variable-reference
   #:free-variable-reference
   #:application-form
   #:local-application-form
   #:lexical-application-form
   #:free-application-form
   #:lambda-application-form
   #:function-form
   #:lambda-function-form
   #:function-object-form
   #:local-function-object-form
   #:free-function-object-form
   #:lexical-function-object-form
   #:function-argument-form
   #:required-function-argument-form
   #:specialized-function-argument-form
   #:optional-function-argument-form
   #:keyword-function-argument-form
   #:allow-other-keys-function-argument-form
   #:rest-function-argument-form
   #:block-form
   #:return-from-form
   #:catch-form
   #:throw-form
   #:eval-when-form
   #:if-form
   #:function-binding-form
   #:flet-form
   #:labels-form
   #:variable-binding-form
   #:let-form
   #:let*-form
   #:locally-form
   #:macrolet-form
   #:multiple-value-call-form
   #:multiple-value-prog1-form
   #:progn-form
   #:progv-form
   #:setq-form
   #:symbol-macrolet-form
   #:tagbody-form
   #:go-tag-form
   #:go-form
   #:the-form
   #:unwind-protect-form
   #:extract-argument-names
   #:walk-lambda-list
   #:walk-implict-progn
   #:arguments
   #:binds
   #:body
   #:cleanup-form
   #:code
   #:consequent
   #:declares
   #:default-value
;; #:else ; iterate
   #:enclosing-tagbody
   #:eval-when-times
   #:first-form
   #:func
   #:keyword-name
   #:name
   #:operator
   #:optimize-spec
   #:other-forms
   #:parent
   #:protected-form
   #:read-only-p
   #:result
   #:source
;; #:specializer ; closer-mop
   #:supplied-p-parameter
   #:tag
   #:target-block
   #:target-progn
   #:then
   #:type-form
   #:value
   #:values-form
   #:var
   #:vars-form

   #:defunwalker-handler
   #:unwalk-form
   #:unwalk-forms
   #:unwalk-lambda-list

   #:to-cps
   #:with-call/cc
   #:kall
   #:call/cc
   #:let/cc
   #:*call/cc-returns*
   #:invalid-return-from
   #:unreachable-code
   #:defun/cc
   #:defgeneric/cc
   #:defmethod/cc
   #:fmakun-cc
   #:*debug-evaluate/cc*
   #:*trace-cc*
   
   #:ppm
   #:ppm1
   #:apropos-list*
   #:apropos*

   #:with-input-from-file
   #:with-output-to-file
   #:read-string-from-file
   #:write-string-to-file
   #:copy-file
   #:copy-stream
   #:string-to-octets
   #:octets-to-string
   #:encoding-keyword-to-native
   #:defprint-object
   
   #:if-bind
   #:aif
   #:when-bind
   #:awhen
   #:cond-bind
   #:acond
   #:aand
   #:and-bind
   #:if2-bind
   #:aif2
;; #:while ; iterate
   #:awhile
;; #:until ; iterate
   #:it
   #:whichever
   #:xor
   #:switch
   #:eswitch
   #:cswitch

   #:build-hash-table
   #:deflookup-table
   #:hash-to-alist
   #:hash-table-keys
   #:hash-table-values
   
   #:write-as-uri
   #:escape-as-uri
   #:unescape-as-uri
   #:nunescape-as-uri
   #:unescape-as-uri-non-strict
   #:uri-parse-error
   #:expected-digit-uri-parse-error
   #:continue-as-is
   
   #:write-as-html
   #:escape-as-html
   #:unescape-as-html
   #:html-entity->char
   
   #:compose
   #:conjoin
   #:curry
   #:rcurry
   #:noop
   #:y
   #:lambda-rec

   #:dolist*
   #:dotree
   #:ensure-list
   #:ensure-cons
   #:partition
   #:partitionx
   #:proper-list-p
   #:push*

   #:get-logger
   #:log-category
   #:stream-log-appender
   #:brief-stream-log-appender
   #:verbose-stream-log-appender
   #:make-stream-log-appender
   #:make-slime-repl-log-appender
   #:file-log-appender
   #:make-file-log-appender
   #:deflogger
   #:with-logger-level
   #:log.level
   #:log.compile-time-level
   #:+dribble+
   #:+debug+
   #:+info+
   #:+warn+
   #:+error+
   #:+fatal+
   #:handle
   #:append-message
   #:ancestors
   #:appenders
   #:children
   
   #:with-unique-names
   #:rebinding
   #:rebind
   #:define-constant
   #:remove-keywords
   #:remf-keywords

   #:make-matcher
   #:match
   #:match-case
   #:list-match-case
   
   #:parse-ieee-double
   #:parse-float
   #:mulf
   #:divf
   #:minf
   #:maxf
   #:map-range
   #:do-range
   #:10^
   
   #:tail
   #:but-tail
   #:head
   #:but-head
   #:starts-with
   #:ends-with
   #:read-sequence*
   #:deletef
   #:copy-array
   #:make-displaced-array
   
   #:+lower-case-ascii-alphabet+
   #:+upper-case-ascii-alphabet+
   #:+ascii-alphabet+
   #:+alphanumeric-ascii-alphabet+
   #:+base64-alphabet+
   #:random-string
   #:strcat
   #:strcat*
   #:princ-csv
   #:parse-csv-string
   #:join-strings
   #:fold-strings
   #:~%
   #:~T
   #:+CR-LF+
   #:~D
   #:~A
   #:~S
   #:~W

   #:def-special-environment
   
   #:intern-concat

   #:vector-push-extend*
   #:string-from-array

   #:queue
   #:enqueue
   #:dequeue
   #:peek-queue
   #:queue-empty-p
   #:queue-count
   #:random-queue-element
   #:queue->list
   #:lru-queue
   
   ;; decimal arith
   #:*precision*
   #:with-precision
   #:decimal-from-float
   #:float-from-decimal
   #:round-down
   #:round-half-up
   #:round-half-even
   #:round-ceiling
   #:round-floor
   #:round-half-down
   #:round-up

   #:enable-sharp-l-syntax
   #:enable-bracket-syntax
   #:enable-pf-syntax
   #:with-sharp-l-syntax
   #:with-package

   #:defclass-struct

   #:with*

   #:quit

   #:wrapping-standard

   #:levenshtein-distance

   #:getenv


   #:lisp1
   #:with-lisp1
   #:defun1
   #:defmethod1

   #:_

   #:eval-always
   #:defalias
   #:defvaralias
   #:defmacalias
   #:fun
   #:set
   #:new
   #:append1
   #:last1
   #:singlep
   #:class-name-of
   #:circularize
   #:let1

   ;; Obsolete stuff for backward compatibility. To be removed eventually.
   #:enable-sharp-l
   #:enable-bracket-reader
   #:enable-pf-reader
   ))

;;;; * Colophon

;;;; This documentation was produced by qbook.

;;;; arnesi, and the associated documentation, is written by Edward
;;;; Marco Baringer <mb@bese.it>.

;;;; ** COPYRIGHT

;;;; Copyright (c) 2002-2006, Edward Marco Baringer
;;;; Copyright (c) 2006 Luca Capello http://luca.pca.it <luca@pca.it>
;;;; All rights reserved. 

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;  - Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.

;;;;  - Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.

;;;;  - Neither the name of Edward Marco Baringer, Luca Capello, nor
;;;;    BESE, nor the names of its contributors may be used to endorse
;;;;    or promote products derived from this software without specific
;;;;    prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;;@include "accumulation.lisp"

;;;;@include "asdf.lisp"

;;;;@include "compat.lisp"

;;;; / @include "cps.lisp"

;;;;@include "csv.lisp"

;;;;@include "debug.lisp"

;;;;@include "decimal-arithmetic.lisp"

;;;;@include "defclass-struct.lisp"

;;;;@include "flow-control.lisp"

;;;;@include "hash.lisp"

;;;;@include "http.lisp"

;;;;@include "io.lisp"

;;;;@include "lambda.lisp"

;;;;@include "list.lisp"

;;;;@include "log.lisp"

;;;;@include "matcher.lisp"

;;;;@include "mop.lisp"

;;;;@include "mopp.lisp"

;;;;@include "numbers.lisp"

;;;;@include "one-liners.lisp"

;;;;@include "sequence.lisp"

;;;;@include "sharpl-reader.lisp"

;;;;@include "specials.lisp"

;;;;@include "string.lisp"

;;;;@include "walk.lisp"

;;;;@include "vector.lisp"

;; Copyright (c) 2002-2006, Edward Marco Baringer
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
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
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
