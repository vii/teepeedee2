(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(in-package #:teepeedee2.system)

(defpackage #:teepeedee2.lib
  (:nicknames #:tpd2.lib)
  (:use #:common-lisp #:iter)
  (:import-from #:cl-utilities #:with-unique-names)
  (:import-from #:trivial-garbage #:finalize #:cancel-finalization)
  (:import-from #:cl-cont #:call/cc #:with-call/cc)
  (:export 
   #:finalize
   #:cancel-finalization

   #:with-unique-names
   #:once-only

   #:make-byte-vector
   #:byte-vector-to-string
   #:make-byte-vector
   #:force-byte-vector
   #:utf8-encode
   #:with-pointer-to-vector-data
   #:byte-vector
   #:simple-byte-vector
   #:byte-vector-cat
   #:byte-to-ascii-upper
   #:byte-vector-parse-integer
   #:byte-vector=-fold-ascii-case

   #:check-symbols
   #:eval-always
   #:def-if-unbound
   #:ignorable-let
   #:force-string
   #:force-keyword
   #:force-list
   #:force-class
   #:force-first
   #:force-rest
   #:merge-constant-arguments
   #:aif
   #:acond
   #:awhen
   #:adolist
   #:appendf
   #:deletef
   #:dohash
   #:case-equalp
   #:case-=
   #:unquote-quoted-symbol
   #:separate-declarations
   #:separate-keywords
   #:filter
   #:filter-non-nil
   #:filter-until-full
   #:mv-filter
   #:defun-consistent
   #:make-displaced-vector

   #:read-only-load-time-value
   #:load-time-constantp

   #:copy
   #:assign
   #:its
   #:with-shorthand-accessor
   #:my-defun
   #:my-declare-fast-inline
   #:my-call
   #:my
   #:me
   #:it
   #:defmyclass
   #:defmystruct
   #:force-class
   #:signal-protect
   #:strcat
   #:random-shuffle
   #:random-elt

   #:cdr-assoc

   #:match-bind
   #:match-failed
   #:match-replace-all
   #:fail-match
   #:if-match
   #:case-match-fold-ascii-case

   #:convert-continuation-to-normal-function
   #:without-call/cc
   #:with-call/cc
   #:call/cc

   #:read-safely
   #:read-safely-from-string
))

(defpackage #:teepeedee2.io
  (:nicknames #:tpd2.io)
  (:use #:common-lisp #:teepeedee2.lib)
  (:export 
   #:defprotocol 
   #:launch-io
   #:io
   #:protocol-error
   #:accept-forever
   #:without-call/cc

   #:with-sendbuf
   #:sendbuf-add
   #:with-sendbuf-continue
   #:sendbuf-len
   #:sendbuf

   #:recv
   #:recvline
   #:send
   #:accept

   #:make-con-connect
   #:make-con-listen
   #:hangup

   #:+newline+

   #:event-loop
   #:event-loop-reset))

(defpackage #:teepeedee2.http
  (:nicknames #:tpd2.http)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io)
  (:export 
   #:dispatcher-register-path 
   #:*default-dispatcher*
   #:http-parse-and-generate-response))

(defpackage #:teepeedee2.ml
  (:nicknames #:tpd2.ml)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io)
  (:export
   #:define-dtd #:output-raw-ml #:output-escaped-ml #:output-object-to-ml #:object-to-ml #:with-ml-output #:without-ml-output))

(defpackage #:teepeedee2.webapp
  (:nicknames #:tpd2.webapp)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.http)
  (:export
   #:defpage))

(defpackage #:teepeedee2.game
  (:nicknames #:tpd2.game)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.webapp #:teepeedee2.ml)
  (:export #:defgameclass #:play))

#.`
(defpackage #:teepeedee2
  (:nicknames #:tpd2)
  ,@(let ((tpd-pkgs '(#:tpd2.io #:tpd2.lib #:tpd2.http #:tpd2.webapp #:tpd2.game)) syms)
	 (dolist (p tpd-pkgs)
	   (do-external-symbols (sym (find-package p)) (push sym syms)))
	 (list
	  `(:use #:common-lisp ,@tpd-pkgs)
	  `(:export ,@syms))))


