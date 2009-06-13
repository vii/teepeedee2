(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(cl:in-package #:teepeedee2.system)

(defpackage #:teepeedee2.lib
  (:nicknames #:tpd2.lib)
  (:use #:common-lisp #:iter #:cl-irregsexp.bytestrings)
  (:import-from #:alexandria #:with-unique-names #:define-constant)
  (:import-from #:trivial-garbage #:finalize #:cancel-finalization)
  (:import-from #:cl-cont #:call/cc #:with-call/cc)
  (:import-from #:cl-irregsexp 
		#:match-replace-all #:match-replace-one #:match-split
		#:match-bind #:if-match-bind
		#:match-failed )
  (:import-from #:cl-irregsexp.utils
		#:alist-get
		#:defun-consistent
		#:declaim-defun-consistent-ftype
		#:defun-speedy
		#:defun-careful
		#:concat-sym
		#:concat-sym-from-sym-package
		#:read-only-load-time-value
		#:load-time-constantp)
  (:export 

   #:defun-consistent
   #:declaim-defun-consistent-ftype
   #:defun-speedy
   #:defun-careful
   #:concat-sym
   #:concat-sym-from-sym-package
   #:read-only-load-time-value
   #:load-time-constantp

   #:match-replace-all #:match-replace-one 
   #:match-bind #:if-match-bind
   #:match-failed #:match-split

   #:superquote
   #:superquote-function
   #:unquote
   #:unquote-splice


   #:finalize
   #:cancel-finalization

   #:with-unique-names
   #:once-only
   #:define-constant
   #:defconstant-string
   #:defconstant-bv

   #:copy-byte-vector
   #:make-byte-vector
   #:byte-vector-to-string
   #:make-byte-vector
   #:force-byte-vector
   #:force-simple-byte-vector
   #:utf8-encode
   #:utf8-decode
   #:with-pointer-to-vector-data
   #:byte-vector
   #:simple-byte-vector
   #:byte-vector-cat
   #:apply-byte-vector-cat
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
   #:make-displaced-vector
   #:let-current-values
   #:with-preserve-specials
   #:with-specials-restored

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

   #:alist-get

   #:case-match-fold-ascii-case

   #:convert-continuation-to-normal-function
   #:without-call/cc
   #:with-call/cc
   #:call/cc

   #:read-safely
   #:read-safely-from-string

   #:make-timeout
   #:timeout-set
   #:timeout-cancel
   #:timeout-remaining
   #:next-timeout
   #:forget-timeouts

   #:debug-assert
   #:debug-assert-skip
   #:debug-unreachable

   #:backtrace-description
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

   #:socket-closed

   #:with-sendbuf
   #:sendbuf-add
   #:with-sendbuf-continue
   #:sendbuf-len
   #:sendbuf-empty
   #:sendbuf
   #:sendbuf-to-byte-vector

   #:recv
   #:recvline
   #:recv-until-close
   #:send
   #:accept
   #:reset-timeout
   #:hangup


   #:lookup-hostname

   #:make-con
   #:make-con-connect
   #:make-con-listen
   #:make-con-bind
   #:con-dead?
   #:con-connected?
   #:con-fail
   #:con-add-failure-callback
   #:con-clear-failure-callbacks
   #:con-when-ready-to-read
   #:con-peek
   #:con-peer-info
   
   #:+newline+
   #:+SOCK_DGRAM+
   #:+SOCK_STREAM+

   #:event-loop
   #:event-loop-reset

   #:forward-port))

(defpackage #:teepeedee2.http
  (:nicknames #:tpd2.http)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io)
  (:export 
   #:http-serve
   #:launch-http-request
   #:http-serve-timeout
   #:test-http-request
   #:percent-hexpair-encode
   #:dispatcher-register-path
   #:dispatcher-canonical-name
   #:build-http-response
   #:respond-http
   #:*default-dispatcher*
   #:dispatcher-add-alias
   #:find-or-make-dispatcher
   #:http-parse-and-generate-response
   #:+http-param-origin+

   #:http-start-server
   ))

(defpackage #:teepeedee2.ml
  (:nicknames #:tpd2.ml)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io)
  (:export
   #:define-dtd 
   #:output-raw-ml 
   #:output-escaped-ml 
   #:output-ml-comment 
   #:output-object-to-ml 
   #:object-to-ml 
   #:with-ml-output 
   #:with-ml-output-start
   #:without-ml-output
   #:with-ml-to-string

   #:css-html-style
   #:css-attrib

   #:js-html-script
   #:js-attrib
   #:js-to-string

   #:w3c-timestring
))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :teepeedee2.ml.html)
    (defpackage #:teepeedee2.ml.html
      (:nicknames #:tpd2.ml.html))))

(defpackage #:teepeedee2.webapp
  (:nicknames #:tpd2.webapp)
  (:use #:common-lisp 
	#:teepeedee2.lib 
	#:teepeedee2.http 
	#:teepeedee2.io 
	#:teepeedee2.ml.html 
	#:teepeedee2.ml)
  (:export
   #:webapp
   #:webapp-section
   #:webapp-select-one
   #:webapp-display
   #:link-to-webapp

   #:defpage
   #:defpage-lambda

   #:webapp-default-page-head-contents
   #:webapp-default-page-footer

   #:page-link
   #:webapp-frame
   #:*default-site*
   #:*webapp-frame*
   #:webapp-frame-var

   #:frame-var
   #:frame-username
   #:frame-messages
   #:frame-trace-info
   #:list-all-frames
   #:find-frame
   #:frame-id
   #:all-http-params
   #:html-action-form
   #:html-action-link
   #:html-replace-link
   #:html-action-form-collapsed
   #:action-script-helper

   #:channel
   #:channel-notify
   #:channel-update
   #:find-channel

   #:webapp-respond-ajax-body

   #:http-peer-info! 
   #:all-http-params!

   #:message-channel
   #:simple-channel
   #:simple-channel-body-ml
   #:list-channel-add
   #:list-channel-del
   #:make-message-channel
   #:message-channel-broadcast
   #:channel-script-helper
   #:js-library
   #:with-site
   #:with-compile-time-site
   #:defsite
   #:current-site

   ))


  
(defpackage #:teepeedee2.game
  (:nicknames #:tpd2.game)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.webapp #:tpd2.ml #:tpd2.ml.html)
  (:export 
   #:defgameclass 
   #:defgame
   #:defplayer
   #:defrules
   #:game
   #:player
   #:move
   #:move-continuation
   #:with-game
   #:choices-list
   #:play
   #:launch-game
   #:robot
   #:robot-bully
   #:robot-sensible
   #:card
   #:make-card-from-number
   #:card-number
   #:card-value
   #:card-suite
   #:make-card
   #:+suits+
   #:+cards-per-suit+

   #:player-controller-name-to-ml
   #:web-state-queue-choice 
   #:current-web-controller
   #:player-controller))

(defpackage #:teepeedee2.datastore
  (:nicknames #:tpd2.datastore)
  (:use #:tpd2.lib #:cl)

  (:export
   #:datastore-delete
   #:datastore-retrieve-all
   #:datastore-retrieve-indexed
   #:datastore-retrieve-unique
   #:defrecord
   #:datastore-use-file))

(defpackage #:teepeedee2.munnel
  (:nicknames #:tpd2.munnel)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io))

(defpackage #:teepeedee2.blog
  (:nicknames #:tpd2.blog)
  (:use #:cl #:tpd2.webapp #:tpd2.ml #:tpd2.ml.html #:tpd2.lib #:tpd2.datastore))

(defpackage #:teepeedee2.game.truc
  (:nicknames #:tpd2.game.truc)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))

(defpackage #:teepeedee2.game.cheat
  (:nicknames #:tpd2.game.cheat)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))

(defpackage #:teepeedee2.game.dating
  (:nicknames #:tpd2.game.dating)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))

(defpackage #:teepeedee2.demo
  (:nicknames #:tpd2.demo)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.webapp #:tpd2.ml #:tpd2.ml.html #:tpd2.datastore))



#.`
(defpackage #:teepeedee2
  (:nicknames #:tpd2)
  ,@(let ((tpd-pkgs '(#:tpd2.io #:tpd2.lib #:tpd2.http #:tpd2.webapp)) syms)
	 (dolist (p tpd-pkgs)
	   (do-external-symbols (sym (find-package p)) (push sym syms)))
	 (list
	  `(:use #:common-lisp ,@tpd-pkgs)
	  `(:export ,@syms))))


