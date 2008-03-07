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

   #:superquote
   #:superquote-function
   #:unquote
   #:unquote-splice


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
   #:let-current-values
   #:with-preserve-specials
   #:with-specials-restored

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
   #:match-replace-one
   #:fail-match
   #:if-match
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

   #:debug-assert
   #:debug-assert-skip
   #:debug-unreachable
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
   #:sendbuf-empty
   #:sendbuf
   #:sendbuf-to-byte-vector

   #:recv
   #:recvline
   #:send
   #:accept
   #:reset-timeout
   #:hangup

   #:make-con-connect
   #:make-con-listen

   #:+newline+

   #:event-loop
   #:event-loop-reset

   #:forward-port))

(defpackage #:teepeedee2.http
  (:nicknames #:tpd2.http)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io)
  (:export 
   #:http-serve-timeout
   #:test-http-request
   #:percent-hexpair-encode
   #:dispatcher-register-path
   #:build-http-response
   #:respond-http
   #:*default-dispatcher*
   #:http-parse-and-generate-response))

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
   #:webapp-page-head-css
   #:webapp-page-head
   #:webapp-page-body-start
   #:link-to-webapp

   #:defpage


   #:page-link
   #:webapp-frame
   #:*webapp-frame*
   #:webapp-frame-var

   #:frame-var
   #:frame-username
   #:frame-messages
   #:list-all-frames
   #:find-frame
   #:frame-id
   #:all-http-params
   #:html-action-form
   #:html-action-link
   #:html-replace-link
   #:action-script-helper
   #:register-channel-page
   #:register-action-page

   #:channel
   #:channel-notify
   #:channel-update

   #:message-channel
   #:simple-channel
   #:list-channel-add
   #:list-channel-del
   #:make-message-channel
   #:message-channel-broadcast
   #:channel-script-helper
   #:js-library
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

(defpackage #:teepeedee2.sutp
  (:nicknames #:tpd2.sutp)
  (:use #:common-lisp #:teepeedee2.lib #:teepeedee2.io))


(defpackage #:teepeedee2.game.truc
  (:nicknames #:tpd2.game.truc)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))

(defpackage #:teepeedee2.game.cheat
  (:nicknames #:tpd2.game.cheat)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))

(defpackage #:teepeedee2.game.dating
  (:nicknames #:tpd2.game.dating)
  (:use #:tpd2.game #:tpd2.ml #:tpd2.lib #:teepeedee2.webapp #:common-lisp #:tpd2.ml.html))


#.`
(defpackage #:teepeedee2
  (:nicknames #:tpd2)
  ,@(let ((tpd-pkgs '(#:tpd2.io #:tpd2.lib #:tpd2.http #:tpd2.webapp)) syms)
	 (dolist (p tpd-pkgs)
	   (do-external-symbols (sym (find-package p)) (push sym syms)))
	 (list
	  `(:use #:common-lisp ,@tpd-pkgs)
	  `(:export ,@syms))))


