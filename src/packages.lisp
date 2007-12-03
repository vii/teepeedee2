(cl:defpackage #:teepeedee2.system
  (:use #:cl))
(in-package #:teepeedee2.system)

(defpackage #:teepeedee2.lib
  (:nicknames #:tpd2.lib)
  (:use #:common-lisp)
  (:import-from #:cl-utilities #:with-unique-names)
  (:import-from #:trivial-garbage #:finalize #:cancel-finalization)
  (:export 
   #:finalize
   #:cancel-finalization

   #:with-unique-names
   #:once-only

   #:check-symbols
   #:eval-always
   #:def-if-unbound
   #:force-string
   #:force-keyword
   #:force-list
   #:force-class
   #:force-first
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
   #:my-call
   #:my
   #:me
   #:it
   #:defmyclass
   #:force-class
   #:signal-protect
   #:strcat))

(defpackage #:teepeedee2.io
  (:nicknames #:tpd2.io)
  (:use #:common-lisp #:teepeedee2.lib)
  (:export 
   #:defprotocol 
   #:launch-io
   #:io
   #:protocol-error

   #:build-sendbuf
   #:recv
   #:recvline
   #:send
   #:accept

   #:make-con-connect
   #:make-con-listen
   #:hangup

   #:byte-vector-to-string
   #:+newline+

   #:event-loop
   #:event-loop-reset))

#.`
(defpackage #:teepeedee2
  (:nicknames #:tpd2)
  ,@(let ((tpd-pkgs '(#:tpd2.io #:tpd2.lib)) syms)
	 (dolist (p tpd-pkgs)
	   (do-external-symbols (sym (find-package p)) (push sym syms)))
	 (list
	  `(:use #:common-lisp ,@tpd-pkgs)
	  `(:export ,@syms))))

