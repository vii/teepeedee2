(in-package #:cl-user)

;;; First ASDF install the dependencies

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require 'asdf-install))


;;; WARNING -- SIGNATURES NOT CHECKED!
(handler-bind (((or asdf-install::key-not-found asdf-install::download-error asdf-install::no-signature) 
		(lambda(c) (declare (ignore c)) (invoke-restart 'asdf-install::skip-gpg-check))))
  (asdf-install:install 		   
   'iterate
   'cffi
   'cl-irregsexp
   'trivial-backtrace))

;;; Load tpd2

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((error (lambda(c) (declare (ignore c)) (invoke-restart 'CONTINUE))))
    (asdf:oos 'asdf:load-op 'teepeedee2)))

(defpackage #:teepeedee2.quickstart
  (:use #:cl #:tpd2 #:tpd2.ml.html))

(in-package #:teepeedee2.quickstart)

;;; Define a /hello page

(defpage "/hello" ((name "Friend"))
  (<h1 "Hello " name))

;;; Start tpd2 listening

(http-start-server 8080)

;;; Enter the event-loop

#+sbcl
(sb-thread:make-thread #'event-loop :name "tpd2")
#-sbcl
(event-loop)

;;; Visit http://localhost:8080/hello

