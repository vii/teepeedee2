(in-package #:cl-user)

;;; First ASDF install the dependencies

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require 'asdf-install))

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

;;; Define a /hello page

(tpd2:defpage "/hello" ((name "Friend")) :create-frame nil
  (tpd2.ml.html:<h1 "Hello " name))

;;; Start tpd2 listening

(let ((socket (tpd2.io:make-con-listen :port 8080)))
  (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http:http-serve))

;;; Enter the event-loop

(tpd2:event-loop)

;;; Visit http://localhost:8080/hello
