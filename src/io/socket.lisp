(in-package #:tpd2.io)

(define-condition socket-error (error)
  ())
(define-condition socket-closed (socket-error) 
  ())
