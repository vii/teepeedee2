;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; babel.asd --- ASDF system definition for Babel.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defpackage #:babel-system
  (:use #:cl #:asdf))
(in-package #:babel-system)

(defsystem babel
  :description "Babel, a charset conversion library."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :version "0.1.0"
  :licence "MIT"
  :components
  ((:module src
    :serial t
    :components
    ((:file "packages")
     (:file "utils")
     (:file "encodings")
     (:file "enc-ascii")
     (:file "enc-ebcdic")
     (:file "enc-iso-8859")
     (:file "enc-unicode")
     (:file "strings")))))

(defmethod perform ((o test-op) (c (eql (find-system :babel))))
  (operate 'asdf:load-op :babel-tests)
  (operate 'asdf:test-op :babel-tests))

;;; vim: ft=lisp et
