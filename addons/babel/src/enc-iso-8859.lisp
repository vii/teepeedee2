;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-iso-8859.lisp --- ISO-8859-* encodings.
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

(in-package #:babel-encodings)

;;; Typically, ISO-8869-* codes in the range #x00-#x9f map straight
;;; through, while codes #xa0-#xff select arbitrary Unicode characters
;;; that are commonly used in some locale.  (Sometimes the break is at
;;; #x80 instead of #xa0).
;;;
;;; (comment from OpenMCL's ccl/level-1/l1-unicode.lisp)

(define-character-encoding :iso-8859-1
    "An 8-bit, fixed-width character encoding in which all
character codes map to their Unicode equivalents.  Intended to
support most characters used in most Western European languages."
  :aliases '(:latin-1 :latin1)
  :literal-char-code-limit 256)

(define-unibyte-encoder :iso-8859-1 (code octet)
  (when (>= code 256)
    (setq code (handle-error)))
  (setq octet code))

(define-unibyte-decoder :iso-8859-1 (octet code)
  (setq code octet))
