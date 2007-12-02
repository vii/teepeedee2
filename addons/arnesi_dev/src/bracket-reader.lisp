;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * {} syntax for local readtable modifications

(defun |{-reader| (stream char)
  (declare (ignore char))
  "A utility read macro for modifying the read table.

The syntax is:

  {SPECIFIER ...}

SPECIFIER is either a symbol naming a function (available at read
time) or a list (SPECIFIER &rest ARGUMENTS). SPECIFIER is applied
to ARGUMENTS to produce a function, this is then called and
passed another function which reads until the #\}
character. During the executen of the function *readtable* is
bound to a copy of the current read table.

See WITH-PACKAGE for an example of a specifier function."
  (let ((*readtable* (copy-readtable *readtable* nil)))
    (destructuring-bind (specifier &rest arguments)
        (ensure-list (read stream t nil t))
      (funcall (apply specifier arguments)
               (lambda ()
                 (read-delimited-list #\} stream t))))))

(defmacro enable-bracket-syntax ()
  "Enable bracket reader for the rest of the file (being loaded or compiled).
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character #\{ #'|{-reader| t *readtable*)
    (set-syntax-from-char #\} #\) *readtable*)))

(defmacro enable-bracket-reader ()
  "TODO Obsolete, use the enable-bracket-syntax macro."
  ;; (warn "Use the enable-bracket-syntax macro instead of enable-bracket-reader")
  `(enable-bracket-syntax))

(defun with-package (package-name)
  "When used as a specifier for the #\{ reader locally rebinds,
at read time, the current package to PACKAGE-NAME.

For example, this:

  {(with-package :cl-user) t}

Will always read cl:t, no matter what the current package
actually is."
  (lambda (reader)
    (let ((*package* (find-package package-name)))
      `(progn ,@(funcall reader)))))

;; Copyright (c) 2006, Edward Marco Baringer
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
