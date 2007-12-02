;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Convience functions for working with hash tables.

(defun build-hash-table (hash-spec inital-contents)
  "Create a hash table containing ``INITAL-CONTENTS``."
  (let ((ht (apply #'make-hash-table hash-spec)))
    (dolist* ((key value) inital-contents)
      (setf (gethash key ht) value))
    ht))

(defmacro deflookup-table
    (name &key (var    (make-lookup-name name "*" name "*"))
               (reader (make-lookup-name name "GET-" name))
               (writer (make-lookup-name name "GET-" name))
               (rem-er (make-lookup-name name "REM-" name))
               (at-redefinition :warn)
               (documentation
                (format nil "Global var for the ~S lookup table" name))
               (test 'eql)
               (initial-contents nil))
  "Creates a hash table and the associated accessors."
  ;; if they explicitly pass in NIL we make the name a gensym
  (unless var
    (setf var    (gensym (strcat "var for " name " lookup table "))))
  (unless reader
    (setf reader (gensym (strcat "reader for " name " lookup table "))))
  (unless writer
    (setf writer (gensym (strcat "writer for " name " lookup table "))))
  (assert (symbolp name) (name)
          "The name of the lookup table must be a symbol.")
  (assert (symbolp var) (var)
          "The name of the underlying var must be a symbol.")
  (assert (symbolp reader) (reader)
          "The name of the reader for a lookup table must be a symbol.")
  (assert (symbolp writer) (writer)
          "The name of the writer for a lookup table must be a symbol.")
  `(progn
     (defvar ,var
       (build-hash-table '(:test ,test) ,initial-contents)
       ,documentation)
     (defun ,reader (key &optional default)
       (gethash key ,var default))
     (defun (setf ,writer) (value key)
       ,(when at-redefinition
          `(when (gethash key ,var)
             ,(case at-redefinition
                (:warn `(warn "Redefining ~A in deflookup-table named ~S"
                         (let ((*package* (find-package "KEYWORD")))
                           (format nil "~S" key))
                         ',name))
                (t at-redefinition))))
       (setf (gethash key ,var) value))
     (defun ,rem-er (key)
       (remhash key ,var))
     (list ',name ',var ',reader '(setf ,writer) ',rem-er)))

(defun make-lookup-name (name &rest parts)
  (funcall #'intern-concat parts (symbol-package name)))

(defun hash-to-alist (hash-table)
  (loop for k being the hash-keys of hash-table
        collect (cons k (gethash k hash-table))))

(defun hash-table-keys (hash-table)
  (loop
     for k being the hash-keys of hash-table
     collect k))

(defun hash-table-values (hash-table)
  (loop
     for v being the hash-values of hash-table
     collect v))

;; Copyright (c) 2002-2006, Edward Marco Baringer
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
