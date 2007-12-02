;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Conversions between strings and UB8 vectors.
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

(in-package #:babel)

;;; Can we handle unicode fully?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< char-code-limit #x110000)
    (pushnew 'small-char-code-limit *features*))
  (when (> char-code-limit #x110000)
    (error "Hmm, (> char-code-limit #x110000) we weren't expecting that. ~
            Bad things would happen if we tried to continue.")))

;;; What's the right thing to do here?
#+babel::small-char-code-limit
(error "we don't properly support Lisps with small CHAR-CODE-LIMITs, yet")

(defvar *default-character-encoding* :utf-8
  "Special variable used to determine the default character
encoding by STRING-TO-OCTETS, OCTETS-TO-STRING,
STRING-SIZE-IN-OCTETS, and VECTOR-SIZE-IN-CHARS")

;;; Adapted from Ironclad.  TODO: check if it's worthwhile adding
;;; implementation-specific accessors such as SAP-REF-* for SBCL.
(defmacro ubref (vector index &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness '(:be #+babel::be :ne))))
    (babel-encodings::once-only (vector index)
      `(logand
        ,(1- (ash 1 (* 8 bytes)))
        (logior
         ,@(loop for i from 0 below bytes
                 for offset = (if big-endian i (- bytes i 1))
                 for shift = (if big-endian
                                 (* (- bytes i 1) 8)
                                 (* offset 8))
                 collect `(ash (aref ,vector (+ ,index ,offset)) ,shift)))))))

(defsetf ubref (vector index &optional (bytes 1) (endianness :ne)) (value)
  (let ((big-endian (member endianness '(:be #+babel::be :ne #+babel::le :re))))
    `(progn
       ,@(loop for i from 1 to bytes
               for offset = (if big-endian (- bytes i) (1- i)) collect
               `(setf (aref ,vector (+ ,index ,offset))
                      (ldb (byte 8 ,(* 8 (1- i))) ,value)))
       (values))))

(defmacro string-accessor (string index)
  `(char-code (schar ,string ,index)))

(defsetf string-accessor (string index) (code)
  `(setf (schar ,string ,index) (code-char ,code)))

(defparameter *string-vector-mappings*
  (instantiate-concrete-mappings
   :optimize ((speed 3) (debug 0) (compilation-speed 0) (safety 0))
   :octet-seq-accessor ubref
   :octet-seq-type (simple-array (unsigned-byte 8) (*))
   :code-point-seq-accessor string-accessor
   :code-point-seq-type simple-string))

;;; Do we want a more a specific error condition here?
(defun check-vector-bounds (vector start end)
  (unless (<= 0 start end (length vector))
    (error "Invalid start (~A) and end (~A) values for vector of length ~A."
           start end (length vector))))

(defmacro with-simple-vector (((v vector) (s start) (e end)) &body body)
  "If VECTOR is a displaced or adjustable array, binds V to the
underlying simple vector, adds an adequate offset to START and
END and binds those offset values to S and E.  Otherwise, if
VECTOR is already a simple array, it's simply bound to V with no
further changes.

START and END are unchecked and assumed to be within bounds.

Note that in some Lisps, a slow copying implementation is
necessary to obtain a simple vector thus V will be bound to a
copy of VECTOR coerced to a simple-vector.  Therefore, you
shouldn't attempt to modify V."
  #+sbcl
  `(sb-kernel:with-array-data ((,v ,vector) (,s ,start) (,e ,end))
     ,@body)
  #+cmu
  `(lisp::with-array-data ((,v ,vector) (,s ,start) (,e ,end))
     ,@body)
  #+openmcl
  `(multiple-value-bind (,v ,s)
       (ccl::array-data-and-offset ,vector)
     (incf ,s ,start)
     (let ((,e (+ ,end ,s)))
       ,@body))
  #+allegro
  `(excl::with-underlying-simple-vector (,vector ,v ,s)
     (incf ,s ,start)
     (let ((,e (+ ,end ,s)))
       ,@body))
  ;; slow, copying implementation
  #-(or sbcl cmu openmcl allegro)
  (once-only (vector)
    `(let* ((,e ,end)
            (,s ,start)
            (,v (make-array (- ,e ,s)
                            :element-type (array-element-type ,vector)
                            :initial-contents ,vector)))
       ,@body)))

(defmacro with-checked-simple-vector (((v vector) (s start) (e end)) &body body)
  "Like WITH-SIMPLE-VECTOR but bound-checks START and END."
  (babel-encodings::once-only (vector start)
    `(let ((,e (or ,end (length ,vector))))
       (check-vector-bounds ,vector ,start ,e)
       (with-simple-vector ((,v ,vector) (,s ,start) (,e ,e))
         ,@body))))

;;; In the these 4 functions below, ERRORP defaults to NIL.  But it
;;; might as well default to T.  TODO: find out a good reason to go
;;; either way.
;;;
;;; Future features these functions should have:
;;;
;;;   * null-terminate
;;;   * use-bom
;;;   * specify target vector/string + offset
;;;   * documentation :)

(defun octets-to-string (vector &key (start 0) end errorp
                         (encoding *default-character-encoding*))
  (check-type vector (vector (unsigned-byte 8)))
  (with-checked-simple-vector ((vector vector) (start start) (end end))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((*suppress-character-coding-errors* (not errorp))
          (mapping (lookup-mapping *string-vector-mappings* encoding)))
      (multiple-value-bind (size new-end)
          (funcall (code-point-counter mapping) vector start end)
        (let ((string (make-string size)))
          (funcall (decoder mapping) vector start new-end string 0)
          string)))))

(defun string-to-octets (string &key (encoding *default-character-encoding*)
                         (start 0) end null-terminate errorp)
  (declare (ignore null-terminate))
  (check-type string string)
  (with-checked-simple-vector ((string string) (start start) (end end))
    (declare (type simple-string string))
    (let* ((*suppress-character-coding-errors* (not errorp))
           (mapping (lookup-mapping *string-vector-mappings* encoding))
           (vector (make-array (funcall (octet-counter mapping)
                                        string start end)
                               :element-type '(unsigned-byte 8))))
      (funcall (encoder mapping) string start end vector 0)
      vector)))

(defun string-size-in-octets (string &key (start 0) end errorp
                              (encoding *default-character-encoding*))
  (check-type string string)
  (with-checked-simple-vector ((string string) (start start) (end end))
    (declare (type simple-string string))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (funcall (octet-counter mapping) string start end))))

(defun vector-size-in-chars (vector &key (start 0) end errorp
                             (encoding *default-character-encoding*))
  (check-type vector (vector (unsigned-byte 8)))
  (with-checked-simple-vector ((vector vector) (start start) (end end))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (funcall (code-point-counter mapping) vector start end))))
