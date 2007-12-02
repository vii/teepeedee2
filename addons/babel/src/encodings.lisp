;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; encodings.lisp --- Character encodings and mappings.
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

;;;; Character Encodings

(defclass character-encoding ()
  ((name :initarg :name :reader enc-name
         :initform (error "Must specify a NAME for this character encoding."))
   ;; Most of these documentation strings are taken from OpenMCL.
   (documentation
    :initarg :documentation :reader enc-documentation :initform nil)
   ;; A non-exhaustive list of aliases for the encoding.
   (aliases :initarg :aliases :initform nil :reader enc-aliases)
   ;; Specified in bits. Usually 8, 16 or 32.
   (code-unit-size
    :initarg :code-unit-size :reader enc-code-unit-size :initform 8)
   (max-units-per-char
    :initarg :max-units-per-char :reader enc-max-units-per-char :initform 1)
   ;; If NIL, it is necessary to swap 16- and 32-bit units.
   (native-endianness
    :initarg :native-endianness :reader enc-native-endianness :initform t)
   ;; Code units and characters codes less than this value map to
   ;; themselves.
   (literal-char-code-limit
    :initarg :literal-char-code-limit :reader enc-literal-char-code-limit
    :initform 0)
   ;; Defines whether it is necessary to prepend a byte-order-mark to
   ;; determine the endianness.
   (use-bom :initarg :use-bom :initform nil :reader enc-use-bom)
   ;; How the byte-order-mark should be encoded, specified as a
   ;; sequence of octets.  NIL if it cannot be encoded.
   (bom-encoding
    :initarg :bom-encoding :reader enc-bom-encoding :initform nil)
   ;; How should NUL be encoded, specified as sequence of octets.
   (nul-encoding
    :initarg :nul-encoding :reader enc-nul-encoding :initform #(0))
   ;; Preferred replacement character code point.
   (default-replacement
    :initarg :default-replacement :reader enc-default-replacement
    :initform #x1a)))

#-(and)
(defmethod describe-object ((enc character-encoding) s)
  "Prints out the name, aliases and documentation slots of a
character encoding object."
  (with-slots (name aliases documentation) enc
    (format s "~&~S" name)
    (when aliases
      (format s " [Aliases:~{ ~S~}]" aliases))
    (format s "~&~A~%~%" documentation))
  (call-next-method))

(defvar *supported-character-encodings* nil)

(defun list-character-encodings ()
  "List of keyword symbols denoting supported character
encodings.  This list does not include aliases."
  *supported-character-encodings*)

(defvar *character-encodings* (make-hash-table :test 'eq))

(defun get-character-encoding (name)
  "Lookups the character encoding denoted by the keyword symbol
NAME.  Signals an error if one is not found."
  (or (gethash name *character-encodings*)
      (error "Unknown character encoding: ~S" name)))

(defun notice-character-encoding (enc)
  (pushnew (enc-name enc) *supported-character-encodings*)
  (dolist (kw (cons (enc-name enc) (enc-aliases enc)))
    (setf (gethash kw *character-encodings*) enc))
  (enc-name enc))

(defmacro define-character-encoding (name docstring &body options)
  `(notice-character-encoding
    (make-instance 'character-encoding :name ,name ,@options
                   :documentation ,docstring)))

;;;; Mappings

;;; TODO: describe what mappings are

(defun 8-bit-fixed-width-counter (accessor type)
  (declare (ignore accessor type))
  `(lambda (seq start end)
     (declare (ignore seq) (fixnum start end))
     (values (the fixnum (- end start)) end)))

;;; Useful to develop new encodings incrementally starting with octet
;;; and code-unit counters.
(defun dummy-coder (sa st da dt)
  (declare (ignore sa st da dt))
  `(lambda (src s e dest i)
     (declare (ignore src s e dest i))
     (error "this encoder/decoder hasn't been implemented yet")))

(defclass mapping ()
  ((encoder :accessor encoder :initform #'dummy-coder)
   (decoder :accessor decoder :initform #'dummy-coder)
   (octet-counter
    :accessor octet-counter :initform #'8-bit-fixed-width-counter)
   (code-point-counter
    :accessor code-point-counter :initform #'8-bit-fixed-width-counter)))

;;; TODO: document here
;;;
;;; ENCODER -- (lambda (src-accessor src-type dest-accessor dest-type) ...)
;;; DECODER -- (lambda (src-accessor src-type dest-accessor dest-type) ...)
;;;
;;; OCTET-COUNTER -- (lambda (accessor type) ...)
;;; CODE-POINT-COUNTER -- (lambda (accessor type) ...)
(defclass abstract-mapping (mapping) ())

;;; TODO: document these
;;;
;;; ENCODER -- (lambda (src start end dest d-start) ...)
;;; DECODER -- (lambda (src start end dest d-start) ...)
;;;
;;; OCTET-COUNTER -- ((seq start end) ...)
;;; CODE-POINT-COUNTER -- (lambda (seq start end) ...) => N-CHARS NEW-END
;;;   (important: describe NEW-END)
(defclass concrete-mapping (mapping) ())

(defvar *abstract-mappings* (make-hash-table :test 'eq))

(defun get-abstract-mapping (encoding)
  (gethash encoding *abstract-mappings*))

(defun (setf get-abstract-mapping) (value encoding)
  (setf (gethash encoding *abstract-mappings*) value))

(defun register-mapping (encoding slot-name fn)
  (let ((m (get-abstract-mapping encoding)))
    (when (null m)
      (setq m (make-instance 'abstract-mapping))
      (setf (get-abstract-mapping encoding) m))
    (setf (slot-value m slot-name) fn)))

;;; See enc-*.lisp for example usages of these 4 macros.

(defmacro define-encoder (encoding (sa st da dt) &body body)
  `(register-mapping ,encoding 'encoder (lambda (,sa ,st ,da ,dt) ,@body)))

(defmacro define-decoder (encoding (sa st da dt) &body body)
  `(register-mapping ,encoding 'decoder (lambda (,sa ,st ,da ,dt) ,@body)))

(defmacro define-octet-counter (encoding (acc type) &body body)
  `(register-mapping ,encoding 'octet-counter (lambda (,acc ,type) ,@body)))

(defmacro define-code-point-counter (encoding (acc type) &body body)
  `(register-mapping
    ,encoding 'code-point-counter (lambda (,acc ,type) ,@body)))

;;; Funcalls (at macro-expansion time) the abstract mappings with the
;;; src/dest accessors and types which generate the appropriate code
;;; for the concrete mappings.  These functions are then saved in
;;; their respective slots of the CONCRETE-MAPPING object.
(defun %am-to-cm (cm am osa ost cpsa cpst)
  `(setf (encoder ,cm) ,(funcall (encoder am) cpsa cpst osa ost)
         (decoder ,cm) ,(funcall (decoder am) osa ost cpsa cpst)
         (code-point-counter ,cm) ,(funcall (code-point-counter am) osa ost)
         (octet-counter ,cm) ,(funcall (octet-counter am) cpsa cpst)))

;;; Expands into code generated by the available abstract mappings
;;; that will be compiled into concrete mappings.  This is used in
;;; e.g. strings.lisp to define mappings between strings and
;;; (unsigned-byte 8) vectors.
(defmacro instantiate-concrete-mappings
    (&key optimize octet-seq-accessor octet-seq-type
     code-point-seq-accessor code-point-seq-type)
  `(let ((ht (make-hash-table :test 'eq)))
     (declare (optimize ,@optimize))
     (flet ((notice-mapping (enc-name cm)
              (let ((aliases (enc-aliases (get-character-encoding enc-name))))
                (dolist (kw (cons enc-name aliases))
                  (setf (gethash kw ht) cm)))))
       ,@(loop for am being the hash-values of *abstract-mappings*
               using (hash-key enc) collect
               `(let ((cm (make-instance 'concrete-mapping)))
                  ,(%am-to-cm 'cm am octet-seq-accessor octet-seq-type
                              code-point-seq-accessor code-point-seq-type)
                  (notice-mapping ,enc cm))))
      ht))

(defun lookup-mapping (ht enc-name)
  "HT should be an hashtable created by
INSTANTIATE-CONCRETE-MAPPINGS. NAME should be a keyword symbol
denoting a character encoding name or alias."
  (or (gethash enc-name ht)
      (error "signal proper error here")))

;;;; Utilities used in enc-*.lisp

;;; Guessing at the endianness...
;;; XXX: investigate how this interacts with CLISP's FASL portability.
(pushnew #+(or x86 x86-64) 'babel::le
         #-(or x86 x86-64) 'babel::be
         *features*)

(defconstant +sub+ #x1a "ASCII substitution character code point.")

;;; We're converting between objects of the (UNSIGNED-BYTE 8) and
;;; (MOD #x110000) types which are here aliased to UB8 and CODE-POINT
;;; for convenience.
(deftype ub8 () '(unsigned-byte 8))
(deftype code-point () '(mod #x110000))

;;; Utility macro around DEFINE-ENCODER that takes care of most of the
;;; work need to deal with an 8-bit, fixed-width character encoding.
;;;
;;; BODY will be inside a loop and OCTET will be bound symbol macro
;;; that'll expand to an accessor for the destination buffer.  It
;;; handles all sorts of type declarations.
;;;
;;; See enc-ascii.lisp for a simple usage example.
(defmacro define-unibyte-encoder (encoding (code octet) &body body)
  (with-unique-names (s-accessor s-type d-accessor d-type
                      src start end dest d-start i di)
    `(define-encoder ,encoding (,s-accessor ,s-type ,d-accessor ,d-type)
       `(lambda (,',src ,',start ,',end ,',dest ,',d-start)
          (declare (type ,,s-type ,',src)
                   (type ,,d-type ,',dest)
                   (fixnum ,',start ,',end ,',d-start))
          (loop for ,',i fixnum from ,',start below ,',end
                and ,',di fixnum from ,',d-start do
                (let ((,',code (,,s-accessor ,',src ,',i)))
                  (declare (type code-point ,',code))
                  (symbol-macrolet ((,',octet (,,d-accessor ,',dest ,',di)))
                    (macrolet
                        ;; this should probably be a function...
                        ((handle-error
                             (&optional (condition ''character-encoding-error))
                           `(encoding-error
                             ,',',code ,',',encoding ,',',src ,',',i
                             +sub+ ,condition)))
                      ,@',body)))
                finally (return (the fixnum (- ,',d-start ,',di))))))))

;;; The decoder version of the above macro.
(defmacro define-unibyte-decoder (encoding (octet code) &body body)
  (with-unique-names (s-accessor s-type d-accessor d-type
                      src start end dest d-start i di)
    `(define-decoder ,encoding (,s-accessor ,s-type ,d-accessor ,d-type)
       `(lambda (,',src ,',start ,',end ,',dest ,',d-start)
          (declare (type ,,s-type ,',src)
                   (type ,,d-type ,',dest)
                   (fixnum ,',start ,',end ,',d-start))
          (loop for ,',i fixnum from ,',start below ,',end
                and ,',di fixnum from ,',d-start do
                (let ((,',octet (,,s-accessor ,',src ,',i)))
                  (declare (type ub8 ,',octet))
                  (symbol-macrolet ((,',code (,,d-accessor ,',dest ,',di)))
                    (macrolet
                        ;; this should probably be a function...
                        ((handle-error
                             (&optional (condition ''character-decoding-error))
                           `(decoding-error
                             (vector ,',',octet) ,',',encoding ,',',src ,',',i
                             +sub+ ,condition)))
                      ,@',body)))
                finally (return (the fixnum (- ,',d-start ,',di))))))))

;;;; Error Conditions
;;;
;;; For now, we don't define any actual restarts.  The only mechanism
;;; for "restarting" a coding error is the
;;; *SUPPRESS-CHARACTER-CODING-ERRORS* special variable which, when
;;; bound to T (the default), suppresses any error and uses a default
;;; replacement character instead.
;;;
;;; If it turns out that other more options are necessary, possible
;;; alternative approaches include:
;;;
;;;   a) use a *REPLACEMENT-CHARACTER* special variable that lets us
;;;      pick our own replacement character.  The encoder must do
;;;      additional work to check if this is character is encodable.
;;;
;;;   b) offer a restart to pick a replacement character.  Same
;;;      problem as above.
;;;
;;; Both approaches pose encoding problems when dealing with a
;;; variable-width encodings because different replacement characters
;;; will need different numbers of octets.  This is not a problem for
;;; UTF but will be a problem for the CJK charsets.  Approach (a) is
;;; nevertheless easier since the replacement character is known in
;;; advance and therefore the octet-counter can account for it.
;;;
;;; For more complex restarts like SBCL's -- that'll let you specify
;;; _several_ replacement characters for a single character error --
;;; will probably need extra support code outside the encoder/decoder
;;; (i.e. in the string-to-octets function, for example) since the
;;; encoders/decoders deal with pre-allocated fixed-length buffers.
;;;
;;; SBCL has ASCII-specific (MALFORMED-ASCII) and UTF8-specific
;;; errors.  Why?  Do we want to add some of those too?

;;; FIXME: We used to deal with this with an extra ERRORP argument for
;;; encoders, decoders, etc...  Still undecided on the best way to do
;;; it.  We could also use a simple restart instead of this...
;;;
;;; In any case, this is not for the users to bind and it's not
;;; exported from the BABEL package.
(defvar *suppress-character-coding-errors* t
  "If non-NIL, encoding or decoding errors are suppressed and the
the current character encoding's default replacement character is
used.")

;;; All of Babel's error conditions are subtypes of
;;; CHARACTER-CODING-ERROR.  This error hierarchy is based on SBCL's.
(define-condition character-coding-error (error)
  ((buffer :initarg :sequence :reader character-coding-error-buffer)
   (position :initarg :position :reader character-coding-error-position)
   (encoding :initarg :encoding :reader character-coding-error-encoding)))

(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code))
  (:report (lambda (c s)
             (format s "Unable to encode character code point ~A as ~S."
                     (character-encoding-error-code c)
                     (character-coding-error-encoding c)))))

(declaim (inline encoding-error))
(defun encoding-error (code enc buf pos
                       &optional (sub +sub+) (e 'character-encoding-error))
  (unless *suppress-character-coding-errors*
    (error e :encoding enc :buffer buf :position pos :code code))
  sub)

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets))
  (:report (lambda (c s)
             (format s "Illegal ~S character starting at position ~D."
                     (character-coding-error-encoding c)
                     (character-coding-error-position c)))))

;;; Signalled by DECODERs or CODE-POINT-COUNTERs of variable-width
;;; character encodings.
(define-condition end-of-input-in-character (character-decoding-error) ())

(declaim (inline decoding-error))
(defun decoding-error (octets enc buf pos
                       &optional (sub +sub+) (e 'character-decoding-error))
  (unless *suppress-character-coding-errors*
    (error e :octets octets :encoding enc :buffer buf :position pos))
  sub)
