;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.lisp --- Trivial Garbage!
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

(defpackage #:trivial-garbage
  (:use #:cl)
  (:shadow #:make-hash-table)
  (:nicknames #:tg)
  (:export #:gc
           #:make-weak-pointer
           #:weak-pointer-value
           #:weak-pointer-p
           #:make-weak-hash-table
           #:hash-table-weakness
           #:finalize
           #:cancel-finalization))

(in-package #:trivial-garbage)

;;;; GC

(defun gc (&key full verbose)
  "Initiates a garbage collection."
  (declare (ignorable verbose full))
  #+:cmu (ext:gc :verbose verbose :full full)
  #+:sbcl (sb-ext:gc :full full)
  #+:allegro (excl:gc (not (null full)))
  #+:clisp (ext:gc)
  #+:openmcl (ccl:gc)
  #+:corman (ccl:gc (if full 3 0))
  #+:lispworks (hcl:mark-and-sweep (if full 3 0))) 

;;;; Weak Pointers

#+:openmcl
(defvar *weak-pointers* (cl:make-hash-table :test 'eq :weak :value)
  "Weak value hash-table mapping between pseudo weak pointers and its values.")

#+(or :allegro :openmcl :lispworks)
(defstruct (weak-pointer (:constructor %make-weak-pointer))
  #-:openmcl pointer)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to OBJECT. For
   portability reasons, OBJECT most not be NIL."
  (assert (not (null object)))
  #+:sbcl (sb-ext:make-weak-pointer object)
  #+:cmu (ext:make-weak-pointer object)
  #+:clisp (ext:make-weak-pointer object)
  #+:allegro
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv))
  #+:openmcl
  (let ((wp (%make-weak-pointer)))
    (setf (gethash wp *weak-pointers*) object)
    wp)
  #+:corman (ccl:make-weak-pointer object)
  #+:lispworks
  (let ((array (make-array 1)))
    (hcl:set-array-weak array t)
    (setf (svref array 0) object)
    (%make-weak-pointer :pointer array)))

#-(or :allegro :openmcl :lispworks)
(defun weak-pointer-p (object)
  "Returns true if OBJECT is a weak pointer and NIL otherwise."
  #+:sbcl (sb-ext:weak-pointer-p object)
  #+:cmu (ext:weak-pointer-p object)
  #+:clisp (ext:weak-pointer-p object)
  #+:corman (ccl:weak-pointer-p object))

(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns its value. Otherwise, returns NIL."
  #+:sbcl (values (sb-ext:weak-pointer-value weak-pointer))
  #+:cmu (values (ext:weak-pointer-value weak-pointer))
  #+:clisp (values (ext:weak-pointer-value weak-pointer))
  #+:allegro (svref (weak-pointer-pointer weak-pointer) 0)
  #+:openmcl (values (gethash weak-pointer *weak-pointers*))
  #+:corman (ccl:weak-pointer-obj weak-pointer)
  #+:lispworks (svref (weak-pointer-pointer weak-pointer) 0))

;;;; Weak Hash-tables

;;; Allegro can apparently create weak hash-tables with both weak keys
;;; and weak values but it's not obvious whether it's an OR or an AND
;;; relation. TODO: figure that out.

(defun weakness-keyword-arg (weakness)
  (declare (ignorable weakness))
  #+:sbcl :weakness
  #+(or :clisp :openmcl) :weak
  #+:lispworks :weak-kind
  #+:allegro (case weakness (:key :weak-keys) (:value :values))
  #+:cmu :weak-p)

(defun weakness-keyword-opt (weakness)
  (ecase weakness
    (:key
     #+(or :lispworks :sbcl :clisp :openmcl) :key
     #+(or :allegro :cmu) t
     #-(or :lispworks :sbcl :clisp :openmcl :allegro :cmu)
     (error "Your Lisp does not support weak key hash-tables."))
    (:value
     #+:allegro :weak
     #+(or :clisp :openmcl :sbcl :lispworks) :value
     #-(or :allegro :clisp :openmcl :sbcl :lispworks)
     (error "Your Lisp does not support weak value hash-tables."))
    (:key-or-value
     #+(or :clisp :sbcl) :key-or-value
     #-(or :clisp :sbcl)
     (error "Your Lisp does not support weak key-or-value hash-tables."))
    (:key-and-value
     #+(or :clisp :sbcl) :key-and-value
     #-(or :clisp :sbcl)
     (error "Your Lisp does not support weak key-and-value hash-tables."))))

(defun make-weak-hash-table (&rest args &key weakness &allow-other-keys)
  "Returns a new weak hash table. In addition to the standard arguments
   accepted by CL:MAKE-HASH-TABLE, this function an extra keyword :WEAKNESS
   that determines the kind of weak table it should create. WEAKNESS can be
   one of :KEY, :VALUE, :KEY-OR-VALUE, :KEY-AND-VALUE.

   TG::MAKE-HASH-TABLE is available as an alias for this function should you
   wish to import it into your package and shadow CL:MAKE-HASH-TABLE."
  (remf args :weakness)
  (if weakness
      (apply #'cl:make-hash-table
             (weakness-keyword-arg weakness)
             (weakness-keyword-opt weakness)
             args)
      (apply #'cl:make-hash-table args)))

;;; If you want to use this function to override CL:MAKE-HASH-TABLE,
;;; it's necessary to shadow-import it. For example:
;;;
;;;   (defpackage #:foo
;;;     (:use #:common-lisp #:trivial-garbage)
;;;     (:shadowing-import-from #:trivial-garbage #:make-hash-table))
;;;
(defun make-hash-table (&rest args)
  (apply #'make-weak-hash-table args))

(defun hash-table-weakness (ht)
  "Returns one of NIL, :KEY, :VALUE, :KEY-OR-VALUE or :KEY-AND-VALUE."
  #-(or :allegro :sbcl :clisp :cmu :openmcl :lispworks)
  (declare (ignore ht))
  ;; keep this first if any of the other lisps bugously insert a NIL
  ;; for the returned (values) even when *read-suppress* is NIL (e.g. clisp)
  #.(if (find :sbcl *features*)
        (if (find-symbol "HASH-TABLE-WEAKNESS" "SB-EXT")
            (read-from-string "(sb-ext:hash-table-weakness ht)")
            nil)
        (values))
  #+:allegro (cond ((excl:hash-table-weak-keys ht) :key)
                   ((eq (excl:hash-table-values ht) :weak) :value))
  #+:clisp (ext:hash-table-weak-p ht)
  #+:cmu (if (lisp::hash-table-weak-p ht) :key nil)
  #+:openmcl (ccl::hash-table-weak-p ht)
  #+:lispworks (system::hash-table-weak-kind ht))

;;;; Finalizers

;;; The fact that SBCL/CMUCL throw away the object *before* running
;;; the finalizer is somewhat unfortunate...

#+(or :allegro :clisp :lispworks :openmcl)
(defvar *finalizers*
  (cl:make-hash-table :test 'eq
                      #+:allegro :weak-keys #+:allegro t
                      #+(or :clisp :openmcl) :weak
                      #+:lispworks :weak-kind
                      #+(or :clisp :openmcl :lispworks) :key)
  "Weak hashtable that holds registered finalizers.")

#+:corman
(progn
  (defvar *finalizers* '()
    "Weak alist that holds registered finalizers.")

  (defvar *finalizers-cs* (threads:allocate-critical-section)))

#+:lispworks
(progn
  (hcl:add-special-free-action 'free-action)
  (defun free-action (object)
    (let ((finalizers (gethash object *finalizers*)))
      (unless (null finalizers)
        (mapc #'funcall finalizers)))))

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
   finalizers. FUNCTION should take no arguments. Returns OBJECT.

   For portability reasons, FUNCTION should not attempt to look
   at OBJECT by closing over it because, in some lisps, OBJECT
   will already have been garbage collected and is therefore not
   accessible when FUNCTION is invoked."
  #+:cmu (ext:finalize object function)
  #+:sbcl (sb-ext:finalize object function)
  #+:allegro
  (progn
    (push (excl:schedule-finalization
           object (lambda (obj) (declare (ignore obj)) (funcall function)))
          (gethash object *finalizers*))
    object)
  #+:clisp
  ;; The CLISP code used to be a bit simpler but we had to workaround
  ;; a bug regarding the interaction between GC and weak hashtables.
  ;; See <http://article.gmane.org/gmane.lisp.clisp.general/11028>
  ;; and <http://article.gmane.org/gmane.lisp.cffi.devel/994>.
  (multiple-value-bind (finalizers presentp)
      (gethash object *finalizers* (cons 'finalizers nil))
    (unless presentp
      (setf (gethash object *finalizers*) finalizers)
      (ext:finalize object (lambda (obj)
                             (declare (ignore obj))
                             (mapc #'funcall (cdr finalizers)))))
    (push function (cdr finalizers))
    object)
  #+:openmcl
  (progn
    (ccl:terminate-when-unreachable
     object (lambda (obj) (declare (ignore obj)) (funcall function)))
    ;; store number of finalizers
    (if (gethash object *finalizers*)
        (incf (gethash object *finalizers*))
        (setf (gethash object *finalizers*) 1))
    object)
  #+:corman
  (flet ((get-finalizers (obj)
           (assoc obj *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
    (threads:with-synchronization *finalizers-cs*
      (let ((pair (get-finalizers object)))
        (if (null pair)
            (push (list (ccl:make-weak-pointer object) function) *finalizers*)
            (push function (cdr pair)))))
    (ccl:register-finalization
     object (lambda (obj)
              (threads:with-synchronization *finalizers-cs*
                (mapc #'funcall (cdr (get-finalizers obj)))
                (setq *finalizers*
                      (delete obj *finalizers*
                              :test #'eq :key #'ccl:weak-pointer-obj)))))
    object)
  #+:lispworks
  (progn
    (push function (gethash object *finalizers*))
    (hcl:flag-special-free-action object)
    object))

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  #+:cmu (ext:cancel-finalization object)
  #+:sbcl (sb-ext:cancel-finalization object)
  #+:allegro
  (progn
    (mapc #'excl:unschedule-finalization
          (gethash object *finalizers*))
    (remhash object *finalizers*))
  #+:clisp
  (multiple-value-bind (finalizers present-p)
      (gethash object *finalizers*)
    (when present-p
      (setf (cdr finalizers) nil))
    (remhash object *finalizers*))
  #+:openmcl
  (let ((count (gethash object *finalizers*)))
    (unless (null count)
      (dotimes (i count)
        (ccl:cancel-terminate-when-unreachable object))))
  #+:corman
  (threads:with-synchronization *finalizers-cs*
    (setq *finalizers*
          (delete object *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
  #+:lispworks
  (progn
    (remhash object *finalizers*)
    (hcl:flag-not-special-free-action object)))
