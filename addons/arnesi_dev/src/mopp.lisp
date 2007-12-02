;; -*- lisp -*-

;;;; * A MOP compatibility protocol

(defpackage :it.bese.arnesi.mopp
    (:nicknames :mopp)
    (:documentation "A MOP compatabilitly layer.

This package wraps the various similar but slightly different MOP
APIs. All the MOP symbols are exported (even those which are
normally exported from the common-lisp package) though not all
maybe be properly defined on all lisps.

The name of the library in an acronym for \"the Meta Object
Protocol Package\".

This package is nominally part of the arnesi utility library but
has been written so that this single file can be included in
other applications without requiring the rest of the arnesi
library.

Implementation Notes:

1) The mopp package also exports the function
   SLOT-DEFINITION-DOCUMENTATION which while not strictly part of
   the MOP specification really should be and is implementened on
   most systems.

2) On Lispworks (tested only lightly) the MOPP package
   implementes an eql-specializer class and defines a version of
   method-specializers built upon clos:method-specializers which
   returns them.")
    (:use)
    (:export
     ;; classes
     #:standard-object
     #:funcallable-standard-object
     #:metaobject
     #:generic-function
     #:standard-generic-function
     #:method
     #:standard-method
     #:standard-accessor-method
     #:standard-reader-method
     #:standard-writer-method
     #:method-combination
     #:slot-definition
     #:direct-slot-definition
     #:effective-slot-definition
     #:standard-slot-definition
     #:standard-direct-slot-definition
     #:standard-effective-slot-definition
     #:specializer
     #:eql-specializer
     #:class
     #:built-in-class
     #:forward-referenced-class
     #:standard-class
     #:funcallable-standard-class
     ;; Taken from the MOP dictionary
     #:accessor-method-slot-definition
     #:add-dependent 
     #:add-direct-method 
     #:add-direct-subclass 
     #:add-method 
     #:allocate-instance
     #:class-default-initargs
     #:class-direct-default-initargs
     #:class-direct-slots
     #:class-direct-subclasses
     #:class-direct-superclasses
     #:class-finalized-p
     #:class-name
     #:class-precedence-list
     #:class-prototype
     #:class-slots
     #:compute-applicable-methods 
     #:compute-applicable-methods-using-classes 
     #:compute-class-precedence-list 
     #:compute-default-initargs 
     #:compute-discriminating-function 
     #:compute-effective-method 
     #:compute-effective-slot-definition 
     #:compute-slots 
     #:direct-slot-definition-class 
     #:effective-slot-definition-class
     #:ensure-class-using-class 
     #:ensure-generic-function 
     #:ensure-generic-function-using-class 
     #:eql-specializer-object 
     #:extract-lambda-list 
     #:extract-specializer-names 
     #:finalize-inheritance 
     #:find-method-combination 
     #:funcallable-standard-instance-access 
     #:generic-function-argument-precedence-order
     #:generic-function-declarations
     #:generic-function-lambda-list
     #:generic-function-method-class
     #:generic-function-method-combination
     #:generic-function-methods
     #:generic-function-name
     #:intern-eql-specializer 
     #:make-instance 
     #:make-method-lambda 
     #:map-dependents
     #:method-function
     #:method-generic-function
     #:method-lambda-list
     #:method-specializers
     #:method-qualifiers
     #:reader-method-class 
     #:remove-dependent 
     #:remove-direct-method 
     #:remove-direct-subclass 
     #:remove-method 
     #:set-funcallable-instance-function 
     #:slot-boundp-using-class
     #:slot-definition-allocation
     #:slot-definition-documentation
     #:slot-definition-initargs
     #:slot-definition-initform
     #:slot-definition-initfunction
     #:slot-definition-location
     #:slot-definition-name
     #:slot-definition-readers
     #:slot-definition-writers
     #:slot-definition-type
     #:slot-makunbound-using-class 
     #:slot-value-using-class 
     #:specializer-direct-generic-functions 
     #:specializer-direct-methods 
     #:standard-instance-access 
     #:update-dependent 
     #:validate-superclass 
     #:writer-method-class))

(defpackage :it.bese.arnesi.mopp%internals
  (:use :common-lisp))

(in-package :it.bese.arnesi.mopp%internals)

(defgeneric provide-mopp-symbol (symbol implementation)
  (:documentation "Provide the implementation of the MOP symbol SYMBOL.

SYMBOL - One of the external symbols of the package it.bese.arnesi.mopp

IMPLEMENTATION - A keyword indetifying the implementation, one
of: :OPENMCL, :SBCL, :CMU, :LISPWORKS, :ALLEGRO.

Do \"something\" such that the external symbol SYMBOL in the mopp
package provides the sematics for the like named symbol in the
MOP. Methods defined on this generic function are free to
destructivly modify SYMBOL (and the mopp package) as long as when
the method terminates there is a symbol with the same name as
SYMBOL exported form the package mopp.

Methods must return a true value if they have successfully
provided SYMBOL and nil otherwise."))

(defun import-to-mopp (symbol)
  (let ((sym (find-symbol (string symbol) :it.bese.arnesi.mopp)))
    (when sym
      (unexport sym :it.bese.arnesi.mopp)
      (unintern sym :it.bese.arnesi.mopp)))
  (import symbol :it.bese.arnesi.mopp)
  (export symbol :it.bese.arnesi.mopp)
  t)

;;;; OpenMCL

(defmethod provide-mopp-symbol ((symbol symbol)
                                (implementation (eql :openmcl)))
  "Provide MOP symbols for OpenMCL.

All of OpenMCL's MOP is defined in the CCL package."
  (when (find-symbol (string symbol) :ccl)
    (import-to-mopp (find-symbol (string symbol) :ccl))))

;;;; SBCL

(defmethod provide-mopp-symbol ((symbol symbol)
                                (implementation (eql :sbcl)))
  (when (find-symbol (string symbol) :sb-mop)
    (import-to-mopp (find-symbol (string symbol) :sb-mop))))

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:slot-definition-documentation))
                                (implementation (eql :sbcl)))
  "Provide SLOT-DEFINITION-DOCUMENTATION for SBCL.

On SBCL SLOT-DEFINITION-DOCUMENTATION is just a call to
sb-pcl:documentation."
  t)

#+sbcl
(defun mopp:slot-definition-documentation (slot)
  (sb-pcl::documentation slot t))

;;;; CMUCL

(defmethod provide-mopp-symbol ((symbol symbol) (implementation (eql :cmu)))
  (when (find-symbol (string symbol) :pcl)
    (import-to-mopp (find-symbol (string symbol) :pcl))))

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:slot-definition-documentation))
                                (implementation (eql :cmu)))
  "Provide SLOT-DEFINITION-DOCUMENTATION on CMUCL.

Like SBCL SLOT-DEFINITION-DOCUMENTATION on CMUCL is just a call
to documentation."
  t)

#+cmu
(defun mopp:slot-definition-documentation (slot)
  (documentation slot t))

;;;; Lispworks

(defmethod provide-mopp-symbol ((symbol symbol) (implementation (eql :lispworks)))
  (when (find-symbol (string symbol) :clos)
    (import-to-mopp (find-symbol (string symbol) :clos))))

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:eql-specializer))
                                (implementation (eql :lispworks)))
  t)

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:eql-specializer-object))
                                (implementation (eql :lispworks)))
  t)

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:method-specializers))
                                (implementation (eql :lispworks)))
  "We can not simply export CLOS:METHOD-SPECIALIZERS as we have
to insert mopp:eql-specializers"
  t)

#+lispworks
(defclass mopp:eql-specializer ()
  ((object :accessor mopp::eql-specializer-object :initarg :object))
  (:documentation "Wrapper class representing eql-specializers.

Lispworks does not implement an eql-specializer class but simply
returns lists form method-specializers, this class (along with a
wrapper for clos:method-specializers) hide this detail."))

#+lispworks
(defun mopp:method-specializers (method)
  "More MOP-y implementation of clos:method-specializers.

For every returned value of clos:method-specializers of the
form `(eql ,OBJECT) this function returns a mopp:eql-specializer
object wrapping OBJECT."
  (mapcar (lambda (spec)
            (typecase spec
              (cons (make-instance 'mopp:eql-specializer :object (second spec)))
              (t spec)))
          (clos:method-specializers method)))

;;;; CLISP

(defmethod provide-mopp-symbol ((symbol symbol) (implementation (eql :clisp)))
  (when (find-symbol (string symbol) :clos)
    (import-to-mopp (find-symbol (string symbol) :clos))))

;;;; ALLEGRO

(defmethod provide-mopp-symbol ((symbol symbol) (implementation (eql :allegro)))
  (when (find-symbol (string symbol) :mop)
    (import-to-mopp (find-symbol (string symbol) :mop))))

(defmethod provide-mopp-symbol ((symbol (eql 'mopp:slot-definition-documentation))
				(implementation (eql :allegro)))
  t)

#+allegro
(defun mopp:slot-definition-documentation (slot)
  (documentation slot t))

;;;; ** Building the MOPP package

;;;; we can't just do a do-external-symbols since we mess with the
;;;; package and that would put us in implementation dependent
;;;; territory, so we first build up a list of all the external symbols
;;;; in mopp and then work on that list.

#+(or
   openmcl
   sbcl
   cmu
   lispworks
   clisp
   allegro)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'mopp::have-mop *features*))

#+mopp::have-mop
(let ((external-symbols '()))
  (do-external-symbols (sym (find-package :it.bese.arnesi.mopp))
    (push sym external-symbols))
  (dolist (sym external-symbols)
    (unless (provide-mopp-symbol sym #+openmcl :openmcl
                                     #+sbcl :sbcl
                                     #+cmu :cmu
                                     #+lispworks :lispworks
                                     #+clisp :clisp
				     #+allegro :allegro)
      (warn "Unimplemented MOP symbol: ~S" sym))))

#-mopp::have-mop
(warn "No MOPP implementation available for this lisp implementation.")

;; Copyright (C) 2004-2006 Edward Marco Baringer
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
