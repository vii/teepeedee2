;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A fare-like matchingfacility

;;;; The code is written in CPS style, it's hard to understand at
;;;; first but once you "get it" it's actually quite simple. Basically
;;;; the idea is that at every point during a match one of two things
;;;; can happen, the match can succeed or it can fail. What we do is
;;;; we pass every match two functions (closures usually), one which
;;;; specifies what to if it succeeds and one which specifies what to
;;;; do if it fails. These two closures can refer to the original
;;;; match parameter and hence we can easily "backtrack" if we
;;;; fail. Another important aspect is that we explicitly pass the
;;;; target against which to match, if we didn't do this it would be
;;;; impossible to really backtrack.

;;;; ** The matching and compiling environment

(deflookup-table match-handler
  :documentation "Table mapping symbol names to the matching function")

(defstruct (match-state (:conc-name ||))
  target
  bindings
  matched)

(defun copy-state (orig-state
		   &key (target nil target-supp)
		        (bindings nil bindings-supp)
			(matched nil matched-supp))
  "Make a copy ORIG-STATE."
  (make-match-state :target (if target-supp
				target
  			        (target orig-state))
		    :bindings (if bindings-supp
				  bindings
				  (bindings orig-state))
		    :matched (if matched-supp
				 matched
			         (matched orig-state))))

(defmacro def-matcher (name args &body body)
  `(progn (setf (get-match-handler ',name)
		(lambda ,args ,@body))
	  ',name))

(defmacro def-matcher-macro (name args &body body)
  `(progn (setf (get-match-handler ',name)
		(lambda ,args
		  (%make-matcher (progn ,@body))))
	  ',name))

;;;; ** Matching

(defun make-matcher (spec)
  "Create a matcher function from SPEC."
  (let ((%bind-vars% '()))
    (declare (special %bind-vars%))
    (values (%make-matcher spec)
	    %bind-vars%)))

(defun %make-matcher (spec)
  ;; NIL means many different things, deal with it explicitly
  (if (eql nil spec)
      (%make-matcher `(:eql ,spec))
      (if (listp spec)
          (aif (get-match-handler (car spec))
               (apply it (cdr spec))
               (error "Don't know how to handle ~S" spec))
          (aif (get-match-handler spec)
               ;; we allow :x as a an abbreviation for (:x)
               (funcall it)
               (if (and (symbolp spec)
                        (not (keywordp spec)))
                   (%make-matcher `(:bind :anything ,spec))
                   (if (constantp spec)
                       (%make-matcher `(:eql ,spec))
                       (error "Don't know how to deal with ~S" spec)))))))

(defun match (matcher target)
  "Attempt to match MATCHER against TARGET. MATCHER can be either a
function or a list."
  (if (functionp matcher)
      (funcall matcher
	       (make-match-state :target target
				 :bindings '()
				 :matched nil)
	       (lambda (s k q)
		 (declare (ignore k q))
		 (return-from match (values t
					    (matched s)
					    (bindings s))))
	       (lambda (s k q)
		 (declare (ignore s k q))
		 (return-from match (values nil nil nil))))
      (match (make-matcher matcher) target)))

(defmacro match-case (form &rest clauses)
  "NB: the clauses wil be compiled at macro expansion time."
  (when clauses
    (destructuring-bind ((spec &rest body) &rest other-clauses) clauses
      (with-unique-names (form-sym matched-p dummy bindings)
        (multiple-value-bind (matcher-func vars)
	    (make-matcher spec)
	  (declare (ignore matcher-func))
	  `(let ((,form-sym ,form))
	     (multiple-value-bind (,matched-p ,dummy ,bindings)
		 (match (make-matcher ',spec) ,form-sym)
	       (declare (ignore ,dummy) (ignorable ,bindings))
	       (if ,matched-p
		   (let ,vars
		     ,@(mapcar (lambda (var-name)
				 `(setf ,var-name (cdr (assoc ',var-name ,bindings))))
			       vars)
		     ,@body)
		   (match-case ,form-sym ,@other-clauses)))))))))

;;;; ** Matching forms

(def-matcher :bind (spec var)
  "The :bind matcher attempts to match MATCHER and bind whatever
   MATCHER consumnd to VAR. group is equivalent to SPEC except the value
   of matched when spec has matched will be bound to var."
  (declare (special %bind-vars%))
  (push var %bind-vars%)
  (let ((spec-matcher (%make-matcher spec)))
    (lambda (s k q)
      (funcall spec-matcher s
	       (lambda (s. k. q.)
		 (declare (ignore k.))
		 ;; SPEC succeded, bind var
		 (funcall k (copy-state s. :bindings (cons (cons var (matched s.)) (bindings s.)))
			  k q.))
	       q))))

(def-matcher :ref (var &key (test #'eql))
  (lambda (s k q)
    (if (and (assoc var (bindings s))
	     (funcall test (target s) (cdr (assoc var (bindings s)))))
	(funcall k (copy-state s :matched (target s))
		 k q)
        (funcall q s k q))))

(def-matcher :alternation (a-spec b-spec)
  (let ((a (%make-matcher a-spec))
	(b (%make-matcher b-spec)))
    (lambda (s k q)
      ;; first try A
      (funcall a s k
	       ;; a failed, try B
	       (lambda (s. k. q.)
		 (declare (ignore s. k. q.))
		 (funcall b s k q))))))

(def-matcher-macro :alt (&rest possibilities)
  (case (length possibilities)
    (0 `(:fail))
    (1 (car possibilities))
    (t `(:alternation ,(car possibilities) (:alt ,@(cdr possibilities))))))

(def-matcher :fail ()
  (lambda (s k q)
    (funcall q s k q)))

(def-matcher :not (match)
  (let ((m (%make-matcher match)))
    (lambda (s k q)
      (funcall m s q k))))

(def-matcher :anything ()
  (lambda (s k q)
    (funcall k (copy-state s :matched (target s))
	     k q)))

;;;; ** Matching within a sequence

(defun next-target ()
  (declare (special *next-target*))
  (funcall *next-target*))

(defun make-greedy-star (m)
  (lambda (s k q)
    (if (funcall m (target s))
        (funcall (make-greedy-star m) (copy-state s
                                                  :matched (target s)
                                                  :target (next-target))
                 k (lambda (s. k. q.)
                     (declare (ignore k. s.))
                     (funcall k s k q.)))
        (funcall q s k q))))

(def-matcher :greedy-star (match)
  (make-greedy-star (%make-matcher match)))

;;;; ** The actual matching operators

;;;; All of the above allow us to build matchers but non of them
;;;; actually match anything.

(def-matcher :test (predicate)
  "Matches if the current matches satisfies PREDICATE."
  (lambda (s k q)
    (if (funcall predicate (target s))
	(funcall k (copy-state s :matched (target s))
		 k q)
        (funcall q s k q))))

(def-matcher-macro :test-not (predicate)
  `(:not (:test ,predicate)))

(def-matcher-macro :satisfies-p (predicate)
  `(:test ,(lambda (target) (funcall predicate target))))

(def-matcher-macro :eq (object)
  `(:test ,(lambda (target) (eq object target))))

(def-matcher-macro :eql (object)
  `(:test ,(lambda (target) (eql object target))))

(def-matcher-macro cl:quote (constant)
  `(:eql ,constant))

(def-matcher-macro :equal (object)
  `(:test ,(lambda (target) (equal object target))))

(def-matcher-macro :equalp (object)
  `(:test ,(lambda (target) (equalp object target))))

(def-matcher :cons (car-spec cdr-spec)
  (let ((car (%make-matcher car-spec))
	(cdr (%make-matcher cdr-spec)))
    (lambda (s k q)
      (if (consp (target s))
	  (funcall car (copy-state s :target (car (target s)))
		   (lambda (s. k. q.)
		     (declare (ignore k.))
		     ;; car matched, try cdr
		     (funcall cdr (copy-state s. :target (cdr (target s)))
			      (lambda (s.. k.. q..)
				(declare (ignore k.. q..))
				;; cdr matched, ok, we've matched!
				(funcall k (copy-state s.. :matched (target s))
					 k q))
			      q.))
		   q)
	  (funcall q s k q)))))

(def-matcher-macro :list (&rest items)
  `(:list* ,@items nil))

(def-matcher-macro :list* (&rest items)
  (case (length items)
    (1 (car items))
    (2 `(:cons ,(first items) ,(second items)))
    (t
     `(:cons ,(first items) (:list* ,@(cdr items))))))

(def-matcher :property (key value-spec)
  (let ((value (%make-matcher value-spec)))
    (lambda (s k q)
      (if (listp (target s))
	  (aif (getf (target s) key)
	       (funcall value (copy-state s :target it)		
			(lambda (s. k. q.)
			  (declare (ignore k. q.))
			  (funcall k (copy-state s. :matched (target s))
				   k q))
			q)
	       (funcall q s k q))
	  (funcall q s k q)))))

(def-matcher :accessor (type accessor value-spec)
  (let ((value (%make-matcher value-spec)))
    (lambda (s k q)
      (if (typep (target s) type)
	  (funcall value (copy-state s :target (funcall accessor (target s)))		
		   (lambda (s. k. q.)
		     (declare (ignore k. q.))
		     (funcall k (copy-state s. :matched (target s))
			      k q))
		   q)
	  (funcall q s k q)))))

(def-matcher :and (a-spec b-spec)
  (let ((a (%make-matcher a-spec))
	(b (%make-matcher b-spec)))
    (lambda (s k q)
      (funcall a s
	       (lambda (s. k. q.)
		 (declare (ignore k. q.))
		 (funcall b (copy-state s. :target (target s))
			  k q))
	       q))))

(def-matcher-macro :plist (&rest items)
  (case (length items)
    (1 (error ":PLIST has been given an odd num of args."))
    (2 `(:PROPERTY ,(first items) ,(second items)))
    (t
     `(:AND (:PROPERTY ,(first items) ,(second items))
	    (:PLIST ,@(nthcdr 2 items))))))

(def-matcher-macro :accessors (type &rest accs-vals)
  (case (length accs-vals)
    (1 (error ":ACCESSORS has been given an odd num of args."))
    (2 `(:ACCESSOR ,type ,(first accs-vals) ,(second accs-vals)))
    (t
     `(:AND (:ACCESSOR ,type ,(first accs-vals) ,(second accs-vals))
	    (:ACCESSORS ,type ,@(nthcdr 2 accs-vals))))))

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
