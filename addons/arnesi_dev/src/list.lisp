;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Working with lists

(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

(defun ensure-list (thing)
  "Returns THING as a list.

If THING is already a list (as per listp) it is returned,
otherwise a one element list containing THING is returned."
  (if (listp thing)
      thing
      (list thing)))

(defun ensure-cons (thing)
  (if (consp thing)
      thing
      (cons thing nil)))

(defun partition (list &rest lambdas)
  "Split LIST into sub lists according to LAMBDAS.

Each element of LIST will be passed to each element of LAMBDAS,
the first function in LAMBDAS which returns T will cause that
element to be collected into the corresponding list.

Examples:

 (partition '(1 2 3) #'oddp #'evenp) => ((1 3) (2))

 (partition '(1 2 3) #'oddp t) => ((1 3) (1 2 3))

 (partition '(1 2 3) #'oddp #'stringp) => ((1 3) nil)"
  (let ((collectors (mapcar (lambda (predicate)
                              (cons (case predicate
                                      ((t :otherwise) 
                                       (constantly t))
                                      ((nil)
                                       (constantly nil))
                                      (t predicate))
                                    (make-collector)))
                            lambdas)))
    (dolist (item list)
      (dolist* ((test-func . collector-func) collectors)
        (when (funcall test-func item)
          (funcall collector-func item))))
    (mapcar #'funcall (mapcar #'cdr collectors))))

(defun partitionx (list &rest lambdas)
  (let ((collectors (mapcar (lambda (l)
                              (cons (if (and (symbolp l)
					     (member l (list :otherwise t)
                                                     :test #'string=))
                                        (constantly t)
                                        l)
                                    (make-collector)))
                            lambdas)))
    (dolist (item list)
      (block item
        (dolist* ((test-func . collector-func) collectors)
          (when (funcall test-func item)
            (funcall collector-func item)
            (return-from item)))))
    (mapcar #'funcall (mapcar #'cdr collectors))))

(defmacro dotree ((name tree &optional ret-val) &body body)
  "Evaluate BODY with NAME bound to every element in TREE. Return RET-VAL."
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
                  (dolist (,list-element ,list)
                    (if (consp ,list-element)
                        (,traverser ,list-element)
                        (let ((,name ,list-element))
                          ,@body)))))
         (,traverser ,tree)
         ,ret-val))))

(define-modify-macro push* (&rest items)
  (lambda (list &rest items)
    (dolist (i items)
      (setf list (cons i list)))
    list)
  "Pushes every element of ITEMS onto LIST. Equivalent to calling PUSH
  with each element of ITEMS.")

(defun proper-list-p (object)
  "Tests whether OBJECT is properlist.

A proper list is a non circular cons chain whose last cdr is eq
to NIL."
  (or
   (null object)
   (and (consp object)
	;; check if the last cdr of object is null. deal with
	;; circular lists.
	(loop 
	 for turtoise = object then (cdr turtoise)
	 for hare = (cdr object) then (cddr hare)
	 ;; we need to agressivly check hare's cdr so that the call to
	 ;; cddr doesn't signal an error
	 when (eq turtoise hare) return nil
	 when (null turtoise) return t
	 when (null hare) return t
	 when (not (consp hare)) return nil
	 when (null (cdr hare)) return t
	 when (not (consp (cdr hare))) return nil
	 when (null (cddr hare)) return t
	 when (not (consp (cddr hare))) return nil))))

;;;; ** Simple list matching based on code from Paul Graham's On Lisp.

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-unique-names (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))

(defun varsymp (x)
  (and (symbolp x) (eq (aref (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun list-match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_))
     (values binds t))
    ((binding x binds) (list-match it y binds))
    ((binding y binds) (list-match x it binds))
    ((varsymp x) (values (cons (cons x y) binds) t))
    ((varsymp y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (list-match (car x) (car y) binds))
     (list-match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun vars (match-spec)
  (let ((vars nil))
    (labels ((find-vars (spec)
               (cond
                 ((null spec) nil)
                 ((varsymp spec) (push spec vars))
                 ((consp spec)
                  (find-vars (car spec))
                  (find-vars (cdr spec))))))
      (find-vars match-spec))
    (delete-duplicates vars)))

(defmacro list-match-case (target &body clauses)
  (if clauses
      (destructuring-bind ((test &rest progn) &rest others)
          clauses
        (with-unique-names (tgt binds success)
          `(let ((,tgt ,target))
             (multiple-value-bind (,binds ,success)
                 (list-match ,tgt ',test)
               (declare (ignorable ,binds))
               (if ,success
                   (let ,(mapcar (lambda (var)
                                   `(,var (cdr (assoc ',var ,binds))))
                                 (vars test))
                     (declare (ignorable ,@(vars test)))
                     ,@progn)
                   (list-match-case ,tgt ,@others))))))
      nil))

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
