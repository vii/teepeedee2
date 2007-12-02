;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; ** CC Version of some common lisp functions.

(defmacro redefun/cc (name args &body body)
  `(progn
     (setf (fdefinition/cc ',name)
           (make-instance 'closure/cc
                          :code (walk-form '(lambda ,args ,@body) nil '())
                          :env '()))
     ',name))

(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(redefun/cc assoc (item alist &key key (test #'eql) test-not)
  "Return the cons in ALIST whose car is equal (by TEST) to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (dolist (pair alist nil)
    (when (and pair (funcall test item (apply-key key (car pair))))
      (return pair))))

(redefun/cc assoc-if (predicate alist &key key)
  "Return the cons in ALIST whose car satisfies PREDICATE."
  (dolist (pair alist nil)
    (when (and pair (funcall predicate (apply-key key (car pair))))
      (return pair))))

(redefun/cc assoc-if-not (predicate alist &key key)
  "Return the cons in ALIST whose car does not satisfy PREDICATE."
  (assoc-if (complement predicate) alist :key key))

(redefun/cc rassoc (item alist &key key (test #'eql) test-not)
  "Return the cons in ALIST whose cdr is equal (by TEST) to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (dolist (pair alist nil)
    (when (and pair (funcall test item (apply-key key (cdr pair))))
      (return pair))))

(redefun/cc rassoc-if (predicate alist &key key)
  "Return the cons in ALIST whose cdr satisfies PREDICATE."
  (dolist (pair alist nil)
    (when (and pair (funcall predicate (apply-key key (cdr pair))))
      (return pair))))

(redefun/cc rassoc-if-not (predicate alist &key key)
  "Return the cons in ALIST whose cdr does not satisfy PREDICATE."
  (rassoc-if (complement predicate) alist :key key))

(redefun/cc sublis (alist tree &key key (test #'eql) test-not)
  "Substitute data of ALIST for subtrees matching keys of ALIST."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(let ((assoc (assoc (apply-key key subtree) alist :test test)))
		  (cond
		   (assoc (cdr assoc))
		   ((atom subtree) subtree)
		   (t (let ((car (sub (car subtree)))
			    (cdr (sub (cdr subtree))))
			(if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			    subtree
			  (cons car cdr))))))))
    (sub tree)))

(redefun/cc nsublis (alist tree &key key (test #'eql) test-not)
  "Substitute data of ALIST for subtrees matching keys of ALIST destructively."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(let ((assoc (assoc (apply-key key subtree) alist :test test)))
		  (cond
		   (assoc (cdr assoc))
		   ((atom subtree) subtree)
		   (t
		    (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree)))))
    (sub tree)))

(redefun/cc subst (new old tree &key key (test #'eql) test-not)
  "Substitute NEW for subtrees matching OLD."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(cond
		 ((funcall test old (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (let ((car (sub (car subtree)))
			  (cdr (sub (cdr subtree))))
		      (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			  subtree
			(cons car cdr)))))))
    (sub tree)))

(redefun/cc nsubst (new old tree &key key (test #'eql) test-not)
  "Substitute NEW for subtrees matching OLD destructively."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(cond
		 ((funcall test old (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree))))
    (sub tree)))

(redefun/cc subst-if (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is true."
  (labels ((sub (subtree)
		(cond
		 ((funcall predicate (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (let ((car (sub (car subtree)))
			  (cdr (sub (cdr subtree))))
		      (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			  subtree
			(cons car cdr)))))))
    (sub tree)))

(redefun/cc subst-if-not (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is false."
  (subst-if new (complement predicate) tree :key key))

(redefun/cc nsubst-if (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is true destructively."
  (labels ((sub (subtree)
		(cond
		 ((funcall predicate (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree))))
    (sub tree)))

(redefun/cc nsubst-if-not (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is false destructively."
  (nsubst-if new (complement predicate) tree :key key))

(redefun/cc tree-equal (a b &key (test #'eql) test-not)
  "Test whether two trees are of the same shape and have the same leaves."
  (when test-not
    (setq test (complement test-not)))
  (labels ((teq (a b)
		(if (atom a)
		    (and (atom b) (funcall test a b))
		  (and (consp b)
		       (teq (car a) (car b))
		       (teq (cdr a) (cdr b))))))
    (teq a b)))

(redefun/cc member (item list &key key (test #'eql) test-not)
  "Return the tail of LIST beginning with an element equal to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (do ((here list (cdr here)))
      ((or (null here) (funcall test item (apply-key key (car here)))) here)))

(redefun/cc member-if (predicate list &key key)
  "Return the tail of LIST beginning with an element satisfying PREDICATE."
  (do ((here list (cdr here)))
      ((or (endp here) (funcall predicate (apply-key key (car here)))) here)))

(redefun/cc member-if-not (predicate list &key key)
  "Return the tail of LIST beginning with an element not satisfying PREDICATE."
  (member-if (complement predicate) list :key key))

(redefun/cc adjoin (item list &key key (test #'eql) test-not)
  "Add ITEM to LIST unless it is already a member."
  (when test-not
    (setq test (complement test-not)))
  (if (member (apply-key key item) list :key key :test test)
      list
    (cons item list)))

(redefun/cc intersection (list-1 list-2 &key key (test #'eql) test-not)
  "Return the intersection of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let (result)
    (dolist (element list-1)
      (when (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))

(redefun/cc nintersection (list-1 list-2 &key key (test #'eql) test-not)
  "Return the intersection of LIST-1 and LIST-2 destructively modifying LIST-1."
  (when test-not
    (setq test (complement test-not)))
  (let* ((result (list nil))
	 (splice result))
    (do ((list list-1 (cdr list)))
	((endp list) (rplacd splice nil) (cdr result))
      (when (member (apply-key key (car list)) list-2 :key key :test test)
	(setq splice (cdr (rplacd splice list)))))))

(redefun/cc union (list-1 list-2 &key key (test #'eql) test-not)
  "Return the union of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result list-2))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))

(redefun/cc nunion (list-1 list-2 &key key (test #'eql) test-not)
  "Return the union of LIST-1 and LIST-2 destructively modifying them."
  (when test-not
    (setq test (complement test-not)))
  (do* ((result list-2)
	(list-1 list-1)
	tmp)
      ((endp list-1) result)
    (if (member (apply-key key (car list-1)) list-2 :key key :test test)
	(setq list-1 (cdr list-1))
      (setq tmp (cdr list-1)
	    result (rplacd list-1 result)
	    list-1 tmp))))

(redefun/cc subsetp (list-1 list-2 &key key (test #'eql) test-not)
  "Return T if every element in LIST-1 is also in LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (dolist (element list-1 t)
    (unless (member (apply-key key element) list-2 :key key :test test)
      (return nil))))

(redefun/cc set-difference (list-1 list-2 &key key (test #'eql) test-not)
  "Return the elements of LIST-1 which are not in LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result nil))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))

(redefun/cc nset-difference (list-1 list-2 &key key (test #'eql) test-not)
  "Return the elements of LIST-1 which are not in LIST-2, modifying LIST-1."
  (when test-not
    (setq test (complement test-not)))
  (do* ((result nil)
	(list-1 list-1)
	tmp)
      ((endp list-1) result)
    (if (member (apply-key key (car list-1)) list-2 :key key :test test)
	(setq list-1 (cdr list-1))
      (setq tmp (cdr list-1)
	    result (rplacd list-1 result)
	    list-1 tmp))))

(redefun/cc set-exclusive-or (list-1 list-2 &key key (test #'eql) test-not)
  "Return a list of elements that appear in exactly one of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result nil))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    (dolist (element list-2)
      (unless (member (apply-key key element) list-1 :key key :test test)
	(push element result)))
    result))

(redefun/cc nset-exclusive-or (list-1 list-2 &key key (test #'eql) test-not)
  "The destructive version of set-exclusive-or."
  (when test-not
    (setq test (complement test-not)))
  (do* ((head-1 (cons nil list-1))
	(head-2 (cons nil list-2))
	(p-1 head-1))
      ((or (endp (cdr p-1)) (endp (cdr head-2)))
       (progn (rplacd (last p-1) (cdr head-2))
	      (cdr head-1)))
    (do ((p-2 head-2 (cdr p-2)))
	((endp (cdr p-2)) (setq p-1 (cdr p-1)))
      (when (funcall test (apply-key key (cadr p-1)) (apply-key key (cadr p-2)))
	(rplacd p-1 (cddr p-1))
	(rplacd p-2 (cddr p-2))
	(return)))))

(redefun/cc mapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return LIST."
  (do* ((lists (cons list more-lists))
	(args (make-list (length lists))))
      ((do ((l lists (cdr l))
	    (a args (cdr a)))
	   ((or (null l) (endp (car l))) l)
	 (rplaca a (caar l))
	 (rplaca l (cdar l)))
       list)
    (apply function args)))

(redefun/cc mapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return list of results."
  (do* ((lists (cons list more-lists))
	(len (length lists))
	(args (make-list len) (make-list len))
	(result (list nil))
	(splice result))
       ((do ((l lists (cdr l))
	     (a args (cdr a)))
	    ((or (null l) (endp (car l))) l)
	  (rplaca a (caar l))
	  (rplaca l (cdar l)))
	(cdr result))
    (setq splice (cdr (rplacd splice (list (apply function args)))))))

(redefun/cc mapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return nconc of results."
  (apply #'nconc (apply #'mapcar function list more-lists)))

(redefun/cc mapl (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of list, return LIST."
  (do* ((lists (cons list more-lists)))
      ((member nil lists) list)
    (apply function lists)
    (do ((l lists (cdr l)))
	((endp l))
      (rplaca l (cdar l)))))

(redefun/cc maplist (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of list, return list of results."
  (do* ((lists (cons list more-lists))
	(result (list nil))
	(splice result))
      ((member nil lists) (cdr result))
    (setq splice (cdr (rplacd splice (list (apply function lists)))))    
    (do ((l lists (cdr l)))
	((endp l))
      (rplaca l (cdar l)))))

(redefun/cc mapcon (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of lists, return nconc of results."
  (apply #'nconc (apply #'maplist function list more-lists)))

(redefun/cc complement (function)
  (lambda (&rest arguments)
    (not (apply function arguments))))

(redefun/cc list-delete-if (test list start end count key)
  (let* ((head (cons nil list))
	 (splice head))
    (do ((i 0 (1+ i))
	 (x list (cdr x)))
	((endp x) (rplacd splice nil) (cdr head))
      (when (and count (<= count 0))
	(rplacd splice x)
	(return (cdr head)))
      (if (and (<= start i) (or (null end) (< i end))
	       (funcall test (apply-key key (car x))))
	  (when count (decf count))
          (setq splice (cdr (rplacd splice x)))))))

(redefun/cc vector-delete-if (test vector start end count key)
  (let* ((length (length vector))
	 (end (or end length))
	 (count (or count length))
	 (i 0))
    (do* ((j 0 (1+ j))
	  element)
         ((>= j length))
      (setq element (aref vector j))
      (if (and (<= start j) (< j end)
	       (plusp count)
	       (funcall test (apply-key key element)))
	  (when count (decf count))
          (progn
            (setf (aref vector i) element)
            (incf i))))
    (cond
      ((array-has-fill-pointer-p vector)
       (setf (fill-pointer vector) i)
       vector)
      ((adjustable-array-p vector) (adjust-array vector i))
      (t (subseq vector 0 i))))) 

(redefun/cc delete-if (predicate sequence &key from-end (start 0) end count key)
  "Modify SEQUENCE by deleting elements satisfying PREDICATE."
  (if from-end
      (let ((length (length sequence)))
	(nreverse (delete-if predicate (nreverse sequence)
			     :start (- length (or end length))
			     :end (- length start)
			     :count count :key key)))
    (etypecase sequence
      (null nil)
      (cons (list-delete-if predicate sequence start end count key))
      (vector (vector-delete-if predicate sequence start end count key)))))

(redefun/cc delete (item sequence &key from-end (test #'eql) test-not (start 0) end
		    count key)
  "Modify SEQUENCE by deleting elements equal to ITEM."
  (when test-not (setq test (complement test-not)))
  (delete-if #'(lambda (arg) (funcall test item arg)) sequence
	     :from-end from-end :start start :end end :count count :key key))

(redefun/cc delete-if-not (predicate sequence &key from-end (start 0) end count key)
  "Modify SEQUENCE by deleting elements not satisfying PREDICATE."
  (delete-if (complement predicate) sequence :from-end from-end
	     :start start :end end :count count :key key))

(redefun/cc remove-if (predicate sequence &key from-end (start 0) end count key)
  "Return a copy of SEQUENCE with elements satisfying PREDICATE removed."
  (delete-if predicate (copy-seq sequence) :from-end from-end :start start :end end
	     :count count :key key))

(redefun/cc remove (item sequence &key from-end (test #'eql) test-not (start 0)
		    end count key)
  "Return a copy of SEQUENCE with elements equal to ITEM removed."
  (when test-not (setq test (complement test-not)))
  (remove-if #'(lambda (arg) (funcall test item arg)) sequence
	     :from-end from-end :start start :end end :count count :key key))

(redefun/cc remove-if-not (predicate sequence &key from-end (start 0) end count key)
  "Return a copy of SEQUENCE with elements not satisfying PREDICATE removed."
  (remove-if (complement predicate) sequence :from-end from-end
	     :start start :end end :count count :key key))

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
