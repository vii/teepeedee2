;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.matcher :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.matcher)

(test eql
  (is-true (match '(:EQL 1) 1))
  (is-false (match `(:EQL ,(gensym)) (gensym)))
  (let ((sym (gensym)))
    (is-true (match `(:EQL ,sym) sym))))

(test cons
  (is-true (match '(:CONS (:EQL NIL) (:EQL NIL)) (cons nil nil)))
  (is-true (match '(:CONS 'a 'b) (cons 'a'b))))

(test list
  (is-true (match '(:LIST 'A) '(a)))
  (is-true (match '(:LIST 'A NIL) '(a nil)))  
  (is-true (match '(:LIST 'A 'B) '(a b)))
  (is-true (match '(:LIST 'A 'B :ANYTHING) '(a b c)))
  (is-true (match '(:LIST* 'A 'B :ANYTHING) '(a b)))
  (is-true (match '(:LIST* 'A 'B :ANYTHING) '(a b . 444)))
  (is-true (match '(:LIST* 'A 'B :ANYTHING) '(a b 444 555 666))))

(test alt
  (is-true (match `(:ALTERNATION (:EQL a) (:EQL b)) 'a))
  (is-true (match `(:ALTERNATION (:EQL a) (:EQL b)) 'b))
  (is-false (match `(:ALTERNATION (:EQL a) (:EQL b)) 'c))
  (is-true (match `(:ALT (:EQL a) (:EQL b) (:EQL c)) 'a))
  (is-true (match `(:ALT (:EQL a) (:EQL b) (:EQL c)) 'b))
  (is-true (match `(:ALT (:EQL a) (:EQL b) (:EQL c)) 'c))
  (is-false (match `(:ALT (:EQL a) (:EQL b) (:EQL c)) 'd)))

(test bind/ref
  (is-true (match `(:CONS (:BIND :ANYTHING $1) (:REF $1)) (cons 1 1)))
  (is-false (match `(:CONS (:BIND :ANYTHING $1) (:REF $1)) (cons 1 2)))
  (is-true (match `(:CONS (:BIND (:ALT (:EQL a) (:EQL b)) $1) (:REF $1)) (cons 'a 'a)))
  (is-true (match `(:CONS (:BIND (:ALT (:EQL a) (:EQL b)) $1) (:REF $1)) (cons 'b 'b)))
  (is-false (match `(:CONS (:BIND (:ALT (:EQL a) (:EQL b)) $1) (:REF $1)) (cons 'b 'a)))
  (is-false (match `(:CONS (:BIND (:ALT (:EQL a) (:EQL b)) $1) (:REF $1)) (cons 'a 'b)))
  (is-false (match `(:CONS (:BIND (:ALT (:EQL a) (:EQL b)) $1) (:REF $1)) (cons 1 1)))
  (is-true (match `(:CONS (:BIND (:EQUALP "AAA") aaa) (:REF aaa :test equalp)) (cons "AAA" "aaa"))))

(test sym-group
  (is-true (match `(:CONS a (:REF a)) (cons 1 1)))
  (is-false (match `(:CONS a (:NOT (:REF a))) (cons 1 1)))
  (is-true (match `(:CONS a (:NOT (:REF a))) (cons 1 2))))

(test match-case
  (match-case '(1 . 1)
    ((:CONS (:BIND (:EQL 1) a) (:REF a)) (is (= 1 a)))
    (:ANYTHING (fail)))
  (match-case '(1 . 2)
    ((:CONS a b) (is (= 1 a)) (is (= 2 b)))
    (:ANYTHING (fail "For some odd reason we didn't match")))
  (match-case '(1 . 2)
	      ((:LIST* (:BIND :ANYTHING a) (:BIND :ANYTHING b)) (is (= 1 a)) (is (= 2 b))))) 

(test and
  (match-case 3
	      ((:AND (:TEST numberp) (:TEST oddp))
	       (pass))
	      (:ANYTHING (fail)))
  (match-case 2
	      ((:AND (:TEST numberp) (:TEST oddp))
	       (fail))
	      (:ANYTHING (pass))))

(defclass foo ()
  ((x :initarg :x :accessor x)
   (z :initarg :z :accessor z)))

(test accessors
  (match-case (make-instance 'foo :x 1 :z 2)
	      ((:ACCESSORS foo x x z z)
	       (is (= 1 x))
	       (is (= 2 z)))
	      (:ANYTHING (fail)))
  (match-case (make-instance 'foo :x 1 :z 2)
	      ((:ACCESSORS standard-object x a z b)
	       (is (= 1 a))
	       (is (= 2 b)))
	      (:ANYTHING (fail)))
  (match-case (make-instance 'foo :x 1 :z 2)
	      ((:ACCESSORS cons x a z b)
	       a b			; we won't need them...
	       (fail))
	      (:ANYTHING (pass))))

(test plist  
  (match-case '(:b 2 :a 1)
	      ((:PLIST :a a :b b)
	       (is (= 1 a))
	       (is (= 2 b)))
	      (:ANYTHING (fail))))
