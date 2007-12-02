;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.call/cc :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.call/cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *call/cc-returns* nil))

(test call/cc-constant
  (is (= 4 (with-call/cc 4)))
  (is (eql :a (with-call/cc :a)))
  (is (eql 'a (with-call/cc 'a)))
  (is (eql #'+ (with-call/cc #'+))))

(test call/cc-progn
  (is (null (with-call/cc)))
  (is (= 1 (with-call/cc 1)))
  (is (= 2 (with-call/cc 1 2)))
  (is (= 3 (with-call/cc 1 2 3)))
  (is (= 4 (with-call/cc 1 2 3 4))))

(test call/cc-progn/cc
  (is (= 1 (kall (with-call/cc (let/cc k k) 1))))
  (is (= 1 (kall (with-call/cc (let/cc k k) 0 1)))))

(test call/cc-let
  (is (= 1 (with-call/cc
	    (let () 1))))
  (is (= 1 (with-call/cc
	    (let ((a 1)) a))))
  (is (= 1 (with-call/cc
             (let ((a 1))
               (let ((a nil)
                     (b a))
                 (declare (ignore a))
                 b)))))
  (with-call/cc
    (let ((a 1))
      (let ((a 2))
        (is (= 2 a)))
      (is (= 1 a))))

  (let ((cont nil))
    (setf cont
          (with-call/cc
            (let ((a (let/cc k k)))
              (+ a 4))))
    (is (= 9 (kall cont 5)))
    (is (= 12 (kall cont 8)))))

(test call/cc-let/cc
  (let ((k (with-call/cc
             (let ((a (arnesi::retk)))
               (+ a 1)))))
  (is (= 1 (arnesi::kall k 0)))
  (is (= 2 (arnesi::kall k 1)))))

(test call/cc-setq
  (is (= 1 (with-call/cc
             (let ((a nil)) (setq a 1)))))
  (is (= 2 (with-call/cc
             (let ((a 1)) (setq a (1+ a)))))))

(test call/cc-let*
  (with-call/cc
    (let* ((a 1)
	   (b a))
      (is (= 1 a))
      (is (= 1 b))))
  (with-call/cc
   (let ((a 0)
	 (b 1))
     (declare (ignore a))
     (let* ((a b)
	    (b a))
       (is (= a 1))
       (is (= b 1))
       (setq a 47)
       (is (= a 47))))))

(test call/cc-apply
  (is (= 0 (with-call/cc (+))))
  (is (= 1 (with-call/cc (+ 1))))
  (is (= 2 (with-call/cc (+ 1 1))))
  (is (= 3 (with-call/cc (+ 1 (+ 1 (+ 1 (+))))))))

(test call/cc-if
  (is (= 1 (with-call/cc (if t 1))))
  (is (= 1 (with-call/cc (if nil 0 1))))
  (is (null (with-call/cc (if nil 1)))))

(test call/cc-block/return-from
  (is (= 1
         (with-call/cc
           (block foo
               nil
               (return-from foo 1)
               nil))))
  (is (eql t 
           (with-call/cc
               (block foo
                 (return-from foo t)
                 nil)))))

(defun reached-unreachable-code ()
  (fail "Somehow we reached unreachable code in a tagbody."))

(test call/cc-tagbody
  (with-call/cc
    (tagbody
       (go a)
       (reached-unreachable-code)
       a
       (pass)))
  (with-call/cc
    (tagbody
       (go a) (reached-unreachable-code)
     b
       (pass)
       (go c) (reached-unreachable-code)
     a
       (pass)
       (go b) (reached-unreachable-code)
     c
       (pass)))
  (with-call/cc
    (let ((counter 0))
      (dotimes (i 5)
        (incf counter))
      (is (= 5 counter))))
  (with-call/cc
    (let ((i 0))
      (tagbody
       a (incf i) (is (= 1 i))
       b (incf i) (is (= 2 i))
       c (is (= 2 i))))))

(test call/cc-flet
  (with-call/cc
    (flet ((foo () 'x))
      (is (eql 'x (foo))))
    (is (= 4 (funcall (let ((a 4))
                        (flet ((foo () a))
                          #'foo)))))
    (flet ((foo ()
             'outer-foo))
      (flet ((foo ()
               'inner-foo)
             (bar ()
               (foo)))
        (is (eql 'outer-foo (bar)))))))

(test call/cc-labels
  (with-call/cc
    (labels ((foo () 'x))
      (is (eql 'x (foo))))
    (labels ((foo () 'outer-foo))
      (labels ((bar () (foo))
               (foo () 'inner-foo))
        (is (eql 'inner-foo (bar))))))
  (finishes 
    (with-call/cc
      (labels ((rec (x) x))
        #'rec
        (is (= 1 (funcall #'rec 1)))
        (is (= 1 (apply #'rec (list 1)))))
      (flet ((f () 1))
        (is (= 1 (f)))
        (is (= 1 (funcall #'f)))
        (is (= 1 (apply #'f '()))))))
  (let ((cont (with-call/cc
                (labels ((rec (n)
                           (if (zerop n)
                               0
                               (+ (rec (1- n))
                                  (let/cc k k)))))
                  (rec 2)))))
    (is (= 5 (kall (kall cont 2) 3)))))

(let ((value 0))
  (defun test-funcall.0 ()
    value)
  (defun (setf test-funcall.0) (new-value)
    (setf value new-value)))

(test call/cc-setf-funcall
  (setf (test-funcall.0) 0)
  (is (= 0 (with-call/cc (test-funcall.0))))
  (is (= 1 (with-call/cc (setf (test-funcall.0) 1))))
  (is (= 2 (with-call/cc (funcall #'(setf test-funcall.0) 2)))))

(test call/cc-lambda-requried-arguments
  (with-call/cc
    (is (eql t (funcall (lambda () t))))
    (is (eql t (funcall (lambda (x) x) t))))
  (signals error
    (with-call/cc
      (funcall (lambda (x) x)))))

(test call/cc-lambda-optional-arguments
  (with-call/cc
    (is (eql t (funcall (lambda (&optional a) a) t)))
    (is (eql t (funcall (lambda (&optional (a t)) a)))))

  (let ((cont (with-call/cc
                (funcall (lambda (&optional (a (let/cc k k)))
                           (+ a 1))))))
    (is (= 1 (kall cont 0)))))

(test call/cc-lambda-keyword-arguments
  (with-call/cc
    (is (eql 'a   (funcall (lambda (&key a) a) :a 'a)))
    (is (eql 'b   (funcall (lambda (&key (a 'b)) a))))
    (is (eql t    (funcall (lambda (&optional a &key (b (not a))) b))))
    (is (eql nil  (funcall (lambda (&optional a &key (b (not a)))
                             b)
                           t)))
    (is (eql 42 (funcall (lambda (&optional a &key (b (not a)))
                           b)
                         t :b 42)))))

(defun/cc test-defun/cc1 ()
  (let/cc k k))

(defun/cc test-defun/cc2 (arg1)
  (let/cc k k)
  arg1)

(defun/cc test-defun/cc3 (a &key (b 1))
  (+ a b))

(test call/cc-defun/cc
  (let ((cont nil))
    (setf cont (with-call/cc (test-defun/cc1)))
    (is (eql nil (kall cont nil)))

    (setf cont (with-call/cc (test-defun/cc2 'foo)))
    (is (eql 'foo (kall cont)))
    (is (eql 'foo (kall cont nil)))

    (with-call/cc
      (is (= 1 (test-defun/cc3 0)))
      (is (= 2 (test-defun/cc3 1))))))

(defgeneric/cc test-generic/cc (a &key v))

(defmethod/cc test-generic/cc ((a symbol) &key (v 3))
  v)

(defmethod/cc test-generic/cc ((a string) &key (v 5))
  v)

(test call/cc-defgeneric/cc
  (with-call/cc
    (is (= 3 (test-generic/cc 'a)))
    (is (= 0 (test-generic/cc 'a :v 0)))
    (is (= 5 (test-generic/cc "a")))
    (is (= 0 (test-generic/cc "a" :v 0)))))

(defmethod/cc test-generic/cc2 :before (a)
  (let/cc k 'before))

(defmethod/cc test-generic/cc2 (a)
  'primary)

(test test-generic/cc2
  (with-call/cc
   (is (eql 'before (test-generic/cc2 t)))))

(defmethod/cc test-generic/cc3 :before (a)
  (let/cc k (cons 'before k)))

(defmethod/cc test-generic/cc3 :around (a)
  (let/cc k (cons 'around k))
  (call-next-method a))

(defmethod/cc test-generic/cc3 (a)
  (let/cc k (cons 'primary k))
  a)

(defmethod/cc test-generic/cc3 :after (a)
  (let/cc k (cons 'after k)))

(test call/cc-defgeneric/cc3
  (destructuring-bind (value . cont)
      (with-call/cc (test-generic/cc3 32))
    (is (eql 'around value))
    (destructuring-bind (value . cont)
        (with-call/cc (kall cont))
      (is (eql 'before value))
      (destructuring-bind (value . cont)
          (with-call/cc (kall cont))
        (is (eql 'primary value))
        (destructuring-bind (value . cont)
            (with-call/cc (kall cont))
          (is (eql 'after value))
          (is (eql 32 (kall cont))))))))

(test call/cc-loop
  (let ((cont (with-call/cc
                (loop
                   repeat 2
                   sum (let/cc k k) into total
                   finally (return (values total total))))))
    (multiple-value-bind (a b)
        (kall (kall cont 1) 2)
      (is (= 3 a))
      (is (= 3 b))))

  (let ((cont (with-call/cc
                (block done
                  (loop
                     for how-many = (let/cc k k)
                     do (loop
                           repeat how-many
                           sum (let/cc k k) into total
                           finally (return-from done total)))))))
    (is (= 26 (kall (kall (kall cont 2) 13) 13)))))

(test common-lisp/cc
  (let (cont value)
    (setf cont (with-call/cc (mapcar (lambda (x)
                                       (+ x (let/cc k k)))
                                     (list 1 2 3))))
    (setf cont (with-call/cc (kall cont -1))
          cont (with-call/cc (kall cont -2))
          value (with-call/cc (kall cont -3)))
    (is (equal (list 0 0 0) value))))

(defun/cc throw-something (something)
  (throw 'done something))

(test catch/cc
  (with-call/cc
    (is (eql t
             (catch 'whatever
               (throw 'whatever t)
               (throw 'whatever nil)
               'something-else)))
    (is (eql t
             (catch 'whatever
               t)))
    (is (eql t
             (flet ((throw-it (it)
                      (throw 'done it)))
               (catch 'done
                 (throw-it t)
                 (throw 'done 'bad-bad-bad)))))
    (is (eql t
             (catch 'done
               (throw-something t)
               nil)))))

(test multiple-value-call
  (with-call/cc
      (is (= 1 (multiple-value-call
                   #'identity
                 (values 1)))))
  (with-call/cc
      (is (= 3 (length (multiple-value-call
                           #'list
                         (values 1)
                         (values 1)
                         (values 1))))))

  (with-call/cc
      (is (= 3 (multiple-value-call
                   (lambda (a b)
                     (+ a b))
                 (values 1 2)))))

  (with-call/cc
      (is (= 3 (multiple-value-call
                   (lambda (&rest numbers)
                     (reduce #'+ numbers))
                 (values -1 1)
                 (values 1)
                 (values -1)
                 (values 1 2))))))

;;; speical variable handling
(defun/cc lookup-special-in-defun/cc (stop)
  (declare (special var))
  (when stop (let/cc k k))
  var)

(defun/cc lookup-special-in-let/cc (stop)
  (let ((normal 0))
    (declare (special var))
    (when stop (let/cc k k))
    var))

(defun/cc lookup-special-in-let*/cc (stop)
  (let* ((normal 0))
    (declare (special var))
    (when stop (let/cc k k))
    var))

(defun lookup-special-in-lisp ()
  (declare (special var))
  var)

(defun/cc define-and-lookup-special-in-defun/cc (stop)
  (let ((var 1))
    (declare (special var))
    (when stop (let/cc k k))
    var))

(defun/cc export-special-from-let/cc-and-lookup-in-defun/cc (stop)
  (let ((var 1))
    (declare (special var))
    (lookup-special-in-defun/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-let/cc (stop)
  (let ((var 1))
    (declare (special var))
    (lookup-special-in-let/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-let*/cc (stop)
  (let ((var 1))
    (declare (special var))
    (lookup-special-in-let*/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-lisp (stop)
  (let ((var 1))
    (declare (special var))
    (when stop (let/cc k k))
    (lookup-special-in-lisp)))

(defun/cc export-special-from-let*/cc-and-lookup-in-defun/cc (stop)
  (let* ((var 1))
    (declare (special var))
    (lookup-special-in-defun/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-let/cc (stop)
  (let* ((var 1))
    (declare (special var))
    (lookup-special-in-let/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-let*/cc (stop)
  (let* ((var 1))
    (declare (special var))
    (lookup-special-in-let*/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-lisp (stop)
  (let* ((var 1))
    (declare (special var))
    (when stop (let/cc k k))
    (lookup-special-in-lisp)))

(defun export-special-from-lisp-and-lookup-in-defun/cc (stop)
  (let ((var 1))
    (declare (special var))
    (with-call/cc
      (lookup-special-in-defun/cc stop))))

(defun export-special-from-lisp-and-lookup-in-let/cc (stop)
  (let ((var 1))
    (declare (special var))
    (with-call/cc
      (lookup-special-in-let/cc stop))))

(defun export-special-from-lisp-and-lookup-in-let*/cc (stop)
  (let ((var 1))
    (declare (special var))
    (with-call/cc
      (lookup-special-in-let*/cc stop))))

(defmacro test-special (name)
  (let ((body-without-stop `(,name nil))
        (body-with-stop `(,name t)))
    `(test ,name
      (is (= 1 (with-call/cc ,body-without-stop)))
      (signals unbound-variable
        (with-call/cc ,body-without-stop (lookup-special-in-lisp)))
      (signals unbound-variable
        (with-call/cc ,body-without-stop (lookup-special-in-defun/cc nil)))
      ;; now stop once
      (is (= 1 (kall (with-call/cc ,body-with-stop))))
      (signals unbound-variable
        (kall (with-call/cc ,body-with-stop (lookup-special-in-lisp))))
      (signals unbound-variable
        (kall (with-call/cc ,body-with-stop (lookup-special-in-defun/cc nil)))))))

;; export and lookup in the same lexical environment
(test-special define-and-lookup-special-in-defun/cc)

;; export and lookup in cc code
(test-special export-special-from-let/cc-and-lookup-in-defun/cc)
(test-special export-special-from-let/cc-and-lookup-in-let/cc)
(test-special export-special-from-let/cc-and-lookup-in-let*/cc)
(test-special export-special-from-let*/cc-and-lookup-in-defun/cc)
(test-special export-special-from-let*/cc-and-lookup-in-let/cc)
(test-special export-special-from-let*/cc-and-lookup-in-let*/cc)

;; export from cc code and lookup in lisp code
(test-special export-special-from-let/cc-and-lookup-in-lisp)
(test-special export-special-from-let*/cc-and-lookup-in-lisp)

;; export from lisp code and lookup in cc code
(test-special export-special-from-lisp-and-lookup-in-defun/cc)
(test-special export-special-from-lisp-and-lookup-in-let/cc)
(test-special export-special-from-lisp-and-lookup-in-let*/cc)

;; export in lisp code let it go through some cc code and lookup in lisp code after continuing
(test export-special-from-lisp-and-lookup-in-lisp
  (is (= 1
         (kall (let ((var 1))
                 (declare (special var))
                 (with-call/cc
                   (let () ;; TODO: shouldn't we allow declares within with-call/cc?
                     (declare (special var))
                     (let/cc k k)
                     (lookup-special-in-lisp))))))))

(defvar *special-variable-in-lisp* 42)

(test special-lisp-var-rebound-in/cc
  (is (= 42
         (with-call/cc
           *special-variable-in-lisp*)))
  (is (= 43
         (with-call/cc
           (let ((*special-variable-in-lisp* 43))
             ;;(declare (special *special-variable-in-lisp*)) ; TODO shouldn't be needed
             *special-variable-in-lisp*)))))
