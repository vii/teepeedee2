;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(in-suite :it.bese.arnesi)

(test bracket-reader
  (enable-bracket-syntax)
  (is (= 7 (read-from-string "{(constantly 7)}")))  
  (destructuring-bind (progn a b c)
      (let ((*package* (find-package :common-lisp-user)))
        (read-from-string "{(arnesi::with-package :arnesi) a b c}"))
    (is (eql 'cl:progn progn))
    (is (eql 'arnesi::a a))
    (is (eql 'arnesi::b b))
    (is (eql 'arnesi::c c))))


