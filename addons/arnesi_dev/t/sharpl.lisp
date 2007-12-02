(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.sharpl :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.sharpl)

(enable-sharp-l-syntax)

(test sharpl-simple
  (is (eql 42 (funcall #L42))))

(test sharpl-mb-example
  (is (eql 6 (funcall #L(block !2 (return-from !2 !1)) 6))))

(test sharpl-finds-variables
  (is (eql 111 (funcall #L(+ !1 !2) 42 69))))

(test sharpl-no-variable-in-quote
  (is (eq (funcall #L'!1) '!1)))

(test sharpl-not-captures-outer-bang
  (let ((!1 42))
    (declare (ignore !1))
    (is (eql 69 (funcall #L!1 69)))))

(test sharpl-nested-simple
  (is (eql 1 (funcall (funcall #L#L1)))))

(test sharpl-nested-arg
  (is (eql 42 (funcall (funcall #L#L!1) 42))))

(test sharpl-nested-complex
  (is (eql 3 (funcall 
	      (funcall #L(let ((a !1)) 
                           #L(+ !1 a))
		       1)
	      2))))

(test sharpl-symbol-macrolet-1
  (is (eql 3 (symbol-macrolet ((sym !1)) (funcall #Lsym 3)))))

(test sharpl-symbol-macrolet-2
  (is (eql 3 (funcall (symbol-macrolet ((sym !1))
                        #Lsym)
                      3))))

(test sharpl-macrolet-1
  (is (eql 15 (macrolet ((mac (arg) `(+ !1 ,arg)))
                (funcall #L(mac 10) 5)))))

(test sharpl-macrolet-2
  (is (eql 15 (funcall (macrolet ((mac (arg) `(+ !1 ,arg)))
                         #L(mac 10))
                       5))))

(test sharpl-inner-macrolet
  (is (eql 15 (funcall 
	       #L(macrolet ((!2 () '!1)) (!2))
	       15))))

(test sharpl-inner-symbol-macrolet
  (is (eql 15 (funcall 
	       #L(symbol-macrolet ((!2 !1)) (+ !2 10))
	       5))))

(test sharpl-bang-binds-to-innermost
  (is (eql 10 (funcall 
	       (funcall #L(let ((a !1))
                            #L(+ a !1))
			6)
	       4))))

(test sharpl-interposed-macrolet
  (is (eql 6 (funcall
              (funcall #L(macrolet ((mac () '!1))
                           #L(mac)))
              6))))

(test sharpl-nested-macrolet
  (is (eql 21 (funcall
               (funcall
                #L(macrolet ((return-bang () ''!1))
                    (macrolet ((multiply-first-bang (arg) `(* ,arg ,(return-bang))))
                      #L(+ (multiply-first-bang 2) 1))))
               10))))
                          
(test sharpl-interposed-symbol-macrolet
  (is (eql 'result (funcall
                    (funcall #L(symbol-macrolet ((mac !1))
                                 #Lmac))
              'result))))
  
