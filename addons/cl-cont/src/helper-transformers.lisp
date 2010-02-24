
(in-package :cont)

(export '(defun/cc without-call/cc))

;;; Nested WITH-CALL/CC
(defcpstransformer with-call/cc (cons k-expr env)
  "Transforms a nested WITH-CALL/CC."
  (expr-sequence->cps (cdr cons) k-expr env))

;;; Turning off WITH-CALL/CC
(defmacro without-call/cc (&body body)
  "A macro that allows writing macros to produce non-CPS code
uniformly within and without with-call/cc."
  `(locally ,@body))

(defcpstransformer without-call/cc (cons k-expr env)
  (declare (ignore env))
  `(funcall ,k-expr (progn ,@(cdr cons))))

;;; DEFUN
(defcpstransformer defun (cons k-expr env)
  "Transforms DEFUN expression to CPS form."
  (let* ((name (cadr cons))
         (args (caddr cons))
         (body (cdddr cons)))
    (multiple-value-bind (body declarations doc-string) (alexandria:parse-body body :documentation t)
      (let ((cps-lambda (lambda-expr->cps `(lambda ,args
                                             ,@declarations
                                             ,doc-string
                                             (block ,name
                                                    ,@body))
                                          nil
                                          env)))
        `(progn
           (setf (fdefinition ',name) ,cps-lambda)
           ,(when doc-string
              `(setf (documentation (function ,name) 'function) ,doc-string))
           (funcall ,k-expr ',name))))))

(defmacro defun/cc (name arglist &body body)
  "A helper macro to define a function that supports CALL/CC."
  `(with-call/cc
     (defun ,name ,arglist
       ;; If CL-CONT is compiled on CMUCL, we need to introduce this
       ;; declaration to turn off compilation for transformed
       ;; code. CMU compiler tends to optimize transformed code in
       ;; ways that don't work.
       #+cmu (declare (optimize (speed 0) (debug 3)))
       ,@body)))

;;; FUNCALL/APPLY
(defcpstransformer funcall (cons k-expr env)
  "Converts FUNCALL to FUNCALL/CC."
  (funcall->cps (cdr cons) k-expr nil env))

(defcpstransformer apply (cons k-expr env)
  "Converts APPLY to APPLY/CC."
  (apply->cps (cdr cons) k-expr nil env))

;;; LIST on ACL
#+allegro
(defun list->cons (members)
  "Accepts a list of elements and compiles them into recursive CONS
call. Effectively, we're compiling LIST call."
  (when members
    `(cons ,(car members) ,(list->cons (cdr members)))))

#+allegro
(defcpstransformer list (cons k-expr env)
  "Allegro seems to compile lists in a special way that
breaks some CL-CONT code."
  (expr->cps (list->cons (cdr cons)) k-expr env))

;;; Mysterious COMPILER::INTERNAL-THE on Lispworks
#+lispworks
(defcpstransformer compiler::internal-the (cons k-expr env)
  "Ignore INTERNAL-THE and process its second argument."
  (expr->cps (third cons) k-expr env))

