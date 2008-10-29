(in-package #:tpd2.lib)

(defmacro unquote (form)
  (warn "Not in a superquote form: ~A" form)
  form)

(defmacro unquote-splice (form)
  (warn "Not in a superquote form: ~A" form)
  form)

(defun superquote-function-go (form)
  (typecase form
    (list (case (first form)
	    (unquote-splice (second form))
	    (unquote (second form))
	    (t `(append ,@(loop for f in form
			     if (eq (first (force-list f)) 'unquote-splice)
			     collect (second f)
			     else
			     collect `(list ,(superquote-function-go f)))))))
    (t `',form)))

(defun superquote-function (form)
  (superquote-function-go form))

(defun superquote-form-constantp (form env)
  (typecase form
    (list (case (first form)
	    ((unquote-splice unquote) (load-time-constantp (second form) env))
	    (t (every (lambda(form) (superquote-form-constantp form env)) form))))
    (t t)))

(defmacro superquote (form &environment env)
  (if (superquote-form-constantp form env)
      `(read-only-load-time-value ,(superquote-function-go form))
      (superquote-function form)))

(define-compiler-macro superquote-function (&whole form quotee &environment env)
  (if (and (superquote-form-constantp quotee env) (load-time-constantp quotee env))
      `(read-only-load-time-value ,(superquote-function-go quotee))
      form))

