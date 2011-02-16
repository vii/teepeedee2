(in-package #:tpd2.lib)

(defmacro once-only ((&rest names-and-decls) &body body)
  ;; Each name is already lexically bound to something in the macro M using once-only
  ;; For each NAME:
  ;; - Generate a unique symbol SYM with value a unique symbol SYM-VAL
  ;; - In the macro-expansion of M, bind SYM-VAL to the value of NAME
  ;; - For BODY, bind NAME to SYM

  ;; It is necessary to use the indirection SYM -> SYM-VAL so that a
  ;; new symbol will be created for each invocation of M, not just
  ;; for each invocation of once-only

    (let* ((names (mapcar 'force-first names-and-decls))
         (declarations (mapcar 'force-rest names-and-decls))
         (symbols (loop for name in names collect (gensym (string name)))))
    `(let ,(loop for symbol in symbols
              for name in names
              collect `(,symbol (gensym ,(string name))))
       `(let ,(list ,@(loop for name in names
                         for symbol in symbols
                         collect `(list ,symbol ,name)))
          ,@(list
             ,@(loop for symbol in symbols for decl in declarations
                  append
                    (loop for d in decl
                       collect ``(declare (,@,(if (listp d) d `(list `,',d)) ,,symbol)))))
          ,(let ,(loop for symbol in symbols
                    for name in names
                    collect `(,name ,symbol))
                ,@body)))))

