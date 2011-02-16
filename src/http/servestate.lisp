(in-package #:tpd2.http)

(eval-always
  (defvar *stored-servestate-header-fields* '(cookie referer user-agent accept content-type host)))

(defmacro def-specialvar-struct (name &rest fields)
  (let ((accessors (loop for f in fields
                         for n = (force-first f)
                         collect (concat-sym name '- n)))
        (special (concat-sym-from-sym-package name '* name '*)))
    (with-unique-names (v)
     `(progn
        (defvar ,special)
        (declaim (inline ,(concat-sym-from-sym-package name 'make- name) ,@accessors
                         ,@(loop for a in accessors collect `(setf ,a))))
        (defstruct ,name ,@fields)
        (declaim (type ,name ,special))

        ,@(loop for a in accessors
                for sa = (concat-sym a '*)
                collect `(progn
                           (declaim (inline ,sa (setf ,sa)))
                           (defun ,sa () (when (boundp ',special) (,a ,special)))
                           (defun (setf ,sa) (,v) (setf (,a ,special) ,v))))))))

(defmacro def-servestate-struct (&rest fields)
  `(def-specialvar-struct servestate
       ,@fields
     ,@(loop for f in *stored-servestate-header-fields*
             collect `(,f nil :type list))))

(def-servestate-struct
    (method nil :type (or null simple-byte-vector))
    (path nil :type (or null simple-byte-vector))
  (query-string nil :type (or null simple-byte-vector))
  (post-parameters nil :type (or null simple-byte-vector))
  (origin nil :type simple-byte-vector)
  (connection-close nil :type (member t nil))

  (content-length 0 :type fixnum)

  (response nil :type (or null sendbuf)))

;; Persuade SBCL's type inference that servestate-response* is a sendbuf
(declaim (inline servestate-response-as-sendbuf*)
         (ftype (function () sendbuf) servestate-response-as-sendbuf*))
(defun servestate-response-as-sendbuf* ()
  #-tpd2-debug (declare (optimize (safety 0)))
  (let ((sendbuf (servestate-response *servestate*)))
    #-tpd2-debug (declare (type sendbuf sendbuf))
    #+tpd2-debug (check-type sendbuf sendbuf)
    sendbuf))