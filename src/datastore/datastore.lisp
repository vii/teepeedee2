(in-package #:tpd2.datastore)

(defvar *datastore*)
(defvar *datastore-id-max* 0)

(defun datastore-load (file)
  ; SBCL struggles as it tries to compile these large files if a simple load is used
  (with-open-file (stream file :if-does-not-exist nil)
    (when stream
      (loop for form = (read stream nil 'eof)
            until (eq form 'eof)
            do (eval form)))))

(defun datastore-open-p ()
  (and (boundp '*datastore*) *datastore*))

(defun datastore-use-file (filename)
  (unless (datastore-open-p)
    (datastore-load filename)
    (setf *datastore* (open filename :direction :output :if-exists :append :if-does-not-exist :create #+ccl :sharing #+ccl :lock))))

(defun datastore-ref-form (object)
  `(datastore-retrieve-unique ',(class-name (class-of object)) 'datastore-id ,(slot-value object 'datastore-id)))

(defun datastore-close ()
  (when (datastore-open-p)
    (close *datastore*)
    (setf *datastore-id-max* 0))
  (makunbound '*datastore*))

(defun datastore-log (list)
  (when (datastore-open-p)
    (with-standard-io-syntax
      (format *datastore* "~S~&" list))
    (force-output *datastore*)))

(defun datastore-id-next ()
  *datastore-id-max*)
(defun datastore-id-register (id)
  (when (>= id *datastore-id-max*)
    (setf *datastore-id-max* (1+ id))))

(defstruct datastore-index
  slot-name
  (table (make-hash-table :test #'equal)))

(my-defun datastore-index object-value (object)
  (slot-value object (my slot-name)))

(my-defun datastore-index add (object)
  (push object (gethash (my object-value object) (my table))))

(my-defun datastore-index del (object)
  (deletef object (gethash (my object-value object) (my table))))


(my-defun datastore-index get (value)
  (gethash value (my table)))

(defgeneric datastore-delete (object))

(defgeneric datastore-retrieve-all (class &optional max-returned))

(defgeneric datastore-retrieve-indexed (class index value))

(defun datastore-retrieve-unique (class index value)
  (let ((ret (datastore-retrieve-indexed class index value)))
    (assert ret)
    (assert (not (cdr ret)))
    (car ret)))

(defgeneric datastore-save-form (object))
(defmethod datastore-save-form (object)
  `',object)
(defmethod datastore-save-form ((object standard-object))
  (make-load-form object))
(defmethod datastore-save-form ((object structure-object))
  (make-load-form object))
(defmethod datastore-save-form ((string string))
  string)
(defmethod datastore-save-form ((number number))
  number)
(defmethod datastore-save-form ((array array))
  (typecase array
    (byte-vector
     `(utf8-encode ,(force-string array)))
    (t
     `(make-array ',(array-dimensions array)
                  :element-type ',(array-element-type array)
                  :initial-contents (list ,@(map 'list 'datastore-save-form array))))))
(defmethod datastore-save-form ((list list))
  (when list
    `(list ,@(map 'list 'datastore-save-form list))))

(defgeneric datastore-record-constructor-form (object))

(defmacro defrecord (name &rest original-slot-defs)
  (labels ((slot-name (slot-def)
           (if (listp slot-def) (first slot-def) slot-def))
           (slot-plist (slot-def)
             (when (listp slot-def) (cdr slot-def)))
           (slot-prop (slot-def prop)
             (getf (slot-plist slot-def) prop))
           (slot-transient (slot-def)
             (slot-prop slot-def :transient))
           (slot-persistent (slot-def)
             (not (slot-transient slot-def)))
           (slot-indexed (slot-def)
             (slot-prop slot-def :index))
           (defstruct-slot-def (slot-def)
               (let ((ret (copy-list (slot-plist slot-def)))
                     (initform nil))
                 (dolist (option '(:transient :index :initform))
                   (remf ret option))
                 (list* (slot-name slot-def) initform ret)))
           (guarded-slot-accessor (slot-name)
            (concat-sym name '- slot-name))
           (real-slot-accessor (slot-name)
             (concat-sym-from-sym-package name 'unlogged- name '- slot-name))
           (real-constructor ()
             (concat-sym-from-sym-package name 'unlogged-make- name))
           (guarded-constructor ()
             (concat-sym-from-sym-package name 'make- name))
           (slot-index (slot-def)
             (let ((slot-name (slot-name slot-def)))
              `(get ',name ',(concat-sym name '-%datastore-index- slot-name)))))
    (let* ((slot-defs (list* '(datastore-id :transient t :index t :initform (datastore-id-next)) original-slot-defs))
           (indexed-slots (filter #'slot-indexed slot-defs))
           (defstruct-slot-defs (mapcar #'defstruct-slot-def slot-defs))
           (persistent? (some #'slot-persistent slot-defs)))
      (with-unique-names (constructed-object)
        `(progn
           (defstruct (,name
                        (:constructor ,(real-constructor))
                        (:conc-name ,(concat-sym-from-sym-package name 'unlogged- name '-)))
             ,@defstruct-slot-defs)

        ;;; Constructor

        (defun ,(guarded-constructor)
            (&key ,@(loop for slot-def in slot-defs
                     collect `(,(slot-name slot-def) ,(slot-prop slot-def :initform))))
          (let ((,constructed-object (,(real-constructor))))
            (datastore-id-register datastore-id)
            ,(when persistent?
                    `(datastore-log `(,',(guarded-constructor) :datastore-id ,datastore-id)))
            ,@(loop for slot-def in slot-defs
                   for slot-name = (slot-name slot-def)
                   collect `(setf (,(guarded-slot-accessor slot-name) ,constructed-object) ,slot-name))

            ,constructed-object))

          ,@(loop for slot-def in indexed-slots
                  collect `(unless ,(slot-index slot-def)
                            (setf ,(slot-index slot-def) (make-datastore-index :slot-name ',(slot-name slot-def)))))

        ;;; Guarded accessors

        ,@(loop for slot-def in slot-defs
                for slot-name = (slot-name slot-def)
                collect
                `(defun ,(guarded-slot-accessor slot-name) (,name)
                  (,(real-slot-accessor slot-name) ,name))
                collect
                `(defun (setf ,(guarded-slot-accessor slot-name)) (new-value ,name)
                  ,(when (slot-indexed slot-def)
                         `(datastore-index-del ,(slot-index slot-def) ,name))
                  (multiple-value-prog1
                      (setf (,(real-slot-accessor slot-name) ,name) new-value)
                    ,(when (slot-indexed slot-def)
                           `(datastore-index-add ,(slot-index slot-def) ,name))
                    ,(when (slot-persistent slot-def)
                           `(datastore-log
                             `(setf (,',(guarded-slot-accessor slot-name) ,(datastore-ref-form ,name)) ,(datastore-save-form new-value)))))))

        (defmethod datastore-delete ((object ,name))
          ,(when persistent?
                 `(when (slot-value object 'datastore-id)
                    (datastore-log `(datastore-delete ,(datastore-ref-form object)))))
          ,@(loop for slot-def in indexed-slots collect
                  `(datastore-index-del ,(slot-index slot-def) object))
          (setf (slot-value object 'datastore-id) nil))
        (defmethod datastore-retrieve-all ((class (eql ',name)) &optional max-returned)
          (let ((i 0))
            (loop for v being the hash-values of (datastore-index-table ,(slot-index 'datastore-id))
                  until (and max-returned (>= i max-returned))
                  do (incf i (length v))
                  append v)))

        (defmethod datastore-record-constructor-form ((object ,name))
          (list ',(guarded-constructor)
                ,@(loop for slot-def in slot-defs
                        for slot-name = (slot-name slot-def)
                        unless (eq 'datastore-id slot-name)
                        collect (intern (symbol-name slot-name) :keyword)
                        and
                        collect `(datastore-save-form (,(real-slot-accessor slot-name) object)))))


        ,@(loop for slot-def in indexed-slots collect
                `(defmethod datastore-retrieve-indexed ((class (eql ',name)) (index (eql ',(slot-name slot-def))) value)
                   (datastore-index-get ,(slot-index slot-def) value)))

        ',name)))))



(defun datastore-delete-all (class)
  (assert (not (boundp '*datastore*)))
  (mapc 'datastore-delete (datastore-retrieve-all class)))
