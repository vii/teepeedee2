;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A Trivial logging facility

;;;; A logger is a way to have the system generate a text message and
;;;; have that messaged saved somewhere for future review. Logging can
;;;; be used as a debugging mechanism or for just reporting on the
;;;; status of a system.

;;;; Logs are sent to a particular log category, each log category
;;;; sends the messages it receives to its handlers. A handler's job
;;;; is to take a message and write it somewhere. Log categories are
;;;; organized in a hierarchy and messages sent to a log category will
;;;; also be sent to that category's ancestors.

;;;; Each log category has a log level which is used to determine
;;;; whether are particular message should be processed or
;;;; not. Categories inherit their log level from their ancestors. If a
;;;; category has multiple fathers its log level is the min of the
;;;; levels of its fathers.

;;;; ** Log Levels

(eval-always
  (defconstant +dribble+ 0)
  (defconstant +debug+   1)
  (defconstant +info+    2)
  (defconstant +warn+    3)
  (defconstant +error+   4)
  (defconstant +fatal+   5)

  (defparameter *log-level-names* (coerce '(+dribble+ +debug+ +info+ +warn+ +error+ +fatal+)
                                          'simple-vector))
  (deflookup-table logger))

(defun log-level-name-of (level)
  (when (not (<= 0 level #.(1- (length *log-level-names*))))
    (error "~S is an invalid log level" level))
  (aref *log-level-names* level))

;;;; ** Log Categories

(defclass log-category ()
  ((ancestors :initform '()     :accessor ancestors :initarg :ancestors
              :documentation "The log categories this category inherits from.")
   (children  :initform '()     :accessor children  :initarg :children
              :documentation "The log categories which inherit from this category.")
   (appenders :initform '()     :accessor appenders :initarg :appenders
              :documentation "A list of appender objects this category sholud send messages to.")
   (level     :initform nil :initarg :level :accessor level
              :type (or null integer)
              :documentation "This category's log level.")
   (compile-time-level
              :initform +dribble+ :initarg :compile-time-level :accessor compile-time-level
              :type integer
              :documentation "This category's compile time log level. Any log expression below this level will macro-expand to NIL.")
   (name      :initarg :name :accessor name)))

(defmethod make-load-form ((self log-category) &optional env)
  (declare (ignore env))
  `(let ((result (get-logger ',(name self))))
     (assert result)
     result))

(defmethod print-object ((category log-category) stream)
  (print-unreadable-object (category stream :type t :identity t)
    (if (slot-boundp category 'name)
        (format stream "~S" (name category))
        (format stream "#<NO NAME>"))))

(defmethod shared-initialize :after ((l log-category) slot-names
                                     &key ancestors &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (anc ancestors)
    (pushnew l (children anc) :test (lambda (a b)
				     (eql (name a) (name b))))))

(defun log-level-setter-inspector-action-for (prompt current-level setter)
  (lambda ()
    (with-simple-restart
        (abort "Abort setting log level")
      (let ((value-string (swank::eval-in-emacs
                           `(condition-case c
                             (let ((arnesi-log-levels '(,@(mapcar #'string-downcase (coerce *log-level-names* 'list)))))
                               (slime-read-object ,prompt :history (cons 'arnesi-log-levels ,(1+ current-level))
                                                  :initial-value ,(string-downcase (log-level-name-of current-level))))
                             (quit nil)))))
        (when (and value-string
                   (not (string= value-string "")))
          (funcall setter (eval (let ((*package* #.(find-package :arnesi)))
                                  (read-from-string value-string)))))))))

(defmethod swank:inspect-for-emacs ((category log-category) inspector)
  (let ((class (class-of category)))
    (values "A log-category."
            `("Class: " (:value ,class) (:newline)
              "Runtime level: " (:value ,(log.level category)
                                 ,(string (log-level-name-of (log.level category))))
              " "
              (:action "[set level]" ,(log-level-setter-inspector-action-for
                                       "Set runtime log level to (evaluated): "
                                       (log.level category)
                                       (lambda (value)
                                         (setf (log.level category) value))))
              (:newline)
              "Compile-time level: " (:value ,(log.compile-time-level category)
                                      ,(string (log-level-name-of (log.compile-time-level category))))
               " "
              (:action "[set level]" ,(log-level-setter-inspector-action-for
                                       "Set compile-time log level to (evaluated): "
                                       (log.compile-time-level category)
                                       (lambda (value)
                                         (setf (log.compile-time-level category) value))))
              (:newline)
              ,@(swank::all-slots-for-inspector category inspector)))))

;;; Runtime levels
(defmethod enabled-p ((cat log-category) level)
  (>= level (log.level cat)))

(defmethod log.level ((cat log-category))
  (or (level cat)
      (if (ancestors cat)
          (loop for ancestor in (ancestors cat)
                minimize (log.level ancestor))
          (error "Can't determine level for ~S" cat))))

(defmethod log.level ((cat-name symbol))
  (log.level (get-logger cat-name)))

(defmethod (setf log.level) (new-level (cat log-category)
                             &optional (recursive t))
  "Change the log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT."
  (setf (slot-value cat 'level) new-level)
  (when recursive
    (dolist (child (children cat))
      (setf (log.level child) new-level)))
  new-level)

(defmethod (setf log.level) (new-level (cat-name symbol) &optional (recursive t))
  (setf (log.level (get-logger cat-name) recursive) new-level))

(defmethod (setf log.level) (new-level (cat-name null) &optional (recursive t))
  (declare (ignore new-level cat-name recursive))
  (error "NIL does not specify a category."))

;;; Compile time levels
(defmethod compile-time-enabled-p ((cat log-category) level)
  (>= level (log.compile-time-level cat)))

(defmethod log.compile-time-level ((cat log-category))
  (or (compile-time-level cat)
      (if (ancestors cat)
          (loop for ancestor in (ancestors cat)
                minimize (log.compile-time-level ancestor))
          (error "Can't determine compile time level for ~S" cat))))

(defmethod log.compile-time-level ((cat-name symbol))
  (log.compile-time-level (get-logger cat-name)))

(defmethod (setf log.compile-time-level) (new-level (cat log-category)
                                          &optional (recursive t))
  "Change the compile time log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT."
  (setf (slot-value cat 'compile-time-level) new-level)
  (when recursive
    (dolist (child (children cat))
      (setf (log.compile-time-level child) new-level)))
  new-level)

(defmethod (setf log.compile-time-level) (new-level (cat-name symbol) &optional (recursive t))
  (setf (log.compile-time-level (get-logger cat-name) recursive) new-level))

(defmethod (setf log.compile-time-level) (new-level (cat-name null) &optional (recursive t))
  (declare (ignore new-level cat-name recursive))
  (error "NIL does not specify a category."))

(defmacro with-logger-level (logger-name new-level &body body)
  "Set the level of the listed logger(s) to NEW-LEVEL and restore the original value in an unwind-protect."
  (cond ((consp logger-name)
         `(with-logger-level ,(pop logger-name) ,new-level
           ,(if logger-name
                `(with-logger-level ,logger-name ,new-level
                  ,@body)
                `(progn
                  ,@body))))
        ((symbolp logger-name)
         (with-unique-names (logger old-level)
           `(let* ((,logger (get-logger ',logger-name))
                   (,old-level (level ,logger)))
             (setf (level ,logger) ,new-level)
             (unwind-protect
                  (progn ,@body)
               (setf (level ,logger) ,old-level)))))
        (t (error "Don't know how to interpret ~S as a logger name" logger-name))))

;;;; ** Handling Messages

(defmacro with-logging-io (&body body)
  `(let ((*print-right-margin* most-positive-fixnum)
         (*print-readably* nil)
         (*print-length* 64)
         (*package* #.(find-package "COMMON-LISP")))
    ,@body))

(defgeneric handle (category message level)
  (:documentation "Message is either a string or a list. When it's a list and the first element is a string then it's processed as args to cl:format."))

(defmethod handle :around ((cat log-category) message level)
  ;; turn off line wrapping for the entire time while inside the loggers
  (with-logging-io
    (call-next-method)))

(defmethod handle ((cat log-category) message level)
  (if (appenders cat)
      ;; if we have any appenders send them the message
      (dolist (appender (appenders cat))
	(append-message cat appender message level))
      ;; send the message to our ancestors
      (dolist (ancestor (ancestors cat))
	(handle ancestor message level))))

(defgeneric append-message (category log-appender message level)
  (:method :around (category log-appender message level)
    ;; what else should we do?
    (ignore-errors
      (call-next-method))))

;;;; *** Stream log appender

(defclass appender ()
  ((verbosity :initform 2 :initarg :verbosity :accessor verbosity-of)))
  
(defclass stream-log-appender (appender)
  ((stream :initarg :stream :accessor log-stream))
  (:documentation "Human readable to the console logger."))

(defmethod make-instance ((class (eql (find-class 'stream-log-appender)))
                          &rest initargs)
  (declare (ignore initargs))
  (error "STREAM-LOG-APPENDER is an abstract class. You must use either brief-stream-log-appender or verbose-stream-log-appender objects."))

(defmethod append-message :around (category (appender stream-log-appender) (message cons) level)
  (append-message category appender (apply #'format nil message) level))

(defclass brief-stream-log-appender (stream-log-appender)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "A subclass of stream-log-appender with minimal
  'overhead' text in log messages. This amounts to: not printing
  the package names of log categories and log levels and a more
  compact printing of the current time."))

(defclass verbose-stream-log-appender (stream-log-appender)
  ()
  (:documentation "A subclass of stream-log-appender which
  attempts to be as precise as possible, category names and log
  level names are printed with a package prefix and the time is
  printed in long format."))

(defmethod append-message :around ((category log-category) (s stream-log-appender)
                                   message level)
  (restart-case
      (call-next-method)
    (use-*debug-io* ()
      :report "Use the current value of *debug-io*"
      (setf (log-stream s) *debug-io*)
      (append-message category s message level))
    (use-*standard-output* ()
      :report "Use the current value of *standard-output*"
      (setf (log-stream s) *standard-output*)
      (append-message category s message level))
    (silence-logger ()
      :report "Ignore all future messages to this logger."
      (setf (log-stream s) (make-broadcast-stream)))))

(eval-always
  (defparameter *max-category-name-length* 12))

(defmethod append-message ((category log-category) (s brief-stream-log-appender)
                           message level)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (with-slots (last-message-year last-message-month last-message-day)
        s
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format (log-stream s) "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (let* ((category-name (symbol-name (name category)))
           (level-name (symbol-name level))
           (category-length (length category-name)))
      (format (log-stream s)
              #.(strcat "~2,'0D:~2,'0D ~"
                        *max-category-name-length*
                        "@A ~7A ")
              hour minute
              (subseq category-name
                      (max 0 (- category-length
                                *max-category-name-length*))
                      category-length)
              (subseq level-name 1 (1- (length level-name)))))
    (format (log-stream s) "~A~%" message)))

(defmethod append-message ((category log-category) (s verbose-stream-log-appender)
                            message level)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format (log-stream s)
            "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D.~2,'0D ~S/~S: "
            year month date hour minute second
            (name category) level)
    (format (log-stream s) "~A~%" message)))

(defun make-stream-log-appender (&rest args &key (stream *debug-io*) (verbosity 2) &allow-other-keys)
  (remf-keywords args :stream :verbosity)
  (apply #'make-instance (case verbosity
                           ((0 1) 'brief-stream-log-appender)
                           (t 'verbose-stream-log-appender))
         :stream stream
         :verbosity verbosity
         args))

(defclass slime-repl-log-appender (appender)
  ()
  (:documentation "Logs to the slime repl when there's a valid swank::*emacs-connection* bound. Arguments are presented ready for inspection.

You may want to add this to your init.el to speed up cursor movement in the repl buffer with many presentations:

\(add-hook 'slime-repl-mode-hook
          (lambda ()
            (setf parse-sexp-lookup-properties nil)))
"))

(defmethod append-message ((category log-category) (appender slime-repl-log-appender)
                           message level)
  (when (swank::default-connection)
    (swank::with-connection ((swank::default-connection))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (get-universal-time))
        (declare (ignore second day month year))
        (swank::present-in-emacs (format nil
                                         "~2,'0D:~2,'0D ~A/~A: "
                                         hour minute
                                         (symbol-name (name category))
                                         (symbol-name level))))
      (if (consp message)
          (let ((format-control (when (stringp (first message))
                                  (first message)))
                (args (if (stringp (first message))
                          (rest message)
                          message)))
            (when format-control
              (setf message (apply #'format nil format-control args)))
            (swank::present-in-emacs message)
            (awhen (and format-control
                        (> (verbosity-of appender) 1)
                        (remove-if (lambda (el)
                                     (or (stringp el)
                                         (null el)))
                                   args))
              (swank::present-in-emacs " (")
              (swank::present-in-emacs it)
              (swank::present-in-emacs ")")))
          (swank::present-in-emacs message))
      (swank::present-in-emacs #.(string #\Newline)))))

(defun arnesi-logger-inspector-lookup-hook (form)
  (when (symbolp form)
    (if-bind logger (get-logger form)
      (values logger t)
      (when-bind logger-name (get form 'logger)
        (when-bind logger (get-logger logger-name)
          (values logger t))))))

(awhen (find-symbol (symbol-name '#:*inspector-dwim-lookup-hooks*) :swank)
  (pushnew 'arnesi-logger-inspector-lookup-hook (symbol-value it)))

(defun make-slime-repl-log-appender (&rest args &key (verbosity 2))
  (remf-keywords args :verbosity)
  (apply #'make-instance 'slime-repl-log-appender :verbosity verbosity args))

(defclass file-log-appender (stream-log-appender)
  ((log-file :initarg :log-file :accessor log-file
             :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file. the output of the file logger
  is not meant to be read directly by a human."))

(defmethod append-message ((category log-category) (appender file-log-appender)
                           message level)
  (with-output-to-file (log-file (log-file appender)
				 :if-exists :append
				 :if-does-not-exist :create)
    (format log-file "(~S ~D ~S ~S)~%" level (get-universal-time) (name category) message)))

(defun make-file-log-appender (file-name)
  (make-instance 'file-log-appender :log-file file-name))

;;;; ** Creating Loggers

(defmacro deflogger (name ancestors &key compile-time-level level appender appenders documentation)
  (declare (ignore documentation)
           (type symbol name))
  (unless (eq (symbol-package name) *package*)
    (warn "When defining a logger named ~A the home package of the symbol is not *package* (not (eq ~A ~A)) "
          (let ((*package* (find-package "KEYWORD")))
            (format nil "~S" name))
          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((ancestors (mapcar (lambda (ancestor-name)
			     `(or (get-logger ',ancestor-name)
				  (error "Attempt to define a sub logger of the undefined logger ~S."
					 ',ancestor-name)))
			   ancestors)))
    (flet ((make-log-helper (suffix level)
	     (let ((logger-macro-name (intern (strcat name "." suffix))))
               `(progn
                 (setf (get ',logger-macro-name 'logger) ',name)
                 (defmacro ,logger-macro-name (message-control &rest message-args)
                     ;; first check at compile time
                     (if (compile-time-enabled-p (get-logger ',name) ,level)
                         ;; then check at runtime
                         `(progn
                           (when (enabled-p (load-time-value (get-logger ',',name)) ,',level)
                             ,(if message-args
                                  `(handle (load-time-value (get-logger ',',name)) (list ,message-control ,@message-args)
                                    ',',level)
                                  `(handle (load-time-value (get-logger ',',name)) ,message-control ',',level)))
                           (values))
                         (values)))))))
      `(progn
         (eval-when (:load-toplevel :execute)
           (setf (get-logger ',name) (make-instance 'log-category
                                                    :name ',name
                                                    ,@(cond (level
                                                             `(:level ,level))
                                                            ((not ancestors)
                                                             `(:level +debug+))
                                                            (t '()))
                                                    ,@(when compile-time-level
                                                        `(:compile-time-level ,compile-time-level))
                                                    :appenders (remove nil (list ,@appenders))
                                                    :ancestors (list ,@ancestors))))
	 ,(make-log-helper '#:dribble '+dribble+)
	 ,(make-log-helper '#:debug '+debug+)
	 ,(make-log-helper '#:info '+info+)
	 ,(make-log-helper '#:warn '+warn+)
	 ,(make-log-helper '#:error '+error+)
	 ,(make-log-helper '#:fatal '+fatal+)
        (values)))))



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
