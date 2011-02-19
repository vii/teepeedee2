(in-package #:tpd2.blog)

(defrecord comment
  (entry-index-name :index t)
  text
  (author :index t)
  (time :initform (get-universal-time))
  trace-details)

(defmethod print-object ((comment comment) stream)
  (with-shorthand-accessor (my comment)
    (print-unreadable-object (comment stream :type t)
      (format stream "~S by ~S/~A at ~A"
              (force-string (my text))
              (force-string (my author))
              (force-string (my trace-details))
              (time-string (my time))))))


(defun split-into-paragraphs (str)
  (match-split (progn #\Newline (* (or #\Space #\Tab #\Return)) #\Newline)
               str))

(defun split-into-paragraphs-by-single-line (str)
  (when str
    (match-split #\Newline
                 str)))

(my-defun comment 'object-to-ml ()
  (<div :class "comment"
        (loop for p in (split-into-paragraphs-by-single-line (my text)) do (<p p))

        (<p :class "time" "Posted " (time-string (my time)) " by " (<span :class "author" (my author)))))

(defvar *score-decay* (exp (/ (log 1/2) (* 6 30 24 60 60))))
(defvar *comment-score* 8)
(defvar *entry-score* 10)

(defmyclass entry
  blog
  name
  tags
  (title "Untitled")
  time
  expiry-time
  paragraphs
  score
  score-update-time)

(defun score-decay (then &key (now (get-universal-time)) (age (- now then)))
  (expt *score-decay* age))

(my-defun comment score ()
  (* *comment-score* (score-decay (my time))))

(my-defun entry set-score ()
  (let ((score (loop for c in (my comments) summing (comment-score c))))
    (incf score (* *entry-score* (score-decay (my time))))
    (setf (my score) score
          (my score-update-time) (get-universal-time))))

(my-defun entry update-score (&optional (inc 0))
  (unless (my score) (my set-score))
  (setf (my score) (+ inc (* (my score) (score-decay (my score-update-time))))
        (my score-update-time) (get-universal-time)))

(defmyclass (entry-channel (:include simple-channel))
    entry)

(my-defun entry-channel 'simple-channel-body-ml ()
  (<div :class "blog-entry-comments"
        (output-object-to-ml
         (let (ret)
           (loop for c in (entry-comments (my entry)) repeat 50 do (push c ret))
           ret))))

(defun time-string (&optional (ut (get-universal-time)))
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time ut 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D GMT" year month date hour minute second)))

(my-defun entry filename ()
  (strcat (its dir (my blog)) (my name)))

(my-defun entry front-page-p (&optional tags)
  (let ((now (get-universal-time)))
    (and (>= now (my time))
         (or (not (my expiry-time)) (>= (my expiry-time) now))

         (or (not tags)
             (intersection (my tags) tags :test #'equalp)))))

(my-defun entry url-path ()
  (byte-vector-cat (its link-base (my blog)) (my name)))

(my-defun entry link ()
  (page-link (my url-path)))

(my-defun entry index-name ()
  (blog-entry-unique-string (my blog) (my name)))

(my-defun entry story-ml ()
  (<div :class "blog-entry-story"
        (loop for p in (my paragraphs)
              do (<p (output-raw-ml p)))))

(my-defun entry comments ()
  (datastore-retrieve-indexed 'comment 'entry-index-name (my index-name)))

(my-defun entry comment-ml ()
  (<div :class "blog-entry-post-comment"
        (html-action-form-collapsed ("Post a comment" :action-link (blog-post-comment-url (my blog)))
            ((text nil :type <textarea :reset "")
             (author (byte-vector-cat "Anonymous from " (tpd2.http:servestate-origin*)))
             (entry-name (my index-name) :type :hidden)
             (keep-this-empty nil :type :hidden)))))

(my-defun entry 'object-to-ml ()
  (my update-score 1)
  (<div :class "blog-entry"
        (<p :class "time" "Posted " (time-string (my time)))
        (let ((v (length (its subscribers (my channel)))))
          (unless (zerop v)
            (<p :class "viewers" v " watching live")))
        (my story-ml)
        (my channel)
        (my comment-ml)))

(defvar *age-units* `(("year" ,(* 365.25 24 60 60))
                      ("week" ,(* 7 24 60 60))
                      ("day" ,(* 24 60 60))
                      ("hour" ,(* 60 60))
                      ("minute" 60)
                      ("second" 1)))

(defun friendly-age-string (time)
    (let ((age (- (get-universal-time) time)))
      (let ((units *age-units*))
        (loop
              for value = (cadr (first units))
              while (and (cdr units) (< age value))
              do (pop units))
        (destructuring-bind (name value)
            (first units)
          (let ((v (floor age value)))
            (format nil "~R ~A~P" v name v))))))

(my-defun entry headline-ml (score-mul)
  (<div :class "blog-front-page-entry"
        :style
             (css-attrib
              :max-width ((format nil "~$%" (* 100 (min 0.8 (max 0.2 (* 1/4 score-mul (my score)))))))
              :width "auto")
        (<h2 :style
             (css-attrib
              :font-size ((format nil "~$em" (min 2.5 (max 1.2 (* 1.5 score-mul (my score)))))))
             (<a :href (my url-path) (my title)))
        (<p :class "time" "Posted " (friendly-age-string (my time)) " ago"
            (when (my comments)
              (with-ml-output ", last comment " (friendly-age-string (comment-time (first (my comments)))) " ago")))))

(my-defun entry combined-title ()
 (with-ml-output
   (its name (my blog)) ": " (my title)))

(my-defun entry set-page ()
  (with-site ((its site (my blog)))
    (defpage-lambda (my url-path)
        (lambda()
          (webapp ((my combined-title))
            (output-object-to-ml me)))))
  (my set-channel))

(my-defun entry channel-id ()
  (blog-entry-channel-id (my blog) (my name)))

(my-defun entry channel ()
  (find-channel (my channel-id)))

(my-defun entry set-channel ()
  (let ((id (my channel-id)))
   (let ((channel (or (find-channel id) (make-entry-channel :id id :entry me))))
     (setf (entry-channel-entry channel) me))))

(my-defun entry read-paragraphs-from-buffer (buffer)
  (setf (my paragraphs)
        (split-into-paragraphs
         (match-replace-all buffer
                            ("${static-base}"  (byte-vector-cat (blog-static-base-url (my blog)) (my name)))))))

(defun parse-time (str)
  (match-bind
   (macrolet ((int (name &optional (len 2))
                `(progn t (,name (unsigned-byte :max-length ,len) 0))))
     (int year 4)
     (int month)
     (int day)
      (:?
       (int hour)
       (int minute)
       (:? (int second))))
   str
   (encode-universal-time second minute hour day month year)))

(defun slurp-file (filename)
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((buf (make-byte-vector (file-length s))))
      (read-sequence buf s)
      buf)))

(defun normally-capitalized-string-to-symbol (string &optional (package (symbol-package 'normally-capitalized-string-to-symbol)))
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* package))
      (read-from-string (force-string string)))))

(defun read-in-entry (blog name)
  (let ((entry (make-entry :blog blog :name name)))
    (with-shorthand-accessor (my entry)
      (let ((remaining (slurp-file (my filename))))
        (setf (my time) (file-write-date (my filename)))
        (loop for line =
              (match-bind (line #\Newline after)
                  remaining
                (setf remaining after)
                line)
              until (if-match-bind ( (* (space)) (last)) line)
              do (when (if-match-bind "XXX" line)
                   (format *debug-io* "Entry not ready (XXX): ~A~&" name)
                   (return-from read-in-entry))
              do (match-bind ((* (space)) header ":" (* (space)) value)
                             line
                             (case-match-fold-ascii-case header
                                                         (("expiry-time" "time")  (setf value (parse-time value)))
                                                         ("tags" (setf value (split-into-list-by-comma value))))
                             (let ((sym (normally-capitalized-string-to-symbol header)))
                              (cond ((slot-exists-p entry sym)
                                     (setf (slot-value entry sym)
                                           value))
                                    (t (warn "~A" (strcat "blog entry " name " has invalid header " sym " (" header ")")))))))
        (my read-paragraphs-from-buffer remaining))
      (my set-page))
    entry))

