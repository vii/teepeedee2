(in-package #:tpd2.webapp)

(defstruct action
  (id (random-web-sparse-key 10))
  (func nil))

(defun register-action-id (function)
  (check-type function function)
  (let ((action (make-action :func function)))
    (push action
          (webapp-frame-var 'actions))
    (action-id action)))

(defmacro page-action-lambda (&body body)
  `(lambda()
     ,@body))

(defmacro page-action-link (&body body)
  `(page-link (site-action-page-name (current-site)) :.id. (register-action-id (page-action-lambda ,@body))))

(defmacro html-replace-link (text &body body)
  (with-unique-names (body-func)
   `(<A :class +replace-link-class+
        :href
        (page-action-link
          (flet ((,body-func () ,@body))
            (cond ((webapp-frame-available-p)
                   (setf (frame-current-page (webapp-frame)) #',body-func)
                   (values))
                  (t
                   (,body-func)))))
        ,text)))

(defmacro html-action-link (text &body body)
  `(<A :class +action-link-class+
       :href (page-action-link ,@body (values)) ,text))

(defmacro html-collapser (toggle &body body)
  `(with-ml-output
       (<div :onclick (js-attrib (toggle-hiding (~ this next-sibling)))
             ,toggle)
       (<div :class +html-class-collapsed+
             ,@body)))

(defmacro html-action-form-collapsed (title lambda-list &body body)
  `(html-collapser (<p ,(force-first title))
                   (html-action-form (nil ,@(force-rest title) :after-submit-js ((toggle-hiding (~ this parent-node)))) ,lambda-list ,@body)))

(defmacro html-action-form (title-and-options lambda-list &body body)
  (destructuring-bind (title
                       &key (action-link
                             `(page-action-link
                                (with-http-params
                                    ,(mapcar (lambda (lambda-arg)
                                               (destructuring-bind (name &optional default &rest keys)
                                                   (force-list lambda-arg)
                                                 (let ((keys (copy-list keys)))
                                                   (alexandria:delete-from-plistf keys :type :reset)
                                                   (list* name default keys))))
                                             lambda-list)
                                  ,@body)))
                       onsubmit-js
                       (async t)
                       (sync-fallback t)
                       after-submit-js)
      (force-list title-and-options)
    (let ((body-ml
           (loop for nv in lambda-list collect
                 (destructuring-bind (name &optional value &key (type '<input) reset label options)
                     (force-list nv)
                    (let ((name (force-byte-vector name)))
                      (when reset
                        (appendf after-submit-js `((setf (slot-value (! (this elements named-item) ,(force-string name)) 'value) ,(if (eq reset t) nil reset)))))
                      (let ((input
                             (ecase type
                               (<input
                                 `(<input :type :text :name ,name
                                          ,@(when value `(:value ,value))))
                               (<textarea
                                 `(<textarea :name ,name ,value))
                               (:select-one
                                `(<select :name ,name
                                          ,@(loop for opt in options collect
                                                  `(<option ,@(when (equalp value opt) `(:selected t)) ,opt))))
                               (:hidden
                                `(<input :type :text :name ,name :value ,value :style (css-attrib :display "none"))))))
                        (cond (label
                               `(progn
                                  (<label :for ,name ,label)
                                  ,input))
                              (t input))))))))
       `(<form
         ,@(when (or async onsubmit-js)
                 `(:onsubmit
                   ,(or onsubmit-js `(js-attrib (return (let ((async-submit-success (async-submit-form this))) ,@after-submit-js async-submit-success))))))
         :method :post
         ,@(when sync-fallback
                 `(:action ,action-link))
         (<p
          ,title
          ,@body-ml
          (<input :class "plain-submit" :type :submit :value "↵"))))))

(defun find-action (id)
  (when id
    (find id (webapp-frame-var 'actions) :key 'action-id :test 'equalp)))

(defun action-respond-body (&key .id. .javascript.)
  (with-frame-site
    (let ((body (awhen (find-action .id.)
                  (funcall (the function (action-func it))))))
      (check-type body (or null sendbuf))
      (cond (.javascript.
             (webapp-respond-ajax-body))
            ((and (not body) (webapp-frame-available-p) (frame-current-page (webapp-frame)))
             (funcall (frame-current-page (webapp-frame))))
            (t
              (or body
                  (with-sendbuf ()
                    "<h1>Sorry, nothing to see here. Please go back.</h1>")))))))

(defun webapp-respond-ajax-body ()
  (with-http-params ((.channels. nil :conv channel-string-to-states))
    (channel-respond-body .channels. :always-body t)))

(defun register-action-page (&optional (url (site-action-page-name (current-site))))
  (defpage-lambda url
      #'action-respond-body :defaulting-lambda-list (.id. .javascript.)))


(my-defun frame 'simple-channel-body-ml ()
  (<div :class "frame"
        (<div :class "change-name"
              (html-action-form "Your name " ((new-name (my username)))
                (my change-username new-name)
                (values)))))

