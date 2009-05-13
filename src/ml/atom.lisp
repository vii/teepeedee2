(in-package #:tpd2.ml)

(define-dtd :teepeedee2.ml.atom
    (:nicknames #:tpd2.ml.atom)
;; Arbitrary subset defined by hand
  (feed :attributes (xmlns) :children (#'identity title subtitle link author id entry updated))
  (title :children (#'identity))
  (subtitle :children (#'identity))
  (link :attributes (href rel) :children ())
  (updated  :children (#'identity))
  (author :children (name email))
  (name  :children (#'identity))
  (email :children (#'identity))
  (id  :children (#'identity))
  (summary :children (#'identity))
  (content :children (#'identity) :attributes (type))
  (entry  :children (title link id updated summary content)))
  
(defun w3c-timestring (&optional (universal-time (get-universal-time)))
  "For example 2003-12-13T18:30:02Z"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month date hour minute second)))