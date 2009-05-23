(in-package #:tpd2.ml)

(define-dtd :teepeedee2.ml.rss
    (:nicknames #:tpd2.ml.rss)
;; Arbitrary subset defined by hand
  (rss :attributes (version xmlns) :children (channel))
  (channel :children (title link description item))
  (item :children (title link description))
  (title :children (#'identity))
  (link  :children (#'identity))
  (description :children (#'identity)))

  
