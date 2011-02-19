(in-package #:tpd2.game)

(eval-always
  (define-constant +suits+ '(:clubs :hearts :spades :diamonds) :test 'equal)
  (defconstant +cards-per-suit+ 13))

(defstruct card
  (suit :clubs :type  #.`(member ,@+suits+))
  (value 0 :type (integer 0 #.+cards-per-suit+)))

(my-defun card value-string ()
  (case (my value)
    (0 "Ace")
    (10 "Jack")
    (11 "Queen")
    (12 "King")
    (t (string-capitalize (format nil "~R" (1+ (my value)))))))

(my-defun card name ()
  (format nil "~A of ~A"
          (my value-string)
          (string-capitalize (symbol-name (my suit)))))

(my-defun card number ()
  (+ (* (position (my suit) +suits+) +cards-per-suit+) (my value)))

(defun make-card-from-number (number)
  (multiple-value-bind
        (s-n v)
      (floor number +cards-per-suit+)
    (make-card :suit (elt +suits+ s-n) :value v)))

(my-defun card 'object-to-ml ()
  (<span
    :class "card"
    (<span :class (symbol-name (my suit))
           (my value-string)
           (output-raw-ml
            "&")
           (case (my suit)
             (:diamonds "diams")
             (t (string-downcase (symbol-name (my suit)))))
           ";")))
