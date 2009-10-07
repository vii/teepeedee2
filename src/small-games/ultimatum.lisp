(in-package #:tpd2.game.ultimatum)

(defvar *max-pot* 20)
(defvar *min-pot* 1)
(defvar *max-penalty* 4)
(defvar *min-penalty* 0)

(defgame ultimatum (coin-game)
  ((pot (random-between *min-pot* *max-pot*))
   (penalty (random-between *min-penalty* *max-penalty*)))
  (defplayer ()
      ((demand 0)))
  (:game-name "The Ultimatum Game")
  (:game-description
   (with-ml-output
     "There is a pot of coins to be shared. The first player decides how to share them. The second player can accept the choice, and receive the allotment, or reject it, in which case the players will both be fined." (<br)
     "Read more at "
     (<a :href "http://en.wikipedia.org/wiki/Ultimatum_game" "Wikipedia") "."))
  (:advertised nil))

(my-defun ultimatum 'play ()
  (with-game
    (my new-state)
    (destructuring-bind (proposer acceptor)
	(random-shuffle (my players))
      (with-its-type (proposer ultimatum-player)
	(with-its-type (acceptor ultimatum-player)

	  (let ((demand (my move :select-demand proposer `(:integer 0 ,(my pot)))))
	    (let ((ok (my move :cooperate acceptor :boolean)))
	      (cond (ok
		     (its give-coins proposer demand)
		     (its give-coins acceptor (- (my pot) demand))
		     (my finished :result :sharing))
		    (t
		     (its give-coins proposer (- (my penalty)))
		     (its give-coins acceptor (- (my penalty)))
		     (my finished :result :penalty))))))))))

(my-defun ultimatum 'object-to-ml ()
  (flet ((coins (c) (format nil "~R coin~:P" c)))
    (with-ml-output
      (call-next-method)
      (<h3 "Pot: " (my pot) ", penalty: " (my penalty) ".")
      (<p "The first player demands some portion of " (coins (my pot)) ". The second player will get the rest, "
	  "but if he or she is unhappy with this division then both players lose " (coins (my penalty)) ". "))))


