(in-package #:tpd2.game.nash-bargain)

(defvar *max-pot* 20)
(defvar *min-pot* 1)
(defvar *max-penalty* 5)
(defvar *min-penalty* 0)

(defgame nash-bargain (coin-game)
  ((pot (random-between *min-pot* *max-pot*))
   (penalty (random-between *min-penalty* *max-penalty*)))
  (defplayer ()
      ((demand 0)))
  (:game-name "Nash's Bargaining Game")
  (:game-description
   (with-ml-output
     "There is a pot of coins to be shared. Players each make a secret demand to receive a certain number of coins. If the players can all be satisfied, they all receive their demand. Otherwise, if their combined demands are too high, then all the players receive a penalty. " (<br)
     "This is a generalisation of the prisoner's dilemma. Read more at "
     (<a :href "http://en.wikipedia.org/wiki/Nash_bargaining_game" "Wikipedia") ".")))



(my-defun nash-bargain 'play ()
  (with-its-type (p nash-bargain-player)
    (with-game
      (my new-state)
      (with-join-spawn/cc ()
        (loop for p in (my players)
              do
              (let-current-values (p)
                (spawn/cc ()
                          (setf (its demand p) (my secret-move :select-demand p `(:integer 0 ,(my pot))))))))
      (let ((total-demand
             (loop for p in (my players)
                   for demand = (its demand p)
                   do
                   (my announce :demand :player p :amount demand)
                   summing demand)))
        (cond ((>= (my pot) total-demand)
               (loop for p in (my players) do
                     (its give-coins p (its demand p)))
               (my finished :result :sharing))
              (t
               (loop for p in (my players) do
                     (its give-coins p (- (my penalty))))
               (my finished :result :penalty)))))))

(my-defun nash-bargain 'object-to-ml ()
  (flet ((coins (c) (format nil "~R coin~:P" c)))
    (with-ml-output
      (call-next-method)
      (<h3 "Pot: " (my pot) ", penalty: " (my penalty) ".")
      (<p "The pot has " (coins (my pot)) "; you can demand zero or more of them. "
          "If the total demanded by the players is less than or equal to " (coins (my pot)) ", then each player receives his or her demand. "
              "Otherwise, if the players are too greedy, they each forfeit " (coins (my penalty)) ". ")
      (<p
        "Your real demand is secret, but you can talk to the other player."))))

(my-defun nash-bargain drop-player :before (p)
          (unless (my game-over)
            (with-shorthand-accessor (p coin-game-player p)
              (p give-coins (- (my penalty))))))

