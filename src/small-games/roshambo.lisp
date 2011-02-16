(in-package #:tpd2.game.roshambo)

(defvar *objects* '(rock paper scissors))

(defgame roshambo ()
  ()
  (defplayer ()
      (choice))
  (:game-name "Rock, Paper, Scissors")
  (:game-description
   (with-ml-output
     "Two players simultaneously choose either rock, paper or scissors. Rock blunts scissors, scissors cut paper, and paper covers rock. Also called roshambo." (<br)
     "Read more at "
     (<a :href "http://en.wikipedia.org/wiki/Rock,_Paper,_Scissors" "Wikipedia") ".")))

(my-defun roshambo 'play ()
  (with-game
    (my new-state)
    (with-join-spawn/cc ()
      (loop for p in (my players)
            do
            (let-current-values (p)
                 (spawn/cc ()
                           (setf (its choice p) (my secret-move :select p `(:one ,@*objects*)))))))

    (loop for p in (my players)
          do (my announce :select :player p :choice (its choice p)))

    (let ((winner
           (without-call/cc
             (flet ((v (c)
                      (position c *objects*)))
              (loop for p in (my players)
                    for vp = (v (its choice p))
                    thereis (loop for q in (my players)
                                  for vq = (v (its choice q))
                                  thereis (when (= vp (mod (1+ vq) (length *objects*)))
                                            p)))))))
      (cond (winner
             (my finished :winner winner))
            (t
             (my finished :result :draw))))))
