(in-package #:tpd2.game)

(defmacro def-web-state-message (message args &body body)
  `(progn
     (defmethod message-to-ml ((message (eql ,message)) &key ,@args &allow-other-keys)
       (declare (ignorable game-state))
       (with-ml-output ,@body))
     (my-defun web-state 'inform (game-state (message (eql ,message)) &key ,@args &allow-other-keys)
       (declare (ignorable game-state))
       (my add-announcement (<p :class "game-message" ,@body)))))

(macrolet ((messages (&body body)
             `(progn
                ,@(loop for (keyword args . ml) in body
                        collect `(def-web-state-message ,keyword ,args
                                   ,@ml)))))

  (messages
   (:talk (sender text)
          (<span :class "game-talk-message" sender ": " (<q text)))
   (:shuffle () "The deck has been shuffled.")
   (:new-player (player)
                player " has joined the game.")
   (:resigned (player)
              player " has resigned.")
   (:timed-out (player)
               player " has timed-out.")
   (:select-card (player choice)
                 player " played " (output-object-to-ml (make-card-from-number choice)) ".")
   (:select-demand (player choice)
                   player " demanded " choice ".")
   (:select (player choice)
            player " chose " (friendly-string choice) ".")
   (:reject-cards (player choice)
                  player
                  (if choice
                      " wants to change cards."
                      " is satisfied with the cards."))
   (:accept-new-stake (player choice)
                  player
                  (if choice
                      " saw the raise."
                      " folded."))
   (:select-new-stake (player choice)
                  player
                  " raised to "
                  choice
                  ".")
   (:winner (player coins)
            player " won"
            (when coins
              (with-ml-output " " coins " coins"))
            "."
            )
   (:game-over (winner result)
               (cond (winner
                      (with-ml-output
                          winner " won the game.")
                      )
                     (t
                      (when result
                        (with-ml-output
                            (friendly-string result) ". ")
                        "Game over."))))
   (:demand (player amount)
            player " demanded " amount ".")

   (:profit (player amount)
            player
            (if (minusp amount) " lost " " gained ")
            (abs amount) ".")

   (:bankrupt (player)
              player " went bankrupt.")

   (:betrayal (player)
              player " betrayed everybody else.")
   (:new-state ()
               "New game.")))

