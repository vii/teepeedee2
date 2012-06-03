(in-package #:tpd2.game)

(defgame coin-game ()
  ()
  (defplayer ()
      ((coins)))
  (:unplayable t))

(my-defun coin-game finished :before (&rest args)
          (declare (ignorable args))
          (loop for p in (my players)
                do
                (setf (player-controller-var p 'coins) (its coins p))))

(my-defun coin-game-player 'player-full-state-to-ml ()
  (<div :class "coin-game-player"
        (call-next-method)
        (let ((coins (or (my coins) (my 'player-controller-var 'coins))))
          (when coins
            (if (plusp coins)
                (<p (format nil "~D coin~:P" coins))
                (<p :class "bankrupt" "No coins"))))))

(my-defun coin-game 'object-to-ml :around ()
  (<div :class "coin-game"
        (call-next-method)
        (let ((coins (webapp-frame-var 'coins)))
          (cond (
                 (and coins (plusp coins))
                 (<h3 (format nil "You have ~D coin~:P." coins)))
                (t
                 (<h3 :class "bankrupt" "You have no coins."))))))

(my-defun coin-game setup-coins ()
  (loop for p in (my players)
        do (setf (its coins p)
                 (setf (player-controller-var p 'coins)
                       (max-nil-ok 0 (its coins p) (player-controller-var p 'coins))))))

(my-defun coin-game players-ready :after ()
          (my setup-coins))

(my-defun coin-game-player give-coins (amount)
  (incf (my coins) amount)
  (my announce :profit :player me :amount amount)
  (when (minusp (my coins))
    (my announce :bankrupt :player me))
  (values))
