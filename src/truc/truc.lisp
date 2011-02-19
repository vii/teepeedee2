(in-package #:tpd2.game.truc)

(define-constant +truc-ranking+ '(6 7 0 12 11 10 9 8) :test 'equal)
(define-constant +truc-deck+
  (map 'vector 'card-number
          (loop for s in +suits+ append (loop for i in +truc-ranking+ collect (make-card :suit s :value i))))
  :test 'equalp)
(defconstant +truc-winning-stack+ 12)

(defgame truc ()
  (stake ordered-winners)
  (defplayer ()
      ((cards nil)
       (wins 0)
       (stack 0)
       chosen-card
       (folded nil)))
  (:advertised nil))

(my-defun truc-player shuffle-init ()
  (setf (my cards) nil)
  (setf (my wins) 0)
  (setf (my folded) nil)
  (setf (my chosen-card) nil))


(my-defun card truc-ranking ()
  (let ((val (position (my value) (reverse +truc-ranking+))))
    (debug-assert val)
    val))

(defun card-number-truc-ranking (n)
  (its truc-ranking (make-card-from-number n)))

(my-defun truc-player chosen-card-value ()
  (if (or (my folded) (not (my chosen-card)))
      -1
      (card-number-truc-ranking (my chosen-card))))

(my-defun truc chosen-card-values ()
   (remove-if (lambda(c)(eql -1 c)) (mapcar (lambda(p) (its chosen-card-value p)) (my players))))

(my-defun truc determine-round-winner ()
  (let ((winning-card
         (reduce 'max (my chosen-card-values) :initial-value -1)))
    (let ((winners (loop for p in (my players)
                                    when (and (eql winning-card (its chosen-card-value p)) (not (its folded p)))
                         collect p)))
      (when (= 1 (length winners))
        (let ((winner (first winners)))
          (push winner (my ordered-winners))
          (incf (its wins winner))
          (my announce :winner :player winner)
          (deletef winner (my players))
          (push winner (my players)))))))

(my-defun truc shuffle ()
  (setf (my stake) 1)
  (setf (my ordered-winners) nil)
  (let ((shuffle (random-shuffle +truc-deck+)))
    (loop for player in (my players) do
          (truc-player-shuffle-init player)
          (setf (its cards player) (subseq shuffle 0 3))
          (setf shuffle (subseq shuffle 3))))
  (setf (my players) (random-shuffle (my players))))


(my-defun truc too-few-players ()
  (let ((active-players
         (remove-if #'truc-player-folded
                    (my players))))
    (>= 1 (length active-players))))

(defrules truc play-round ()
  (loop for p in (my players)
        do (setf (its chosen-card p) nil))
  (loop for player in (my players)
        unless (its folded player)
        do
        (when (> +truc-winning-stack+ (+ (its stack player) (my stake)))
          (let ((new-stake
                 (my move :select-new-stake player `(:integer ,(my stake) ,(min (* 2 (my stake)) +truc-winning-stack+)))))
            (when (> new-stake (my stake))
              (loop for p in (my players)
                    do (unless
                           (or
                            (eql p player)
                            (its folded p)
                            (>= (1+ (its stack p)) +truc-winning-stack+)
                            (my move :accept-new-stake p :boolean :new-stake new-stake))
                         (setf (its folded p) t)
                         (when (my too-few-players)
                           (return-from truc-play-round))))
              (setf (my stake) new-stake))))
        (setf (its chosen-card player)
              (let ((card (my move :select-card player `(:one ,@(its cards player)))))
                (deletef card (its cards player) :count 1)
                card)))
  (my determine-round-winner))

(defrules truc play-rubber ()
  (loop do
        (my shuffle)
        (my new-state)
        while
        (loop for player in (reverse (my players))
              always (my move :reject-cards player :boolean)))

  (loop for round from 3 downto 1
        do (my play-round)
        until (or
               (let ((wins (mapcar (lambda(p)(its wins p)) (my players))))
                 (>= (- (reduce 'max wins) (reduce 'min wins)) round))
               (my too-few-players)))
  (my determine-rubber-winner))

(my-defun truc determine-rubber-winner ()
  (let* ((active-players (filter (lambda(p)(not (its folded p))) (my players)))
         (top-wins (reduce 'max (mapcar (lambda(p)(its wins p)) active-players)))
         (winners (remove-if-not (lambda(p) (eql (its wins p) top-wins)) active-players)))
    (let ((winner (first (if (> (length winners) 1)
                             (remove-if-not (lambda(p) (member p winners)) (my ordered-winners))
                             winners))))
      (unless winner
        (my announce :draw))

      (when winner
        (incf (its stack winner) (my stake))
        (my announce :winner :coins (my stake) :player winner)))))



(my-defun truc 'play ()
  (with-game
    (loop until (loop for p in (my players) until
                      (when (<= +truc-winning-stack+ (its stack p))
                        (my finished :winner p)
                        t))
          do (my play-rubber))))
