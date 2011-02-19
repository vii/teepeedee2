(in-package #:tpd2.game)

(defmyclass game
    game-over
  players
  other-listeners)

(defmacro defrules (game func lambda-list &body body)
  `(eval-always
     (with-call/cc
       (my-defun ,game ,func ,lambda-list ,@body))))


(defmacro with-game (&body body)
  `(with-call/cc
     ,@body))


(defun validate-choice (choices choice)
  (acond ((and (not choice) (member nil (choices-list choices)))
          nil)
         ((and choice (find choice (choices-list choices) :test 'equalp))
          it)
         (t
          'invalid-choice)))

(defun random-choice (choices)
  (random-elt (choices-list choices)))

(my-defun game finished (&rest args)
  (setf (my game-over) t)
  (apply 'game-announce me :game-over args)
  (values))

(defmyclass player
    controller
  game
  waiting-for-input)

(my-defun player announce (message &rest args)
  (apply 'game-announce (my game) message args))

(my-defun game listeners ()
  (append (mapcar 'player-controller (my players)) (my other-listeners)))

(my-defun game announce (message &rest args)
  (loop for l in (my listeners)
        do (apply 'inform l me message args)))

(defstruct game-generator
  make-game
  advertised
  unassigned-controllers-waiting
  name
  description)

(my-defun game-generator 'game-name ()
  (my name))

(eval-always (defvar *games* (make-hash-table :test 'equalp)))

(defmacro defgame (name superclasses slots defplayer &rest options)
  (let ((options (copy-list options)))
    (flet ((opt (name &optional default)
             (prog1 (second (or (assoc name options) (list nil default)))
               (deletef name options :key 'car))))
     (let* (
            (game-name-string (force-byte-vector (or (opt :game-name) (string-capitalize (symbol-name name)))))
            (game-description (opt :game-description))
            (playable (not (opt :unplayable))) ; for abstract base classes
            (advertised (opt :advertised playable)))
       (flet ((defgameclass-form (name superclasses options slots)
                `(defmyclass (,name
                              ,@(mapcar (lambda(c) `(:include ,c))
                                        superclasses)
                              ,@options)
                     ,@slots)))
         (destructuring-bind
               (defplayer-sym df-superclasses df-slots &rest df-options)
             defplayer
           (assert (eq 'defplayer defplayer-sym))
           `(eval-always
              ,(when playable
                `(setf (gethash ,game-name-string *games*)
                       (make-game-generator
                        :name ,game-name-string
                        :description ,(when game-description `(tpd2.io:sendbuf-to-byte-vector (with-ml-output ,game-description)))
                        :advertised ,advertised
                        :make-game (lambda(controllers)
                                     (let ((game (,(concat-sym-from-sym-package name 'make- name))))
                                       (let ((players
                                              (mapcar (lambda(c)
                                                      (,(concat-sym-from-sym-package name 'make- name '-player)
                                                        :game game
                                                        :controller c)) controllers)))
                                         (setf (game-players game) players))
                                       game)))))
              ,(defgameclass-form (concat-sym name '-player)
                                  `(,@df-superclasses
                                    ,@(loop for s in superclasses collect
                                            (concat-sym s '-player))
                                    player)
                                  df-options
                                  df-slots)
              ,(defgameclass-form name
                                  (or superclasses (list 'game))
                                  options
                                  slots)
              (defmethod game-name ((,name ,name))
                ,game-name-string)

              ,@(when playable
                      `((eval-when (:load-toplevel :execute)
                          (web-add-game (find-game-generator ,game-name-string)
                                        ,(force-byte-vector (string-downcase (force-string name))))))))))))))

(my-defun game generator ()
  (gethash (my name) *games*))

(defun find-game-generator (game-name)
  (gethash (force-byte-vector game-name) *games*))

(defrules game secret-move (type player choices &rest args)
  (check-type type keyword)
  (let ((ret (call/cc
              (lambda(cc)
                (apply 'move-continuation
                       (lambda(&rest a)
                         (unless (my game-over)
                           (apply cc a)))
                       (player-controller player) player type choices
                       args)
                'with-call/cc))))
    (let ((vc (validate-choice choices ret)))
      (when (eq vc 'invalid-choice)
        (error "invalid choice picked ~A from ~A" ret choices))
      vc)))

(defrules game move (type player choices &rest args)
  (debug-assert (not (player-waiting-for-input player)) (me player))
  (setf (player-waiting-for-input player) t)
  (let ((ret (apply 'game-secret-move me type player choices args)))
    (my announce type :choice ret :player player)
    (debug-assert (player-waiting-for-input player) (me player))
    (setf (player-waiting-for-input player) nil)
    ret))

(defun launch-game (game-name players)
  (play (funcall (game-generator-make-game (find-game-generator game-name)) players)))


(my-defun game talk (sender text)
  (my announce :talk :sender sender :text text))

(my-defun game drop-player (p)
  (deletef p (my players))
  (unless (cdr (my players))
    (my finished :winner (first (my players)))))

(my-defun game resign (player-controller &key (reason :resigned) )
  (let ((p (find player-controller (my players) :key 'player-controller)))
    (when (and p (not (my game-over)))
      (my announce reason :player p)
      (my drop-player p))))

(defrules game new-state ()
  (my announce :new-state)
  (loop for p in (my players) do (my secret-move :ready-to-play p '(:one t)))
  (my players-ready))

(defmethod player-controller-var ((player player) var)
  (player-controller-var (player-controller player) var))
(defmethod (setf player-controller-var) (new-value (player player) var)
  (setf (player-controller-var (player-controller player) var) new-value))
(defmethod player-controller-name-to-ml ((player player))
  (player-controller-name-to-ml (player-controller player)))