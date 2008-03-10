(in-package #:tpd2.game)

(defgeneric move (controller player-state move-type choices &rest args))
(defmethod move (controller player-state move-type choices &rest args)
  (declare (ignore args controller player-state move-type))
  (random-choice choices))
(defgeneric move-continuation (k controller player-state move-type choices &rest args))
(defmethod move-continuation (k controller player-state move-type choices &rest args)
  (funcall k (apply 'move controller player-state move-type choices args)))

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

(defgeneric choices-list (choice))
(defgeneric choices-list-form (first &rest rest))

(defmethod choices-list ((choices list))
  (apply 'choices-list-form (first choices) (rest choices)))

(defmethod choices-list ((choices (eql :boolean)))
  (list t nil))

(defmethod choices-list-form ((first (eql :integer)) &rest args)
  (destructuring-bind
	(min-inclusive max-exclusive)
      args
    (loop for i from min-inclusive below max-exclusive
	  collect i)))

(defmethod choices-list-form ((first (eql :one)) &rest args)
  args)

(defun random-choice (choices)
  (random-elt (choices-list choices)))

(eval-always
  (defgeneric play (game)))

(defgeneric game-name (game))

(my-defun game finished (winner)
  (setf (my game-over) t)
  (my announce :game-over :player winner)
  (values))

(defmyclass player
    controller
  game
  waiting-for-input)

(my-defun game listeners ()
  (append (mapcar 'player-controller (my players)) (my other-listeners)))

(defgeneric inform (listener game-state message &rest args))
(defmethod inform (listener game-state message &rest args)
  (declare (ignore args listener game-state message)))

(my-defun game announce (message &rest args)
  (loop for l in (my listeners)
	do (apply 'inform l me message args)))

(defstruct game-generator
  make-game
  unassigned-controllers-waiting)

(eval-always (defvar *games* (make-hash-table :test 'equalp)))
  
(defmacro defgame (name superclasses slots defplayer &rest options)
  (let ((game-name-string (force-byte-vector (or (getf options :documentation) (string-capitalize (symbol-name name))))))
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
	 (setf (gethash ,game-name-string *games*) 
	       (make-game-generator
		:make-game (lambda(controllers)
						  (let ((game (,(intern (strcat 'make- name)))))
						    (let ((players 
							   (mapcar (lambda(c) 
								     (,(intern (strcat 'make- name '-player)) 
								       :game game 
								       :controller c)) controllers)))
						      (setf (game-players game) players))
						    game))))
	 ,(defgameclass-form (intern (strcat name '-player)) 
			     (or df-superclasses (list 'player))
			     df-options
			     df-slots)
	 ,(defgameclass-form name
			     (or superclasses (list 'game))
			     options
			     slots)
	 (defmethod game-name ((,name ,name))
	   ,game-name-string))))))

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
  (debug-assert (not (player-waiting-for-input player)))
  (setf (player-waiting-for-input player) t)
  (let ((ret (apply 'game-secret-move me type player choices args)))
    (my announce type :choice ret :player player)
    (debug-assert (player-waiting-for-input player))
    (setf (player-waiting-for-input player) nil)
    ret))

(defun launch-game (game-name players)
  (play (funcall (game-generator-make-game (find-game-generator game-name)) players)))


(my-defun game talk (sender text)
  (my announce :talk :sender sender :text text))

(my-defun game resign (player-controller)
  (let ((p (find player-controller (my players) :key 'player-controller)))
    (when p
      (my announce :resigned player-controller)
      (deletef p (my players))
      (when (eql 1 (length (my players)))
	(my finished (first (my players)))))))


(defrules game new-state ()
  (my announce :new-state)
  (loop for p in (my players) do (my secret-move :ready-to-play p '(:one t))))

(defgeneric player-controller-name-to-ml (controller))
