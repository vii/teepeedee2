(in-package #:tpd2.game)

(defgeneric move-continuation (k controller player-state move-type choices &rest args))
(defmethod move-continuation (k controller player-state move-type choices &rest args)
  (funcall k (apply 'move controller player-state move-type choices args)))

(defmacro with-game (&body body)
  `(with-call/cc
     ,@body))

(defgeneric move (controller player-state move-type choices &rest args))
(defmethod move (controller player-state move-type choices &rest args)
  (declare (ignore args))
  (random-choice choices))

(defun validate-choice (choices choice)
  (acond ((and (not choice) (member nil (choices-list choices)))
	  nil)
	 ((and choice (find choice (choices-list choices) :test 'equalp))
	  it)
	 (t
	  (error "Forbidden move ~A: choose from ~A" choice choices))))


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
  (defgeneric game-vars (obj) (:method-combination append))
  (defgeneric secret-game-vars (obj) (:method-combination append))
  (defgeneric play (game)))

(defmacro defgameclass (name-and-options &rest slots)
  (let ((name (force-first name-and-options)))
    (flet ((slot-to-cons (slot)
	     `(cons 
	       ,(or (getf (rest (force-rest slot)) :documentation) (symbol-name (force-first slot)))
	       (,(intern (strcat name '- (force-first slot))) ,name))))
      `(eval-always 
	 (defmystruct ,name-and-options
	     ,@(mapcar (lambda(s)
			 (destructuring-bind (name &optional initform &rest args)
			     (force-list s)
			   `(,name ,initform
				   ,@(let ((n (copy-list args))) 
					  (remf n :secret)
					  (remf n :documentation)
					  n)))) slots))
	 (defmethod game-vars append ((,name ,name))
	   (list
	    ,@(loop for slot in slots 
		    unless (getf (rest (force-rest slot)) :secret) 
		    collect (slot-to-cons slot))))
	 (defmethod secret-game-vars append ((,name ,name))
	   (list
	    ,@(loop for slot in slots 
		    when (getf (rest (force-rest slot)) :secret) 
		    collect (slot-to-cons slot))))
	 (find-class ',name)))))

(defgameclass game
    players
  other-listeners)

(defgameclass player
    controller
  game
  waiting-for-input)

(my-defun game listeners ()
  (append (mapcar 'player-controller (my players)) (my other-listeners)))

(defgeneric inform (listener game-state message &rest args))
(defmethod inform (listener game-state message &rest args)
  (declare (ignore args)))

(my-defun game announce (message &rest args)
  (loop for l in (my listeners)
	do (apply 'inform l me message args)))

(defgeneric player-controller-name (controller))
(defmethod player-controller-name (controller)
  controller)

(my-defun player name ()
  (player-controller-name (my controller)))

(eval-always (defvar *games* (make-hash-table :test 'equalp)))
  
(defmacro defgame (name superclasses slots defplayer &rest options)
  (let ((friendly-name (or (getf options :documentation) (symbol-name name))))
    (flet ((defgameclass-form (name superclasses options slots)
	     `(defgameclass (,name 
			   ,@(mapcar (lambda(c) `(:include ,c)) 
				     superclasses)
			   ,@options)
		  ,@slots)))
    (destructuring-bind
	  (defplayer-sym df-superclasses df-slots &rest df-options)
	defplayer
      (assert (eq 'defplayer defplayer-sym))
      `(eval-always
	 (setf (gethash (force-byte-vector ,friendly-name) *games*) (lambda(controllers)
						  (let ((game (,(intern (strcat 'make- name)))))
						    (let ((players 
							   (mapcar (lambda(c) 
								     (,(intern (strcat 'make- name '-player)) 
								       :game game 
								       :controller c)) controllers)))
						      (setf (game-players game) players))
						    game)))
	 ,(defgameclass-form (intern (strcat name '-player)) 
			     (or df-superclasses (list 'player))
			     df-options
			     df-slots)
	 ,(defgameclass-form name
			     (or superclasses (list 'game))
			     options
			     slots))))))

(defmacro defrules (game func lambda-list &body body)
  `(eval-always
     (with-call/cc
       (my-defun ,game ,func ,lambda-list ,@body))))

(defrules game secret-move (type player choices &rest args)
  (check-type type keyword)
  (let ((ret (call/cc (lambda(cc)
			(apply 'move-continuation cc (player-controller player) player type choices 
			       args)
			'waiting-for-move-from-call/cc))))
    (validate-choice choices ret)))

(defrules game move (type player choices &rest args)
  (debug-assert (not (player-waiting-for-input player)))
  (setf (player-waiting-for-input player) t)
  (let ((ret (apply 'game-secret-move me type player choices args)))
    (my announce type :choice ret :player player)
    (debug-assert (player-waiting-for-input player))
    (setf (player-waiting-for-input player) nil)
    ret))

(defun launch-game (game players)
  (play (funcall (gethash (force-byte-vector game) *games*) players)))
