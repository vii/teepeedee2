(in-package #:tpd2.game.truc)

#.(progn (use-package '#:tpd2.ml.html) nil)

(my-defun truc-player 'object-to-ml ()
  (<div :class "truc-player"
	(output-raw-ml (call-next-method))
	(<p (my stack) (format nil " chip~P." (my stack)))
	(cond 
	  ((my folded)
	   (<p :class "folded" "FOLDED"))
	  ((not (zerop (my wins)))
	   (<p :class "wins" (my wins) (format nil " win~P this round." (my wins)))))))

(my-defun truc 'object-to-ml ()
  (<div :class "truc"
	(output-raw-ml (call-next-method))
	(<p :class "stake" "Playing for "
	     (my stake) (format nil " chip~P" (my stake)) ".")
	(<div :class "table"
	      (loop for p in (my players)
		    when (and (not (its folded p)) (its chosen-card p))
		    do (<p (output-raw-ml (tpd2.game::player-controller-name-to-ml (player-controller  p))) ": " (output-object-to-ml (make-card-from-number (its chosen-card p))))))
	
	(loop for p in (my players)
	      when (and (current-web-controller (player-controller p)) (its cards p)) do
	      (<div :class "hand"
		    (<p "Your cards are: " 
			(loop for card in (sort (copy-list (its cards p)) '> :key 'card-number-truc-ranking) do
			      (output-escaped-ml " ")
			      (cond ((loop for m in (its waiting-for-input (player-controller p)) 
					   thereis (eql (its move-type m) :select-card))
				     (let-current-values (p card)
				       (html-action-link (output-object-to-ml (make-card-from-number card))
					 (web-state-queue-choice (player-controller p) :select-card card)
					 (values))))
				    (t (output-raw-ml (output-object-to-ml (make-card-from-number card))))))
			".")))))

(defmacro preserve-specials (specials &body body)
  (let ((tmps (mapcar (lambda(x)(gensym (symbol-name x))) specials)))
    `(let ,(loop for s in specials
		 for m in tmps
		 collect `(,m ,s))
       (macrolet ((with-restored-specials (&body body)
		  `(let ,',(loop for s in specials
				 for m in tmps
				 collect `(,s ,m))
		     ,@body)))
	 ,@body))))

(progn
  (let ((socket (tpd2.io:make-con-listen :port 8888)))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve))

  #+sbcl
  (preserve-specials (*trace-output* *standard-output* *error-output* *debug-io* 
				     swank::*emacs-connection*)
    (sb-thread:make-thread 
     (lambda() 
       (with-restored-specials
	   (tpd2.io:event-loop)))
     :name "EVENT-LOOP")))
