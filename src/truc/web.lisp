(in-package #:tpd2.game.truc)

(my-defun truc-player 'object-to-ml ()
  (<div :class "truc-player"
	(call-next-method)
	(<p (my stack) (format nil " chip~P." (my stack)))
	(cond 
	  ((my folded)
	   (<p :class "folded" "FOLDED"))
	  ((not (zerop (my wins)))
	   (<p :class "wins" (my wins) (format nil " win~P this round." (my wins)))))))

(my-defun truc 'object-to-ml ()
  (<div :class "truc"
	(call-next-method)
	(<p :class "stake" "Playing for "
	     (my stake) (format nil " chip~P" (my stake)) ".")
	(<div :class "table"
	      (loop for p in (my players)
		    when (and (not (its folded p)) (its chosen-card p))
		    do (<p (player-controller-name-to-ml (player-controller  p)) ": " (output-object-to-ml (make-card-from-number (its chosen-card p))))))
	
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
				    (t (output-object-to-ml (make-card-from-number card)))))
			".")))))




