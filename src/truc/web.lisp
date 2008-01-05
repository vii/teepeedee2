(in-package #:tpd2.game.truc)

#.(progn (use-package '#:tpd2.ml.html) nil)

(my-defun truc-player 'object-to-ml ()
  (<div :class "truc-player"
	(output-raw-ml (call-next-method))
	(<p (my stack) (format nil " point~P." (my stack)))
	(cond 
	  ((my folded)
	   (<p :class "folded" "FOLDED"))
	  ((not (zerop (my wins)))
	   (<p :class "wins" (my wins) (format nil " win~P this round." (my wins)))))))

(my-defun truc 'object-to-ml ()
  (<div :class "truc"
	(output-raw-ml (call-next-method))
	(<h2 :class "stake" "Playing for "
	     (my stake) (format nil " point~P" (my stake)))
	(<div :class "table"
	      (loop for p in (my players)
		    when (and (not (its folded p)) (its chosen-card p))
		    do (<p (its name p) ": " (output-object-to-ml (make-card-from-number (its chosen-card p))))))
	
	(loop for p in (my players)
	      when (and (current-web-controller (player-controller p)) (its cards p)) do
	      (<div :class "hand"
		    (<p "Your cards are: " 
			(loop for card in (sort (copy-list (its cards p)) '> :key 'card-number-truc-ranking) do
			      (output-escaped-ml " ")
			      (let-current-values (p card)
				(html-action-link (output-object-to-ml (make-card-from-number card))
						  (web-state-queue-choice (player-controller p) :select-card card)
						  (values))))
			".")))))
