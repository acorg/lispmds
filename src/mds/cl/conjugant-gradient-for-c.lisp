(in-package user)

;; this function is very similar to hillclimbs.  they should really be integrated, but it is messy to do so,
;; at least in the way i currently see it
;; for now have this function

;; note, the perturbation-f is used in hillclimbs to determine how to move the points (and which points can be moved)
;; here it is used just to say which can be moved.

;; perhaps the perturbation-f should be supplied by the metric-mds or ordinal-mds (and not mds), and should go
;; hand in hand with the fitness-f.  ahhh, maybe it should be the function to get the next iteration--that 
;; conceptually it is the function to provide the next set of coords (the next step of the iteration, and 
;; perhaps in the case of the hillclimber it can call the hillclimber?

(defun conjugant-gradient-iterations (l perturbation-f fitness-f   ;; perturbation-f is in this case a list of moveable coords
				      &key (max-trials 100)
					   (max-climbs 100)
					   (acceptance-f #'>=)
					   optimum-acceptance-f
					   initial-dribble-f
					   incremental-dribble-f
					   (incremental-dribble-modulus 1))  ;;1 is every trial, 20 is every 20th
  (if (or max-trials acceptance-f)
      (format t "Warning: passing max-trials or acceptance-f for conjugant gradiet optimization does not make sense, ~
                 ignoring those arguments~%"))
  (let ((current-fitness (funcall fitness-f l perturbation-f 'initial)))
    (if initial-dribble-f (funcall initial-dribble-f current-fitness l))
    (loop for i below max-climbs do
	  (multiple-value-bind (current-fitness-not-used new-fitness new-l num-trials)
	      (funcall fitness-f l perturbation-f 'next)
	    current-fitness-not-used  ;; to stop compiler warning
	    (if (eql num-trials -7777777)    ;;change here
		(return (values new-l new-fitness 'exhausted-trials-on-individual-climb)))  ;;change here
	    (if optimum-acceptance-f
		(if (funcall optimum-acceptance-f new-fitness)
		    (return (values new-l new-fitness 'optimum-acceptance-reached))))
	    (if incremental-dribble-f 
		(if (zerop (mod i incremental-dribble-modulus))
		    (funcall incremental-dribble-f new-fitness num-trials (inc i) new-l)))
	    (setq l new-l)
	    (setq current-fitness new-fitness))
	finally (return (values l current-fitness 'exhausted-trials-on-number-of-climbs)))))