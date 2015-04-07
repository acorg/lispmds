(in-package user)

(defun hillclimb (l perturbation-f fitness-f &optional (max-trials 100) (acceptance-f #'>))
  ;;as an optimization we could pass in the starting fitness from hillclimbs optionally
  (let ((starting-fitness (funcall fitness-f l)))
    (loop for trial below max-trials do
          (let* ((trial-l (funcall perturbation-f l))
                 (trial-fitness (funcall fitness-f trial-l l)))
	    ;;(fi (print (list 'tried trial-l 'got trial-fitness))
		;;"~/bb/derek/src/ppi-hillclimb-run.lisp"
		;;:append)
            (if (funcall acceptance-f trial-fitness starting-fitness)
              (return (values trial-l trial-fitness trial))))
          finally (return 'no-climb))))

(defun hillclimbs (l perturbation-f fitness-f 
		   &key (max-trials 100)
			(max-climbs 100)
			(acceptance-f #'>=)
			optimum-acceptance-f
			initial-dribble-f
			incremental-dribble-f
			(incremental-dribble-modulus 1))  ;;1 is every trial, 20 is every 20th
  (let ((current-fitness (funcall fitness-f l)))
    (if initial-dribble-f (funcall initial-dribble-f current-fitness l))
    (loop for i below max-climbs do
	  (multiple-value-bind (new-l new-fitness num-trials)
	      (hillclimb l perturbation-f fitness-f max-trials acceptance-f)
	    (if (eql new-l 'no-climb)
		(return (values l current-fitness 'exhausted-trials-on-individual-climb)))
	    (if optimum-acceptance-f
		(if (funcall optimum-acceptance-f new-fitness)
		    (return (values new-l new-fitness 'optimum-acceptance-reached))))
	    (if incremental-dribble-f 
		(if (zerop (mod i incremental-dribble-modulus))
		    (funcall incremental-dribble-f new-fitness num-trials (inc i) new-l)))
	    (setq l new-l)
	    (setq current-fitness new-fitness))
	finally (return (values l current-fitness 'exhausted-trials-on-number-of-climbs)))))
		  
#|
(defun hillclimbs-onemax (l &key (dribble t) hillclimbs-args)
  (apply #'hillclimbs l
	 (^ (l) 
	    (scatter l 0.1))
	 (^ (l) 
	    (apply #'+ l))
	 :initial-dribble-f (if dribble (^ (initial-fitness &rest ignore)
					   (g-plot (list (list 0 initial-fitness))
						   :element-name 'foo)))
	 :incremental-dribble-f (if dribble (^ (next-fitness num-trials x &rest ignore)
					       num-trials
					       (g-plot (list (list x next-fitness)) 
						       :element-name 'foo :append t :refresh nil)
					       (print next-fitness)))
	 :optimum-acceptance-f (^ (fitness)
				  (= fitness (length l)))
	 hillclimbs-args))
|#