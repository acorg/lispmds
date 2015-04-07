(in-package user)

;;;----------------------------------------------------------------------
;;;                            METRIC MDS
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm (target-dist-matrix stress-component-f &rest args)
  ;;there is an optimization we could do here to reduce this O(n^2) algorithm to O(n)
  ;;we do not do all comparisions when there has only been one coords that has moved (the
  ;;usual case).  What we do is see which coord has moved (or pass it from hillclimb) and
  ;;(maybe after checking that there really only was one) then do not calc stress from
  ;;scratch with all dists, but take the old stress (passed from hillclimb) and calc the
  ;;stress components of what has changed (substracting the old and adding in the new).
  (apply #'mds 
	 (let ((target-dists (apply #'append (hi-table-values-upper-short-triangle target-dist-matrix))))
	   (^ (coordss &optional old-coordss)
	      old-coordss
	      (let ((all-comparisons (all-comparisons coordss #'e-dist)))
		(- (sqrt (/ (loop for mds-dist in all-comparisons
				for target-dist in target-dists
				when (not (eql 'dont-care target-dist))
				sum (if (listp target-dist) ;;merged tables for piecing together
					(loop for a-target-dist in target-dist sum
					      (funcall stress-component-f (- a-target-dist mds-dist)))
				      (funcall stress-component-f (- target-dist mds-dist))))
			    (loop for mds-dist in all-comparisons
				for target-dist in target-dists
				when (not (eql 'dont-care target-dist))
				sum (if (listp target-dist)
					(* (length target-dist) (funcall stress-component-f mds-dist))
				      (funcall stress-component-f mds-dist)))))))))
	 (let ((moveable-coords (snoop-moveable-coords args))
	       (unmoveable-coords (snoop-unmoveable-coords args)))
	   (^ (coordss)
	      (let ((krandom (if (listp moveable-coords)
				 (progn
				   ;; inefficient if long lists, but we should be using 
				   ;; conjugant-gradient anyhow, and that does not have this problem
				   (if unmoveable-coords 
				       (setq moveable-coords (set-difference moveable-coords unmoveable-coords)))
				   (nth (krandom (length moveable-coords)) moveable-coords))
			       (if unmoveable-coords
				   (loop for i below 1000 do
					 (let ((krandom (krandom (length coordss))))
					   (if (not (member krandom unmoveable-coords))
					       (return krandom)))
					 finally (error "cannot find a moveable point"))
				 (krandom (length coordss))))))
		(loop for coords in coordss for i from 0 collect
		      (if (= i krandom)
			  (uniform-perturbs coords 0.1)
			coords)))))
	 args))

(defun metric-mds-local-norm (target-dist-matrix stress-component-f &rest args)
  ;; same as the above, but use local-normalized stress
  (apply #'mds 
	 (let ((target-dists (apply #'append (hi-table-values-upper-short-triangle target-dist-matrix))))
	   (^ (coordss &optional old-coordss)
	      old-coordss
	      (let ((all-comparisons (all-comparisons coordss #'e-dist)))
		(- (loop for mds-dist in all-comparisons
		       for target-dist in target-dists
		       when (not (eql 'dont-care target-dist))
		       sum (if (listp target-dist) ;;merged tables for piecing together
			       (loop for a-target-dist in target-dist sum
				     (funcall stress-component-f 
					      (/ (- a-target-dist mds-dist)
						 (if (zerop a-target-dist)
						     0.001
						   a-target-dist))))
			     (funcall stress-component-f 
				      (/ (- target-dist mds-dist)
					 (if (zerop target-dist)
					     0.001
					   target-dist)))))))))
	 (let ((moveable-coords (snoop-moveable-coords args))
	       (unmoveable-coords (snoop-unmoveable-coords args)))
	   (^ (coordss)
	      (let ((krandom (if (listp moveable-coords)
				 (progn
				   ;; inefficient if long lists, but we should be using 
				   ;; conjugant-gradient anyhow, and that does not have this problem
				   (if unmoveable-coords 
				       (setq moveable-coords (set-difference moveable-coords unmoveable-coords)))
				   (nth (krandom (length moveable-coords)) moveable-coords))
			       (if unmoveable-coords
				   (loop for i below 1000 do
					 (let ((krandom (krandom (length coordss))))
					   (if (not (member krandom unmoveable-coords))
					       (return krandom)))
				       finally (error "cannot find a moveable point"))
				 (krandom (length coordss))))))
		(loop for coords in coordss for i from 0 collect
		      (if (= i krandom)
			  (uniform-perturbs coords 0.1)
			coords)))))
	 args))


;;this was the mds until jan00 (replaced with the above to allow lists of target dists)
'(defun metric-mds (target-dists stress-component-f &rest args)
  (apply #'mds 
	 (^ (coordss &optional old-coordss)
	    old-coordss
	    (let ((all-comparisons (all-comparisons coordss #'e-dist)))
	      (- (sqrt (/ (loop for mds-dist in all-comparisons
			      for target-dist in target-dists
			      when (not (eql 'dont-care target-dist))
			      sum (funcall stress-component-f (- target-dist mds-dist)))
			  (loop for mds-dist in all-comparisons
			      for target-dist in target-dists
			      when (not (eql 'dont-care target-dist))
			      sum (funcall stress-component-f mds-dist)))))))
	 args))

;;lance's median error
'(defun metric-mds (target-dists &rest args)
  (apply #'mds 
	 (^ (coordss)
	    (nth (round (/ (length (loop for mds-dist in (all-comparisons coordss #'e-dist)
				       for target-dist in target-dists collect
					 (- (square (- target-dist mds-dist))))) 2))
		 (my-sort (loop for mds-dist in (all-comparisons coordss #'e-dist)
			      for target-dist in target-dists collect
				(- (square (- target-dist mds-dist)))))))
	 args))


