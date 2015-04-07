(in-package user)

;;;----------------------------------------------------------------------
;;;                      ORDINAL MDS
;;;----------------------------------------------------------------------

(defun ordinal-full-mds (target-dist-matrix stress-component-f &optional (comparison-f #'<) &rest args)
    (apply #'mds
	 (let* ((target-dists (apply #'append (hi-table-values-upper-short-triangle target-dist-matrix)))
		(num-comparisons 
		 (print (loop for (target-dist . target-dist-rest) on target-dists 
			    when (not (dont-care-p target-dist)) sum
			      (loop for next-target-dist in target-dist-rest
				  when (not (dont-care-p next-target-dist)) sum
				    1)))))
	   (^ (coordss &optional old-coordss)
	      old-coordss
	      (/ (loop for (mds-dist . mds-dist-rest) on (all-comparisons coordss #'e-dist)
		     for (target-dist . target-dist-rest) on target-dists 
		     when (not (eql 'dont-care target-dist)) sum
		       (loop for next-mds-dist in mds-dist-rest
			   for next-target-dist in target-dist-rest
			   when (not (eql 'dont-care next-target-dist)) sum
			     (if (funcall comparison-f target-dist next-target-dist)
				 (funcall stress-component-f (- next-mds-dist mds-dist))
			       (if (funcall comparison-f next-target-dist target-dist)
				   (funcall stress-component-f (- mds-dist next-mds-dist))
				 (progn
				   ;;(print (list 'neither target-dist next-target-dist))
				   0)))))
		 num-comparisons)))
	 (let ((moveable-coords (snoop-moveable-coords args)))
	   (^ (coordss)
	      (let ((krandom (if (listp moveable-coords)
				 (nth (krandom (length moveable-coords)) moveable-coords)
			       (krandom (length coordss)))))
		(loop for coords in coordss for i from 0 collect
		      (if (= i krandom)
			  (uniform-perturbs coords 0.1)
			coords)))))
	 args))


