(in-package user)

#|
;; this superseded below with a faster version
;; the dhat-ll was taking 99% of 1 min for all-strains merge launching the mds-visualization window
(defun hi-table-values-to-base-1-symmetric-array-with-threshold-info (table-values)
  (let* ((length (length (car table-values)))
	 (array (make-array (list (inc length) (inc length)))))
    (loop for r from 1 to length do
	  (loop for s from 1 to length do
		(let ((table-value (dhat-ll table-values r s)))
		  (if (thresholdp table-value)
		      (setq table-value (list '< (threshold-number table-value))))
		  (setf (aref array r s) table-value)
		  (setf (aref array s r) table-value))))
    array))
|#


(defun hi-table-values-to-base-1-symmetric-array-with-threshold-info (table-values)
  (let* ((length (length (car table-values)))
	 (array (make-array (list (inc length) (inc length)))))
    (loop for r from 1 to length 
	for row in table-values do
	  (loop for s from 1 to length 
	      for table-value in row do
		(if (<= r s)
		    ;; upper triangle, copy into the array, to both upper and lower
		    (progn
		      (setq table-value
			(if (listp table-value)   ;; when we have multiple titers (from a merge, or a bootstrap)
			    (loop for tv in table-value collect
				  (if (thresholdp tv)
				      (list '< (threshold-number tv))
				    tv))
			  (if (thresholdp table-value)
			      (list '< (threshold-number table-value))
			    table-value)))
		      (setf (aref array r s) table-value)
		      (setf (aref array s r) table-value))
		  ;; otherwise do nothing
		  nil)))
    array))



(defvar *metric-conjugant-gradient-stress-f* nil)  ;; name the stress fuction so we can (temporarily) (nasty hack)
                                                   ;; so we can switch on it in mds

(defun dhat-ll (ll r s)
  ;; NOTE, for speed, this should be an aref in the future
  (if (> r s)   ;; to pick out of the upper triangle (we assume symetric matrix)
      (nth (dec r) (nth (dec s) ll))
    (nth (dec s) (nth (dec r) ll))))

(defun d-from-array (p r s num-dims)
  (let ((r-start (inc (* (dec r) num-dims)))
	(s-start (inc (* (dec s) num-dims))))
    (sqrt
     (loop for dims below num-dims
	 for ri from r-start
	 for si from s-start sum
	   (let ((x (- (fref p ri) (fref p si))))
	     (* x x))))))

(defun coord-from-array (p point dimension num-dims)
  (fref p (+ (* (dec point) num-dims) dimension)))



(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; 2002-04-19  DEREK
  ;; note, there are 2 speed hacks in the hi-metric optimizer below that have not been put in here yet 
  ;;  a cache for dmds (useful when we run in higher dimensions), and not caculating dmds if dtarget is a dont-care
  ;;  also the dim anneal code has not been added here either (though it would be easy to add)
  ;;  and the restricted dimensions to allow movement in has not been added either (again easy)
  ;;  none of these are hard, but they would require more time to check the results are good than anything else
  ;;  i should add a warning in case i try to use the restricted dimensions or dim anneal in here

  ;; with the global mds normalization
  ;; optimization method in metric-mds-conjugant-gradient-speed-optimization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (let ((disconnected-coords (mapcar #'inc (snoop-disconnected-coords args))))
	     (^ (p)
		(let ((numerator 0.0d0)
		      (denominator 0.0d0))
		  (loop for r from 1 below NUM_POINTS do
			(loop for s from (+ r 1) to NUM_POINTS do
			    (if (or (member r disconnected-coords)
				    (member s disconnected-coords))
				'do-nothing-to-increment-sums
			      (let ((dtarget (aref target-dist-matrix r s))
				    (dmds    (d-from-array p r s NUM_DIMS)))
				(if (not (dont-care-p dtarget))
				    (progn
				      (setq numerator   (+ numerator   (square (- dmds dtarget))))
				      (setq denominator (+ denominator (square dmds)))))))))
		  (sqrt (/ numerator denominator)))  ;; would be good to get for divide by zero
		)))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args)))
	       (unmoveable-coords (mapcar #'inc (snoop-unmoveable-coords args)))
	       (disconnected-coords (mapcar #'inc (snoop-disconnected-coords args))))
	   (^ (p df)

	      ;; these terms can be calculated once for each iteration
	      (let ((numerator-term-1-rs-loop 0.0d0)
		    (numerator-term-2-rs-loop 0.0d0)
		    (denominator-rs-loop      0.0d0))
		(loop for r from 1 below NUM_POINTS do
		      (loop for s from (+ r 1) to NUM_POINTS do
			    (if (or (member r disconnected-coords)
				    (member s disconnected-coords))
				'do-nothing-to-increment-sums
			      (let ((dtarget (aref target-dist-matrix r s))
				    (dmds    (d-from-array p r s NUM_DIMS)))
				(if (not (dont-care-p dtarget))
				    (progn
				      (setq numerator-term-1-rs-loop 
					(+ numerator-term-1-rs-loop (square dmds)))
				      (setq numerator-term-2-rs-loop 
					(+ numerator-term-2-rs-loop (square (- dmds dtarget))))
				      (setq denominator-rs-loop
					numerator-term-1-rs-loop)))))))
		
		;; these terms are different for each point and coordinate
		(loop for a from 1 to NUM_POINTS do       ;; a is the point      
		      (loop for k from 1 to NUM_DIMS do   ;; k is the dimension      ******** speed hack to come, but k inside the s loop ******
			    (setf (fref df (+ (* (dec a) NUM_DIMS) k))
			      (if (and (not (member a disconnected-coords))
				       (and (or (eql 'all moveable-coords)
						(member a moveable-coords))
					    (not (member a unmoveable-coords))))
				  (let ((numerator-term-1-s-loop  0.0d0)
					(numerator-term-2-s-loop  0.0d0))
				    (loop for s from 1 to NUM_POINTS do
					  (if (or (= a s)
						  (member s disconnected-coords))
					      0.0d0
					    (let ((dmds    (d-from-array p a s NUM_DIMS))
						  (dtarget (aref target-dist-matrix a s)))
					      (if (dont-care-p dtarget)
						  0.0d0
						(let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								  (coord-from-array p s k NUM_DIMS))))
						  (setq numerator-term-1-s-loop
						    (+ numerator-term-1-s-loop
						       (* (- 1.0d0 (/ dtarget dmds))
							  Xak-Xsk)))
						  (setq numerator-term-2-s-loop
						    (+ numerator-term-2-s-loop
						       Xak-Xsk)))))))
				    (* 0.5d0
				       (sqrt (/ numerator-term-1-rs-loop
						numerator-term-2-rs-loop))
				       (/ (- (* numerator-term-1-rs-loop
						2.0d0 numerator-term-1-s-loop)
					     (* numerator-term-2-rs-loop
						2.0d0 numerator-term-2-s-loop))
					  (square denominator-rs-loop))))
				0.0d0))))
		(values p df)))
	   )
	 args))


(defun metric-mds-local-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; 2002-04-19  DEREK
  ;; note, there are 2 speed hacks in the hi-metric optimizer below that have not been put in here yet 
  ;;  a cache for dmds (useful when we run in higher dimensions), and not caculating dmds if dtarget is a dont-care
  ;;  also the dim anneal code has not been added here either (though it would be easy to add)
  ;;  and the restricted dimensions to allow movement in has not been added either (again easy)
  ;;  none of these are hard, but they would require more time to check the results are good than anything else
  ;;  i should add a warning in case i try to use the restricted dimensions or dim anneal in here

  ;; with the local target normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (let ((disconnected-coords (mapcar #'inc (snoop-disconnected-coords args))))
	     (^ (p)
		(loop for r from 1 below NUM_POINTS sum
		      (loop for s from (+ r 1) to NUM_POINTS sum
			    (if (or (member r disconnected-coords)
				    (member s disconnected-coords))
				0.0d0
			      (let ((dtarget (aref target-dist-matrix r s))
				    (dmds    (d-from-array p r s NUM_DIMS)))
				(if (dont-care-p dtarget)
				    0
				  (expt (/ (- dmds dtarget)
					   (if (zerop dtarget)
					       0.001   ;; arbitrary for this normalization, to avoid division by zero
					     dtarget))
					2)))))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args)))
	       (unmoveable-coords (mapcar #'inc (snoop-unmoveable-coords args)))
	       (disconnected-coords (mapcar #'inc (snoop-disconnected-coords args))))
	   (^ (p df)
	      (loop for k from 1 to NUM_POINTS do       ;; k is the point       NOTE, k AND a ARE SWITCHED ABOVE
		    (loop for a from 1 to NUM_DIMS do   ;; a is the dimension
			  (setf (fref df (+ (* (dec k) NUM_DIMS) a))
			    (let ((so-far 0.0d0))
			      (if (and (not (member k disconnected-coords))
				       (and (or (eql 'all moveable-coords)
						(member k moveable-coords))
					    (not (member k unmoveable-coords))))
				  (loop for tt from 1 to NUM_POINTS do   ;; use tt, because t is not useable in lisp
					(if (and (not (= tt k))
						 (not (member tt disconnected-coords)))
					    (let ((dtarget (aref target-dist-matrix k tt))
						  (dmds    (d-from-array p k tt NUM_DIMS)))
					      (if (not (dont-care-p dtarget))
						  (setq so-far
						    (+ so-far
						       (* (/ (- dmds dtarget)
							     (let ((denominator (* dmds (expt dtarget 2))))
							       (if (zerop denominator)
								   0.001    ;; arbitrary (real fix is by better normalization in the function
								 denominator)))
							  (- (coord-from-array p k a NUM_DIMS) 
							     (coord-from-array p tt a NUM_DIMS))))))))))
			      (* so-far 2)))))
	      (values p df)))
	 args))

;;;----------------------------------------------------------------------
;;;           for hi-metric, row and col adjusts, and <10 
;;;----------------------------------------------------------------------

(defvar *dim-anneal-coefficient*)
(setq   *dim-anneal-coefficient* 0.03)  ;; taken from Alan's code

(defvar *adjust-coordss*)
(defvar *adjust-rows*)
(defvar *adjust-columns*)
(setq *adjust-coordss* t)
(setq *adjust-rows*    t)
(setq *adjust-columns* t)
(setq *adjust-rows*    nil)
(setq *adjust-columns* nil)

;; for the sharpness of the switch in the <10
(defvar *beta*)
(setq *beta* 10)

;; defined in :mds somewhere
;;(defun squash (x) 
;;  (/ 1.0 (+ 1 (exp (- x)))))

(defun dsquash (x)
  (if (< x -500)   ;; to avoid arithmetic underflow (case happened when we did multiple thresholds on sequence
    0.0d0
    (let ((e-to-the-minus-x (exp (- x))))
      (/ e-to-the-minus-x
	 (square (+ 1 e-to-the-minus-x))))))

(defun Yi-from-array (p i num-points num-dims)
  (fref p (+ i (* num-points num-dims))))

(defun Zi-from-array (p i num-points num-dims)
  (fref p (+ i num-points (* num-points num-dims))))

(defmacro zero-safe-dmds (dmds a s)
  `(if (zerop ,dmds)
       (progn
	 (format t "~%Warning: Points ~d and ~d are conincident" ,a ,s)
	 0.0000001)
     ,dmds))

(defun metric-mds-global-norm-conjugant-gradient-hi-metric (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  ;; optimization method in metric-mds-conjugant-gradient-speed-optimization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (let ((disconnected-coords (mapcar #'inc (snoop-disconnected-coords args)))
		 (dim-anneal-coefficients (snoop-dim-anneal-coefficients args)))
	     (^ (p)
		(let ((numerator 0.0d0))
		  (loop for r from 1 below NUM_POINTS do
			(loop for s from (+ r 1) to NUM_POINTS do
			      (if (or (member r disconnected-coords)
				      (member s disconnected-coords))
				  'do-nothing
				(let* ((dtargets-raw (aref target-dist-matrix r s)))      ;; targets for listed merge, or bootstrapped titers
				  (loop for dtarget-raw in (if (and (listp dtargets-raw)
								   (not (eql '< (car dtargets-raw))))
							      dtargets-raw
							     (list dtargets-raw)) do
					(let* ((thresholded-value (listp dtarget-raw))
					       ;; note the dec below for the thresholded value.  we are really treating a <10 as a <5.
					       ;; best would really to treat it as (the geometric equivalent of <7.5).
					       ;; we do the same in the derivative function below.  LINK into the error-line-endpoint function,
					       ;; stress-component, and the master error-line function.
					       (dtarget (if thresholded-value (nth 1 dtarget-raw) dtarget-raw)))
					  (if (true-dont-care-p dtarget)
					      'do-nothing
					    (let* ((dmds (d-from-array p r s NUM_DIMS))
						   (dtarget-dmds (- (- (Yi-from-array p s NUM_POINTS NUM_DIMS) 
								       (+ dtarget (Zi-from-array p r NUM_POINTS NUM_DIMS))) 
								    dmds)))
					      (setq numerator 
						(+ numerator 
						   (if (not thresholded-value)
						       (square dtarget-dmds)
						     (* (square dtarget-dmds)
							(squash (* *beta* dtarget-dmds))))))))))))))

		  (let ((dim-anneal-penalty 0.0))
		    (loop for dim-anneal-coefficient in dim-anneal-coefficients
			for dimension from 1 do
			  (if (not (zerop dim-anneal-coefficient))  ;; efficiency hack
			      (loop for point from 1 below NUM_POINTS do
				    (setq dim-anneal-penalty
				      (+ dim-anneal-penalty
					 (* dim-anneal-coefficient
					    (square (coord-from-array p point dimension NUM_DIMS))))))))

		    (+ numerator dim-anneal-penalty)))  ;; would be good to get for divide by zero
		)))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args)))
	       (unmoveable-coords (mapcar #'inc (snoop-unmoveable-coords args)))
	       (unmoveable-dimensions (snoop-unmoveable-dimensions args))
	       (disconnected-coords (mapcar #'inc (snoop-disconnected-coords args)))
	       (adjustable-columns (mapcar #'inc (snoop-adjustable-columns args)))
	       (adjustable-rows (mapcar #'inc (snoop-adjustable-rows args)))
	       (dim-anneal-coefficients (snoop-dim-anneal-coefficients args))
	       (dmds-cache (make-array (list (array-dimension target-dist-matrix 0)))))
	   (^ (p df)
	      (loop for a from 1 to NUM_POINTS do       ;; a is the point    
		    ;; populate the mds distance cache for this point (so we don't have to do it for every dimension
		    (loop for s from 1 to NUM_POINTS do
			  (setf (aref dmds-cache s) (if (true-dont-care-p (aref target-dist-matrix a s)) 'not-set (d-from-array p a s NUM_DIMS))))
		    (loop for k from 1 to NUM_DIMS do   ;; k is the dimension  
			  (setf (fref df (+ (* (dec a) NUM_DIMS) k))
			    (if (and *adjust-coordss*
				     (not (member a disconnected-coords))  ;; these could be moved higher to save writing the dmds-cache unnecessarily
				     (and (or (eql 'all moveable-coords)
					      (member a moveable-coords))
					  (not (member a unmoveable-coords)))
				     (not (member (dec k) (assoc-value (dec a) unmoveable-dimensions))))
				(let ((numerator-term-1-s-loop  0.0d0))
				  (loop for s from 1 to NUM_POINTS do
					(if (or (= a s)
						(member s disconnected-coords))
					    'do-nothing
					  (let* ((dtargets-raw (aref target-dist-matrix a s)))
					    (loop for dtarget-raw in (if (and (listp dtargets-raw)
									     (not (eql '< (car dtargets-raw))))
									dtargets-raw
								       (list dtargets-raw)) do
						  (let* ((thresholded-value (listp dtarget-raw))
							 (dtarget (if thresholded-value (nth 1 dtarget-raw) dtarget-raw)))
						    (if (true-dont-care-p dtarget)
							'do-nothing
						      (let* ((dmds    (aref dmds-cache s))
							     (dtarget-dmds (- (- (Yi-from-array p (if (< s a) a s) NUM_POINTS NUM_DIMS)
										 (+ dtarget 
										    (Zi-from-array p (if (< s a) s a) NUM_POINTS NUM_DIMS))) 
									      dmds))
							     (Xak-Xsk (- (coord-from-array p a k NUM_DIMS) (coord-from-array p s k NUM_DIMS)))
							     (derivX-dtarget-dmds (- (/ Xak-Xsk (zero-safe-dmds dmds a s)))))
							(setq numerator-term-1-s-loop
							  (+ numerator-term-1-s-loop
							     (if (not thresholded-value)
								 (* 2 dtarget-dmds derivX-dtarget-dmds)
							       (+ (* 2
								     dtarget-dmds
								     derivX-dtarget-dmds
								     (squash (* *beta* dtarget-dmds)))
								  (* (square dtarget-dmds)
								     (dsquash (* *beta* dtarget-dmds))
								     *beta*
								     derivX-dtarget-dmds))))))))))))

				  ;; for dimensional annealing
				  (let* ((dim-anneal-coefficient (nth (dec k) dim-anneal-coefficients))
					 (dim-anneal-penalty (if dim-anneal-coefficient
								 (* 2 dim-anneal-coefficient (coord-from-array p a k NUM_DIMS))
							       0.0)))

				    (+ numerator-term-1-s-loop dim-anneal-penalty)))

			      0.0d0))))
		
	      ;; setting Yi, the column adjust
	      (loop for i from 1 to NUM_POINTS do
		    (setf (fref df (+ (* NUM_POINTS NUM_DIMS) i))
		      (if (or *adjust-columns*
			      (eql t adjustable-columns)
			      (and (listp adjustable-columns)
				   (member i adjustable-columns)))
			  (if (and (not (member i disconnected-coords))
				   (and (or (eql 'all moveable-coords)
					    (member i moveable-coords))
					(not (member i unmoveable-coords))))
			      (let ((Yi-component  0.0d0))
				(loop for r from 1 below i do
				      (if (not (member r disconnected-coords))
					  (let* ((dtargets-raw (aref target-dist-matrix r i)))
					    (loop for dtarget-raw in (if (and (listp dtargets-raw)
									     (not (eql '< (car dtargets-raw))))
									dtargets-raw
								       (list dtargets-raw)) do
						  (let* ((thresholded-value (listp dtarget-raw))
							 (dtarget (if thresholded-value (nth 1 dtarget-raw) dtarget-raw)))
						    (if (true-dont-care-p dtarget)
							'do-nothing
						      (let* ((dmds (d-from-array p r i NUM_DIMS))
							     (dtarget-dmds (- (- (Yi-from-array p i NUM_POINTS NUM_DIMS) 
										 (+ dtarget (Zi-from-array p r NUM_POINTS NUM_DIMS))) dmds)))
							(setq Yi-component
							  (+ Yi-component
							     (if (not thresholded-value)
								 (* 2 dtarget-dmds)
							       (* (+ (* (* 2 dtarget-dmds)
									(squash (* *beta* dtarget-dmds)))
								     (* (square dtarget-dmds)
									(dsquash (* *beta* dtarget-dmds)) *beta*)))))))))))))
				Yi-component)
			    0.0d0)
			0.0d0)
		      ))
	      
	      ;; setting Zi, the row adjust
	      (loop for i from 1 to NUM_POINTS do
		    (setf (fref df (+ NUM_POINTS (* NUM_POINTS NUM_DIMS) i))
		      (if (or *adjust-rows*
			      (eql t adjustable-rows)
			      (and (listp adjustable-rows)
				   (member i adjustable-rows)))
			  (if (and (not (member i disconnected-coords))
				   (and (or (eql 'all moveable-coords)
					    (member i moveable-coords))
					(not (member i unmoveable-coords))))
			      (let ((Zi-component 0.0d0))
				(loop for s from (inc i) to NUM_POINTS do
				      (if (not (member s disconnected-coords))
					  (let* ((dtargets-raw (aref target-dist-matrix i s)))
					    (loop for dtarget-raw in (if (and (listp dtargets-raw)
									      (not (eql '< (car dtargets-raw))))
									dtargets-raw
								      (list dtargets-raw)) do
						  (let* ((thresholded-value (listp dtarget-raw))
							 (dtarget (if thresholded-value (nth 1 dtarget-raw) dtarget-raw)))
						    (if (true-dont-care-p dtarget)
							'do-nothing
						      (let* ((dmds (d-from-array p i s NUM_DIMS))
							     (dtarget-dmds (- (- (Yi-from-array p s NUM_POINTS NUM_DIMS) 
										 (+ dtarget (Zi-from-array p i NUM_POINTS NUM_DIMS))) dmds)))
							(setq Zi-component
							  (+ Zi-component
							     (if (not thresholded-value)
								 (* -2 dtarget-dmds)
							       (* (+ (* (* -2 dtarget-dmds)
									(squash (* *beta* dtarget-dmds)))
								     (* (square dtarget-dmds)
									(dsquash (* *beta* dtarget-dmds))
									(* *beta* -1))))))))))))))
				Zi-component)
			    0.0d0)
			0.0d0)
		      ))

	      (values p df))
	   )
	 args))
	 


#|

;;--------------------------- testing basic functionality -------------------------

(setq 345hi
  (make-hi-table
   '(a b c)
   '(a b c)
   '((0 3 5)
     (0 0 4)
     (0 0 0))))

(batch-mds 345hi
	   2
	   100
	   100)

(make-strain-selection-window 345hi t nil 2)   ;;right angle 345 triangle


and here is the lisp
(0.345435 0.0 1.0 4.0 2.0 6.0 5.0) 
(0.06041 0.803937 1.327623 3.443517 1.79028 5.752546 4.882097) 
(0.036964 0.875174 1.538107 3.678948 1.764087 5.445878 4.697806) 
(0.005068 0.904527 1.953606 3.960162 1.208675 5.135311 4.837719) 
(2.8000003e-5 1.024568 2.010799 3.890134 1.081119 5.085298 4.908082) 
(3.9999998e-6 1.026426 2.004321 3.885206 1.091856 5.088368 4.903823) 
(0.0 1.024764 2.000969 3.883079 1.092429 5.092156 4.906602) 
(0.0 1.023972 2.000776 3.883571 1.092724 5.092457 4.906499) 
(0.0 1.023915 2.000551 3.883554 1.093294 5.092531 4.906156) 
(0.0 1.023898 2.0004 3.88343 1.093369 5.092672 4.906231) 
(0.0 1.023839 2.000373 3.883449 1.093376 5.092712 4.90625) 
(0.0 1.023831 2.000367 3.883456 1.0934 5.092714 4.906234) 
(0.0 1.023832 2.000362 3.883451 1.093404 5.092716 4.906234) 
(0.0 1.023829 2.000359 3.88345 1.093405 5.09272 4.906236) 
(0.0 1.023829 2.000359 3.883451 1.093406 5.09272 4.906235) 
(0.0 1.023829 2.000359 3.883451 1.093406 5.09272 4.906235) 
(0.0 1.023829 2.000359 3.883451 1.093406 5.092721 4.906235)


here is the c run from the command line (do a make in mds/conjugant-gradient)  (they are the same)

berlin:conjugant-gradient $ ~/junk/misc/mds
3 3 5 4 1 1 1 2 0 1 4 2 6 5
0.345435    0.060410    0.803941  1.327625  3.443514  1.790279  5.752545  4.882096  
-2
0.060410    0.036964    0.875198  1.538167  3.679010  1.764079  5.445792  4.697755  
-2
0.036964    0.005072    0.904583  1.953592  3.960191  1.208831  5.135226  4.837577  
-2
0.005072    0.000028    1.024587  2.010668  3.890058  1.081283  5.085355  4.908049  
-2
0.000028    0.000004    1.026403  2.004281  3.885207  1.091849  5.088391  4.903870  
-2
0.000004    0.000000    1.024764  2.000974  3.883102  1.092396  5.092134  4.906630  
-2
0.000000    0.000000    1.023964  2.000761  3.883587  1.092722  5.092449  4.906517  
-2
0.000000    0.000000    1.023916  2.000531  3.883561  1.093300  5.092522  4.906169  
-2
0.000000    0.000000    1.023903  2.000388  3.883441  1.093368  5.092656  4.906244  
-2
0.000000    0.000000    1.023845  2.000361  3.883459  1.093377  5.092696  4.906262  
-2
0.000000    0.000000    1.023837  2.000355  3.883466  1.093401  5.092697  4.906244  
-2
0.000000    0.000000    1.023837  2.000347  3.883460  1.093407  5.092703  4.906245  


		
		


;;--------------------------  testing dont-care  ----------------------------

;; 2 right triangles
;; 
;;
;;         a
;;
;;    c    b    d
;;



(setq 345hi2
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 5)
     (0 0 4 4)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2 t nil 2)  ;; no dont-cares   

(setq 345hi2-dc
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 5)
     (0 0 4 dont-care)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2-dc t nil 2)  ;;should be able to reconstruct perfectly

(setq 345hi2-dc-ambiguous
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 dont-care)
     (0 0 4 dont-care)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2-dc-ambiguous t nil 2)  ;;should be ambiguous for d
                                                            ;; d is anywhere so long as it is 8 from a


;;------------------------------  test moveable-coordss  -------------------------------

(setq foo '((-0.004871674691109583d0 -6.479839855164267d-5)
	    (0.1093374876421643d0 0.10278447688922916d0)
	    (0.19089994365858262d0 0.20264462795891888d0)
	    (0.3002073287032161d0 0.29119595821713684d0)
	    (0.39293014616281463d0 0.4043028841169162d0)
	    (0.5038059937103181d0 0.5036315643366671d0)
	    (0.5975844620655193d0 0.6093825359939885d0)
	    (0.708180462797555d0 0.6925928025231204d0)
	    (0.8076721927041602d0 0.7924457321052585d0)
	    (0.8928366793096009d0 0.9022692391740021d0)))


(show-coordss
 (setq diagonal-foo 
  (loop for (x y) in foo 
      for i from 0 by 0.1 collect
	(list x (- y i)))))

(make-strain-selection-window 
 (f-hi-table (^ (x)
		(if (dont-care-p x) 
		    x
		  (if (> x 0.35) 'dont-care x)))
	     (put-hi-table-into-range-0-1 
	      (coordss-to-hi-table foo)))
 t
 nil
 2
 'name-based
 (^ ()
    (append (list (nth 0 foo))
	    (loop for i from 1 below (dec (length foo)) collect
		  ;; (random-coords-in-bounding-box (bounding-box foo)))
		  ;; (random-coords-in-bounding-box '((0 0) (1 1))))
		  (random-coords-in-bounding-box (bounding-box foo 2)))	          
	    (last foo)))
 (append (list 4)
	 (loop for coords in (cddr foo) collect 2)
	 (list 4))
 (loop for i below (length foo) collect 10)
 t
 (series 1 (- (length foo) 2))
 nil)

(disable-basis-vector-point-indices)   ;; can do this with a right click on the "01", the basis vectors display.


;;-------------------------------------  large array -----------------------------------------

(setq nl-pro (fi-in-hi-table "~/mds/data/all-seq/latest-version/nl-pro.lisp"))
(make-master-mds-window (extract-hi-table-window nl-pro 0 100) :show-hi-table nil)

(remote-mds "pp" nl-pro 10 2 1000000 200 "/tmp/nl-pro-runs")
(show-remote-runs-in-2d-tk "/tmp/nl-pro-runs" (hi-table-antigens nl-pro))
|#


