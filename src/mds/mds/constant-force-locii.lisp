(in-package user)

;;;----------------------------------------------------------------------
;;;                      NOTES
;;;----------------------------------------------------------------------

;; right now only works for hi-metric  (the stress-component-given-point was snarfed from there)
;; and has the Yi Zi stuff.

;; only works for 2d data -- the restrictions are in the rectangular to polar, don't know how


;;;----------------------------------------------------------------------
;;;                      calculating spoke data
;;;----------------------------------------------------------------------

(defun offset-coords (coords offsets)
  (mapcar #'+ coords offsets))

(defun offset-coords-2d (coords r theta)
  (mapcar #'+ coords (polar-to-rectangular r theta)))

(defun offset-ith-coords-in-coordss-2d (i coordss r theta &optional &key additional-coords-offsets)
  (replace-nth i (offset-coords-2d 
		  (if additional-coords-offsets
		      (offset-coords (nth i coordss) additional-coords-offsets)
		    (nth i coordss))
		  r theta) 
	       coordss))

(defun absolute-to-delta-stresses (baseline stresses)
  (f-lists
   (^ (x) (- x baseline))
   stresses))


(defun stress-component-given-point (r target-dist-matrix p num_points num_dims)
  ;; this fuction was snarfed from metric-mds-global-norm-conjugant-gradient-hi-metric
  ;; and changed to only calc for the changed points.  we loop only along the row
  ;; but all the way along, and we had to add the >= in the Yi and Zi
  (setq r (inc r))  ;; because we want to be 0 based, but p is 1 based
  (let ((numerator 0.0d0))
    (loop for s from 1 to NUM_POINTS do   ;; note, we have to loop s from 1 now, not from r+1
	  (let* ((dtarget-raw (aref target-dist-matrix r s))
		 (thresholded-value (listp dtarget-raw))
		 (dtarget (if thresholded-value (dec (nth 1 dtarget-raw)) dtarget-raw)))
	    (if (true-dont-care-p dtarget)
		'do-nothing
	      (let* ((dmds (d-from-array p r s NUM_DIMS))
		     (dtarget-dmds (- (- (Yi-from-array p (if (>= s r) s r) NUM_POINTS NUM_DIMS)   
					 (+ dtarget (Zi-from-array p (if (>= s r) r s) NUM_POINTS NUM_DIMS))) 
				      dmds)))
		(setq numerator 
		  (+ numerator 
		     (if (not thresholded-value)
			 (square dtarget-dmds)
		       (* (square dtarget-dmds)
			  (squash (* *beta* dtarget-dmds))))))))))
    numerator))



(defun point-offset-delta-stresses (point-index target-dist-matrix coordss &optional &key 
										     (max-radius 10.00001)
										     additional-coords-offsets
										     (additional-stress 0)
										     (data-step 0.1))
  (let* ((num-points (length (butlast coordss)))
	 (num-dims (length (car coordss)))
	 (point-offset-stress-components
	  (loop for degrees from 0 to 350 by 10 collect
		(loop for r from 0 to max-radius by data-step collect
		      (coerce
		       (stress-component-given-point
			point-index
			target-dist-matrix
			(coordss-to-array
			 (offset-ith-coords-in-coordss-2d
			  point-index
			  coordss
			  r
			  (degrees-to-radians degrees)
			  :additional-coords-offsets additional-coords-offsets))
			num-points
			num-dims)
		       'single-float))))
	 (point-offset-delta-stresses
	  (absolute-to-delta-stresses 
	   (- (nth 0 (nth 0 point-offset-stress-components)) additional-stress)
	   point-offset-stress-components)))
    point-offset-delta-stresses))


;;;----------------------------------------------------------------------
;;;                      calculating locii
;;;----------------------------------------------------------------------

#|
supeseded by below
(defun constant-stress-locus (data &optional &key (stress-delta 1) (data-step 0.1))
  (loop for data-for-angle in data collect
	(multiple-value-bind (value residue position)
	    (find-closest stress-delta data-for-angle)
	  value 
	  residue
	  (* position data-step))))
|#

(defun interpolate-x-position-from-y-in-ys (target-y ys)
  (loop for this-x from 0 
      for (this-y next-y) on ys
      until (null next-y)
      when (<= target-y next-y)
      do (return (values (coerce (inverse-linear-interpolation target-y this-x this-y (inc this-x) next-y) 'single-float)
			 this-x
			 this-y next-y
			 ))
      finally (return (values ;; case when moving the point along this radius always decreases stress (upto our limit of data)
		       0
		       0 1))))

(defun constant-stress-locus (data &optional &key (stress-delta 1) (data-step 0.1))
  ;; the data-step is the number that was put into point-offset-delta-stresses
  (loop for data-for-angle in data collect
	(let ((radius (interpolate-x-position-from-y-in-ys stress-delta data-for-angle)))
	  (* radius data-step))))



;;;----------------------------------------------------------------------
;;;                         calc raw data
;;;----------------------------------------------------------------------

(defun calc-constant-stress-radial-data (table coordss &optional &key 
								 (data-step 0.1)
								 (point-index-from 0)
								 (point-index-below (hi-table-length table)))
  (let ((target-dist-matrix (hi-table-values-to-base-1-symmetric-array-with-threshold-info (hi-table-values table))))
    (loop for i from point-index-from below (min point-index-below (hi-table-length table))
	for strain in (nthcdr point-index-from (hi-table-antigens table)) collect
	  (list
	    (print (list i strain))
	    (time
	     (point-offset-delta-stresses i target-dist-matrix coordss :data-step data-step))))))
  
(defun calc-constant-stress-radial-data-from-save-aux (save &optional &key 
								      (data-step 0.1)
								      (point-index-from 0)
								      (point-index-below (hi-table-length (table-from-save save))))
  (calc-constant-stress-radial-data 
   (table-from-save save)
   (starting-coordss-from-save save)
   :data-step data-step
   :point-index-from  point-index-from
   :point-index-below point-index-below))

(defun calc-constant-stress-radial-data-from-save-split-batch-mode (save &optional &key 
										   (data-step 0.1)
										   (num-points-per-batch-run 10)
										   gridware)
  ;; very close to multiple-optima-check-by-single-point-randomize-multiple-machines
  ;; both operate from saves, so that makes things easier
  ;; only differences are marked with "<<<<<<<<< difference"
  ;; at some time i should abstract this common functionality, best then to also cons up the args so does not need to be a save
  (let* ((random-id (progn (seed-random -1 7) (krandom 467739585 7)))
	 (local-scratch-save-filename (format nil "mds/cl/batch-scratch/~a.save" random-id))
	 (remote-scratch-save-filename (format nil "mds/cl/batch-scratch/~a.save" random-id)))
    (write-save-form save local-scratch-save-filename)
    (if gridware (run-shell-command (format nil "scp -C ~a sfi:~a" local-scratch-save-filename remote-scratch-save-filename)))
    (cons 'list
	  (loop for i below (hi-table-length (table-from-save save)) by num-points-per-batch-run collect
		(batch-lisp `(calc-constant-stress-radial-data-from-save-aux      ;; <<<<<<<<<<< difference in function name
			      (fi-in ,remote-scratch-save-filename)
			      :point-index-from  ,i
			      :point-index-below ,(+ i num-points-per-batch-run)
			      :data-step ,data-step)                              ;; <<<<<<<<<< difference in remaining keyword args passed
			    :gridware gridware
			    )))))

(defun calc-constant-stress-radial-data-from-save (save &optional &key 
								  (data-step 0.1)
								  run-in-split-batch-mode
								  (num-points-per-batch-run 10)
								  gridware)
  (if run-in-split-batch-mode
      (calc-constant-stress-radial-data-from-save-split-batch-mode
       save
       :num-points-per-batch-run num-points-per-batch-run
       :gridware gridware)
    (calc-constant-stress-radial-data-from-save-aux
     save
     :data-step data-step)))
    

;;;----------------------------------------------------------------------
;;;                      calc blob shapes
;;;----------------------------------------------------------------------

(defun calc-constant-stress-shapes (constant-stress-radial-data &optional &key (stress-delta 1.0) (data-step 0.1))
  (loop for ((index name) data) in constant-stress-radial-data collect
	(progn
	  index  ;; not used
	  name   ;; not used
	  (list 'polygon-scaled 
		(loop for degrees from 0 to 350 by 10 
		    for r in (constant-stress-locus data :stress-delta stress-delta :data-step data-step) collect
		      (polar-to-rectangular r (degrees-to-radians degrees)))))))

;; same as above, but just want for one point
(defun calc-constant-stress-shape-from-index (index constant-stress-radial-data &optional &key (stress-delta 1.0) (data-step 0.1))
  (let ((data (nth 1 (nth index constant-stress-radial-data))))
    (loop for degrees from 0 to 350 by 10 
	for r in (constant-stress-locus data :stress-delta stress-delta :data-step data-step) collect
	  (polar-to-rectangular r (degrees-to-radians degrees)))))

;; same as above, but just want for one point by passing in the points constant-stress-radial-datum
(defun calc-constant-stress-shape (constant-stress-radial-datum &optional &key (stress-delta 1.0) (data-step 0.1))
  (let ((data constant-stress-radial-datum))
    (loop for degrees from 0 to 350 by 10 
	for r in (constant-stress-locus data :stress-delta stress-delta :data-step data-step) collect
	  (polar-to-rectangular r (degrees-to-radians degrees)))))


;;;----------------------------------------------------------------------
;;;                    integrate wit the gui
;;;----------------------------------------------------------------------

(defun mds-window-set-constant-stress-radial-data (mds-window)
  (set-constant-stress-radial-data
   (get-table-window-for-mds-window mds-window)   ;; note we are putting this on the table-window, when really it is an mds-window item
   (calc-constant-stress-radial-data-from-save    ;;   i'm doing this temporaily as we are launching a new window to change point item shapes
    (make-save-form
     :hi-table         (get-hi-table-working-copy (get-table-window-for-mds-window mds-window))
     :starting-coordss (get-mds-coordss mds-window)))))

(defun mds-window-set-constant-stress-shapes (mds-window &optional &key (stress-delta 1.0) (data-step 0.1))
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-coords-shapes
     table-window  ;; like above we're setting in table-window where really is an mds-window item, but because we are launching a new window
     (calc-constant-stress-shapes (get-constant-stress-radial-data table-window) :stress-delta stress-delta :data-step data-step))
    (hillclimb-from-mds-window mds-window t 0)))

(defun mds-window-set-constant-stress-shapes-color-graded (mds-window &key (stress-delta 1.0))
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-coords-shapes
     table-window  ;; like above we're setting in table-window where really is an mds-window item, but because we are launching a new window
     (loop for i below (hi-table-width (get-hi-table table-window)) collect (list 'color-graded-polygon-scaled stress-delta)))
    (hillclimb-from-mds-window mds-window t 0)))
    

;;;----------------------------------------------------------------------
;;;                  operate directly on saves
;;;----------------------------------------------------------------------

(defun set-constant-stress-radial-data-to-save-and-remove-batch-runs (save constant-stress-radial-data &optional &key (not-found-action :error))
  (set-save-keyword-entry
   (set-batch-runs-in-save save nil)     ;; we remove batch runs, because this data is just for one set of coordss -- a bit ugly
   :constant-stress-radial-data          ;;    the real solution is to be able to attach things to mds-windows as part of a save
   constant-stress-radial-data
   :not-found-action not-found-action))
  
(defun calc-and-set-constant-stress-radial-data-to-save-and-remove-batch-runs (save &optional &key 
											      (not-found-action :error)
											      (data-step 0.1)
											      ;; does not make sense to split here w/o more coding
											      ;; because have to leave a future of the calc
											      ;; like we do for batch-mds-runs
											      ;; maybe we can do that later
											      ;; run-in-split-batch-mode
											      ;; gridware
											      ;; (num-points-per-batch-run 10)
											      )
  (set-save-keyword-entry
   (set-batch-runs-in-save save nil)     ;; we remove batch runs, because this data is just for one set of coordss -- a bit ugly
   :constant-stress-radial-data          ;;    the real solution is to be able to attach things to mds-windows as part of a save
   (calc-constant-stress-radial-data-from-save
    save
    :data-step data-step)
   :not-found-action not-found-action))

(defun set-coords-shapes-from-constant-stress-radial-data-in-save (save-with-constant-stress-radial-data
								   &optional &key (stress-delta 1.0) (data-step 0.1))
  (set-save-keyword-entry
   save-with-constant-stress-radial-data
   :coords-shapes (calc-constant-stress-shapes 
		   (get-save-keyword-entry save-with-constant-stress-radial-data :constant-stress-radial-data)
		   :stress-delta stress-delta :data-step data-step)
   :not-found-action :add))


#|
TODO:
  gui checking we have the radial data calced
  gui indicatation of progress
  saving both the raw data, and the coords-shapes, the issue of being associated with the table-window or an mds-window

;; -------------------on the vaccine table ----------------
(setq vac-save (fi-in "mds/investigations/merge-hi-tables/vac-table.save"))
(setq vac-save-annotated (add-constant-stress-radial-data-to-save-and-remove-batch-runs vac-save))
(setq vac-save-shapes (set-coords-shapes-from-constant-stress-radial-data-in-save vac-save-annotated :stress-delta 0.5))
(eval vac-save-shapes)

;; ------------------ and on seq-t9a ----------------------
(setq seq-t9a (fi-in "mds/investigations/merge-hi-tables/seq-t9a.save"))
(calc-constant-stress-radial-data-from-save seq-t9a :run-in-split-batch-mode t :num-points-per-batch-run 50)  ;; approx 1 hour for all of t9a
(fi * "mds/investigations/merge-hi-tables/seq-t9a-constant-force-locii-raw-data.lisp")

(setq seq-t9a (fi-in "mds/investigations/merge-hi-tables/seq-t9a.save"))
(setq seq-t9a-constant-force-locii-raw-data (fi-in "mds/investigations/merge-hi-tables/seq-t9a-constant-force-locii-raw-data.lisp"))
(setq seq-t9a-with-blob-raw-data
  (set-constant-stress-radial-data-to-save-and-remove-batch-runs 
   seq-t9a
   seq-t9a-constant-force-locii-raw-data))
(write-save-form
 seq-t9a-with-blob-raw-data
 "mds/investigations/merge-hi-tables/seq-t9a-with-blob-raw-data.save")



|#



;; ------------------------------------- now for mutliple optima -----------------------------------------

;;;----------------------------------------------------------------------
;;;                 utils for finding min and max
;;;----------------------------------------------------------------------

(defun find-next-forward-max-from-min (l)
  (loop for i from 0 
      for (this-stress next-stress . rest) on l 
      until (null rest)
      when (< next-stress this-stress)
      do (return (values (cons this-stress (cons next-stress rest))
			 i 
			 this-stress
			 ))))

(defun find-next-forward-min-from-max (l)
  (loop for i from 0 
      for (this-stress next-stress . rest) on l 
      until (null rest)
      when (> next-stress this-stress)
      do (return (values (cons this-stress (cons next-stress rest))
			 i 
			 this-stress))))

(defun find-next-forward-max-from-max (l)
  (loop for i from 0 
      for (this-stress next-stress . rest) on (find-next-forward-min-from-max l)
      until (null rest)
      when (< next-stress this-stress)
      do (return (values (cons this-stress (cons next-stress rest))
			 (position this-stress l)  ;; could add in the i we get from finding the max, but this is easier
			 this-stress
			 ))))

(defun find-next-forward-min-from-min (l)
  (loop for i from 0 
      for (this-stress next-stress . rest) on (find-next-forward-max-from-min l)
      until (null rest)
      when (> next-stress this-stress)
      do (return (values (cons this-stress (cons next-stress rest))
			 (position this-stress l)  ;; could add in the i we get from finding the max, but this is easier
			 this-stress))))


;;;----------------------------------------------------------------------
;;;                   calculating alternate minima
;;;----------------------------------------------------------------------

(defun spoke-mins (stress-deltas-on-spoke &optional &key 
						    (position-offset 0)
						    stress-delta-reporting-threshold
						    distance-reporting-threshold)
  (multiple-value-bind (rest position value)
      (find-next-forward-min-from-min stress-deltas-on-spoke)
    (if (null rest)
	nil
      (append (if (or (and stress-delta-reporting-threshold (>= value stress-delta-reporting-threshold))
		      (and distance-reporting-threshold     (<= position distance-reporting-threshold)))
		  nil
		(list  (list (+ position position-offset) (coerce value 'single-float))))
	      (spoke-mins rest 
			  :position-offset (+ position position-offset) 
			  :stress-delta-reporting-threshold stress-delta-reporting-threshold
			  :distance-reporting-threshold distance-reporting-threshold
			  )))))

(defun contiguous-spoke-minima (mins-on-spokes)
  (setq mins-on-spokes
    (loop for mins-on-spoke in mins-on-spokes collect
	  (if (> (length mins-on-spoke) 1)
	      (error "more than on min on this spoke, enhance the code to deal with it.")
	    (car mins-on-spoke))))
  (setq mins-on-spokes
    (loop for mins-on-spoke in mins-on-spokes for spoke-index from 0 collect
	  (if (null mins-on-spoke)
	      nil
	    (cons spoke-index mins-on-spoke))))
  (if (not (member nil mins-on-spokes))
      (list mins-on-spokes)
    (let* ((first-null-position (position nil mins-on-spokes))
	   ;; to deal with the wrap around boundary condition
	   (rotated-mins-on-spokes (rotate-list-n-times first-null-position mins-on-spokes)))
      (let (all-contiguous
	    this-contiguous)
	(loop for spoke-min in rotated-mins-on-spokes do
	      (if (null spoke-min)
		  (if this-contiguous
		      (progn
			(push-end this-contiguous all-contiguous)
			(setq this-contiguous nil)))
		(push-end spoke-min this-contiguous)))
	(if this-contiguous (push-end this-contiguous all-contiguous))
	all-contiguous))))

(defun minima-across-spokes (spoke-minima)
  ;; check no gaps are too wide
  (loop for spoke-minimum in spoke-minima collect
	(let* ((spoke-indices (nths 1 spoke-minimum))
	       (spoke-indices-gaps (loop for (a b) on spoke-indices until (null b) collect (abs (- b a)))))
	  (if spoke-indices-gaps
	      (if (> (apply #'max spoke-indices-gaps) 10)
		  (cerror "continue" "distance >1 of min from spoke to spoke, enhance the code to take care of multiple minima, if that is what these are")))))
  ;; find the min
  (loop for spoke-minimum in spoke-minima collect
	(let ((values-for-same-minimum-on-different-spokes (nths 2 spoke-minimum)))
	  (nth (position (apply #'min values-for-same-minimum-on-different-spokes) (nths 2 spoke-minimum)) spoke-minimum))))



#|
(setq save (fi-in "mds/investigations/merge-hi-tables/seq-t9a.save"))
(setq coordss (starting-coordss-from-save save))
(setq table (table-from-save save))
(setq target-dist-matrix (hi-table-values-to-base-1-symmetric-array-with-threshold-info (hi-table-values table)))
(setq annotated-datas (fi-in "mds/investigations/merge-hi-tables/seq-t9a-constant-force-locii-raw-data.lisp") ;; calculated above, for blob plots

(setq alternate-minima
  (loop for ((index name) strain-data) in annotated-datas append
	(let ((strain-results (loop for spoke-index from 0 
				  for spoke-data in strain-data collect
				    (spoke-mins spoke-data 
						:stress-delta-reporting-threshold 1.0  ;; <<<<<<<<< this 1.0 here must match the 1.0 below
						:distance-reporting-threshold 2))))
	  (if (equal '((nil 36)) (hist strain-results))
	      nil
	    (list (list (list index name) 
			(minima-across-spokes
			 (contiguous-spoke-minima strain-results))))))))

(100 AS/1/91-AG)     ((3 10 -0.53121185))
(213 HK/2/94-AG)     ((23 18 -0.05267334))
(224 GE/9509/95-AG)  ((27 61 -0.042974472))
(235 NL/47/95-AG)    ((16 7 0.69607544) (34 14 0.79981613))
(256 NE/491/97-AG)   ((33 41 -1.5223958))
(269 NL/126/01-AG)   ((9 16 0.9948474))
(290 SH/31/80-SR)    ((3 15 0.038695484))

(setq alternate-minima-data 
  (loop for (name alternate-minima) in alternate-minima collect
	(list name
	      (loop for (spoke distance value) in alternate-minima collect
		    (point-offset-delta-stresses
		     (print (nth 0 name))
		     target-dist-matrix coordss 
		     :additional-coords-offsets (offset-coords-2d '(0 0) (/ distance 10) (degrees-to-radians (* spoke 10)))
		     :additional-stress value)))))

(setq alternate-minima-data-base
  (loop for (name alternate-minima) in alternate-minima collect
	(list name
	      (loop for (spoke distance value) in alternate-minima collect
		    (point-offset-delta-stresses
		     (print (nth 0 name))
		     target-dist-matrix coordss)))))

(setq minima-full
  (loop for (name head) in alternate-minima-data-base
      for (name2 rest) in alternate-minima-data collect
	(progn
	  (if (not (equal name name2))
	      (error ""))
	  (list name
		(append head rest)))))


(setq alternate-minima-coords
  (loop for ((index name) minima) in alternate-minima collect
	(list (list index name)
	      (cons '(0 0)
		    (loop for (spoke distance value) in minima collect
			  (offset-coords-2d '(0 0) ;; (nth index seq-t4-coordss) 
					    (/ distance 10)
					    (degrees-to-radians (* spoke 10))))))))

(eval 
 (setq save-coords-shapes-with-alts-10
   (blank-save
    (set-save-keyword-entry
     save
     :coords-shapes (let ((coords-shapes-with-alts-1-alts-only-indices
			   (loop for ((index name) datas) in minima-full collect
				 index))
			  (multiple-optima-processed-data
			   (loop for ((index name) datas) in minima-full
			       for ((xindex xname) coordss) in alternate-minima-coords collect
				 (list 'polygon-scaled 
				       (let ((all-optima-coordss
					      (loop for data in datas for coords in coordss append
						    (let ((this-optima-coordss
							   (loop for degrees from 0 to 350 by 10 
							       for r in (constant-stress-locus data :stress-delta 1.0) collect  ;; <<<<< 1.0 here
								 (offset-coords                                           ;; must match above
								  (polar-to-rectangular r (degrees-to-radians degrees))
								  coords))))
						      (append this-optima-coordss
							      (list (car this-optima-coordss)))))))
					 (append all-optima-coordss
						 (list (car all-optima-coordss))))))))
		      (loop for i below (hi-table-length table) collect
			    (if (member i coords-shapes-with-alts-1-alts-only-indices)
				(nth (position i coords-shapes-with-alts-1-alts-only-indices) 
				     multiple-optima-processed-data)
			      'circle)))
     :not-found-action :add))))
|#