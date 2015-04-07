(in-package user)

;;;----------------------------------------------------------------------
;;;                      batch (ie no UI) runs
;;;----------------------------------------------------------------------

(defun one-batch-mds (hi-table 
		      &key (starting-coordss 2)
			   (moveable-coords 'all)
			   unmoveable-coords
			   (num-climbs 10000)
			   (num-trials-per-climb 10000)
			   (mds-f
			    (if (similarity-table-p hi-table)
				#'metric-mds-global-norm-conjugant-gradient-hi-metric
			      #'metric-mds-global-norm-conjugant-gradient))
			   adjustable-rows
			   adjustable-columns)
  (batch-mds
   hi-table
   starting-coordss
   num-climbs
   num-trials-per-climb
   mds-f
   moveable-coords
   unmoveable-coords
   adjustable-rows
   adjustable-columns))

  
(defun batch-mds-keyword-args (hi-table
			       &key starting-coordss
				    (num-climbs 10000)
				    (num-trials-per-climb 10000)
				    (mds-f
				     (if (similarity-table-p hi-table)
					 #'metric-mds-global-norm-conjugant-gradient-hi-metric
				       #'metric-mds-global-norm-conjugant-gradient))
				    (moveable-coords 'all)
				    unmoveable-coords
				    adjustable-rows
				    adjustable-columns
				    unmoveable-dimensions
				    unmoveable-dimensions-in-second-phase
				    dim-anneal-f
				    dim-anneal-coefficients
				    dim-anneal-starting-dimension
				    dim-anneal-ending-dimension
				    num-runs)  ;; hack, for batching up to sfi 20040913

  ;; set the default like this, so we can call without starting-coordss and default to the dim-anneal-starting-dimension
  (if (not starting-coordss)
      (if dim-anneal-starting-dimension
	  (setq starting-coordss dim-anneal-starting-dimension)
	2))

  (if dim-anneal-f
      (apply dim-anneal-f
	     hi-table
	     :starting-coordss starting-coordss
	     :mds-f mds-f
	     :moveable-coords moveable-coords
	     :unmoveable-coords unmoveable-coords
	     :adjustable-rows adjustable-rows
	     :adjustable-columns adjustable-columns
	     :unmoveable-dimensions unmoveable-dimensions
	     :unmoveable-dimensions-in-second-phase unmoveable-dimensions-in-second-phase
	     :dim-anneal-starting-dimension dim-anneal-starting-dimension
	     :dim-anneal-ending-dimension dim-anneal-ending-dimension
	     (if num-runs
		 (list :num-runs num-runs)))
    (batch-mds
     hi-table
     starting-coordss
     num-climbs
     num-trials-per-climb
     mds-f
     moveable-coords
     unmoveable-coords
     adjustable-rows
     adjustable-columns
     unmoveable-dimensions
     unmoveable-dimensions-in-second-phase
     dim-anneal-coefficients)))
   
(defun batch-mds-from-save (save &rest args)
  (apply #'batch-mds-keyword-args
	 (table-from-save save)
	 (append
	  args
	  (loop for keyword in '(:starting-coordss
				 ;;:batch-runs
				 ;;:mds-dimensions
				 :moveable-coords
				 :unmoveable-coords
				 :adjustable-rows
				 :adjustable-columns
				 :unmoveable-dimensions
				 :unmoveable-dimensions-in-second-phase				 
				 :dim-anneal-coefficients)
	      when (get-save-keyword-entry save keyword :not-found-action :return-nil)
	      append (list keyword
			   (get-save-keyword-entry save keyword :not-found-action :return-nil))))))

(defun first-or-more-batch-mds-runs-from-save-return-save (num-runs save &rest args)
  (let ((new-batch-runs (sort-batch-runs (loop for i below num-runs collect (progn (format t "~%Run ~d (starting at ~a)" i (time-and-date)) (apply #'batch-mds-from-save save args))))))
    (format t "~%End time ~a" (time-and-date))
    (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
     save
     (sort-batch-runs
      (append 
       new-batch-runs
       (batch-runs-from-save save)))
     :not-found-action :add)))


(defun first-or-more-batch-mds-runs-from-save-return-save-iterate (num-runs save &rest args)
  ;; useful to get around optimizer problem when optimizer stops too soon
  ;; this function eqivalent to pressing the run button in the GUI num-run times
  (if (zerop num-runs)
      save
    (apply #'first-or-more-batch-mds-runs-from-save-return-save-iterate
           (dec num-runs)
           (apply #'first-or-more-batch-mds-runs-from-save-return-save
                  1
                  save
                  args)
           args)))


(defun relax-save (save &rest args)
  (apply #'first-or-more-batch-mds-runs-from-save-return-save
         1
         (apply #'first-or-more-batch-mds-runs-from-save-return-save
                1
                save
                args)
   args))
    


(defun mds-table (table &optional &key
				  (num-dimensions 2)
				  dim-anneal-starting-dimension
				  (num-runs 1))
  (apply #'first-or-more-batch-mds-runs-from-save-return-save 
	 num-runs
	 (make-save-form :hi-table table)
	 :starting-coordss num-dimensions
	 (if dim-anneal-starting-dimension
	     (list :dim-anneal-f #'lisp-dim-anneal-two-phase
		   :dim-anneal-ending-dimension num-dimensions
		   :dim-anneal-starting-dimension dim-anneal-starting-dimension))))


(defun batch-mds (hi-table 
		  &optional (starting-coordss 2)
			    (num-climbs 10000)
			    (num-trials-per-climb 10000)
			    (mds-f
			     (if (similarity-table-p hi-table)
				 #'metric-mds-global-norm-conjugant-gradient-hi-metric
			       #'metric-mds-global-norm-conjugant-gradient))
			    (moveable-coords 'all)
			    unmoveable-coords
			    adjustable-rows
			    adjustable-columns
			    unmoveable-dimensions
			    unmoveable-dimensions-in-second-phase
			    dim-anneal-coefficients)
  (multiple-value-bind (coords
			tk
			fitness
			termination-reason
			fitness-history)
      (run-mds-from-dists
        mds-f
        #'square
        nil
        (hi-table-values-to-base-1-symmetric-array-with-threshold-info (hi-table-values hi-table))
	(cond ((or (functionp starting-coordss)
		   (and (listp starting-coordss) (eq 'lambda (car starting-coordss))))
	       (funcall starting-coordss))
	      ((listp starting-coordss)   ;; later do the same checking that is done in tk-interface
	       starting-coordss)
	      ((integerp starting-coordss)
	       (make-random-coordss 
		(hi-table-length hi-table)
		starting-coordss
		(hi-table-max-value hi-table)
		(or (ag-sr-table-p hi-table)
		    (similarity-table-p hi-table))
		hi-table))
	      (t (error "unexpected starting-coords, expected function, coordss, or num-dimensions, but got ~a"
			starting-coordss)))
	:moveable-coords moveable-coords
	:unmoveable-coords unmoveable-coords
	:adjustable-rows adjustable-rows
	:adjustable-columns adjustable-columns
	:unmoveable-dimensions unmoveable-dimensions
	:unmoveable-dimensions-in-second-phase unmoveable-dimensions-in-second-phase
	:dim-anneal-coefficients dim-anneal-coefficients
        :dribble nil
        :record-fitness-history nil
        :hillclimbs-args (list :max-trials num-trials-per-climb
			       :max-climbs num-climbs
			       ;;:incremental-dribble-modulus incremental-dribble-modulus
			       ))
    tk
    (list coords fitness termination-reason fitness-history)))


;;;----------------------------------------------------------------------
;;;                      multiple runs
;;;----------------------------------------------------------------------

(defun multiple-batch-mds (times ;; this is the only difference to function batch-runs
			    hi-table 
			    &optional (starting-coordss 2)
				      (num-climbs 10000)
				      (num-trials-per-climb 10000)
				      (mds-f
				       (if (ag-sr-table-p hi-table)
					   #'metric-mds-global-norm-conjugant-gradient-hi-metric
					 #'metric-mds-global-norm-conjugant-gradient)))
				      ;; should add moveable, disconnected and adjustable-rows and adjustable cols
  (loop for i below times collect
	(batch-mds hi-table starting-coordss num-climbs num-trials-per-climb mds-f)))

(defun stresses (multiple-batch-mds-results)
  (nths 1 (sort-batch-runs multiple-batch-mds-results)))

(defun run-best-of-multiple-batch-mds (hi-table multiple-batch-mds-results)
  (make-master-mds-window 
   hi-table
   :starting-coordss (nth 0 (car (sort-batch-runs multiple-batch-mds-results)))))


;; note, there is also show function (not running, just display) in remote.lisp


;;;----------------------------------------------------------------------
;;;                     BATCH RUNS FROM THE GUI
;;;----------------------------------------------------------------------

(defun make-batch-mds-window (table-window)
  (let ((batch-runs-ui-window (tk-open)))
    (set-batch-runs-batch-runs-ui-window table-window batch-runs-ui-window)
    (tk-put batch-runs-ui-window "set bitmapDir ~s" (uw-sfnr "bitmaps" :assertIsDir t))
    (tk-put batch-runs-ui-window "source ~s" (uw-sfnr "mds/batch-runs-ui.tk" :assertIsFile t))
    (let ((previous-batch-runs (get-batch-runs-data table-window)))
      (if previous-batch-runs
	  (initialize-batch-runs-ui batch-runs-ui-window previous-batch-runs
				    :comment "Initialized with previous runs")))
    batch-runs-ui-window))

(defun close-batch-runs-ui (batch-runs-ui-window)
  (set-batch-runs-batch-runs-ui-window 
   (get-table-window-for-batch-runs-ui-window batch-runs-ui-window)
   nil))

(defvar *stop-batch-runs-from-batch-runs-ui*)
(defun run-from-batch-runs-ui-window (batch-runs-ui-window &optional &key 
								     (num-runs 10)
								     c-dim-anneal-f
								     dim-anneal-f
								     (dim-anneal-starting-dimension 
                                                                      (let ((map-num-dims (get-mds-num-dimensions
                                                                                           (get-table-window-for-batch-runs-ui-window 
                                                                                            batch-runs-ui-window))))
                                                                        (if (numberp map-num-dims)
                                                                            (max 
                                                                             5
                                                                             (+ map-num-dims 2))
                                                                          5))))
  ;; run in background so we can do a stop
;; no, because then we get multiple running at the same time, and i don't know if they are ok like that
;;  (mp:process-run-function
;;   (format nil "background-batch-mds-hi-table-~a" (gensym))
;;   (^ () 
      (let* ((table-window (get-table-window-for-batch-runs-ui-window batch-runs-ui-window))
	     (starting-coordss (get-mds-original-starting-coordss table-window))
	     (num-non-annealed-dimensions (get-mds-num-dimensions table-window)))
	(setq *stop-batch-runs-from-batch-runs-ui* nil)
	(tk-put batch-runs-ui-window ".counter conf -text {Running 1 of ~d}" num-runs)
	(loop for i below num-runs do
	      (if *stop-batch-runs-from-batch-runs-ui*
		  (progn
		    (tk-put batch-runs-ui-window ".counter conf -text {Stopped}" num-runs)
		    (return 'stopped-by-ui))
		(update-batch-runs-ui
		 batch-runs-ui-window
		 (if c-dim-anneal-f
		     (lapedes-dim-anneal 
		      (get-hi-table-working-copy-with-sera table-window)
		      ;; NEED TO INTEGRATE DO THE FUNCTIONP TEST BELOW for dim anneal too
		      ;; and the unmoveable-dimensions too (as in below, but alan does not do that yet)
		      :starting-coordss (make-random-coordss (hi-table-length (get-hi-table table-window)) 
							     dim-anneal-starting-dimension
							     (hi-table-max-value (get-hi-table table-window))
							     (if (similarity-table-p (get-hi-table table-window))
								 (get-mds-coordss table-window)
							       nil))
		      :starting-dimension dim-anneal-starting-dimension
		      :final-dimension num-non-annealed-dimensions
		      :dim-anneal-f (eval (read-from-string (string-append "#'" (string c-dim-anneal-f))))
		      ;; the moveable and unmoveable we are not implementing -- does alan so c and C?
		      )
		   (batch-mds-keyword-args
		    (get-hi-table-working-copy-with-sera table-window)
		    :starting-coordss (if (functionp starting-coordss)
					  (funcall starting-coordss)   ;; NEED TO DO THIS FOR DIM ANNEAL
					(make-random-coordss (hi-table-length (get-hi-table table-window)) 
							     (if dim-anneal-f
								 dim-anneal-starting-dimension
							       (get-mds-num-dimensions table-window))
							     (hi-table-max-value (get-hi-table table-window))
							     (if (similarity-table-p (get-hi-table table-window))
								 (get-mds-coordss table-window)
							       nil)))
		    :moveable-coords (get-moveable-coords table-window)
		    :unmoveable-coords (get-unmoveable-coords table-window)
		    :unmoveable-dimensions (get-unmoveable-dimensions table-window)
		    :unmoveable-dimensions-in-second-phase (get-unmoveable-dimensions-in-second-phase table-window)
		    :adjustable-rows (get-adjustable-rows table-window) 
		    :adjustable-columns (get-adjustable-columns table-window)
		    :dim-anneal-f dim-anneal-f
		    :dim-anneal-starting-dimension dim-anneal-starting-dimension
		    :dim-anneal-ending-dimension   (get-mds-num-dimensions table-window)))
		 (inc i)
		 num-runs)))))
;;))

(defvar *batch-runs-sorted-data*)

(defun initialize-batch-runs-ui (batch-runs-ui-window batch-results &optional &key (comment "No comment"))
  (set-batch-runs-data (get-table-window-for-batch-runs-ui-window batch-runs-ui-window) batch-results)
  (setq *batch-runs-sorted-data* batch-results)
  (tk-put batch-runs-ui-window "removeAllEntries")
  (loop for stress in (nths 1 batch-results) for i from 0 do
  	(tk-put batch-runs-ui-window "addEntry ~d ~d" i stress))
  ;;(tk-put batch-runs-ui-window "addEntries ~d \"~{~a ~}\"" 0 (nths 1 batch-results))
  (tk-put batch-runs-ui-window ".counter conf -text {~a}" comment))

(defun reset-batch-runs-ui (batch-runs-ui-window)
  (initialize-batch-runs-ui batch-runs-ui-window nil :comment "List reset"))

(defun sort-batch-runs (batch-runs)
  (append
   (my-sort (collect (^ (batch-run) (numberp (nth 1 batch-run))) batch-runs) (^ (a b) (< (nth 1 a) (nth 1 b))))
   (filter (^ (batch-run) (numberp (nth 1 batch-run))) batch-runs)))

(defun update-batch-runs-ui (batch-runs-ui-window new-batch-result this-iteration max-iterations)
  (let* ((table-window (get-table-window-for-batch-runs-ui-window batch-runs-ui-window))
	 (new-batch-runs-sorted-data (sort-batch-runs
				      (cons new-batch-result
					    (get-batch-runs-data table-window)))))
    (set-batch-runs-data table-window new-batch-runs-sorted-data)
    (setq *batch-runs-sorted-data* new-batch-runs-sorted-data)  ;; hack
    (let ((new-stress (nth 1 new-batch-result)))
      (tk-put batch-runs-ui-window "addEntry ~d ~d" 
	      (position new-stress (nths 1 new-batch-runs-sorted-data))
	      new-stress))
    (if (= this-iteration max-iterations)
	(tk-put batch-runs-ui-window ".counter conf -text {Done}")
      (tk-put batch-runs-ui-window ".counter conf -text {Running ~d of ~d}" 
	      (inc this-iteration)
	      max-iterations))))

(defun mds-visualization-from-batch-runs-ui (batch-runs-ui-window stress)
  (mds-hi-table (get-table-window-for-batch-runs-ui-window batch-runs-ui-window)
		'metric-mds-global-norm-conjugant-gradient  ;; hardcode for now
		'square
		nil
		:existing-mds-window nil
		:new-window-canvas-coord-transformations 
		(get-canvas-coord-transformations (get-table-window-for-batch-runs-ui-window batch-runs-ui-window))
		:num-trials 0
		:num-climbs 0
		:incremental-dribble-modulus 1
		:starting-coordss
		(let ((batch-runs-data (get-batch-runs-data (get-table-window-for-batch-runs-ui-window batch-runs-ui-window))))
		  (nth 0 
		       (nth 
			;; not perfect because lisp stores a double, tk a single float
			(nth-value 2 (find-closest stress (stresses batch-runs-data)
						   :comparison-f (^ (a b) (if (and (numberp a) (numberp b))
									      (abs (- a b))
									    *large-double*))))
			batch-runs-data)))))


;;;----------------------------------------------------------------------
;;;                save configuration from batch ui
;;;----------------------------------------------------------------------

(defun save-configuration-from-batch-runs-ui-window (batch-runs-ui-window filename)
  (save-configuration (get-table-window-for-batch-runs-ui-window batch-runs-ui-window)
		      filename))



;;;----------------------------------------------------------------------
;;;   add strains to a save, randomize just those strains, and rerun
;;;----------------------------------------------------------------------

(defun average-coords-of-common-save-table-points (save table)
  (let ((common-points (my-intersection 
			(antigens-from-save save)
			(hi-table-antigens table))))
    (if (null common-points)
	(error "~2%No points in common between save and table~2%")
      (mapcar #'av (apply-transpose (multiple-coords-from-save save common-points))))))

(defun average-coords-of-save-points (save points)
  (if (null points)
      (error "~2%No points given~2%")
    (mapcar #'av (apply-transpose (multiple-coords-from-save save points)))))
    
  
(defun coords-from-antigen-or-coords-on-which-to-center-new-points (save 
								    table
								    antigen-or-coords-on-which-to-center-new-points)
  (cond ((and (not (null antigen-or-coords-on-which-to-center-new-points))
	      (atom antigen-or-coords-on-which-to-center-new-points))
	 (coords-from-save save antigen-or-coords-on-which-to-center-new-points))
	((and (listp antigen-or-coords-on-which-to-center-new-points)
	      (equal '(t) (remove-duplicates (mapcar #'numberp antigen-or-coords-on-which-to-center-new-points))))
	 antigen-or-coords-on-which-to-center-new-points)
	(t 
	 (average-coords-of-common-save-table-points save table))))

;; rename below, quick hack
(defun coords-from-antigen-or-coords-on-which-to-center-new-points-points (save 
									   points
									   antigen-or-coords-on-which-to-center-new-points)
  (cond ((and (not (null antigen-or-coords-on-which-to-center-new-points))
	      (atom antigen-or-coords-on-which-to-center-new-points))
	 (coords-from-save save antigen-or-coords-on-which-to-center-new-points))
	((and (listp antigen-or-coords-on-which-to-center-new-points)
	      (equal '(t) (remove-duplicates (mapcar #'numberp antigen-or-coords-on-which-to-center-new-points))))
	 antigen-or-coords-on-which-to-center-new-points)
	(t 
	 (average-coords-of-save-points save points))))

(defun addition-runs-randomizing-only-new-points (original-save 
						  new-table
						  merged-save
						  num-runs 
						  &optional &key 
							    antigen-or-coords-on-which-to-center-new-points
							    (scatter-distance 6)
							    (dim-anneal-f #'lisp-dim-anneal-two-phase)
							    (dim-anneal-starting-dimension 5)
							    (dim-anneal-ending-dimension (mds-dimensions-from-save original-save))
							    unmoveable-points-or-freeze-existing-points)
  ;; does not add additional runs, though from the name one would think it does
  (let* ((merged-save-antigens (antigens-from-save merged-save))
	 (merged-save-coordss  (coordss (starting-coordss-from-save merged-save)))
	 (new-antigens 
	  (reverse (set-difference (hi-table-antigens new-table) (hi-table-antigens (table-from-save original-save)))))
	 (center-for-new-points-scatter 
	  (coords-from-antigen-or-coords-on-which-to-center-new-points
	   original-save
	   new-table
	   antigen-or-coords-on-which-to-center-new-points))
	 (dim-anneal-pad (if dim-anneal-starting-dimension 
			     (zero-base (max 0 (- dim-anneal-starting-dimension dim-anneal-ending-dimension)))
			   nil))
	 (unmoveable-coords  ;; 1 based index into points
	  (if unmoveable-points-or-freeze-existing-points
	      (if (listp unmoveable-points-or-freeze-existing-points)
		  (error "no implemented freezing arbitary points yet--easy just not done")
		;; freeze existing points
		(let ((merged-save-strains (hi-table-antigens (table-from-save merged-save))))
		  (mapcar (^ (strain) (position strain merged-save-strains)) (hi-table-antigens (table-from-save original-save)))))
	    nil))
	 (batch-runs-results
	  (loop for i below num-runs collect
		(progn 
		  (print (list 'run i (time-and-date)))
		  (batch-mds-from-save
		   (set-starting-coordss-coordss-in-save 
		    merged-save
		    (loop for merged-save-antigen in merged-save-antigens
			for merged-save-coords in merged-save-coordss collect
			  (if (member merged-save-antigen new-antigens)
			      (append 
			       (uniform-perturbs center-for-new-points-scatter scatter-distance)
			       (uniform-perturbs dim-anneal-pad scatter-distance)) ;; might be nil
			    (append merged-save-coords
				    dim-anneal-pad))))   ;; might be nil
		   :dim-anneal-f dim-anneal-f
		   :dim-anneal-starting-dimension dim-anneal-starting-dimension
		   :dim-anneal-ending-dimension dim-anneal-ending-dimension
		   :unmoveable-coords unmoveable-coords
		   )))))
    (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
     merged-save
     (sort-batch-runs batch-runs-results)
     :not-found-action :add)))

(defun add-table-to-save-and-run-randomizing-only-new-points (original-save-or-filename
							      new-table-or-filename
							      num-runs 
							      &optional &key 
									antigen-or-coords-on-which-to-center-new-points ;; if null, then average of common points in save
									(scatter-distance 6)
									(new-points-color "black")
									(new-points-size  4)
									filename-to-write-save
									(merge-diagnostics-filename "/tmp/merge-diagnostics")
									(if-exists-action :error)
									(dim-anneal-f #'lisp-dim-anneal-two-phase)
									(dim-anneal-starting-dimension 5)
									dim-anneal-ending-dimension
									unmoveable-points-or-freeze-existing-points)
  (let* ((original-save
	  (if (stringp original-save-or-filename)
	      (fi-in original-save-or-filename)
	    original-save-or-filename))
	 (new-table
	  (if (stringp new-table-or-filename)
	      (read-hi-table-and-convert new-table-or-filename 1 0)
	    new-table-or-filename))
	 (coords-to-center-new-points 
	  (coords-from-antigen-or-coords-on-which-to-center-new-points
	   original-save
	   new-table
	   antigen-or-coords-on-which-to-center-new-points))
	 (merged-save 
	  (extend-save-form-with-table
	   original-save
	   new-table
	   :new-coords coords-to-center-new-points 
	   :new-points-color new-points-color
	   :new-points-size  new-points-size
	   :merge-diagnostics-filename merge-diagnostics-filename
	   :if-exists-action if-exists-action
	   ))
	 (merged-save-post-optimization
	  (addition-runs-randomizing-only-new-points   ;; does not add to existing runs
	   original-save
	   new-table
	   merged-save
	   num-runs
	   :unmoveable-points-or-freeze-existing-points unmoveable-points-or-freeze-existing-points
	   :antigen-or-coords-on-which-to-center-new-points coords-to-center-new-points 
	   :scatter-distance scatter-distance
	   :dim-anneal-f dim-anneal-f
	   :dim-anneal-starting-dimension dim-anneal-starting-dimension
	   :dim-anneal-ending-dimension (if dim-anneal-ending-dimension
					    dim-anneal-ending-dimension
					  (mds-dimensions-from-save original-save)))))
    (if filename-to-write-save
	(write-save-form
	 merged-save-post-optimization
	 filename-to-write-save))
    (print "Done")
    merged-save-post-optimization))


(defun add-table-from-save-to-save-and-run-randomizing-only-new-points (original-save-or-filename
									new-save-or-filename
									num-runs 
									&optional &key 
										  antigen-or-coords-on-which-to-center-new-points ;; if null, then average of common points in save
										  (scatter-distance 6)
										  (new-points-color "black")
										  (new-points-size  4)
										  filename-to-write-save
										  (merge-diagnostics-filename "/tmp/merge-diagnostics")
										  (if-exists-action :error)
										  (dim-anneal-f #'lisp-dim-anneal-two-phase)
										  (dim-anneal-starting-dimension 5)
										  dim-anneal-ending-dimension)

  (add-table-to-save-and-run-randomizing-only-new-points 
   original-save-or-filename
   (table-from-save (if (stringp new-save-or-filename) (fi-in new-save-or-filename) new-save-or-filename))
   num-runs 
   :antigen-or-coords-on-which-to-center-new-points antigen-or-coords-on-which-to-center-new-points
   :scatter-distance scatter-distance
   :new-points-color new-points-color
   :new-points-size  new-points-size
   :filename-to-write-save filename-to-write-save
   :merge-diagnostics-filename merge-diagnostics-filename
   :if-exists-action if-exists-action
   :dim-anneal-f dim-anneal-f
   :dim-anneal-starting-dimension dim-anneal-starting-dimension
   :dim-anneal-ending-dimension dim-anneal-ending-dimension))


(defun run-randomizing-only-selected-points (save-or-filename
					     selected-points
					     &optional &key 
						       (num-runs 1)
						       antigen-or-coords-on-which-to-center-new-points ;; if null, then average of common points in save
						       (scatter-distance 6)
						       ;;(new-points-color "black")   ;; <<<<<<<<<<< to implement
						       ;;(new-points-size  4)         ;; <<<<<<<<<<< to implement
						       filename-to-write-save 
						       (if-exists-action :error)
						       (dim-anneal-f #'lisp-dim-anneal-two-phase)
						       (dim-anneal-starting-dimension 5)
						       dim-anneal-ending-dimension)
  (let* ((save
	  (if (stringp save-or-filename)
	      (fi-in save-or-filename)
	    save-or-filename))
	 (dim-anneal-ending-dimension
	  (if dim-anneal-ending-dimension
	      dim-anneal-ending-dimension
	    (mds-dimensions-from-save save)))
	 (save-points (antigens-from-save save))
	 (save-coordss  (coordss (starting-coordss-from-save save)))
	 (center-for-new-points-scatter 
	  (coords-from-antigen-or-coords-on-which-to-center-new-points-points
	   save
	   selected-points
	   antigen-or-coords-on-which-to-center-new-points))
	 (dim-anneal-pad (if dim-anneal-starting-dimension 
			     (zero-base (max 0 (- dim-anneal-starting-dimension dim-anneal-ending-dimension)))
			   nil))
	 (batch-runs-results
	  (loop for i below num-runs collect
		(progn 
		  (print (list 'run i (time-and-date)))
		  (batch-mds-from-save
		   (set-starting-coordss-coordss-in-save 
		    save
		    (loop for save-point in save-points
			for save-coords in save-coordss collect
			  (if (member save-point selected-points)
			      (append 
			       (uniform-perturbs center-for-new-points-scatter scatter-distance)
			       (uniform-perturbs dim-anneal-pad scatter-distance)) ;; might be nil
			    (append save-coords
				    dim-anneal-pad))))   ;; might be nil
		   :dim-anneal-f dim-anneal-f
		   :dim-anneal-starting-dimension dim-anneal-starting-dimension
		   :dim-anneal-ending-dimension dim-anneal-ending-dimension
		   ))))
	 (save-post-optimization
	  (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
	   save
	   (sort-batch-runs batch-runs-results)
	   :not-found-action :add)))
    (if filename-to-write-save
	(write-save-form
	 save-post-optimization
	 filename-to-write-save
	 :if-exists-action if-exists-action))
    (print "Done")
    save-post-optimization))




