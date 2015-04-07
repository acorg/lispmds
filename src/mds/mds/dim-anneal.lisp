(in-package user)

;;;----------------------------------------------------------------------
;;;                      dim anneal in lisp
;;;----------------------------------------------------------------------

(defun lisp-dim-anneal-two-phase-from-save (save &optional &key
							   dim-anneal-starting-dimension
							   dim-anneal-ending-dimension
							   num-runs)
  (if num-runs
      (let ((starting-coordss (starting-coordss-from-save save)))
	(if (and starting-coordss
		 (listp starting-coordss))
	    (error "you probably don't want to specifiy both starting coords and num-runs, as each run will have the same starting, and thus ending, position.  if you want random starting positions, provide an integer (the num dimensions)")
	  (loop for i below num-runs collect
		(lisp-dim-anneal-two-phase-from-save 
		 save 
		 :dim-anneal-starting-dimension dim-anneal-starting-dimension
		 :dim-anneal-ending-dimension   dim-anneal-ending-dimension))))
    (apply #'lisp-dim-anneal-two-phase
	   (table-from-save save)
	   (append
	    (loop for keyword in '(:starting-coordss
				   :moveable-coords
				   :adjustable-rows
				   :adjustable-columns
				   :unmoveable-dimensions
				   :unmoveable-dimensions-in-second-phase)
		when (get-save-keyword-entry save keyword :not-found-action :return-nil)
		append (list keyword (get-save-keyword-entry save keyword)))
	    (list :dim-anneal-starting-dimension dim-anneal-starting-dimension
		  :dim-anneal-ending-dimension dim-anneal-ending-dimension)))))

;;(lisp-dim-anneal-two-phase (table-from-save (fi-in "/home/dsmith/mds/investigations/seasons/sequenced-hi-take-4-upto-season-74-75.save")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2)
;;(lisp-dim-anneal-two-phase (table-from-save (fi-in "/home/dsmith/cmucl/test.save")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2)
;;(progn (seed-random 4) (lisp-dim-anneal-two-phase (table-from-save (fi-in "/tmp/in")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2))

(defun lisp-dim-anneal-two-phase (hi-table &optional &key
						     starting-coordss
						     (mds-f (if (similarity-table-p hi-table)
								#'metric-mds-global-norm-conjugant-gradient-hi-metric
							      #'metric-mds-global-norm-conjugant-gradient))
						     (moveable-coords 'all)
						     unmoveable-coords
						     adjustable-rows
						     adjustable-columns
						     unmoveable-dimensions
						     unmoveable-dimensions-in-second-phase
						     dim-anneal-starting-dimension
						     dim-anneal-ending-dimension)
  (if (and unmoveable-dimensions unmoveable-dimensions-in-second-phase)
      (error "do we really want to specify both unmoveable-dimensions and unmoveable-dimensions-in-second-phase.  maybe, but i've not thought it through"))
  (let-list ((coords fitness termination-reason fitness-history)
	     (batch-mds-keyword-args hi-table 
				     :starting-coordss (if starting-coordss
							   starting-coordss
							 dim-anneal-starting-dimension)
				     :mds-f mds-f
				     :moveable-coords moveable-coords
				     :unmoveable-coords unmoveable-coords
				     :adjustable-rows adjustable-rows
				     :adjustable-columns adjustable-columns
				     :unmoveable-dimensions unmoveable-dimensions
				     :dim-anneal-coefficients 
				     (append (loop for i below dim-anneal-ending-dimension collect 
						   0.0)
					     (loop for i from dim-anneal-ending-dimension below dim-anneal-starting-dimension collect
						   *dim-anneal-coefficient*))))
	    fitness termination-reason fitness-history  ;; to stop compiler warnings
	    (batch-mds-keyword-args hi-table 
				    :starting-coordss (let ((reduced-coordss (reduce-coordss-to-dimension dim-anneal-ending-dimension coords)))
							(if unmoveable-dimensions-in-second-phase
							    (set-dimension-coordinates-to-zero reduced-coordss unmoveable-dimensions-in-second-phase)
							  reduced-coordss))
				    :mds-f mds-f
				    :moveable-coords moveable-coords
				    :unmoveable-coords unmoveable-coords
				    :adjustable-rows adjustable-rows
				    :adjustable-columns adjustable-columns
				    :unmoveable-dimensions (if unmoveable-dimensions
							       unmoveable-dimensions 
							     unmoveable-dimensions-in-second-phase))))


#|
superseded by the above, which allows unmoveable coords in only the second phase
(defun lisp-dim-anneal-two-phase (hi-table &optional &key
						     starting-coordss
						     (mds-f (if (similarity-table-p hi-table)
								#'metric-mds-global-norm-conjugant-gradient-hi-metric
							      #'metric-mds-global-norm-conjugant-gradient))
						     (moveable-coords 'all)
						     unmoveable-coords
						     adjustable-rows
						     adjustable-columns
						     unmoveable-dimensions
						     dim-anneal-starting-dimension
						     dim-anneal-ending-dimension)
  (let-list ((coords fitness termination-reason fitness-history)
	     (batch-mds-keyword-args hi-table 
				     :starting-coordss (if starting-coordss
							   starting-coordss
							 dim-anneal-starting-dimension)
				     :mds-f mds-f
				     :moveable-coords moveable-coords
				     :unmoveable-coords unmoveable-coords
				     :adjustable-rows adjustable-rows
				     :adjustable-columns adjustable-columns
				     :unmoveable-dimensions unmoveable-dimensions
				     :dim-anneal-coefficients 
				     (append (loop for i below dim-anneal-ending-dimension collect 
						   0.0)
					     (loop for i from dim-anneal-ending-dimension below dim-anneal-starting-dimension collect
						   *dim-anneal-coefficient*))))
	    fitness termination-reason fitness-history  ;; to stop compiler warningds
	    (batch-mds-keyword-args hi-table 
				    :starting-coordss (reduce-coordss-to-dimension dim-anneal-ending-dimension coords)
				    :mds-f mds-f
				    :moveable-coords moveable-coords
				    :unmoveable-coords unmoveable-coords
				    :adjustable-rows adjustable-rows
				    :adjustable-columns adjustable-columns
				    :unmoveable-dimensions unmoveable-dimensions)))
|#

(defun reduce-coordss-to-dimension (d coordss)
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more coordss)
    (reconstruct-coordss-plus-more (mapcar (^ (coords) (firstn d coords)) coordss) more)))

(defun add-zeros-to-increase-coordss-to-dimension (d coordss)
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more coordss)
    (reconstruct-coordss-plus-more (mapcar (^ (coords) (append coords (loop for i from (length coords) below d collect 0))) coordss) more)))


(defun set-dimension-coordinates-to-zero (coordss list-of-index-and-dimensions-to-zero)
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more coordss)
    (reconstruct-coordss-plus-more 
     (loop for coords in coordss
	 for i from 0 collect
	   (if (assoc i list-of-index-and-dimensions-to-zero)
	       (let ((dimensions-to-zero (assoc-value-1 i list-of-index-and-dimensions-to-zero)))
		 (loop for coord in coords for j from 1 collect
		       (if (member j dimensions-to-zero)
			   0.0
			 coord)))
	     coords))
     more)))



;;(lisp-dim-anneal-two-phase (table-from-save (fi-in "/home/dsmith/mds/investigations/seasons/sequenced-hi-take-4-upto-season-74-75.save")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2)
;;(lisp-dim-anneal-two-phase (table-from-save (fi-in "/home/dsmith/cmucl/test.save")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2)

;;(progn (seed-random 4) (lisp-dim-anneal-two-phase (table-from-save (fi-in "/tmp/in")) :dim-anneal-starting-dimension 5 :dim-anneal-ending-dimension 2))

#|
(progn
  (seed-random 4)
  (setq batch-runs
    (loop for i below 3 collect
	  (batch-mds-keyword-args
	   (table-from-save (fi-in "/tmp/in"))
	   :dim-anneal-f #'lisp-dim-anneal-two-phase-gridware
	   :dim-anneal-starting-dimension 5
	   :dim-anneal-ending-dimension   2))))

(setq batch-runs (recover-gridware-runs batch-runs))   ;; ok to run multiple times (see we reassign batch runs)

(write-save-form
 (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
  (fi-in "/tmp/in")
  batch-runs)
 "/tmp/in-da.save")

;; then to run on muliple saves,
;;   do extracts, and set up the saves, to do this on

;; and do on the sequence so far
;; and the all-strains so far (figure out the col adjusts) (for the first few years, they are very close i think)

|#

(defun lisp-dim-anneal-two-phase-gridware (hi-table &optional &key
							      starting-coordss
							      (mds-f (if (similarity-table-p hi-table)
									 #'metric-mds-global-norm-conjugant-gradient-hi-metric
								       #'metric-mds-global-norm-conjugant-gradient))
							      (moveable-coords 'all)
							      unmoveable-coords
							      adjustable-rows
							      adjustable-columns
							      unmoveable-dimensions
							      unmoveable-dimensions-in-second-phase
							      dim-anneal-starting-dimension
							      dim-anneal-ending-dimension
							      num-runs)
  (batch-mds-gridware-submit
   #'cmucl-dim-anneal-two-phase-gridware
   :hi-table                               hi-table
   :starting-coordss                       (if starting-coordss
					       starting-coordss
					     dim-anneal-starting-dimension)
   :mds-f                                  mds-f
   :moveable-coords                        moveable-coords
   :unmoveable-coords                      unmoveable-coords
   :adjustable-rows                        adjustable-rows
   :adjustable-columns                     adjustable-columns
   :unmoveable-dimensions                  unmoveable-dimensions
   :unmoveable-dimensions-in-second-phase  unmoveable-dimensions-in-second-phase
   :num-runs                               num-runs
   :additional-gridware-args               (list dim-anneal-starting-dimension 
					         dim-anneal-ending-dimension)))

(defun batch-mds-gridware-submit (gridware-submit-f
				  &optional &key
					    hi-table
					    starting-coordss
					    (mds-f (if (similarity-table-p hi-table)
						       #'metric-mds-global-norm-conjugant-gradient-hi-metric
						     #'metric-mds-global-norm-conjugant-gradient))
					    (moveable-coords 'all)
					    unmoveable-coords
					    adjustable-rows
					    adjustable-columns
					    unmoveable-dimensions
					    unmoveable-dimensions-in-second-phase
					    dim-anneal-coefficients
					    num-runs
					    additional-gridware-args)

  (let* ((unique-filename (uw-sfnr
			   (string-append 
			    "mds/alan-dim-anneal/tmp-files/"
			    (string (hi-table-name hi-table))
			    "-"
			    (anything->string (krandom 9999999 *dim-anneal-random-number-generator*)))
			   :users-to-home t :assertNonExistent t))   ;; because this is a pathname at sfi, not on local machine
	 (in-filename  (string-append unique-filename "-infile"))
	 (out-filename (string-append unique-filename "-outfile")))
    (write-save-form
     (make-save-form
      :hi-table hi-table
      :starting-coordss (cond ((or (functionp starting-coordss)
				   (and (listp starting-coordss) (eq 'lambda (car starting-coordss))))
			       (funcall starting-coordss))
			      ((listp starting-coordss)   ;; later do the same checking that is done in tk-interface
			       starting-coordss)
			      ((integerp starting-coordss)
			       (if num-runs
				   starting-coordss
				 (make-random-coordss 
				  (hi-table-length hi-table)
				  starting-coordss
				  (hi-table-max-value hi-table)
				  (or (ag-sr-table-p hi-table)
				      (similarity-table-p hi-table))
				  hi-table)))
			      (t (error "unexpected starting-coords, expected function, coordss, or num-dimensions, but got ~a"
					starting-coordss)))
      :moveable-coords         moveable-coords
      :unmoveable-coords       unmoveable-coords
      :adjustable-rows         adjustable-rows
      :adjustable-columns      adjustable-columns
      :unmoveable-dimensions   unmoveable-dimensions
      :unmoveable-dimensions-in-second-phase unmoveable-dimensions-in-second-phase
      :dim-anneal-coefficients dim-anneal-coefficients)

     in-filename)

    ;; the gridware call (make this happen)
    (apply gridware-submit-f in-filename out-filename (if num-runs (append additional-gridware-args (list num-runs)) additional-gridware-args))

    (if (running-on-windows-p)
	(print "not removing intermediate files under windows yet")
      (run-shell-command (format nil "rm ~a*" unique-filename)))

    (list (list 'recover-gridware-run out-filename unique-filename) "Queued" 'no-termination-reason-gridware-queued 'no-fitness-history)))


(defun batch-mds-from-save-gridware-submit (save)
  (let* ((hi-table (table-from-save save))
	 (unique-filename (uw-sfnr
			   (string-append 
			    "mds/alan-dim-anneal/tmp-files/"
			    (string (hi-table-name hi-table))
			    "-"
			    (anything->string (krandom 9999999 *dim-anneal-random-number-generator*))) :assertNonExistent t))
	 (in-filename  (string-append unique-filename "-infile"))
	 (out-filename (string-append unique-filename "-outfile")))
    
    ;; expand the starting coords, otherwise we'll just get the same random number each time and the same starting coordss
    (let ((starting-coordss (get-save-keyword-entry save :starting-coordss :not-found-action :return-nil)))
      (setq starting-coordss
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
			starting-coordss))))
      (write-save-form (set-starting-coordss-in-save save starting-coordss :not-found-action :ignore) in-filename)
      (funcall #'cmucl-batch-mds-from-save-gridware in-filename out-filename)
      (if (running-on-windows-p)
	  (print "not removing intermediate files under windows yet")
	(run-shell-command (format nil "rm ~a*" unique-filename)))
      (list (list 'recover-gridware-run out-filename unique-filename) "Queued" 'no-termination-reason-gridware-queued 'no-fitness-history))))



(defun recover-gridware-runs-from-save (save)
  (let ((batch-runs (recover-gridware-runs (batch-runs-from-save save))))
    (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
     save
     batch-runs
     :not-found-action :add)))






