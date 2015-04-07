(in-package user)

;;;----------------------------------------------------------------------
;;;                            MDS
;;;----------------------------------------------------------------------

(defvar *stop-optimization*)  ;;if true stops all mds optimizations
                              ;;is set from the mds-visualization window

(defun snoop-moveable-coords (args)
  (let ((position (position :moveable-coords args)))
    (if position
	(nth (inc position) args)
      'all)))   ;; here we hack up a default value for moveable-coords if it is not supplied

(defun snoop-unmoveable-coords (args)
  (let ((position (position :unmoveable-coords args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun snoop-unmoveable-dimensions (args)
  (let ((position (position :unmoveable-dimensions args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun snoop-dim-anneal-coefficients (args)
  (let ((position (position :dim-anneal-coefficients args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun snoop-disconnected-coords (args)
  (let ((position (position :disconnected-coords args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun snoop-adjustable-rows (args)
  (let ((position (position :adjustable-rows args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun snoop-adjustable-columns (args)
  (let ((position (position :adjustable-columns args)))
    (if position
	(nth (inc position) args)
      nil)))

(defun mds (stress-f 
	    perturbation-or-gradient-f 
	    coordss 
	    &key dribble 
		 visualization-window new-or-update? 
		 coords-names coords-colors 
		 coords-dot-sizes coords-transparencies coords-name-sizes coords-name-colors
		 (scale-to-fit-mds-window t)
		 show-error-lines
		 (moveable-coords 'all)
		 unmoveable-coords
		 unmoveable-dimensions
		 unmoveable-dimensions-in-second-phase
		 dim-anneal-coefficients
		 disconnected-coords
		 adjustable-columns 
		 adjustable-rows 
		 title 
		 hillclimbs-args
		 record-fitness-history
		 coords-shapes
		 coords-full-names)
  (setq *stop-optimization* nil)
  (let (fitness-history)
    (multiple-value-bind (coordss current-fitness termination-reason)
	(apply (if (eql stress-f *metric-conjugant-gradient-stress-f*)
		   #'conjugant-gradient-iterations
		 #'hillclimbs)
	       coordss
	       perturbation-or-gradient-f
	       stress-f	    
	       ;;this does acceptance of moves that decrease fitness
	       ;;:acceptance-f (^ (new old)
	       ;;	   (if (>= new old)
	       ;;	       t
	       ;;           (if (> new (* old 1.1))
	       ;;		 (if (< (knuth-random) 0.01)
	       ;;		     t
	       ;;		   nil))))
	       :initial-dribble-f (^ (initial-fitness coordss)
				     (if dribble
					 (g-plot (list (list 0 (2dp initial-fitness)))
						 :element-name 'foo
						 :y-max 0))
				     (if visualization-window
					 (visualize-mds-coordss visualization-window new-or-update?
								coordss coords-names coords-colors 
								initial-fitness 
								:title title
								:coords-name-sizes coords-name-sizes 
								:coords-name-colors coords-name-colors
								:coords-dot-sizes coords-dot-sizes
								:coords-transparencies coords-transparencies
								:coords-shapes coords-shapes
								:scale-to-fit-mds-window scale-to-fit-mds-window
								:show-error-lines show-error-lines
								:unmoveable-coords unmoveable-coords
								:coords-full-names coords-full-names)))
	       :incremental-dribble-f (^ (next-fitness num-trials climb-number coordss)
					 num-trials
					 ;;(if (zerop (mod climb-number 100))
					 ;;  (show-coordss coordss (format nil "~d" climb-number)))
					 (if record-fitness-history
					     (push (list next-fitness num-trials)
						   fitness-history))
					 (if dribble 
					     (g-plot (list (list climb-number (2dp next-fitness))) 
						     :element-name 'foo :append t :refresh nil))
					 (if visualization-window
					     (visualize-mds-coordss visualization-window 'update-changes-only 
								    coordss coords-names coords-colors 
								    next-fitness 
								    :title title
								    :coords-name-sizes coords-name-sizes
								    :coords-name-colors coords-name-colors
								    :coords-dot-sizes coords-dot-sizes
								    :coords-shapes coords-shapes
								    :scale-to-fit-mds-window scale-to-fit-mds-window
								    :show-error-lines show-error-lines
								    :coords-full-names coords-full-names)))
	       :optimum-acceptance-f (^ (new-fitness)
	       				new-fitness
	       				*stop-optimization*)
	       hillclimbs-args)
      (values 
       coordss
       visualization-window
       current-fitness
       termination-reason
       fitness-history))))



;;;----------------------------------------------------------------------
;;;                MDS STRESS COMPONCOMPARISON FUNCTIONS
;;;----------------------------------------------------------------------

(defun minus-square-zero (x)
  (if (minusp x)
      (- (square x))
    0))

(defun sign-square (x)
  (if (minusp x)
      (- (square x))
    (square x)))

(defun sign-square-half (x)
  (if (minusp x)
      (- (square x))
    (/ (square x) 2)))

(defun minus-zero (x)
  (if (minusp x)
      x
    0))

(defun min-zero (x)
  (min x 0))

(defun squash (x) 
  (if (> x 85)
      1.0
    (if (< x -85)
        0.0
      (/ 1.0 (+ 1 (exp (- x)))))))

(defun log-squash-zero (x)
  (if (minusp x)
      (log (squash x))
    0))

(defun min-log-half-log-squash (x)
  ;;this is the same as log-squash-zero in terms of values,
  ;;but is much slower because we always do the log-squash
  (min -0.6931472 (log (squash x))))

(defun log-squash (x)
  (log (squash x)))

(defun log-squash-minus (x)
  ;;for similarity not distance
  (log (squash (- x))))
