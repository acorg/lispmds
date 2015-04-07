(in-package user)

;;;----------------------------------------------------------------------
;;;                             UTILS
;;;----------------------------------------------------------------------

(defun tk-set-text-text (tk id new-text &optional (pp-f #'identity) &rest pp-f-args)
  ;; we may want to memoize here so we do less writing in the tk-window
  (if (get-show-hi-table-numbers tk)
      (apply #'tk-set-text-text-always-show tk id new-text pp-f pp-f-args)))

(defun tk-set-text-text-always-show (tk id new-text &optional (pp-f #'identity) &rest pp-f-args)
  (tk-put tk "setTextText ~d ~a" id (apply pp-f new-text pp-f-args)))

(defun pp-hi-table-value-tk (x ag-index sr-index &key table-type)
  (cond ((and (listp x) (eql 'prediction (car x)))
	 (format nil "~s" (format nil "~,1f(~,1f)" (nth 1 x) (nth 2 x))))
	((and (or (true-dont-care-p x) (and (numberp x) (zerop x)))
	      (not (eql 'ag-sr-table table-type))
	      (and (numberp ag-index) (numberp sr-index) (= ag-index sr-index)))
	 "--")   ;;there is more info if this is after the next 2 lines, 
	((true-dont-care-p x) "\".  \"")                                             ;;then we see diag is don't care
	((and (listp x) (dont-care-p (car x))) (format nil "(~,2f)" (cadr x)))       ;;but it is a good visual clue on upper-rectangle hi-tables
	((listp x) (format nil "\"~s\"" (mapcar #'1dp x)))                           ;; for a listed merge, or bootstrapped titers (does not
	((integerp x) (format nil "~d"                                                                      ;; print right yet)
			      (if (eql table-type 'ag-sr-table) 
				  (log-to-std-titer x)
				x)))
	((stringp x) (format nil "~s" x))
	;;((thresholdp x) (format nil "~s" x))  ;; for threshold values
	(t (if (eql table-type 'ag-sr-table) 
	       (format nil (if (thresholdp x) "~a" "~d") (log-to-std-titer x))
	     (format nil (if (thresholdp x) "~a" "~,2f") x)))))


;;;----------------------------------------------------------------------
;;;                          DRIVING FROM TK
;;;----------------------------------------------------------------------

(defvar *mouse-adapted-antigens*)
(setq *mouse-adapted-antigens*
  '(A/HongKong/8/68
    A/Victoria/3/75
    A/Philippines/2/82
    A/Philippines/2/82_eq
    A/Mississippi/1/85
    A/Beijing/353/89))
(setq *mouse-adapted-antigens* nil)  ;;because now I'm looking at different HI tables

(defun paired-hi-table-p (hi-table)
  ;;here we assume pairing is 1/2 table then other 1/2 table, not one then pair, one then pair etc..
  (let ((one (string (car (hi-table-antigens hi-table))))
	(other (string (car (nthcdr (floor (/ (hi-table-length hi-table) 2)) (hi-table-antigens hi-table))))))
    (if (> (length one) 3)
	(equal (substring one 0 (- (length one) 4))
	       (substring other 0 (- (length other) 4))))))

(defun tk-color-p (x)
  (and (stringp x)
       (= 7 (length x))
       (equal "#" (substring x 0 0))))

(defun determine-coords-colors (coords-colors num-colors &optional hi-table coords-names)
  (cond ((null coords-colors)
	 (loop for i below num-colors collect 'black))
	((listp coords-colors)
	 coords-colors)
	((eql 'random coords-colors)
	 (loop for i below num-colors collect (random-tk-color)))
	((eql 'random-all-same coords-colors)
	 (let ((random-color (random-tk-color)))
	   (loop for i below num-colors collect random-color)))
	((eql 'random-paired coords-colors)
	 (let ((half-colors 
		(loop for i below num-colors collect (random-tk-color))))
	   (append half-colors half-colors)))
	((eql 'pair-if-paired-table coords-colors)
	 (if (paired-hi-table-p hi-table)
	     (determine-coords-colors 'name-based-paired num-colors hi-table)
	   (determine-coords-colors 'name-based num-colors hi-table)))
	((eql 'name-based coords-colors)
	 (let ((hi-table-antigens 
		(if coords-names 
		    coords-names
		  (if hi-table
		      (hi-table-antigens hi-table)
		    (loop for i below num-colors collect nil)))))
	   (loop for i below num-colors for antigen in hi-table-antigens collect (color-from-name antigen))))
	((eql 'name-based-paired coords-colors)
	 (let* ((hi-table-antigens 
		 (if coords-names 
		     coords-names
		   (if hi-table
		       (hi-table-antigens hi-table)
		     (loop for i below num-colors collect nil))))
		(half-colors 
		 (loop for i below num-colors for antigen in hi-table-antigens collect (color-from-name antigen))))
	   (append half-colors half-colors)))
        ((eql 'name-based-antigens-transparent-sera coords-colors)
	 (let* ((hi-table-antigens 
                 (collect #'ag-name-p
                          (if coords-names 
                              coords-names
                            (if hi-table
                                (hi-table-antigens hi-table)
                              (loop for i below num-colors collect nil)))))  ;; bit rotten, and never used
                (hi-table-sera
                 (collect #'sr-name-p
                          (if coords-names 
                              coords-names
                            (if hi-table
                                (hi-table-antigens hi-table)))))
		(antigen-colors 
		 (loop for i below num-colors for antigen in hi-table-antigens collect (color-from-name antigen)))
                (sera-colors 
		 (loop for antigen in hi-table-sera collect "{}")))
	   (append antigen-colors sera-colors)))
	((tk-color-p coords-colors)
	 (loop for i below num-colors collect coords-colors))
	(t (error "Don't understand coords-colors option: ~s" coords-colors))))

;;;----------------------------------------------------------------------
;;;                         tk font
;;;----------------------------------------------------------------------

(defun tk-font (&optional (size 9))
  (if (running-on-windows-p)
      (format nil "{MSSansSerif ~d}" (- size 2))     ;; had been subtracting 3 from the font size, dont' remember why
    ;;(format nil "{Times ~d}" size)
    (format nil "{Helvetica ~d}" (- size 1))))


;;;----------------------------------------------------------------------
;;;                      linear coords
;;;----------------------------------------------------------------------

(defun make-linear-coordss-thunk (number dimensions &optional (range 1) (include-row-and-col-adjusts nil))
  (^ ()
     (let* ((perturb (/ range number))
	    (coordss (loop for i below number collect
			   (loop for d below dimensions collect
				 (uniform-perturb (* range (/ i (- number dimensions))) perturb)))))
       (if include-row-and-col-adjusts
	   (append
	    coordss
	    (make-extras-hack 
	     (append (loop for i below number collect 7)
		     (loop for i below number collect 0))))
	 coordss))))

;;;----------------------------------------------------------------------
;;;                      make functions
;;;----------------------------------------------------------------------

(defun make-master-mds-window-f (hi-table hi-table-name
				 &optional &key
					   (show-hi-table t) ;; (< (hi-table-length hi-table) 40))
					   (show-hi-table-numbers show-hi-table)
					   (show-hi-table-antigens show-hi-table)
					   (show-hi-table-gray-scale nil)
					   (mds-dimensions 2)
					   coords-colors
					   starting-coordss
					   coords-dot-sizes
                                           coords-transparencies
					   coords-names
					   coords-name-sizes
					   (moveable-coords 'all)
					   unmoveable-coords
					   show-error-lines
					   coords-outline-colors
					   batch-runs
					   (show-hi-table-sera 'short)
					   coords-names-working-copy
					   coords-name-colors
					   plot-spec
					   adjustable-rows
					   adjustable-columns
					   unmoveable-dimensions
					   unmoveable-dimensions-in-second-phase
					   dim-anneal-coefficients
					   canvas-coord-transformations
					   coords-shapes
					   (hi-table-working-copy (hi-table-copy hi-table))
					   constant-stress-radial-data
					   reference-antigens
					   procrustes-data
                                           raise-points
                                           lower-points
                                           coords-transparencies
                                           pre-merge-tables
                                           acmacs-a1-antigens
                                           acmacs-a1-sera
                                           acmacs-b1-antigens
                                           acmacs-b1-sera
                                           date                  
                                           rbc-species           
                                           lab                   
                                           minimum-column-basis  
                                           allow-titers-less-than
                                           titer-type            
					   )
  (make-strain-selection-window-f
    hi-table
    hi-table-name
    show-hi-table-numbers
    show-hi-table-gray-scale
    mds-dimensions
    coords-colors
    starting-coordss
    coords-dot-sizes
    coords-names
    coords-name-sizes
    t ;; because this option seems broken when not t
    moveable-coords
    unmoveable-coords
    show-hi-table-antigens
    show-error-lines
    coords-outline-colors
    batch-runs
    show-hi-table-sera
    coords-names-working-copy
    coords-name-colors
    plot-spec
    adjustable-rows
    adjustable-columns
    unmoveable-dimensions
    unmoveable-dimensions-in-second-phase
    dim-anneal-coefficients
    canvas-coord-transformations
    coords-shapes
    hi-table-working-copy
    constant-stress-radial-data
    reference-antigens
    procrustes-data
    raise-points
    lower-points
    coords-transparencies
    pre-merge-tables
    acmacs-a1-antigens
    acmacs-a1-sera
    acmacs-b1-antigens
    acmacs-b1-sera
    date                  
    rbc-species           
    lab                   
    minimum-column-basis  
    allow-titers-less-than
    titer-type            
    ))

#|
(defmacro make-master-mds-window (hi-table 
				  &optional &key
					    (show-hi-table (< (hi-table-length (eval hi-table)) 40))
					    (show-hi-table-numbers show-hi-table)
					    (show-hi-table-antigens show-hi-table)
					    (show-hi-table-gray-scale nil)
					    (mds-dimensions 2)
					    coords-colors
					    starting-coordss
					    coords-dot-sizes
					    coords-names
					    coords-name-sizes
					    (moveable-coords ''all)
					    unmoveable-coords
					    (scale-to-fit-mds-window t)
					    show-error-lines)
  `(make-strain-selection-window-f
    ,hi-table
    ',hi-table   ;; this is the only reason we make this a macro
    ,show-hi-table-numbers
    ,show-hi-table-gray-scale
    ,mds-dimensions
    ,coords-colors
    ,starting-coordss
    ,coords-dot-sizes
    ,coords-names
    ,coords-name-sizes
    t ;; because this option seems broken when not t
    ,moveable-coords
    ,unmoveable-coords
    ,scale-to-fit-mds-window
    ,show-hi-table-antigens
    ,show-error-lines))
|#

(defun make-master-mds-window (hi-table 
                               &optional &key
                                         (hi-table-name (hi-table-name hi-table))
                                         (show-hi-table t) ;; (<= (hi-table-length hi-table) 40))
                                         (show-hi-table-numbers show-hi-table)
                                         (show-hi-table-antigens show-hi-table)
                                         (show-hi-table-gray-scale nil)
                                         mds-dimensions  ;; default set below, so we can check starting coordss
                                         coords-colors
                                         starting-coordss
                                         coords-dot-sizes
                                         coords-names
                                         coords-name-sizes
                                         (moveable-coords 'all)
                                         unmoveable-coords
                                         show-error-lines
                                         coords-outline-colors
                                         batch-runs
                                         (show-hi-table-sera 'short)
                                         coords-names-working-copy
                                         coords-name-colors
                                         plot-spec
                                         adjustable-rows
                                         adjustable-columns
                                         unmoveable-dimensions
                                         unmoveable-dimensions-in-second-phase
                                         dim-anneal-coefficients
                                         canvas-coord-transformations
                                         coords-shapes
                                         (hi-table-working-copy (hi-table-copy hi-table))
                                         constant-stress-radial-data
                                         reference-antigens
                                         procrustes-data
                                         raise-points
                                         lower-points
                                         coords-transparencies
                                         pre-merge-tables
                                         acmacs-a1-antigens
                                         acmacs-a1-sera
                                         acmacs-b1-antigens
                                         acmacs-b1-sera
                                         date                  
                                         rbc-species           
                                         lab                   
                                         minimum-column-basis  
                                         allow-titers-less-than
                                         titer-type            
                                         )
  (make-strain-selection-window-f
   hi-table
   hi-table-name
   show-hi-table-numbers
   show-hi-table-gray-scale
   (if mds-dimensions
       mds-dimensions
     (if (and starting-coordss
	      (listp starting-coordss)
	      (listp (car starting-coordss)))
	 (length (car starting-coordss))
       2))
   coords-colors
   starting-coordss
   coords-dot-sizes
   coords-names
   coords-name-sizes
   t ;; because this option seems broken when not t
   moveable-coords
   unmoveable-coords
   show-hi-table-antigens
   show-error-lines
   coords-outline-colors
   batch-runs
   show-hi-table-sera
   coords-names-working-copy
   coords-name-colors
   plot-spec
   adjustable-rows
   adjustable-columns
   unmoveable-dimensions
   unmoveable-dimensions-in-second-phase
   dim-anneal-coefficients
   canvas-coord-transformations
   coords-shapes
   hi-table-working-copy
   constant-stress-radial-data
   reference-antigens
   procrustes-data
   raise-points
   lower-points
   coords-transparencies
   pre-merge-tables
   acmacs-a1-antigens
   acmacs-a1-sera
   acmacs-b1-antigens
   acmacs-b1-sera
   date                  
   rbc-species           
   lab                   
   minimum-column-basis  
   allow-titers-less-than
   titer-type            
   ))
						    

(defmacro make-strain-selection-window (hi-table &optional (show-hi-table-numbers t)
							   gray-scale-hi-table 
							   (mds-dimensions 2)
							   coords-colors
							   starting-coordss
							   coords-dot-sizes
							   coords-names
							   coords-name-sizes
							   (display-table-window? t)
							   (moveable-coords ''all)
							   unmoveable-coords
							   (show-hi-table-antigens t)
							   show-error-lines
							   coords-outline-colors
							   batch-runs
							   (show-hi-table-sera 'short)
							   coords-names-working-copy
							   coords-name-colors
							   plot-spec
							   adjustable-rows
							   adjustable-columns
							   unmoveable-dimensions
							   unmoveable-dimensions-in-second-phase
							   dim-anneal-coefficients
							   canvas-coord-transformations
							   coords-shapes
                                                           ;;(hi-table-working-copy (hi-table-copy hi-table))  causes compile barfing
							   constant-stress-radial-data
							   reference-antigens
							   procrustes-data
                                                           raise-points
                                                           lower-points
                                                           coords-transparencies
                                                           pre-merge-tables
                                                           acmacs-a1-antigens
                                                           acmacs-a1-sera
                                                           acmacs-b1-antigens
                                                           acmacs-b1-sera
                                                           date                  
                                                           rbc-species           
                                                           lab                   
                                                           minimum-column-basis  
                                                           allow-titers-less-than
                                                           titer-type            
							   )
  `(make-strain-selection-window-f ,hi-table ',hi-table ,show-hi-table-numbers 
				   ,gray-scale-hi-table ,mds-dimensions
				   ,coords-colors ,starting-coordss
				   ,coords-dot-sizes ,coords-names ,coords-name-sizes
				   ,display-table-window?
				   ,moveable-coords
				   ,unmoveable-coords
				   ,show-hi-table-antigens
				   ,show-error-lines
				   ,coords-outline-colors
				   ,batch-runs
				   ,show-hi-table-sera
				   ,coords-names-working-copy
				   ,coords-name-colors
				   ,plot-spec
				   ,adjustable-rows
				   ,adjustable-columns
				   ,unmoveable-dimensions
				   ,unmoveable-dimensions-in-second-phase
				   ,dim-anneal-coefficients
				   ,canvas-coord-transformations
				   ,coords-shapes
                                   ;;,hi-table-working-copy
				   ,constant-stress-radial-data
				   ,reference-antigens
				   ,procrustes-data
                                   ,raise-points
                                   ,lower-points
                                   ,coords-transparencies
                                   ,pre-merge-tables
                                   ,acmacs-a1-antigens
                                   ,acmacs-a1-sera
                                   ,acmacs-b1-antigens
                                   ,acmacs-b1-sera
                                   ,date                  
                                   ,rbc-species           
                                   ,lab                   
                                   ,minimum-column-basis  
                                   ,allow-titers-less-than
                                   ,titer-type            
				   ))

(defun make-strain-selection-window-f (hi-table hi-table-name &optional (show-hi-table-numbers t)
									gray-scale-hi-table 
									(mds-dimensions 2)
									coords-colors
									starting-coordss
									coords-dot-sizes
									coords-names   ;; can be none, or the names
									coords-name-sizes
									(display-table-window? t)
									(moveable-coords 'all)
									unmoveable-coords
									(show-hi-table-antigens t)
									show-error-lines
									coords-outline-colors
									batch-runs
									(show-hi-table-sera 'short)
									coords-names-working-copy
									coords-name-colors
									plot-spec
									adjustable-rows
									adjustable-columns
									unmoveable-dimensions
									unmoveable-dimensions-in-second-phase
									dim-anneal-coefficients
									canvas-coord-transformations
									coords-shapes
									(hi-table-working-copy (hi-table-copy hi-table))
									constant-stress-radial-data
									reference-antigens
									procrustes-data
                                                                        raise-points
                                                                        lower-points
                                                                        coords-transparencies
                                                                        pre-merge-tables
                                                                        acmacs-a1-antigens
                                                                        acmacs-a1-sera
                                                                        acmacs-b1-antigens
                                                                        acmacs-b1-sera
                                                                        date                  
                                                                        rbc-species           
                                                                        lab                   
                                                                        minimum-column-basis  
                                                                        allow-titers-less-than
                                                                        titer-type            
									)

  (let ((tk (tk-open))
	(ag-sr-table-p (ag-sr-table-p hi-table))
	(x-step 27)    ;;was 35 for font size 12 and 40 for font size 14
	(y-step 13)   ;;was 18 for font size 12 and 20 for font size 14  (the 12 is hardcoded above, search for y-step in comment
	(strain-name-width
	 (+ 10
	    (* 6 ;;8
	       (apply-max (mapcar (^ (strain) (length (string strain))) 
				    (hi-table-antigens-short hi-table)))))))
    (set-hi-table-name tk (if (symbolp hi-table-name) hi-table-name "Compound Table"))  ;;must pass atom
    (if display-table-window?  
	(progn 
	  (tk-put tk "set bitmapDir ~s" (uw-sfnr "bitmaps" :assertIsDir t))
	  (tk-put tk "source ~s" (uw-sfnr "mds/tk-interface.tk" :assertIsFile t))
	  (if show-hi-table-antigens 
	      (progn
		(tk-put tk "wm geometry . =~dx~d" 350 420)  ;; set the window size smart here
		(tk-put tk 
			".c-antigens conf -width ~d -scrollregion {0 0 0 ~d}"
			strain-name-width
			(+ 16 (* y-step (dec (hi-table-length-short hi-table)))))   ;; the 16 should be font dependent
		(tk-put tk ".c-sera-left conf -width ~d" strain-name-width)
		(tk-put tk ".xscroll-left conf -width ~d" strain-name-width)
		))
	  (if show-hi-table-sera
	      (tk-put tk
		      ".c-sera conf -scrollregion {0 0 ~d 0}"
		      (+ 15 (* x-step (hi-table-width-short hi-table)))))
	  (tk-put tk "wm title . {~s}" (get-hi-table-name tk))
	  (set-display-errors-in-lower-triangle tk nil)) ;; stop doing this, later set up properly in separate window and proper errors/predictions, and sort out absolute or not
      (set-display-errors-in-lower-triangle tk nil))
    ;;(if (not show-hi-table-numbers)
	;;(set-display-errors-in-lower-triangle tk nil))  ;; overrides the setting above (hack on...)
    ;;(if ag-sr-table-p
	;;(set-display-errors-in-lower-triangle tk nil))  ;; overrides the setting above (hack on...)
    (set-show-hi-table-numbers tk show-hi-table-numbers)

    (if canvas-coord-transformations
	(apply #'set-canvas-coord-transformations tk canvas-coord-transformations)
      (progn
	(reset-canvas-coord-transformations tk)
	(set-basis-vectors tk (firstn 2 (make-unit-vectors mds-dimensions)))))

    (push (list (list tk 'vaccine1) 0) *selections*)
    (push (list (list tk 'vaccine2) 0) *selections*)
    (push (list (list tk 'epidemic) 1) *selections*)
    (push (list (list tk 'hi-table) hi-table) *selections*)
    (push (list (list tk 'hi-table-working-copy) hi-table-working-copy) *selections*)
    (set-mds-num-dimensions tk mds-dimensions)
    (set-hi-table-width-short tk (hi-table-width-short hi-table))   ;; optimization to cache value for get-hi-table-id-from-indices
    (set-hi-table-length-short tk (hi-table-length-short hi-table)) ;; optimization to cache value for get-hi-table-id-from-indices

    (set-mds-original-starting-coordss tk starting-coordss)

    (let ((starting-coordss
	   (cond ((or (null starting-coordss)
		      (and (listp starting-coordss)
			   (eql 'RECOVER-GRIDWARE-RUN (car starting-coordss))))
		  (make-random-coordss (hi-table-length hi-table) 
				       mds-dimensions
				       (hi-table-max-value hi-table)
				       (similarity-table-p hi-table)
				       hi-table))
		 ((eql 'linear starting-coordss)
		  (make-linear-coordss-thunk (hi-table-length hi-table)
					     mds-dimensions
					     (hi-table-max-value hi-table)
					     (similarity-table-p hi-table)))
		 ((and (listp starting-coordss)
		       (listp (car starting-coordss)))
		  (multiple-value-bind (coordss more)
		      (deconstruct-coordss-plus-more starting-coordss)
		    (if (not (= (length coordss) (hi-table-length hi-table)))
			(error "the number of starting coordss (~d) does not match the length of the table (~d)" 
			       (length coordss) (hi-table-length hi-table)))
		    (if (not (and (listp (car coordss))
				  (= (length (car coordss)) mds-dimensions)))
			(error "the dimensionality of the starting coordss (~d) does not match the chosen mds-dimensions (~d)"
			       (length (car coordss)) mds-dimensions))
		    (if (and (> (length (collect #'numberp (flatten more))) 0)
			     (not (similarity-table-p hi-table)))
			(error "you have supplied row-and-col adjusts in the starting-coordss, but the table is not a similarity matrix")))
		  starting-coordss)
		 ((functionp starting-coordss)
		  starting-coordss)
		 (t (error "unrecognized specification of starting coordss, see function make-strain-selection-window-f")))))
      (set-mds-starting-coordss tk starting-coordss)
      (set-mds-coordss tk (get-mds-starting-coordss tk)))  ;;go thru the function as there may be a thunk to call

    ;; do plot spec before the plot spec by keywords below, so we can overide the plotspect with keywords
    (if plot-spec
	(process-plot-specs tk (hi-table-antigens hi-table) plot-spec))

    ;; the pattern below is to test if the param was past, in which case we process it
    ;; to override the plot-spec (which is typically stored in a save)

                                            ;; set coords colors if 
    (if (or coords-colors                   ;; they were explicitly passed as a parameter (which we allow to override being in plotspec)
	    (not (get-coords-colors tk)))   ;; or were not set plot spec and not passed as parameter
	(set-coords-colors tk (determine-coords-colors 
                               (or coords-colors (if (hi-table-p hi-table)
                                                     'name-based-antigens-transparent-sera
                                                   'name-based))
                               (hi-table-length hi-table)
                               hi-table)))

    (if (or coords-outline-colors
	    (not (get-coords-outline-colors tk)))
	(set-coords-outline-colors tk (if coords-outline-colors
					  coords-outline-colors
					(loop for i below (hi-table-length hi-table) collect (progn i 'not-set)))))
    (if (or coords-dot-sizes
	    (not (get-coords-dot-sizes tk)))
	(set-coords-dot-sizes  tk (if coords-dot-sizes 
				      (if (atom coords-dot-sizes)
					  (n-of (hi-table-length hi-table) coords-dot-sizes)
					coords-dot-sizes)
				    (loop for i below (hi-table-length hi-table) collect (progn i 4)))))
    (if (or coords-transparencies
	    (not (get-coords-transparencies tk)))
	(set-coords-transparencies  tk (if coords-transparencies 
				      (if (atom coords-transparencies)
					  (n-of (hi-table-length hi-table) coords-transparencies)
					coords-transparencies)
				    (loop for i below (hi-table-length hi-table) collect (progn i 0.0)))))
    (if (or coords-names
	    (not (get-coords-names tk)))
	(set-coords-names tk (if coords-names
				 (if (eql 'none coords-names)
				     (loop for i below (hi-table-length hi-table) collect (progn i ""))
				   coords-names)
			       (mapcar #'smart-long-strain-abbreviation (hi-table-antigens hi-table)))))
    (if (or coords-names-working-copy
	    (not (get-coords-names-working-copy tk)))
	(set-coords-names-working-copy tk (if coords-names-working-copy
					      (if (atom coords-names-working-copy)
						  (n-of (hi-table-length hi-table) coords-names-working-copy)
						coords-names-working-copy)
					    (get-coords-names tk))))
    (if (or coords-name-sizes
	    (not (get-coords-name-sizes tk)))
	(set-coords-name-sizes tk (if coords-name-sizes
				      (if (atom coords-name-sizes)
					  (n-of (hi-table-length hi-table) coords-name-sizes)
					coords-name-sizes)
				    (loop for i below (hi-table-length hi-table) collect (progn i 10)))))
    (if (or coords-name-colors
	    (not (get-coords-name-colors tk)))
	(set-coords-name-colors tk (if coords-name-colors
				       (if (atom coords-name-colors)
					   (n-of (hi-table-length hi-table) coords-name-colors)
					 coords-name-colors)
                                     (n-of (hi-table-length hi-table) "#555555")
				     ;;(get-coords-colors tk)
                                     )))

    (if (or coords-shapes
	    (not (get-coords-shapes tk)))
	(set-coords-shapes tk (if coords-shapes
				  (if (atom coords-shapes)
				      (n-of (hi-table-length hi-table) coords-shapes)
				    coords-shapes)
				(loop for name in (hi-table-antigens hi-table) collect
				      (if (sr-name-p name)
					  'rectangle
					'circle)))))

    (set-moveable-coords tk moveable-coords)
    (set-unmoveable-coords tk unmoveable-coords)
    
    (set-unmoveable-dimensions tk unmoveable-dimensions)
    (set-unmoveable-dimensions-in-second-phase tk unmoveable-dimensions-in-second-phase)
    
    (set-dim-anneal-coefficients tk dim-anneal-coefficients)

    (set-adjustable-rows tk adjustable-rows)
    (set-adjustable-columns tk adjustable-columns)

    (set-show-error-lines tk show-error-lines)
    
    (set-batch-runs-data tk batch-runs)
    
    (set-constant-stress-radial-data tk constant-stress-radial-data)

    (set-reference-antigens tk reference-antigens)

    (set-procrustes-data tk procrustes-data)

    (set-raise-points tk raise-points)
    (set-lower-points tk lower-points)

    (set-pre-merge-tables tk pre-merge-tables)

    (set-acmacs-a1-antigens tk acmacs-a1-antigens)
    (set-acmacs-a1-sera     tk acmacs-a1-sera)
    (set-acmacs-b1-antigens tk acmacs-b1-antigens)
    (set-acmacs-b1-sera     tk acmacs-b1-sera)

    (set-date tk date)                  
    (set-rbc-species tk rbc-species)           
    (set-lab tk lab)                   
    (set-minimum-column-basis tk minimum-column-basis)  
    (set-allow-titers-less-than tk allow-titers-less-than)
    (set-titer-type tk titer-type)            

    ;;    (tk-put tk "mkText Vaccine2--> 0 0 none sw") (tk-put tk "mkText
    ;;    Epidemic--> 0 0 none sw") (set-v2-e-pointers tk ;; (nth 0
    ;;    (hi-table-antigens hi-table)) ;; (nth 1 (hi-table-antigens
    ;;    hi-table)) ;; hi-table) by commenting out the above i need to
    ;;    change the following 2 function too (ugh!)
    ;;    get-hi-table-id-from-indices get-ag-sr-indices-from-tk-id

    
    (if display-table-window?
	(progn
	  (if show-hi-table-antigens
	      (loop for ag in (hi-table-antigens-short hi-table) 
		  for y from 16 by y-step         ;; this 16 should be font dependent
		  do (tk-put tk "mkTextCanvas .c-antigens ~a 10 ~d antigen sw ~a"
			     (if (member ag *mouse-adapted-antigens*) 
				 (format nil "~a-(ma)" ag)
			       ag)
			     y
			     (tk-font))))
	  (if show-hi-table-sera
	      (loop for sr in (hi-table-sera-short hi-table) 
		  for x from 30 by x-step
		  do (tk-put tk "mkTextCanvas .c-sera ~a ~d 20 serum se ~a"
			     (serum-abbreviation
			      (if (sr-name-p sr)
				  (remove-ag-sr-from-name sr)
				sr))
			     x
			     (tk-font))))))

    (if show-hi-table-numbers
	(progn
	  (tk-put tk
		  ;; ".c conf -width ~d -height ~d" 
		  ;; "wm geometry . ~dx~d" 
		  ".c conf -scrollregion {0 0 ~d ~d}" 
		  (+ 15 (* x-step (hi-table-width-short hi-table)))
		  (+ 16 (* y-step (dec (hi-table-length-short hi-table)))))  ;; the 16 should be font dependent
	  (let ((num-values-written-to-tk 0))
	    (loop for row in (hi-table-values-short hi-table-working-copy) for y from 16 by y-step for ag-index from 0 do ;; this 14 should depend on the font
		  (loop for value in row 
		      for x from 30 by x-step ;;40
		      for sr-index from 0 do
			(tk-put tk "mkTableValue ~a ~d ~d ~a"
				(pp-hi-table-value-tk value ag-index sr-index :table-type (if ag-sr-table-p 'ag-sr-table nil))
				x y
				(tk-font))
			(setq num-values-written-to-tk (inc num-values-written-to-tk))
			;;now work around linux problem, we get out of sync without this rest
			(if (zerop (mod num-values-written-to-tk 100))  ;; i had this at 250 for a while, then started to see errors
			    (sleep 0.08)))))))
    
    (if gray-scale-hi-table
	(progn
	  (error "grey scale is well bitrotten")
	  (tk-put tk ".c conf -width ~d" (+ 410 (* 20 (hi-table-width hi-table))))
	  (loop for row in (hi-table-values hi-table) for y from 20 by 20 do
		(loop for value in row for x from 380 by 20 do
		      (tk-put tk ".c create rect ~d ~d ~d ~d -fill gray~d -outline {}"
			      x (+ 2 y) (+ x 20) (- (+ y 20) 2)
			      (hi-value-to-gray-level value))))))
    tk))

(defun hi-value-to-gray-level (value)
  ;;(round (* 10 (log (/ value 10) 2)))     ;for a raw hi-table
  (- 100 (round (* value 10))))
  

(defvar *selections*)
(setq *selections* nil)

(defun selection-series (tk)
  (let* ((vaccine1-position (assoc-value-1 (list tk 'vaccine1) *selections* :test #'equal))
	 (vaccine2-position (assoc-value-1 (list tk 'vaccine2) *selections* :test #'equal))
	 (epidemic-position (assoc-value-1 (list tk 'epidemic) *selections* :test #'equal))
	 (hi-table          (assoc-value-1 (list tk 'hi-table) *selections* :test #'equal))
	 (antigens (hi-table-antigens hi-table)))
    (list (nth vaccine1-position antigens)
	  (nth vaccine2-position antigens)
	  (nth epidemic-position antigens)
	  hi-table)))

(defun set-v2-e-pointers (tk vaccine2 epidemic hi-table)
  (tk-put tk "setTextXY 1 ~d ~d" 20 (+ 40 (* 20 (position vaccine2 (hi-table-antigens hi-table)))))
  (tk-put tk "setTextXY 2 ~d ~d" 20 (+ 40 (* 20 (position epidemic (hi-table-antigens hi-table))))))

(defun new-selection (tk node what normalize hi-table-name)
  (push (list (list tk what) (- node 3)) *selections*)
  (let-list ((vaccine1 vaccine2 epidemic hi-table) (selection-series tk))
	    vaccine1
	    (set-v2-e-pointers tk vaccine2 epidemic hi-table)
	    ;;(format t "~%Vaccine 2 is ~a, epidemic is ~a" vaccine2 epidemic)
	    (plot-v1-v2-e-xys vaccine2 epidemic hi-table :normalize normalize :hi-table-name hi-table-name)))
  

#|
(make-strain-selection-window hi77d)
(make-strain-selection-window hi85d)
(make-strain-selection-window hi90d)
(make-strain-selection-window hi92d)
(make-strain-selection-window hi77a)
(make-strain-selection-window hi85a)
(make-strain-selection-window hi90a)
(make-strain-selection-window hi92a)
(make-strain-selection-window hi77laa)
(make-strain-selection-window hi85laa)
(make-strain-selection-window hi90laa)
(make-strain-selection-window hi77lasa)
(make-strain-selection-window hi85lasa)
(make-strain-selection-window hi90lasa)
|#



;;;----------------------------------------------------------------------
;;;                      CHANGING TABLE VALUES
;;;----------------------------------------------------------------------

(defun set-table-value (tk ag-index sr-index value)
  (let* ((id (get-hi-table-id-from-indices tk ag-index sr-index))
	 (hi-table-working-copy (get-hi-table-working-copy tk)))
    ;; maybe check here to see if we really need to set (is there a change)
    ;; but better as memoized tk-set-text-text (in case we change something only in pp)?  or both!
    (unsafe-set-hi-table-value-by-indices! hi-table-working-copy ag-index sr-index value)
    (if (not (eql id 'no-id-because-ag-sr-table))  ;; yes, because w/ ag-sr table we only want dc outside of the active area?
	(tk-set-text-text tk id value 
			  #'pp-hi-table-value-tk ag-index sr-index 
			  :table-type (if (ag-sr-table-p (get-hi-table tk))
					  'ag-sr-table
					nil)))))

(defun finest-measure (finest-measure x)
  (if (dont-care-p x)
      x
    (* (round (+ (/ x finest-measure) 0.0000000001))
       finest-measure)))

(defun convert-to-hi-value (what x)
  (if (dont-care-p x)
      x
    (case what
      ;;(convert-to-hi (round (* 10 (dec (expt 2 (* 7 x))))))            ;;to non-rounded hi, but still distance, not similarity
      (convert-to-hi (round (* 10 (expt 2 (- 7 (* 7 x))))))            ;;to hi without rounding, but with similarity not distance
      (convert-to-hi-and-round (round (* 10 (expt 2 (- 7 (round (* 7 x)))))))    ;;full to hi with rounding and similarity
      (convert-from-hi (/ (- 7 (log (/ x 10) 2)) 7))
      (up (* x 2))
      (down (/ x 2))
      (t (error "")))))

(defun +-dc (x y)
  (cond ((dont-care-p x) x)
	((dont-care-p y) y)
	(t (+ x y))))

(defun *-dc (x y)
  (cond ((dont-care-p x) x)
	((dont-care-p y) y)
	(t (* x y))))
      
(defun change-table-value (tk id function amount &optional (change-what 'single-value))
  (setq function (case function 
		   (+ #'+-dc)
		   (* #'*-dc)
		   (finest-measure #'finest-measure)
		   (dont-care (^ (ignore0 ignore1) ignore0 ignore1 'dont-care))
		   (hi #'convert-to-hi-value)))
  (let* ((hi-table-working-copy (get-hi-table-working-copy tk))
	 (hi-table-length (hi-table-length hi-table-working-copy)))
    (let-list ((ag-index sr-index) 
	       (if (eql id 'no-id)
		   ;; special case for when we select 10pc random on the random button (not in the window)
		   '(none none)
		 (get-ag-sr-indices-from-tk-id tk id)))
	      (change-what-iterations
	       change-what
	       (^ (ag sr) (change-table-value-aux tk function amount hi-table-working-copy ag sr))
	       ag-index sr-index hi-table-length))))

(defvar *prediction-random-generator*)
(setq *prediction-random-generator* 2)

(defun change-what-iterations (change-what f ag-index sr-index hi-table-length)
  (case change-what
    (single-value 
     (funcall f ag-index sr-index))
    (row-values
     (loop for sr below hi-table-length do
	   (funcall f ag-index sr)))
    (column-values
     (loop for ag below hi-table-length do
	   (funcall f ag sr-index)))
    (ortho-values
     (loop for ag below hi-table-length do
	   (loop for sr below hi-table-length do
		 (if (or (= ag ag-index) (= sr sr-index))
		     (funcall f ag sr)))))
    (all-values
     (loop for ag below hi-table-length do
	   (loop for sr below hi-table-length do
		 (funcall f ag sr))))
    (ten-percent-random-values
     (loop for ag below hi-table-length do
	   (loop for sr below hi-table-length do
		 (if (zerop (coin 10))
		     (funcall f ag sr)))))
    (ten-percent-random-values-same-seed
     (seed-random 467739585 *prediction-random-generator*)
     (loop for ag below hi-table-length do
	   (loop for sr below hi-table-length do
		 (if (zerop (coin 10 *prediction-random-generator*))
		     (funcall f ag sr)))))
    (t (error "unexpected change-what: ~a~%" change-what))))

(defun change-table-value-aux (tk function amount hi-table-working-copy ag-index sr-index)
  (set-table-value tk ag-index sr-index
		   (funcall function amount 
			    (hi-table-value-by-indices hi-table-working-copy ag-index sr-index))))

(defun reset-table-value (tk id &optional (change-what 'single-value) 
			  &key ag-to-exclude-resetting sr-to-exclude-resetting)
  (let* ((hi-table-original (get-hi-table tk))
	 (hi-table-length (hi-table-length hi-table-original)))
    (let-list ((ag-index sr-index) 
	       (if (eql id 'no-id)
		   ;; special case for when we select 10pc random on the random button (not in the window)
		   '(none none)
		 (get-ag-sr-indices-from-tk-id tk id)))
	      (change-what-iterations
	       change-what
	       (^ (ag sr) 
		  (if (or (member ag ag-to-exclude-resetting)    ;; to get the UI feel right we want to be able to do this
			  (member sr sr-to-exclude-resetting))
		      'do-nothing
		    (set-table-value tk ag sr
				     (hi-table-value-by-indices hi-table-original ag sr))))
	       ag-index sr-index hi-table-length))))


;;;----------------------------------------------------------------------
;;;                      WINDOW SPECIFIC ACCESSORS
;;;----------------------------------------------------------------------

(defun get-input-ui-tk () (assoc-value-1 'input-ui-tk *selections*))

(defun get-hi-table              (tk) (assoc-value-1 (list tk 'hi-table)              *selections* :test #'equal))
(defun get-hi-table-name         (tk) (assoc-value-1 (list tk 'hi-table-name)         *selections* :test #'equal))
(defun get-hi-table-working-copy (tk) (assoc-value-1 (list tk 'hi-table-working-copy) *selections* :test #'equal))
(defun get-hi-table-residuals    (tk) (assoc-value-1 (list tk 'hi-table-residuals)    *selections* :test #'equal))
(defun get-mds-num-dimensions    (tk) (assoc-value-1 (list tk 'mds-num-dimensions)    *selections* :test #'equal))
(defun get-mds-coordss           (tk) (assoc-value-1 (list tk 'mds-coordss)           *selections* :test #'equal))
(defun get-coords-colors         (tk) (assoc-value-1 (list tk 'coords-colors)         *selections* :test #'equal))
(defun get-coords-outline-colors (tk) (assoc-value-1 (list tk 'coords-outline-colors) *selections* :test #'equal))
(defun get-coords-names          (tk) (assoc-value-1 (list tk 'coords-names)          *selections* :test #'equal))
(defun get-coords-name-sizes     (tk) (assoc-value-1 (list tk 'coords-name-sizes)     *selections* :test #'equal))
(defun get-coords-name-colors    (tk) (assoc-value-1 (list tk 'coords-name-colors)    *selections* :test #'equal))
(defun get-coords-shapes         (tk) (assoc-value-1 (list tk 'coords-shapes)         *selections* :test #'equal))
(defun get-coords-dot-sizes      (tk) (assoc-value-1 (list tk 'coords-dot-sizes)      *selections* :test #'equal))
(defun get-coords-transparencies (tk) (assoc-value-1 (list tk 'coords-transparencies) *selections* :test #'equal))
(defun get-show-error-lines      (tk) (assoc-value-1 (list tk 'show-error-lines)      *selections* :test #'equal))
(defun get-error-lines-made-p    (tk) (assoc-value-1 (list tk 'error-lines-made-p)    *selections* :test #'equal))
(defun get-first-error-line-item (tk) (assoc-value-1 (list tk 'first-error-line-item) *selections* :test #'equal))
(defun get-show-stress-components(tk) (assoc-value-1 (list tk 'show-stress-components)*selections* :test #'equal))
(defun get-moveable-coords       (tk) (assoc-value-1 (list tk 'moveable-coords)       *selections* :test #'equal))
(defun get-unmoveable-coords     (tk) (assoc-value-1 (list tk 'unmoveable-coords)     *selections* :test #'equal))
(defun get-unmoveable-dimensions (tk) (assoc-value-1 (list tk 'unmoveable-dimensions) *selections* :test #'equal))
(defun get-disconnected-points   (tk) (assoc-value-1 (list tk 'disconnected-points)   *selections* :test #'equal))
(defun get-adjustable-rows       (tk) (assoc-value-1 (list tk 'adjustable-rows)       *selections* :test #'equal))
(defun get-adjustable-columns    (tk) (assoc-value-1 (list tk 'adjustable-columns)    *selections* :test #'equal))
(defun get-show-hi-table-numbers (tk) (assoc-value-1 (list tk 'show-hi-table-numbers) *selections* :test #'equal))
(defun get-hi-table-width-short  (tk) (assoc-value-1 (list tk 'hi-table-width-short)  *selections* :test #'equal))
(defun get-hi-table-length-short (tk) (assoc-value-1 (list tk 'hi-table-length-short) *selections* :test #'equal))
(defun get-error-lines-selected-subset (tk) (assoc-value-1 (list tk 'error-lines-selected-subset) *selections* :test #'equal))
(defun get-coords-names-working-copy   (tk) (assoc-value-1 (list tk 'coords-names-working-copy)   *selections* :test #'equal))
(defun get-num-vertical-grid-lines     (tk) (assoc-value-1 (list tk 'num-vertical-grid-lines)     *selections* :test #'equal))
(defun get-num-horizontal-grid-lines   (tk) (assoc-value-1 (list tk 'num-horizontal-grid-lines)   *selections* :test #'equal))
(defun get-dim-anneal-coefficients     (tk) (assoc-value-1 (list tk 'dim-anneal-coefficients)     *selections* :test #'equal))
(defun get-procrustes-kmeans           (tk) (assoc-value-1 (list tk 'procrustes-kmeans)           *selections* :test #'equal))
(defun get-constant-stress-radial-data (tk) (assoc-value-1 (list tk 'constant-stress-radial-data) *selections* :test #'equal))
(defun get-reference-antigens          (tk) (assoc-value-1 (list tk 'reference-antigens)          *selections* :test #'equal))
(defun get-procrustes-data             (tk) (assoc-value-1 (list tk 'procrustes-data)             *selections* :test #'equal))
(defun get-raise-points                (tk) (assoc-value-1 (list tk 'raise-points)                *selections* :test #'equal))
(defun get-lower-points                (tk) (assoc-value-1 (list tk 'lower-points)                *selections* :test #'equal))
(defun get-pre-merge-tables            (tk) (assoc-value-1 (list tk 'pre-merge-tables)            *selections* :test #'equal))
(defun get-acmacs-a1-antigens          (tk) (assoc-value-1 (list tk 'acmacs-a1-antigens)          *selections* :test #'equal))
(defun get-acmacs-a1-sera              (tk) (assoc-value-1 (list tk 'acmacs-a1-sera)              *selections* :test #'equal))
(defun get-acmacs-b1-antigens          (tk) (assoc-value-1 (list tk 'acmacs-b1-antigens)          *selections* :test #'equal))
(defun get-acmacs-b1-sera              (tk) (assoc-value-1 (list tk 'acmacs-b1-sera)              *selections* :test #'equal))
(defun get-date                        (tk) (assoc-value-1 (list tk 'date)                        *selections* :test #'equal))
(defun get-rbc-species                 (tk) (assoc-value-1 (list tk 'rbc-species)                 *selections* :test #'equal))
(defun get-lab                         (tk) (assoc-value-1 (list tk 'lab)                         *selections* :test #'equal))
(defun get-minimum-column-basis        (tk) (assoc-value-1 (list tk 'minimum-column-basis)        *selections* :test #'equal))
(defun get-allow-titers-less-than      (tk) (assoc-value-1 (list tk 'allow-titers-less-than)      *selections* :test #'equal))
(defun get-titer-type                  (tk) (assoc-value-1 (list tk 'titer-type)                  *selections* :test #'equal))


(defun get-mds-window-first-coords-canvas-item          (tk) (assoc-value-1 (list tk 'mds-window-first-coords-canvas-item)          *selections* :test #'equal))
(defun get-mds-window-first-dimension-pairs-canvas-item (tk) (assoc-value-1 (list tk 'mds-window-first-dimension-pairs-canvas-item) *selections* :test #'equal))
(defun get-mds-window-first-axis-canvas-item            (tk) (assoc-value-1 (list tk 'mds-window-first-axis-canvas-item)            *selections* :test #'equal))
(defun get-mds-window-run-indicator-canvas-item         (tk) (assoc-value-1 (list tk 'mds-window-run-indicator-canvas-item)         *selections* :test #'equal))
(defun get-hi-table-prediction-error-id                 (tk) (assoc-value-1 (list tk 'hi-table-prediction-error-id)                 *selections* :test #'equal))
(defun get-unmoveable-dimensions-in-second-phase        (tk) (assoc-value-1 (list tk 'unmoveable-dimensions-in-second-phase)        *selections* :test #'equal))
(defun get-error-connection-prediction-line-data        (tk) (assoc-value-1 (list tk 'error-connection-prediction-line-data)        *selections* :test #'equal))
  
  
#|
(defun set-canvas-item-sets (tk canvas-item-sets)
  (push (list (list tk canvas-item-sets) canvas-item-sets) *selections*)
  canvas-item-sets)

(defun get-canvas-item-sets (tk)
  (assoc-value-1 (list tk 'canvas-item-sets) *selections* :test #'equal))

(defun get-canvas-item-set (tk set-name)
  (assoc-value-1 set-name (get-canvas-item-sets tk)))

(defun add-canvas-item-set (tk set)
  ;; could check not already set
  (set-canvas-item-sets tk (cons set (get-canvas-item-sets tk))))

(defun remove-canvas-item-set (tk set-name)
  ;; could check not already set
  (set-canvas-item-sets tk (remove set-name (get-canvas-item-sets tk) :test (^ (a b) (equal a (car b))))))
|#





(defun get-mds-col-bases (tk) (col-bases (get-mds-coordss tk)))
(defun get-mds-row-adjusts (tk) (row-adjusts (get-mds-coordss tk)))

(defun get-mds-original-starting-coordss  (tk)
  (assoc-value-1 (list tk 'mds-original-starting-coordss)  *selections* :test #'equal))

(defun get-mds-starting-coordss  (tk)
  (let ((starting-coordss (assoc-value-1 (list tk 'mds-starting-coordss)  *selections* :test #'equal)))
    (if (functionp starting-coordss)
	(if (= 0 (length (arglist starting-coordss)))  ;;this test for backwards compatibility in the original piecing code
	    (funcall starting-coordss)                ;; experimental code only, so it only matters for redoing those expts
	  (funcall starting-coordss tk))
      starting-coordss)))

;;(defun get-mds-num-coordss    (tk) (length (get-mds-coordss tk)))
(defun get-mds-num-coordss    (tk) (length (deconstruct-coordss-plus-more (get-mds-coordss tk))))

(defun get-display-errors-in-lower-triangle (tk) 
  (assoc-value-1 (list tk 'display-errors-in-lower-traingle) *selections* :test #'equal))

(defun get-mds-point-index-from-mds-window-canvas-id (mds-window canvas-id)
  (/ (- canvas-id (get-mds-window-first-coords-canvas-item mds-window))
     2))

(defun get-mds-point-indices-from-mds-window-canvas-ids (mds-window canvas-ids)
  (loop for canvas-id in canvas-ids collect
	(get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))

(defun get-mds-point-index-from-mds-window-strain-name-canvas-id (mds-window strain-name-canvas-id)
  (/ (- strain-name-canvas-id (get-mds-window-first-coords-canvas-item mds-window) 1)
     2))
  
(defun get-mds-window-dot-canvas-id-from-strain-index (mds-window strain-index)
  (+ (get-mds-window-first-coords-canvas-item mds-window)
     (* 2 strain-index)))

(defun get-mds-window-name-canvas-id-from-strain-index (mds-window strain-index)
  (+ (get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index)
     1))
	 
(defun get-point-name-from-mds-window-canvas-id (mds-window canvas-id)
  (nth
   (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)
   (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))

(defun get-point-names-from-mds-window-canvas-ids (mds-window canvas-ids)
  (mapcar (^ (canvas-id) (get-point-name-from-mds-window-canvas-id mds-window canvas-id)) canvas-ids))

(defun get-hi-table-id-from-indices (tk ag-index sr-index)
  (let* ((hi-table (get-hi-table tk))
	 (hi-table-width (hi-table-width hi-table))
	 ;;(hi-table-width-short (hi-table-width-short hi-table))
	 ;;(hi-table-length-short (hi-table-length-short hi-table))
	 (hi-table-width-short (get-hi-table-width-short tk))
	 (hi-table-length-short (get-hi-table-length-short tk))
	 (sr-short-start-in-long (- hi-table-width hi-table-width-short)))  ;; this is 0 if we do not have an ag-sr-table
    (if (or (> ag-index hi-table-length-short)
	    (< sr-index sr-short-start-in-long))
	'no-id-because-ag-sr-table
      (let ((unadjusted-id (+ (* ag-index hi-table-width-short) (- sr-index sr-short-start-in-long))))
	(+ 1				;; tk is 1-based
	   ;; hi-table-length-short	;; the antigen names
	   unadjusted-id)))))
;;the above was (car (find (list ag-index sr-index) (get-hi-table-id-alist tk) :test (^ (key l) (equal key (cdr l))))))

#|
(defun get-ag-sr-indices-from-tk-id (tk id)
  (let* ((hi-table (get-hi-table tk))
	 (hi-table-width (hi-table-width hi-table))
	 (hi-table-length (hi-table-length hi-table))
	 (hi-table-width-short (hi-table-width-short hi-table))
	 (hi-table-length-short (hi-table-length-short hi-table))
	 (adjusted-id (- id 
			 1		          ;; tk is 1 based
			 hi-table-length-short))) ;; the antigen names
    (if (ag-sr-table-p hi-table)
	(let ((ag-index-short (floor (/ adjusted-id hi-table-width-short)))
	      (sr-index-short (mod adjusted-id hi-table-width-short))
	      (sr-short-start-in-long (- hi-table-width hi-table-width-short)))
	  (list ag-index-short
		(+ sr-short-start-in-long sr-index-short)))
      (let ((ag-index (floor (/ adjusted-id hi-table-length)))
	    (sr-index (mod adjusted-id hi-table-length)))
	(list ag-index
	      sr-index)))))
|#

;; the above can collapse down
(defun get-ag-sr-indices-from-tk-id (tk id)
  (let* ((hi-table (get-hi-table tk))
	 (hi-table-width (hi-table-width hi-table))
	 (hi-table-width-short (hi-table-width-short hi-table))
	 (hi-table-length-short (hi-table-length-short hi-table))
	 (sr-short-start-in-long (- hi-table-width hi-table-width-short))  ;; this is 0 if we do not have an ag-sr-table
	 (adjusted-id (- id 
			 1		          ;; tk is 1 based
			 ;; hi-table-length-short    ;; the antigen names
			 )))
    (let ((ag-index-short (floor (/ adjusted-id hi-table-width-short)))
	  (sr-index-short (mod adjusted-id hi-table-width-short)))
      (list ag-index-short
	    (+ sr-short-start-in-long sr-index-short)))))

;(defun get-dimension-pair-from-tk-id (tk id)
;  (let* ((hi-table-length (hi-table-length (get-hi-table tk)))
;	 (num-mds-dimensions (get-mds-num-dimensions tk))
;	 (dimension-pair-id (- id (+ 1	;tk is 1 based
;				     1	;the stress value
;				     (* 2 hi-table-length) ;the points and their labels
;				     num-mds-dimensions ;the number of axes drawn
;				     ;;2	;the basis axes
;				     ))))
;    (nth dimension-pair-id (combs 2 (series 0 (dec num-mds-dimensions))))))

;; replace 2004-03-05
;(defun get-dimension-pair-from-tk-id (tk id)
;  ;;CAREFUL  this is counting the tk-id, so if we change the number of items created in the canvas this will
;  ;;need changing here too (for example the drawing of the axis or not based on the num dims below
;  ;;this is bad programming practice, it would be best if we did not have this 'distant' connection
;  (let* ((num-coordss (get-mds-num-coordss tk))
;	 (num-dimensions (get-mds-num-dimensions tk))
;	 (dimension-pair-id (- id (+ 1	;tk is 1 based
;				     1	;the stress value
;				     (* 2 num-coordss) ;the points and their labels
;				     ;;a tricky one here, in visualize-mds-coordss i changed the drawing of the
;				     ;;reference frame to not do so if num was less than 2, and needs this change here
;				     (if (> num-dimensions 2)
;					 num-dimensions      	;the number of axes drawn
;				       0)
;				     ;;2	;the basis axes
;				     ))))
;    (if (not (= dimension-pair-id (get-mds-window-first-dimension-pairs-canvas-item tk)))
;	(error "error in dimension pair canvas item calculation"))
;    (nth (get-mds-window-first-dimension-pairs-canvas-item tk) (combs 2 (series 0 (dec num-dimensions))))))

(defun get-dimension-pair-from-tk-id (tk id)
  (let* ((num-coordss (get-mds-num-coordss tk))
	 (num-dimensions (get-mds-num-dimensions tk)))
    (nth (- id (get-mds-window-first-dimension-pairs-canvas-item tk)) (combs 2 (series 0 (dec num-dimensions))))))


(defun get-axis-from-tk-id (tk id)
  (let* ((hi-table-length (hi-table-length (get-hi-table tk)))
	 (old-ans (+ 1			;tk is 1 based
		     1			;the stress value
		     (* 2 hi-table-length) ;the points and their labels
		     )))
    (if (not (= old-ans (get-mds-window-first-axis-canvas-item tk)))
	(error "error in ais from tk id")))
  (- id (get-mds-window-first-axis-canvas-item tk)))

(defun get-hi-table-working-copy-with-sera (table-window)
  ;; nasty hack because working copy have have values changed, and as an efficency hack we do not have 
  ;; new table make on every change.  but sometimes we want the sera set.
  (let ((hi-table-working-copy (get-hi-table-working-copy table-window)))
    (if (eql 'no-sera-set (hi-table-sera hi-table-working-copy :error-if-not-set nil))
	(make-hi-table
	 (hi-table-antigens hi-table-working-copy)
	 (hi-table-sera (get-hi-table table-window))  ;; sera from the non-working-copy
	 (hi-table-values hi-table-working-copy)
	 (glue-up (list (hi-table-name (get-hi-table table-window))) 'mod))
      hi-table-working-copy)))





;;;----------------------------------------------------------------------
;;;                      set methods
;;;----------------------------------------------------------------------

(defun set-input-ui-tk (tk) (push (list 'input-ui-tk tk) *selections*))

(defun set-hi-table-mds-coordss (tk mds-coords)
  (error "not used now"))

(defun set-display-errors-in-lower-triangle (tk display?)
  (push (list (list tk 'display-errors-in-lower-traingle) display?) *selections*))

(defun set-mds-num-dimensions (tk mds-num-dimensions)
  (push (list (list tk 'mds-num-dimensions) mds-num-dimensions) *selections*)
  mds-num-dimensions)

(defun set-mds-coordss (tk mds-coords)
  (push (list (list tk 'mds-coordss) mds-coords) *selections*)
  mds-coords)

(defun set-mds-starting-coordss (tk mds-coords)
  (push (list (list tk 'mds-starting-coordss) mds-coords) *selections*)
  mds-coords)

(defun set-mds-original-starting-coordss (tk mds-coords)
  (push (list (list tk 'mds-original-starting-coordss) mds-coords) *selections*)
  mds-coords)

(defun set-coords-colors (tk coords-colors)
  (push (list (list tk 'coords-colors) coords-colors) *selections*)
  coords-colors)

(defun set-coords-outline-colors (tk coords-outline-colors)
  (push (list (list tk 'coords-outline-colors) coords-outline-colors) *selections*)
  coords-outline-colors)

(defun set-coords-names (tk coords-names)
  (push (list (list tk 'coords-names) coords-names) *selections*)
  coords-names)

(defun set-coords-names-working-copy (tk coords-names-working-copy)
  (push (list (list tk 'coords-names-working-copy) coords-names-working-copy) *selections*)
  coords-names-working-copy)

(defun set-coords-name-sizes (tk coords-name-sizes)
  (push (list (list tk 'coords-name-sizes) coords-name-sizes) *selections*)
  coords-name-sizes)

(defun set-coords-name-colors (tk coords-name-colors)
  (push (list (list tk 'coords-name-colors) coords-name-colors) *selections*)
  coords-name-colors)

(defun set-coords-shapes (tk coords-shapes)
  (push (list (list tk 'coords-shapes) coords-shapes) *selections*)
  coords-shapes)

(defun set-coords-dot-sizes (tk coords-dot-sizes)
  (push (list (list tk 'coords-dot-sizes) coords-dot-sizes) *selections*)
  coords-dot-sizes)

(defun set-coords-transparencies (tk coords-transparencies)
  (push (list (list tk 'coords-transparencies) coords-transparencies) *selections*)
  coords-transparencies)

(defun set-show-error-lines (tk show-error-lines)
  (push (list (list tk 'show-error-lines) show-error-lines) *selections*)
  show-error-lines)

(defun set-error-lines-selected-subset (tk error-lines-selected-subset)
  (push (list (list tk 'error-lines-selected-subset) error-lines-selected-subset) *selections*)
  error-lines-selected-subset)

(defun set-error-lines-made-p (tk error-lines-made-p)
  (push (list (list tk 'error-lines-made-p) error-lines-made-p) *selections*)
  error-lines-made-p)

(defun set-first-error-line-item (tk first-error-line-item)
  (push (list (list tk 'first-error-line-item) first-error-line-item) *selections*)
  first-error-line-item)

(defun set-show-stress-components (tk show-stress-components)
  (push (list (list tk 'show-stress-components) show-stress-components) *selections*)
  show-stress-components)

(defun set-moveable-coords (tk moveable-coords)
  ;; could do consistency check here that no moveables are also in unmoveable
  (push (list (list tk 'moveable-coords) moveable-coords) *selections*)
  moveable-coords)

(defun set-unmoveable-coords (tk unmoveable-coords)
  ;; could do consistency check here that no unmoveables are also in moveable
  (push (list (list tk 'unmoveable-coords) unmoveable-coords) *selections*)
  unmoveable-coords)

(defun set-unmoveable-dimensions (tk unmoveable-dimensions)
  (push (list (list tk 'unmoveable-dimensions) unmoveable-dimensions) *selections*)
  unmoveable-dimensions)

(defun set-unmoveable-dimensions-in-second-phase (tk unmoveable-dimensions-in-second-phase)
  (push (list (list tk 'unmoveable-dimensions-in-second-phase) unmoveable-dimensions-in-second-phase) *selections*)
  unmoveable-dimensions-in-second-phase)

(defun set-dim-anneal-coefficients (tk dim-anneal-coefficients)
  (push (list (list tk 'dim-anneal-coefficients) dim-anneal-coefficients) *selections*)
  dim-anneal-coefficients)

(defun set-procrustes-kmeans (tk procrustes-kmeans)
  (push (list (list tk 'procrustes-kmeans) procrustes-kmeans) *selections*)
  procrustes-kmeans)

(defun set-constant-stress-radial-data (tk constant-stress-radial-data)
  (push (list (list tk 'constant-stress-radial-data) constant-stress-radial-data) *selections*)
  constant-stress-radial-data)

(defun set-reference-antigens (tk reference-antigens)
  (push (list (list tk 'reference-antigens) reference-antigens) *selections*)
  reference-antigens)

(defun set-procrustes-data (tk procrustes-data)
  (push (list (list tk 'procrustes-data) procrustes-data) *selections*)
  procrustes-data)

(defun set-raise-points (tk raise-points)
  (push (list (list tk 'raise-points) raise-points) *selections*)
  raise-points)

(defun set-lower-points (tk lower-points)
  (push (list (list tk 'lower-points) lower-points) *selections*)
  lower-points)

(defun set-pre-merge-tables (tk pre-merge-tables)
  (push (list (list tk 'pre-merge-tables) pre-merge-tables) *selections*)
  pre-merge-tables)


(defun set-acmacs-a1-antigens (tk acmacs-a1-antigens)
  (push (list (list tk 'acmacs-a1-antigens) acmacs-a1-antigens) *selections*)
  acmacs-a1-antigens)

(defun set-acmacs-a1-sera (tk acmacs-a1-sera)
  (push (list (list tk 'acmacs-a1-sera) acmacs-a1-sera) *selections*)
  acmacs-a1-sera)

(defun set-acmacs-b1-antigens (tk acmacs-b1-antigens)
  (push (list (list tk 'acmacs-b1-antigens) acmacs-b1-antigens) *selections*)
  acmacs-b1-antigens)

(defun set-acmacs-b1-sera (tk acmacs-b1-sera)
  (push (list (list tk 'acmacs-b1-sera) acmacs-b1-sera) *selections*)
  acmacs-b1-sera)


(defun set-date (tk date)
  (push (list (list tk 'date) date) *selections*)
  date)

(defun set-rbc-species (tk rbc-species)
  (push (list (list tk 'rbc-species) rbc-species) *selections*)
  rbc-species)

(defun set-lab (tk lab)
  (push (list (list tk 'lab) lab) *selections*)
  lab)

(defun set-minimum-column-basis (tk minimum-column-basis)
  (push (list (list tk 'minimum-column-basis) minimum-column-basis) *selections*)
  minimum-column-basis)

(defun set-allow-titers-less-than (tk allow-titers-less-than)
  (push (list (list tk 'allow-titers-less-than) allow-titers-less-than) *selections*)
  allow-titers-less-than)

(defun set-titer-type (tk titer-type)
  (push (list (list tk 'titer-type) titer-type) *selections*)
  titer-type)


(defun set-error-connection-prediction-line-data (tk error-connection-prediction-line-data)
  (push (list (list tk 'error-connection-prediction-line-data) error-connection-prediction-line-data) *selections*)
  error-connection-prediction-line-data)

(defun set-disconnected-points (tk disconnected-points)
  (push (list (list tk 'disconnected-points) disconnected-points) *selections*)
  disconnected-points)

(defun set-adjustable-rows (tk adjustable-rows)
  (push (list (list tk 'adjustable-rows) adjustable-rows) *selections*)
  adjustable-rows)

(defun set-adjustable-columns (tk adjustable-columns)
  (push (list (list tk 'adjustable-columns) adjustable-columns) *selections*)
  adjustable-columns)

(defun set-show-hi-table-numbers (tk show-hi-table-numbers)
  (push (list (list tk 'show-hi-table-numbers) show-hi-table-numbers) *selections*)
  show-hi-table-numbers)

(defun set-hi-table-width-short (tk hi-table-width-short)
  (push (list (list tk 'hi-table-width-short) hi-table-width-short) *selections*)
  hi-table-width-short)

(defun set-hi-table-length-short (tk hi-table-length-short)
  (push (list (list tk 'hi-table-length-short) hi-table-length-short) *selections*)
  hi-table-length-short)

(defun set-num-vertical-grid-lines (tk num-vertical-grid-lines)
  (push (list (list tk 'num-vertical-grid-lines) num-vertical-grid-lines) *selections*)
  num-vertical-grid-lines)

(defun set-num-horizontal-grid-lines (tk num-horizontal-grid-lines)
  (push (list (list tk 'num-horizontal-grid-lines) num-horizontal-grid-lines) *selections*)
  num-horizontal-grid-lines)

(defun set-mds-window-first-coords-canvas-item (tk mds-window-first-coords-canvas-item)
  (push (list (list tk 'mds-window-first-coords-canvas-item) mds-window-first-coords-canvas-item) *selections*)
  mds-window-first-coords-canvas-item)

(defun set-mds-window-first-dimension-pairs-canvas-item (tk mds-window-first-dimension-pairs-canvas-item)
  (push (list (list tk 'mds-window-first-dimension-pairs-canvas-item) mds-window-first-dimension-pairs-canvas-item) *selections*)
  mds-window-first-dimension-pairs-canvas-item)

(defun set-mds-window-first-axis-canvas-item (tk mds-window-first-axis-canvas-item)
  (push (list (list tk 'mds-window-first-axis-canvas-item) mds-window-first-axis-canvas-item) *selections*)
  mds-window-first-axis-canvas-item)

(defun set-mds-window-run-indicator-canvas-item (tk mds-window-run-indicator-canvas-item)
  (push (list (list tk 'mds-window-run-indicator-canvas-item) mds-window-run-indicator-canvas-item) *selections*)
  mds-window-run-indicator-canvas-item)

(defun set-hi-table-prediction-error-id (tk hi-table-prediction-error-id)
  (push (list (list tk 'hi-table-prediction-error-id) hi-table-prediction-error-id) *selections*)
  hi-table-prediction-error-id)

(defun set-mds-coordss-by-one-point (tk which-point new-point-coords)
  (set-mds-coordss tk (replace-nth which-point new-point-coords (get-mds-coordss tk))))

(defun set-mds-col-bases-by-one-adjust (tk n new-value)
  (set-mds-coordss 
   tk
   (let ((coordss-plus-more (get-mds-coordss tk)))
     (make-coordss-plus-more 
      (coordss coordss-plus-more) 
      (replace-nth n new-value (col-bases coordss-plus-more) )
      (row-adjusts coordss-plus-more)))))

(defun set-mds-row-adjusts-by-one-adjust (tk n new-value)
  (set-mds-coordss 
   tk
   (let ((coordss-plus-more (get-mds-coordss tk)))
     (make-coordss-plus-more 
      (coordss coordss-plus-more) 
      (col-bases coordss-plus-more)
      (replace-nth n new-value (row-adjusts coordss-plus-more))))))

(defun set-hi-table-residuals (tk hi-table-residuals)
  (push (list (list tk 'hi-table-residuals) hi-table-residuals) *selections*)
  hi-table-residuals)

(defun set-hi-table-name (tk hi-table-name)
  (push (list (list tk 'hi-table-name) hi-table-name) *selections*)
  hi-table-name)

(defun set-hi-table-value-id (tk id sr-index ag-index)
  (error "not used now"))
;;  (setf (cdr (assoc (list tk 'hi-table-id-alist) *selections* :test #'equal))
;;    (cons (list id ag-index sr-index) (assoc-value (list tk 'hi-table-id-alist) *selections* :test #'equal))))

(defun set-table-window-for-mds-window (mds-window table-window)
  (push (list (list mds-window 'table-window-for-mds-window) table-window) *selections*))
(defun get-table-window-for-mds-window (mds-window) 
  (assoc-value-1 (list mds-window 'table-window-for-mds-window) *selections* :test #'equal))

(defun set-mds-f-for-mds-window (mds-window mds-f)
  (push (list (list mds-window 'mds-f-for-mds-window) mds-f) *selections*))
(defun set-stress-component-f-for-mds-window (mds-window stress-component-f)
  (push (list (list mds-window 'stress-component-f-for-mds-window) stress-component-f) *selections*))
(defun set-comparison-f-for-mds-window (mds-window comparison-f)
  (push (list (list mds-window 'comparison-f-for-mds-window) comparison-f) *selections*))

(defun get-mds-f-for-mds-window (mds-window) 
  (assoc-value-1 (list mds-window 'mds-f-for-mds-window) *selections* :test #'equal))
(defun get-stress-component-f-for-mds-window (mds-window) 
  (assoc-value-1 (list mds-window 'stress-component-f-for-mds-window) *selections* :test #'equal))
(defun get-comparison-f-for-mds-window (mds-window) 
  (assoc-value-1 (list mds-window 'comparison-f-for-mds-window) *selections* :test #'equal))


(defun get-coords-color-mds-window (mds-window i)
  (print 'thought-get-coords-color-mds-window-was-not-used)
  (get-coords-color-table-window (get-table-window-for-mds-window mds-window) i))
(defun get-coords-color-table-window (table-window i)
  (print 'thought-get-coords-color-table-window-was-not-used)
  (nth i (get-coords-colors table-window)))
  

;;;----------------------------------------------------------------------
;;;                 the batch runs ui get and set methods
;;;----------------------------------------------------------------------

(defun set-batch-runs-batch-runs-ui-window (table-window batch-runs-ui-window) 
  (push (list (list table-window 'batch-runs-ui-window) batch-runs-ui-window) *selections*)
  (push (list (list batch-runs-ui-window 'table-window-for-batch-runs-ui-window) table-window) *selections*)
  batch-runs-ui-window)
(defun set-batch-runs-data (table-window batch-runs-data)
  (push (list (list table-window 'batch-runs-data) batch-runs-data) *selections*)
  batch-runs-data)

(defun get-batch-runs-batch-runs-ui-window (table-window)
  (assoc-value-1 (list table-window 'batch-runs-ui-window) *selections* :test #'equal))
(defun get-table-window-for-batch-runs-ui-window (batch-runs-ui-window)
  (assoc-value-1 (list batch-runs-ui-window 'table-window-for-batch-runs-ui-window) *selections* :test #'equal))

(defun get-batch-runs-data (table-window)
  (assoc-value-1 (list table-window 'batch-runs-data) *selections* :test #'equal))



;;;----------------------------------------------------------------------
;;;    get and sets of the canvas data (would be better as object!)
;;;----------------------------------------------------------------------

(defun get-canvas-width (tk)
  (snoop-keyword-arg :canvas-width
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-height (tk)
  (snoop-keyword-arg :canvas-height
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-x-coord-translation (tk)
  (snoop-keyword-arg :canvas-x-coord-translation
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-y-coord-translation (tk)
  (snoop-keyword-arg :canvas-y-coord-translation
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-x-coord-scale (tk)
  (snoop-keyword-arg :canvas-x-coord-scale
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-y-coord-scale (tk)
  (snoop-keyword-arg :canvas-y-coord-scale
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-basis-vector-0 (tk)
  (snoop-keyword-arg :canvas-basis-vector-0
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-canvas-basis-vector-1 (tk)
  (snoop-keyword-arg :canvas-basis-vector-1
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-first-dimension (tk)
  (snoop-keyword-arg :first-dimension
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-second-dimension (tk)
  (snoop-keyword-arg :second-dimension
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-point-indices (tk)
  (snoop-keyword-arg :basis-vector-point-indices
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-point-indices-backup (tk)
  (snoop-keyword-arg :basis-vector-point-indices-backup
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-x-coord-translation (tk)
  (snoop-keyword-arg :basis-vector-x-coord-translation
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-y-coord-translation (tk)
  (snoop-keyword-arg :basis-vector-y-coord-translation
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-x-coord-scale (tk)
  (snoop-keyword-arg :basis-vector-x-coord-scale
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-basis-vector-y-coord-scale (tk)
  (snoop-keyword-arg :basis-vector-y-coord-scale
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-translate-to-fit-mds-window (tk)
  (snoop-keyword-arg :translate-to-fit-mds-window
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))
(defun get-scale-to-fit-mds-window (tk)
  (snoop-keyword-arg :scale-to-fit-mds-window
		     (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)))



(defun set-canvas-width (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-width
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-height (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-height
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-x-coord-translation (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-x-coord-translation
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-y-coord-translation (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-y-coord-translation
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-x-coord-scale (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-x-coord-scale
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-y-coord-scale (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-y-coord-scale
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-basis-vector-0 (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-basis-vector-0
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-canvas-basis-vector-1 (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :canvas-basis-vector-1
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-first-dimension (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :first-dimension
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-second-dimension (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :second-dimension
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-point-indices (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-point-indices
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-point-indices-backup (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-point-indices-backup
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-x-coord-translation (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-x-coord-translation
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-y-coord-translation (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-y-coord-translation
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-x-coord-scale (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-x-coord-scale
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-basis-vector-y-coord-scale (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :basis-vector-y-coord-scale
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-translate-to-fit-mds-window (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :translate-to-fit-mds-window
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)
(defun set-scale-to-fit-mds-window (tk arg)
  (push (list (list tk 'canvas-coord-transformations)
	      (subst-keyword-arg :scale-to-fit-mds-window
				 (assoc-value-1 (list tk 'canvas-coord-transformations) *selections* :test #'equal)
				 arg
				 :not-found-action :add)) *selections*)
  arg)



;;;----------------------------------------------------------------------
;;;                    CALLING MDS ON TABLE VALUES
;;;----------------------------------------------------------------------

(defun minus-with-dont-care (x y)
  (if (or (eql x 'dont-care) (eql y 'dont-care) 
	  (and (listp x) (eql (car x) 'dont-care)) (and (listp y) (eql (car y) 'dont-care)))
      (if (numberp x) 
	  (list 'dont-care x)
	(if (numberp y)
	    (list 'dont-care y)
	  'dont-care))
    (- x y)))

;; mod the error reporting at the bottom of this function
(defvar last-mds-coordss)  ;; ugly, should at least by *last-mds-coordss*
(defun mds-hi-table (tk mds-f stress-component-f comparison-f 
		     &optional &key 
			       existing-mds-window 
			       new-window-canvas-coord-transformations
			       (num-trials 250) 
			       (num-climbs 100)
			       (incremental-dribble-modulus 1)
			       (starting-coordss (get-mds-coordss  (if existing-mds-window existing-mds-window tk)))
                               (raise-points     (get-raise-points tk))
                               (lower-points     (get-lower-points tk)))
  
  ;; tk is the tk-interface (not the mds-window)

  (if (and new-window-canvas-coord-transformations
	   existing-mds-window)
      (error "should not be passing :new-window-seed-canvas-coord-transformations if not a new window"))
       
  (if (and (eql mds-f 'metric-mds-global-norm-conjugant-gradient)
	   (similarity-table-p (get-hi-table tk)))
      (setq mds-f 'metric-mds-global-norm-conjugant-gradient-hi-metric))
      
  (multiple-value-bind (new-mds-coordss mds-window) 
      (run-mds-from-dists
       (eval (list 'function mds-f))
       (eval (list 'function stress-component-f))
       (if comparison-f
	   (eval (list 'function comparison-f)))
       (hi-table-values-to-base-1-symmetric-array-with-threshold-info (hi-table-values (get-hi-table-working-copy tk)))
       starting-coordss
       :dribble nil
       :visualization-window (let ((visualization-window
				    (if existing-mds-window 
					existing-mds-window
				      (let ((mds-window (make-mds-visualization-window 
							 (get-hi-table-name tk)
							 (if new-window-canvas-coord-transformations
							     (snoop-keyword-arg :canvas-width new-window-canvas-coord-transformations)
							   (get-canvas-width tk))
							 (if new-window-canvas-coord-transformations
							     (snoop-keyword-arg :canvas-height new-window-canvas-coord-transformations)
							   (get-canvas-height tk)))))
					(set-table-window-for-mds-window mds-window tk)
					(set-mds-f-for-mds-window mds-window mds-f)
					(set-stress-component-f-for-mds-window mds-window stress-component-f)
					(set-comparison-f-for-mds-window mds-window comparison-f)
					(set-mds-num-dimensions mds-window (get-mds-num-dimensions tk))
					(set-procrustes-data mds-window (get-procrustes-data tk))
					(set-raise-points    mds-window raise-points)
					(set-lower-points    mds-window lower-points)
					;; canvas data set if passed (typically from an existing mds-window)
					;; or set from the table-window if not passed
					(apply #'set-canvas-coord-transformations 
					 mds-window
					 (if new-window-canvas-coord-transformations
					     new-window-canvas-coord-transformations
					   (get-canvas-coord-transformations tk)))
					mds-window))))
			       (set-mds-window-run-indicator-text visualization-window "Running...")
			       visualization-window)
       :new-or-update? (if existing-mds-window 'update 'new)
       ;;:coords-names  (hi-table-antigens (get-hi-table-working-copy tk))
       ;;:coords-names  (mapcar #'long-strain-abbreviation (hi-table-antigens (get-hi-table-working-copy tk)))
       ;;:coords-names  (mapcar #'smart-strain-abbreviation (hi-table-antigens (get-hi-table-working-copy tk)))
       :coords-names  (get-coords-names-working-copy tk)
       :coords-full-names  (hi-table-antigens (get-hi-table-working-copy tk))
       :coords-colors (get-coords-colors tk)
       :coords-dot-sizes (get-coords-dot-sizes tk)
       :coords-transparencies (get-coords-transparencies tk)
       :coords-name-sizes (get-coords-name-sizes tk)
       :coords-name-colors (get-coords-name-colors tk)
       :coords-shapes (get-coords-shapes tk)
       :show-error-lines (get-show-error-lines tk)
       :moveable-coords (get-moveable-coords tk)
       :unmoveable-coords (get-unmoveable-coords tk)
       :unmoveable-dimensions (get-unmoveable-dimensions tk)
       :unmoveable-dimensions-in-second-phase (get-unmoveable-dimensions-in-second-phase tk)
       :dim-anneal-coefficients (get-dim-anneal-coefficients tk)
       :disconnected-coords (get-disconnected-points tk)
       :adjustable-rows (get-adjustable-rows tk)
       :adjustable-columns (get-adjustable-columns tk)
       :scale-to-fit-mds-window (get-scale-to-fit-mds-window tk)
       ;; :title (format nil "~a:~a:~a" mds-f stress-component-f (if comparison-f comparison-f ""))
       :hillclimbs-args (list :max-trials num-trials 
			      :max-climbs num-climbs 
			      :incremental-dribble-modulus incremental-dribble-modulus))
    ;;(set-table-window-for-mds-window mds-window tk)
    ;;(set-mds-f-for-mds-window mds-window mds-f)
    ;;(set-stress-component-f-for-mds-window mds-window stress-component-f)
    ;;(set-comparison-f-for-mds-window mds-window comparison-f)
    (set-mds-coordss mds-window new-mds-coordss)
    (set-mds-coordss tk new-mds-coordss)
    (if (get-display-errors-in-lower-triangle tk)
	;;(show-table-plot-errors tk new-mds-coordss)   ;; both are usefull, eventually put these into separate windows
	(show-table-predictions tk new-mds-coordss)
      )
    (setq last-mds-coordss new-mds-coordss)  ;; ugly, for picking up coords later
    ;;(sleep 0.08) (print new-mds-coordss)   ;;this is a nasty hack, that i use to get the final coordss, not used
    ;;by anything programatic, i just cut and paste, (get better way)
    (values
     tk
     mds-window)
    ))

(defun distance-to-std-log-titer (distance col-basis row-adjust)
  (- col-basis (+ distance row-adjust)))

(defun show-table-plot-errors (tk new-mds-coordss)
  (set-lower-triangle-distances-tk 
   tk 
   (hi-table-transpose 
    (f-hi-table
     (let* ((ag-sr-table-p (ag-sr-table-p (get-hi-table tk)))
	    (mds-coordss (get-mds-coordss tk))
	    (col-bases (col-bases mds-coordss))
	    (row-adjusts (row-adjusts mds-coordss))
	    (hi-table-working-copy (get-hi-table-working-copy tk)))
       (^ (mds-dist ag-index sr-index)
	  (let ((target-dist (hi-table-value-by-indices hi-table-working-copy ag-index sr-index)))
	    (if (dont-care-p target-dist)
		target-dist
	      (if (zerop target-dist)
		  'dont-care
		;; we used to take a percentage, but i think absolute is better
		(2dp (- 
		      (if ag-sr-table-p
			  (distance-to-std-log-titer 
			   mds-dist
			   (nth sr-index col-bases)
			   (nth ag-index row-adjusts))
			mds-dist)
		      target-dist)))))))
     (coordss-to-hi-table new-mds-coordss)
     :pass-ag-sr-indices t))))

(defun show-table-predictions (tk new-mds-coordss)
  (set-lower-triangle-distances-tk 
   tk 
   (hi-table-transpose 
    (calc-table-prediction-table tk new-mds-coordss))))

(defun set-lower-triangle-distances-tk (tk hi-table)
  (loop for ag-index below (hi-table-length hi-table) do
	(loop for sr-index below ag-index do
	      (set-table-value tk ag-index sr-index 
			       (hi-table-value-by-indices hi-table ag-index sr-index)))))

(defun set-lower-rectangle-distances-tk (tk hi-table)
  (loop for ag-index from (/ (hi-table-length hi-table) 2) below (hi-table-length hi-table) do
	(loop for sr-index below (/ (hi-table-length hi-table) 2) do
	      (set-table-value tk ag-index sr-index 
			       (hi-table-value-by-indices hi-table ag-index sr-index)))))

