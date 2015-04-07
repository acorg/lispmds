(in-package user)

;;;----------------------------------------------------------------------
;;;                error array, show errors graphically
;;;----------------------------------------------------------------------

;; THIS SHOULD BE FULLY CHECKED OUT AFTER THE NEW THRESHOLD STUFF WAS ADDED (2002-04-20)

(defun calculate-errors (hi-table coordss coordss-more)
  (let* ((similarity-table-p (similarity-table-p hi-table))
	 (table-values (hi-table-values hi-table))
	 (col-bases (extract-col-bases-from-coordss coordss-more))
	 (row-adjusts (extract-row-adjusts-from-coordss coordss-more)))
    (loop for i from 0 for i-coords in coordss collect
	  (loop for j from 0 for j-coords in coordss collect
		(let ((target-table-value   ;; (pick out of upper triangle)
		       (nth (max i j) (nth (min i j) table-values)))
		      (target-col-base (nth (max i j) col-bases))
		      (target-row-adjust (nth (min i j) row-adjusts))
		      (actual-distance (e-dist i-coords j-coords)))
		  (if (true-dont-care-p target-table-value)
		      target-table-value   ;; dont-care
		    (let ((target-distance 
			   (if similarity-table-p
			       (if (thresholdp target-table-value)
				   ;; note the dec below.  LINK with error-line-endpoint, stress-component, the master error line fucntion
				   ;; and metric-mds-conjugant-gradient
				   (adjust-table-value (dec (threshold-number target-table-value)) 
						       target-col-base target-row-adjust)
				 (adjust-table-value target-table-value target-col-base target-row-adjust))
			     (if (thresholdp target-table-value)
				 (error "assumption violation, did not expect a threshold value in a non-ag-sr-table")
			       target-table-value))))
		      (if (thresholdp target-table-value)
			  (if (> actual-distance target-distance)
			      0   ;; put a 0 here to not show when <threshold (they can only be red, "too close")
			    (- actual-distance target-col-base))  ;; will be negative distance, which shows up as too close
			(- actual-distance target-distance)))))))))


(defun plus-one-to-minus-one-onto-color-scale (x)
  ;; x is in range -1 to +1
  ;; put hue into range 0 to 0.6666667, red to blue thru green
  (let ((hue (* (/ 0.666667 2) (+ x 1))))
    (hsv-tk-color hue 1.0 1.0)))



(defun make-tk-error-display (errors &key box-size)
  (let ((tk (tk-open)))
    (tk-put tk "canvas .c -width ~d -height ~d" (* box-size (+ 2 (length (car errors)))) (* box-size (+ 2 (length errors))))
    (tk-put tk "pack .c")
    (loop for row in errors for y from box-size by box-size do
	  (loop for entry in row for x from box-size by box-size do
		(tk-put tk ".c create rect ~d ~d ~d ~d -fill ~a -outline {}"
			x y (+ x box-size) (+ y box-size)
			(plus-one-to-minus-one-onto-color-scale entry))))
    tk))




#|
                         TESTING

(progn
			 
  (setq hamming-similarity-lte25-305ags-sorted-batch-runs
    (fi-in "~/mds/investigations/sequence-mds/hamming-similarity-lte25-305ags-sorted-batch-runs.lisp"))


  (setq 315-sequences-raw (fi-in-s "~/mds/data/all-seq/2001-06/315PRO.fas.lisp"))

  (setq 315-sequences 
    (mapcar (^ (l) (list (nth 0 l) (explode-symbol (nth 1 l)))) 315-sequences-raw))

  (setq 305-sequences 
    (filter (^ (l) (member 'x (nth 1 l))) 315-sequences))

  (setq hamming
    (make-hi-table
     (nths 0 305-sequences)
     (nths 0 305-sequences)
     (all-comparisons-square (nths 1 305-sequences) #'hd)
     'hamming))

  (setq hamming-similarity-lte25
    (f-hi-table
     (^ (x)
	(if (> x 25)
	    '<10
	  (- 25 x)))
     hamming))
  )


(setq foo
  (multiple-value-bind (coordss adjusts)
      (deconstruct-coordss-plus-more 
       (car (nth 0 hamming-similarity-lte25-305ags-sorted-batch-runs)))
    (calculate-errors hamming-similarity-lte25 coordss adjusts)))

(make-tk-error-display
 (put-in-range foo -1 1)
 :box-size 2)


(make-tk-error-display
 (f-lists
  (^ (x)
     (if (< x -1)
	 -1
       (if (> x 1)
	   1
	 x)))
  (put-in-range foo -2 2))
 :box-size 2)

(make-tk-error-display
 (f-lists
  (^ (x)
     (if (< x -1)
	 -1
       (if (> x 1)
	   1
	 x)))
  (put-in-range foo -2 2))
 :box-size 2)


(make-tk-error-display
 (leave-zero-make-max-absolute-value foo 1)
 :box-size 2)

(make-tk-error-display
 (f-lists
  (^ (x)
     (if (< x -1)
	 -1
       (if (> x 1)
	   1
	 x)))
  (leave-zero-make-max-absolute-value foo 2))
 :box-size 2)
 
|#
