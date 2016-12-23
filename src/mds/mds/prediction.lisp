(in-package user)

;;;----------------------------------------------------------------------
;;;                    prediciton from gui
;;;----------------------------------------------------------------------

;; there is a lot of redundancy in these next two functions calc-table-prediction-table and calc-table-prediction-summary
;; they used to be the same function, but i wanted to speed up the prediction summary, and did so by not making
;; the hi tables made in the table summary.

(defun calc-table-prediction-table (table-window new-mds-coordss)
  ;; LINK  changes here are probably also required in calc-table-prediction-summary below  (derek 2001-11-19)
  (f-hi-table
   (let ((hi-table-working-copy (get-hi-table-working-copy table-window))
	 (hi-table-mds-distances (coordss-to-hi-table new-mds-coordss))
	 (col-bases (col-bases new-mds-coordss))
	 (row-adjusts (row-adjusts new-mds-coordss))
	 (ag-sr-table-p (ag-sr-table-p (get-hi-table table-window)))
	 (disconnected-points (get-disconnected-points table-window)))
     (^ (original-target-distance ag-index sr-index)
	(if (and (dont-care-p (hi-table-value-by-indices hi-table-working-copy ag-index sr-index))
		 (not (dont-care-p original-target-distance))
		 (not (or (member ag-index disconnected-points)    ;; when a point is disconnected, we do not 
		 	  (member sr-index disconnected-points)))) ;; want to be predicting it
	    (let ((actual-distance
		   (if ag-sr-table-p
		       (distance-to-std-log-titer 
			(hi-table-value-by-indices hi-table-mds-distances ag-index sr-index)
			(nth sr-index col-bases)
			(nth ag-index row-adjusts))
		     (hi-table-value-by-indices hi-table-mds-distances ag-index sr-index))))
	      (list 'prediction 
		    actual-distance
		    original-target-distance))
	  'dont-care)))
   (get-hi-table table-window)
   :pass-ag-sr-indices t))

(defun calc-table-prediction-summary (table-window new-mds-coordss)
  ;; LINK  changes here are probably also required in calc-table-prediction-table above  (derek 2001-11-19)
  (let (prediction-errors
	(hi-table-original (get-hi-table table-window))
	(hi-table-working-copy (get-hi-table-working-copy table-window))
	(col-bases (col-bases new-mds-coordss))
	(row-adjusts (row-adjusts new-mds-coordss))
	(ag-sr-table-p (ag-sr-table-p (get-hi-table table-window)))
	(disconnected-points (get-disconnected-points table-window)))
    (loop for original-row in (hi-table-values hi-table-original)
	for working-row in (hi-table-values hi-table-working-copy)
	for ag-index from 0 do
	  (loop for original-target-distance in (nthcdr ag-index original-row)
	      for working-value in (nthcdr ag-index working-row)
	      for sr-index from ag-index do
		(if (and (dont-care-p working-value)
			 (not (dont-care-p original-target-distance))
			 (not (or (member ag-index disconnected-points)  
			 	  (member sr-index disconnected-points))))
		    (let* ((mds-distance (e-dist (nth ag-index new-mds-coordss) (nth sr-index new-mds-coordss)))
			   (actual-distance
			    (if ag-sr-table-p
				(distance-to-std-log-titer 
				 mds-distance
				 (nth sr-index col-bases)
				 (nth ag-index row-adjusts))
			      mds-distance)))
		      (push (- original-target-distance actual-distance) prediction-errors)))))
    prediction-errors))

(defun generate-prediction-report-string (prediction-errors hi-table-num-values)
  (format nil "~s"
	  (if (null prediction-errors)
	      ;;"no predictions"
	      ""
	    (format nil "Predicting ~d of ~d values (~d%): av ~4,2f   sd ~4,2f   sum-sq ~4,2f" 
		    (length prediction-errors)
		    hi-table-num-values
		    (round (%age (length prediction-errors) hi-table-num-values))
		    (av (mapcar #'abs prediction-errors))
		    (sd prediction-errors)
		    (apply-+ (mapcar #'square prediction-errors))))))

(defun show-table-prediction-summary-in-mds-window (mds-window new-mds-coordss)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (tk-set-text-text-always-show
     mds-window
     (get-hi-table-prediction-error-id mds-window)
     (generate-prediction-report-string 
      (calc-table-prediction-summary table-window new-mds-coordss)
      (hi-table-num-values (get-hi-table table-window))))))


;;;----------------------------------------------------------------------
;;;                      output predictions
;;;----------------------------------------------------------------------

(defvar *last-predictions*)

(defun output-predictions (mds-window filename)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (format out ";; MDS predictions for ~a~%;; This file created on ~a~%;; Format: Name-A   Name-B   Target-A-B    Prediction-of-A-B    Difference~%~%"
	    (hi-table-name (get-hi-table (get-table-window-for-mds-window mds-window)))
	    (time-and-date))
    (let ((hi-table-predictions (setq *last-predictions* (hi-table-predictions mds-window))))
      (if (ag-sr-table-p (get-hi-table (get-table-window-for-mds-window mds-window)))
	  (loop for serum-group in (group-hi-table-predictions-by-sera 
				    hi-table-predictions
				    (collect #'sr-name-p (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window))))) do
		(fll serum-group :stream out))
	(fll hi-table-predictions :stream out)))))

(defun hi-table-predictions (mds-window)
  (let ((hi-table (calc-table-prediction-table
		   (get-table-window-for-mds-window mds-window)
		   (get-mds-coordss mds-window))))
    (loop for row in (hi-table-values hi-table)
	for antigen in (hi-table-antigens hi-table) append
	  (loop for e in row 
	      for serum in (hi-table-sera hi-table) 
	      when (prediction-p e)
	      collect (list antigen serum 
			    ;;(nth 2 e)  ;; actual measurement
			    ;;(coerce (nth 1 e) 'single-float)  ;; mds measurement
			    ;;(coerce (- (nth 2 e) (nth 1 e)) 'single-float)  ;; difference
			    (dps (nth 2 e) 3)  ;; actual measurement
			    (dps (nth 1 e) 3)  ;; mds measurement
			    (dps (- (nth 2 e) (nth 1 e)) 3)  ;; difference
			    )))))

(defun group-hi-table-predictions-by-sera (hi-table-predictions sera-names)
  (loop for serum in sera-names collect
	(collect (^ (prediction) (member serum prediction)) hi-table-predictions)))
		       
(defun last-predictions-scatter-plot ()
  (gnuplot-correlation
   (transpose (nths 3 *last-predictions*) (nths 2 *last-predictions*))
   :x-title "Predicted titer (in standard logs)"
   :y-title "Acutal titer (in standard logs)"
   :x-min 0 :y-min 0
   :x-max (inc (ceiling (apply-max (append (nths 2 *last-predictions*) (nths 3 *last-predictions*)))))
   :y-max (inc (ceiling (apply-max (append (nths 2 *last-predictions*) (nths 3 *last-predictions*)))))))



;;;----------------------------------------------------------------------
;;;                     testing batch predictions 
;;;----------------------------------------------------------------------

(defun predictions-from-matched-tables (master-table prediction-table predicted-coordss)
  ;; a postive  prediction error indicates the predicted distance is shorter than it should be for a distance matrix, but longer for a similarity matrix
  ;; a negative prediction error indicates the predicted distance is longer than it should be for a distance matrix, but shorter for a similarity matrix
  (let (prediction-errors
	prediction-errors-details
	(col-bases (col-bases predicted-coordss))
	(row-adjusts (row-adjusts predicted-coordss))
	(ag-sr-table-p (ag-sr-table-p prediction-table))
	(table-antigens (hi-table-antigens master-table))
	(table-sera     (hi-table-sera     master-table)))
    (loop for master-row in (hi-table-values master-table)
	for prediction-row in (hi-table-values prediction-table)
	for ag-index from 0 do
	  (loop for master-target-distance in (nthcdr ag-index master-row)
	      for prediction-value in (nthcdr ag-index prediction-row)
	      for sr-index from ag-index do
		(if (and (true-dont-care-p prediction-value)
			 (not (true-dont-care-p master-target-distance)))
		    (let* ((mds-distance (e-dist (nth ag-index predicted-coordss) (nth sr-index predicted-coordss)))
			   (actual-distance
			    (if ag-sr-table-p
				(distance-to-std-log-titer 
				 mds-distance
				 (nth sr-index col-bases)
				 (nth ag-index row-adjusts))
			      mds-distance))
			   (prediction-error (if (thresholdp master-target-distance)
						 'non-because-master-is-threshold
					       (- master-target-distance actual-distance))))
		      (if (not (thresholdp master-target-distance))
			  (push prediction-error prediction-errors))
		      (push (list (nth ag-index table-antigens)
				  (nth sr-index table-sera)
				  prediction-error
				  master-target-distance
				  actual-distance)
			    prediction-errors-details)))))
    (values
     (reverse prediction-errors)
     (reverse prediction-errors-details)
     (av (mapcar #'abs prediction-errors))
     (sd prediction-errors)
     (correlation (mapcar #'cdddr (filter (^ (l) (eql 'non-because-master-is-threshold (nth 2 l))) prediction-errors-details))))))

(defun predictions-from-tables (master-table prediction-table predicted-coordss)
  (if (not (and (equal (hi-table-antigens master-table)     (hi-table-sera master-table))
		(equal (hi-table-antigens prediction-table) (hi-table-sera prediction-table))))
      (error "expected symmetric master and prediction tables"))
  (let* ((prediction-antigens (hi-table-antigens prediction-table))
	 (intersection-names (reverse (intersection (hi-table-antigens master-table) prediction-antigens)))
	 (master-table-intersection     (extract-hi-table master-table     intersection-names intersection-names))
	 (prediction-table-intersection (extract-hi-table prediction-table intersection-names intersection-names))
	 (intersection-prediction-indices (loop for name in intersection-names collect (position name prediction-antigens)))
	 (predicted-coordss-intersection (coordss-plus-more-subset predicted-coordss intersection-prediction-indices)))
    (predictions-from-matched-tables master-table-intersection prediction-table-intersection predicted-coordss-intersection)))

(defun predictions-of-master-table-from-prediction-save (master-table prediction-save)
  (predictions-from-tables master-table (table-from-save prediction-save) (starting-coordss-from-save prediction-save)))

(defun predictions-from-saves (master-save prediction-save)
  (predictions-from-tables (table-from-save master-save) (table-from-save prediction-save) (starting-coordss-from-save prediction-save)))


;;;----------------------------------------------------------------------
;;;    the predicted HI table from table and coordss, or from save
;;;----------------------------------------------------------------------

(defun predicted-table-from-save (save)
  (predicted-table-from-table-and-coordss (table-from-save save) (coordss-from-save save)))

(defun make-proportion-of-table-values-dont-cares (table proportion)
  (f-hi-table
   (^ (e)
      (if (true-dont-care-p e)
	  e
	(if (< (knuth-random) proportion)
	    'dont-care
	  e)))
   table))

 ;;;----------------------------------------------------------------------
;;;                    making prediction tables
;;;----------------------------------------------------------------------

(defun make-similarity-table-from-distance-table (distance-table &key col-bases row-adjusts)
  (if row-adjusts
      (if (not (equal '(t) (remove-duplicates (mapcar #'zerop row-adjusts))))
	  (error "Don't know what to do with row adjusts yet, whether to add them on before or after column conversion")))
  (make-hi-table
   (hi-table-antigens distance-table)
   (hi-table-sera     distance-table)
   (let ((distances (hi-table-values distance-table)))
     (loop for row in distances collect
	   (loop for x in row for col-basis in col-bases collect
		 (- col-basis x))))))

(defun make-predicted-hi-table (coordss-plus-more square-ag-sr-names 
				&key (unlog-f #'log-to-std-titer)
				     (round-to-2-fold-titers t)
				     (lt-threshold '(10 <10)))   ;; the numeric threshold, and the symbol
  (let* ((coordss (coordss coordss-plus-more))
	 (row-adjusts (row-adjusts coordss-plus-more))
	 (col-bases (col-bases coordss-plus-more))
	 (short-ag-names (mapcar #'remove-ag-sr-from-name (collect #'ag-name-p square-ag-sr-names)))
	 (short-sr-names (mapcar #'remove-ag-sr-from-name (collect #'sr-name-p square-ag-sr-names)))
	 (num-antigens (length short-ag-names))
	 (short-row-adjusts (firstn num-antigens row-adjusts))
	 (short-col-bases (nthcdr num-antigens col-bases))
	 (distance-table (make-mock-distance-hi-table
			  (firstn num-antigens coordss)
			  (nthcdr num-antigens coordss)
			  :antigen-names short-ag-names
			  :sera-names    short-sr-names))
	 (similarity-table (make-similarity-table-from-distance-table
			      distance-table
			      :col-bases short-col-bases
			      :row-adjusts short-row-adjusts))
	 (maybe-rounded-similarity-table (if round-to-2-fold-titers 
					     (f-hi-table #'round similarity-table)
					   similarity-table))
	 (hi-table (f-hi-table unlog-f maybe-rounded-similarity-table))
	 (hi-table-maybe-thresholded (if lt-threshold
					 (f-hi-table
					  (^ (x)
					     (if (< x (nth 0 lt-threshold))
						 (nth 1 lt-threshold)
					       x))
					  hi-table)
				       hi-table)))
    hi-table-maybe-thresholded))
						 
(defun make-predicted-hi-table-from-save (save 
					  &optional &key (unlog-f #'log-to-std-titer)
						    (round-to-2-fold-titers t)
						    (lt-threshold '(10 <10)))   ;; the numeric threshold, and the symbol
  (make-predicted-hi-table 
   (starting-coordss-from-save save)
   (hi-table-antigens (table-from-save save))
   :unlog-f unlog-f
   :round-to-2-fold-titers round-to-2-fold-titers
   :lt-threshold lt-threshold))


;;;----------------------------------------------------------------------
;;;   show gui prediction errors from master-table and prediction-save
;;;----------------------------------------------------------------------

(defun modify-prediction-save-with-master-table (master-table prediction-save)
  ;; set the save working hi table to be the existing save's table,
  ;; and augmement the saves table with the master table, replacing
  ;; ag-sr locations they have in common with titers from the master table
  (let* (replacements
	 (table-from-save (table-from-save prediction-save))
	 (master-table-antigens (hi-table-antigens master-table))
	 (master-table-sera     (hi-table-sera     master-table))
	 (modified-table (f-hi-table
			  (^ (titer ag sr)
			     (if (and (member ag master-table-antigens)
				      (member sr master-table-sera))
				 (let ((master-table-titer (hi-table-value master-table ag sr :hi-table-sera-efficiency-hack master-table-sera)))
				   (if (not (true-dont-care-p master-table-titer))
				       (progn
					 (push (list ag sr titer master-table-titer) replacements)
					 master-table-titer)
				     titer))
			       titer))
			  table-from-save
			  :pass-ag-sr t)))
    (values 
     (set-table-in-save
      (set-save-keyword-entry prediction-save :hi-table-working-copy table-from-save :not-found-action :add)
      modified-table)
     replacements)))


;;;----------------------------------------------------------------------
;;;                      making partial tables
;;;----------------------------------------------------------------------

(defun replace-titers-in-table-from-table (existing-table new-table)
  (let ((new-table-antigens (hi-table-antigens new-table))
	(new-table-sera     (hi-table-sera new-table)))    ;; for efficiency reasons
    (f-hi-table
     (^ (existing-table-titer ag sr)
	(let ((new-table-titer (if (and (member ag new-table-antigens)
					(member sr new-table-sera))
				   (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
	  (if new-table-titer
	      new-table-titer
	    existing-table-titer)))
     existing-table
     :pass-ag-sr t)))
  
#|
(setq existing
  (make-hi-table
   '(a b c)
   '(x y z)
   '((0 dont-care 2)
     (3 4 5)
     (6 7 <8))))

(setq new
  (make-hi-table
   '(a c)
   '(x y)
   '((10 11)
     (13 <14))))

(replace-titers-in-table-from-table existing new)

          X      Y      Z 
A        10     11      2 
B         3      4      5 
C        13    <14     <8 

|#



;; --------------------- fill in a percentage of the dont-cares ------------------------

(defvar *prediction-random-number-generator*)
(setq *prediction-random-number-generator* 7)

(defun fill-in-dont-cares-in-table-from-table (existing-table new-table proportion &optional &key seed)
  ;; the proportion we specify is the ending proportion of titers covered by the ag and sr in the new-table
  (if seed
      (seed-random seed *prediction-random-number-generator*))
  (let* ((new-table-antigens (hi-table-antigens new-table))
	 (new-table-sera     (hi-table-sera new-table)))
    (if (or (set-difference new-table-antigens (hi-table-antigens existing-table))
	    (set-difference new-table-sera     (hi-table-sera     existing-table)))
	(error "To make things easy, we assume the new table is an ag-sr subset of the existing table, this assumption is violated"))
    (let* ((numExistingTiters
	    (length (filter #'null (flatten
				    (hi-table-values
				     (f-hi-table
				      (^ (existing-table-titer ag sr)
					 (and (not (true-dont-care-p existing-table-titer))
					      (member ag new-table-antigens)
					      (member sr new-table-sera)
					      (not (true-dont-care-p
						    (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera)))))
				      existing-table
				      :pass-ag-sr t))))))
	   (numPossibleAdditions
	    (length (filter #'null (flatten
				    (hi-table-values
				     (f-hi-table
				      (^ (existing-table-titer ag sr)
					 (if (true-dont-care-p existing-table-titer)
					     (let ((new-table-titer (if (and (member ag new-table-antigens)
									     (member sr new-table-sera))
									(hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
					       (and new-table-titer
						    ;;(not (true-dont-care-p new-table-titer))  ;; this makes it just the non-dont-cares
						    ))))
				      existing-table
				      :pass-ag-sr t))))))
	   (proportion-of-new-titers (/ (- (* proportion 
					      (+ numExistingTiters numPossibleAdditions))
					   numExistingTiters)
					numPossibleAdditions))
	   (new-table (f-hi-table
		       (^ (existing-table-titer ag sr)
			  (let ((new-table-titer (if (and (member ag new-table-antigens)
							  (member sr new-table-sera))
						     (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
			    (if (and new-table-titer
				     (true-dont-care-p existing-table-titer))
				(if (< proportion-of-new-titers (knuth-random *prediction-random-number-generator*))
				    new-table-titer
				  existing-table-titer)
			      existing-table-titer)))
		       existing-table
		       :pass-ag-sr t)))
      (values new-table
	      numExistingTiters
	      numPossibleAdditions
	      proportion-of-new-titers
	      seed))))
	



#|
100% should be the exisiting plus new table titers, not 100% of all titers

we ask for some proportion of all to be filled in.
we already have some initial proportion filled in.
what proportion of all remaining should be filled in?

  [ numExistingTiters + (newProportion * numPossibleAdditions) ] / (numExisitingTiters + numPossibleAdditions) = proportion

  newProportion = { [ proportion * (numExisitingTiters + numPossibleAdditions) ] - numExisitingTiters } / numPossibleAdditions


check: let's say we have 60% currently (60 of 100), and we want 70%, then we add what proportion of the remaining 40: 25%

    ( (.7 * 100) - 60) / 40
    70 -60 / 40
    10 / 40 
    0.25
    correct

another example (try this one computationally below)
  say we currently have 1/4, and all 3 more are possible, and we want 100% then 100%  -- yes
  say we currently have 1/4, and all 3 more are possible, and we want 50% then 1/3    -- yes
  say we currently have 1/4, and all 3 more are possible, and we want 75% then 2/3    -- yes


(setq existing
  (make-hi-table
   '(a b c)
   '(x y z)
   '((0 dont-care 2)
     (dont-care 4 5)
     (dont-care dont-care <8))))

(setq new
  (make-hi-table
   '(a c)
   '(x y)
   '((10 11)
     (13 <14))))

(fill-in-dont-cares-in-table-from-table existing new 0.75 :seed 467739585)
((A 0 DONT-CARE 2) (B DONT-CARE 4 5) (C DONT-CARE DONT-CARE <8))
1
3
0.6666667
467739585


;;---------------------------- a larger test ---------------------------

(pp-hi-table
 (setq existing-large
   (make-hi-table
    (series 0 9)
    (series 0 9)
    (loop for ag below 10 collect
	  (loop for sr below 10 collect
		(if (and (< ag 5)
			 (< sr 5))
		    (if (bit->bool (coin))
			'<10
		      20)
		  'dont-care))))))

              0      1      2      3      4      5      6      7      8      9 
0           <10    <10     20    <10    <10      .      .      .      .      . 
1           <10     20    <10     20    <10      .      .      .      .      . 
2           <10    <10     20    <10    <10      .      .      .      .      . 
3           <10     20     20    <10     20      .      .      .      .      . 
4            20     20     20     20     20      .      .      .      .      . 
5             .      .      .      .      .      .      .      .      .      . 
6             .      .      .      .      .      .      .      .      .      . 
7             .      .      .      .      .      .      .      .      .      . 
8             .      .      .      .      .      .      .      .      .      . 
9             .      .      .      .      .      .      .      .      .      . 


(pp-hi-table
 (setq new-large
   (make-hi-table
    (series 0 9)
    (series 5 9)
    (loop for ag below 10 collect
	  (loop for sr from 5 below 10 collect
		(if (bit->bool (coin))
		    '<100
		  200))))))

                                    5      6      7      8      9 
0                                 200   <100   <100   <100    200 
1                                 200   <100   <100    200    200 
2                                 200    200    200    200    200 
3                                 200    200   <100   <100   <100 
4                                <100   <100   <100    200   <100 
5                                <100   <100   <100    200    200 
6                                <100    200    200   <100   <100 
7                                <100    200    200    200    200 
8                                 200    200    200   <100    200 
9                                <100    200    200    200    200 


(fill-in-dont-cares-in-table-from-table existing-large new-large 0.75 :seed 467739585)


;; now some overlap
(pp-hi-table
 (setq new-large-overlap
   (make-hi-table
    (series 0 9)
    (series 0 4)
    (loop for ag below 10 collect
	  (loop for sr from 5 below 10 collect
		(if (bit->bool (coin))
		    '<100
		  200))))))

              0      1      2      3      4 
0          <100   <100   <100    200   <100 
1          <100    200   <100    200    200 
2           200   <100   <100    200   <100 
3           200   <100    200   <100   <100 
4          <100    200    200    200    200 
5           200    200   <100   <100   <100 
6          <100    200    200   <100   <100 
7           200   <100    200   <100    200 
8           200    200   <100    200   <100 
9          <100   <100    200   <100    200 


(fill-in-dont-cares-in-table-from-table existing-large new-large-overlap 0.75 :seed 467739585)
25
25
0.5

(length (filter #'true-dont-care-p (flatten (hi-table-values **))))
40
USER(73): (loop for i below 100 collect (length (filter #'true-dont-care-p (flatten (hi-table-values (fill-in-dont-cares-in-table-from-table existing-large new-large-overlap 0.75 :seed (+ i 467739585)))))))
(40 42 41 37 39 37 34 36 39 40 ...)
USER(74): (gnuplot (sort-hist *))
USER(75): (av **)
37.33
USER(76): (* 0.75 50)
37.5


maybe we want exactly the proportion, not the expected number
yes, that would be best


|#




(defun fill-in-dont-cares-in-table-from-table-exact-proportion (existing-table new-table proportion &optional &key seed replace-existing-titers)
  ;; the proportion we specify is the ending proportion of titers in the new table that for ag and sr that are also in the existing table
  (if seed
      (seed-random seed *prediction-random-number-generator*))
  (let* ((new-table-antigens (hi-table-antigens new-table))
	 (new-table-sera     (hi-table-sera new-table))
	 (existing-table-antigens (hi-table-antigens existing-table))
	 (existing-table-sera     (hi-table-sera existing-table)))
    (if (or (set-difference new-table-antigens (hi-table-antigens existing-table))
	    (set-difference new-table-sera     (hi-table-sera     existing-table)))
	(error "To make things easy, we assume the new table is an ag-sr subset of the existing table, this assumption is violated.  new table has these extra antigens ~a, and these extra sera ~a"
	       (reverse (set-difference new-table-antigens (hi-table-antigens existing-table)))
	       (reverse (set-difference new-table-sera     (hi-table-sera     existing-table)))	
	       ))
    (let* ((numExistingTiters
	    (length (filter #'null (flatten
				    (hi-table-values
				     (f-hi-table
				      (^ (existing-table-titer ag sr)
					 (and (not (true-dont-care-p existing-table-titer))
					      (member ag new-table-antigens)
					      (member sr new-table-sera)
					      (not (true-dont-care-p
						    (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera)))))
				      existing-table
				      :pass-ag-sr t))))))
	   (possibleAdditionNonTrueDontCareIndicesInExistingTable
	    (filter #'null 
		    (apply-append
			   (hi-table-values
			    (f-hi-table
			     (^ (existing-table-titer ag sr)
				(if (true-dont-care-p existing-table-titer)
				    (let ((new-table-titer (if (and (member ag new-table-antigens)
								    (member sr new-table-sera))
							       (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
				      (if (and new-table-titer
					       (not (true-dont-care-p new-table-titer)))
					  (list (position ag existing-table-antigens)
						(position sr existing-table-sera)
						new-table-titer)))))
			     existing-table
			     :pass-ag-sr t)))))
	   (replacementTitersIndicesInExistingTable
	    (filter #'null 
		    (apply-append
			   (hi-table-values
			    (f-hi-table
			     (^ (existing-table-titer ag sr)
				(let ((new-table-titer (if (and (member ag new-table-antigens)
								(member sr new-table-sera))
							   (hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
				  (if (and new-table-titer
					   (not (true-dont-care-p new-table-titer))
					   (not (true-dont-care-p existing-table-titer)))
				      (list (position ag existing-table-antigens)
					    (position sr existing-table-sera)
					    new-table-titer))))
			     existing-table
			     :pass-ag-sr t)))))
	   (numPossibleAdditionNonTrueDontCareIndicesInExistingTable (length possibleAdditionNonTrueDontCareIndicesInExistingTable))
	   (numPossibleAdditions
	    (length (filter #'null (flatten
				    (hi-table-values
				     (f-hi-table
				      (^ (existing-table-titer ag sr)
					 (if (true-dont-care-p existing-table-titer)
					     (let ((new-table-titer (if (and (member ag new-table-antigens)
									     (member sr new-table-sera))
									(hi-table-value new-table ag sr :hi-table-sera-efficiency-hack new-table-sera))))
					       (and new-table-titer
						    ;;(not (true-dont-care-p new-table-titer))  ;; this makes it just the non-dont-cares
						    ))))
				      existing-table
				      :pass-ag-sr t))))))
	   (proportion-of-new-titers (/ (- (* proportion 
					      (+ numExistingTiters numPossibleAdditions))
					   numExistingTiters)
					numPossibleAdditions))
	   (num-new-titers (round (* proportion-of-new-titers numPossibleAdditionNonTrueDontCareIndicesInExistingTable)))
	   (new-titers-with-indices (random-samples-without-replacement
				     (max 0 num-new-titers)  ;; so we can get non
				     PossibleAdditionNonTrueDontCareIndicesInExistingTable
				     *prediction-random-number-generator*))
	   (new-table (f-hi-table
		       (^ (existing-table-titer ag-index sr-index)
			  (let ((new-titer
				 (nth 2 (car (member (list ag-index sr-index) new-titers-with-indices :test (^ (a b) (equal a (firstn 2 b))))))))
			    (if new-titer
				new-titer
			      (if replace-existing-titers
				  (let ((replacement-titer
					 (nth 2 (car (member (list ag-index sr-index) replacementTitersIndicesInExistingTable
							     :test (^ (a b) (equal a (firstn 2 b))))))))
				    (if (true-dont-care-p existing-table-titer)
					(if replacement-titer
					    (error "assumption violation: should not be a titer here")
					  existing-table-titer)
				      (if replacement-titer
					  replacement-titer
					existing-table-titer)))
				existing-table-titer))))
		       existing-table
		       :pass-ag-sr-indices
		       t)))
      (values new-table
	      numExistingTiters
	      numPossibleAdditions
	      possibleAdditionNonTrueDontCareIndicesInExistingTable
	      numPossibleAdditionNonTrueDontCareIndicesInExistingTable
	      proportion-of-new-titers
	      num-new-titers
	      new-titers-with-indices
	      seed))))

#|
(fill-in-dont-cares-in-table-from-table-exact-proportion existing-large new-large-overlap 0.75 :seed 467739585)

(pp-hi-table
 (setq new-large-overlap
   (make-hi-table
    (series 0 9)
    (series 0 4)
    (loop for ag below 10 collect
	  (loop for sr from 5 below 10 collect
		(if (bit->bool (coin))
		    '<100
		  200))))))

;; ------------------------------------ 

|#

;;;----------------------------------------------------------------------
;;;         specific code for making tables from seq-t6 
;;;----------------------------------------------------------------------

(defvar *jo-wu-extract*)
(setq *jo-wu-extract*
  (MAKE-HI-TABLE '(BE/353/89 SH/24/90 NL/938/92 SE/C273/92 ST/12/92 AK/4/93 GD/25/93 LY/672/93 LY/1815/93 LY/22686/93 LY/23602/93 MA/G109/93
		   MA/G122/93 MA/G130/93 MA/G252/93 NL/101/93 NL/115/93 NL/126/93 NL/357/93 NL/371/93 NL/372/93 NL/398/93 NL/399/93 NL/440/93
		   OS/2219/93 OS/2352/93 SG/6/93 SL/142/93 SL/160/93 SP/3/93 SP/19/93 ST/20/93 VI/104/93 WE/59/93 YA/56/93 YA/61/93 YA/62/93 NL/3/93
		   EN/7/94 HK/1/94 HK/2/94 HK/55/94 HK/56/94 JO/47/94 SA/15/94 SA/25/94 NL/18/94 NL/226/95 NL/271/95 FI/338/95 FI/339/95 FI/381/95
		   GE/9846/95 GE/298971/95 GE/A9509/95 HK/3/95 HK/32/95 HK/38/95 HK/49/95 HK/55/95 NA/933/95 NL/47/95 ST/506/95 VI/75/95 NL/91/96
		   BR/8/96 GE/3958/96 HK/20/96 HK/42/96 HK/357/96 HK/358/96 HK/434/96 LY/1781/96 SP/1/96 HK/1/97 JO/10/97 OS/21/97 OS/244/97 SY/5/97
		   NL/5/98)
		 '(HK/34/90 BE/32A/92 GD/25/93 SD/9/93 NL/18/94 JO/33/94 LY/2279/95 FI/338/95 NL/172/96)
		 '((211 14 <10 DONT-CARE <10 <10 <10 DONT-CARE <10) (10240 320 DONT-CARE 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (5120 403 DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (80 DONT-CARE 1171 DONT-CARE 88 DONT-CARE 240 DONT-CARE 136) (243 231 DONT-CARE 691 DONT-CARE 172 DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (640 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (640 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (57 67 320 197 DONT-CARE 320 DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 2560 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (640 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (80 160 DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (80 40 DONT-CARE 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (2560 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (115 160 DONT-CARE 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (640 1109 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (160 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (358 403 58 905 DONT-CARE 345 DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (1280 DONT-CARE DONT-CARE 905 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (160 DONT-CARE DONT-CARE 453 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 DONT-CARE 2560 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (80 98 DONT-CARE 320 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (403 462 DONT-CARE 1810 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (196 392 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 640 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE 2560 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (80 DONT-CARE DONT-CARE 160 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (160 160 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 160 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 2560 1280 DONT-CARE 2560 DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 DONT-CARE 1280 640 DONT-CARE 2560 DONT-CARE DONT-CARE DONT-CARE)
		   (320 320 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE)
		   (320 160 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE) (DONT-CARE DONT-CARE 315 <10 500 250 960 DONT-CARE 190)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 213 DONT-CARE 320 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 226 DONT-CARE 640 DONT-CARE)
		   (160 DONT-CARE DONT-CARE 160 DONT-CARE 279 320 1280 DONT-CARE) (DONT-CARE DONT-CARE DONT-CARE 80 DONT-CARE 453 640 640 DONT-CARE)
		   (DONT-CARE DONT-CARE 1280 DONT-CARE DONT-CARE 474 640 613 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 1810 DONT-CARE 1280 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 640 DONT-CARE 640 DONT-CARE)
		   (160 DONT-CARE 640 160 DONT-CARE 359 226 640 DONT-CARE) (DONT-CARE DONT-CARE 960 1810 DONT-CARE 1280 DONT-CARE DONT-CARE DONT-CARE)
		   (320 DONT-CARE 3620 1810 DONT-CARE 5120 DONT-CARE DONT-CARE DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 1280 DONT-CARE 1280 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 960 DONT-CARE 960 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 292 485 533 1114) (DONT-CARE DONT-CARE 72 40 92 204 320 533 832)
		   (DONT-CARE DONT-CARE 466 DONT-CARE 1040 407 1707 640 1040) (80 DONT-CARE 905 338 DONT-CARE 708 DONT-CARE 1280 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 588 DONT-CARE 560 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 1280 DONT-CARE 1280 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 61 170 53 962)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 553 640 896 1202)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 320 640 640 1218)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 1114 267 1280 93)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 453 DONT-CARE 640 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 226 DONT-CARE 240 DONT-CARE)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 905 160 320 61)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 640 DONT-CARE 640) (DONT-CARE DONT-CARE 70 35 180 197 433 160 596)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 504 DONT-CARE 1012)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 320 DONT-CARE 453)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 320 DONT-CARE 453)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 320 DONT-CARE 640)
		   (DONT-CARE DONT-CARE <10 <10 <10 <10 40 DONT-CARE 117)
		   (DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 320 DONT-CARE 454))
		 'JO-WU-EXTRACT))


(defun make-jo-wu-prediction-tables (new-table-filename &optional &key (replace-existing-titers t))
  (let ((new-table (hi-table-ferret-to-strain (read-hi-table new-table-filename))))
    (write-save-form 
     (make-save-form 
      :hi-table (fill-in-dont-cares-in-table-from-table-exact-proportion
		 *jo-wu-extract*  ;;*seq-t6*
		 new-table
		 0
		 :replace-existing-titers
		 replace-existing-titers))
     (print (format nil "~a-base.save" new-table-filename)))
    (loop for iteration below 10
	for seed in '(301711548 173873092 212754556 445819491 425264684 460911832 117639161 318721346 319640610 279308484) do
	  (loop for proportion in '(0.40 0.50 0.60 0.70 0.80 0.90 0.95 0.99) do
		(write-save-form
		 (make-save-form 
		  :hi-table (fill-in-dont-cares-in-table-from-table-exact-proportion 
			     *jo-wu-extract*  ;; *seq-t6* 
			     new-table
			     proportion 
			     :seed seed
			     :replace-existing-titers replace-existing-titers))
		 (print (format nil "~a-set-~d-~d.save" new-table-filename iteration (round (* proportion 100)))))))))

#|
(setq fake-table
  (make-hi-table
   (reverse (set-difference antigens '(NL/438/93 HK/33/94)))
   sera
   (loop for i in (reverse (set-difference antigens '(NL/438/93 HK/33/94))) collect
	 (loop for j in sera collect
	       '999))))

(pp-hi-table fake-table 'full)
and write to "/tmp/fake-table"
   
(make-jo-wu-prediction-tables "/tmp/fake-table")
(make-jo-wu-prediction-tables "mds/investigations/hi-prediction-from-hi/alt-fake-table")

(float (/ (length (filter #'true-dont-care-p (apply #'append (hi-table-values (un-asl-hi-table (table-from-save (fi-in "/tmp/alt-fake-table-set-0-80.save" ))))))) 720))


|#
