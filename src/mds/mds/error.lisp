(in-package user)

(defun errors-from-table-and-coordss (table coordss)
  (let (errors
	errors-details
	(col-bases (col-bases coordss))
	(row-adjusts (row-adjusts coordss))
	(ag-sr-table-p (ag-sr-table-p table))
	(table-antigens (hi-table-antigens table))
	(table-sera     (hi-table-sera     table)))
    (loop for row in (hi-table-values table)
	for ag-index from 0 do
	  (loop for target-distance in (nthcdr ag-index row)
	      for sr-index from ag-index do
		(if (true-dont-care-p target-distance)
		    nil ;; do nothing
		  (let* ((mds-distance (e-dist (nth ag-index coordss) (nth sr-index coordss)))
			 (actual-distance
			  (if ag-sr-table-p
			      (distance-to-std-log-titer 
			       mds-distance
			       (nth sr-index col-bases)
			       (nth ag-index row-adjusts))
			    mds-distance))
			 (error (if (thresholdp target-distance)
				    'threshold
				  (- target-distance actual-distance))))
		    (if (not (thresholdp target-distance))
			(push error errors))
		    (push (list (nth ag-index table-antigens)
				(nth sr-index table-sera)
				error
				target-distance
				actual-distance)
			  errors-details)))))
    (values
     (reverse errors)
     (reverse errors-details))))

(defun errors-from-save (save)
  (errors-from-table-and-coordss (table-from-save save) (starting-coordss-from-save save)))

