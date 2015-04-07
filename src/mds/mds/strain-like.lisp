(in-package user)

;;;----------------------------------------------------------------------
;;;                       <strain>-like 
;;;----------------------------------------------------------------------

(defun points-within-radius-of-coordss-in-save (save reference-coordss radius)
  (let* ((names (hi-table-antigens (table-from-save save)))
	 (coordss (coordss (starting-coordss-from-save save))))
    (loop for reference-coords in reference-coordss collect
	  (loop for name in names
	      for coords in coordss
	      when (<= (e-dist reference-coords coords) radius)
	      collect name))))

(defun points-within-radius-of-coords-in-save (save reference-coords radius)
  (points-within-radius-of-coordss-in-save save (list reference-coords) radius))

(defun strain-like-antigens-in-save (save strain radius)
  (collect #'ag-name-p
	   (points-within-radius-of-coords-in-save save (coords-from-save save strain) radius)))

(defun strains-like-antigens-in-save (save strains radius)
  (mapcar (^ (l) (collect #'ag-name-p l))
	  (points-within-radius-of-coordss-in-save save (multiple-coords-from-save save strains) radius)))



;;;----------------------------------------------------------------------
;;;                          time slices
;;;----------------------------------------------------------------------

(defun split-strains-into-timeseries (strains strain-isolation-date-s start-date end-date date-step-size-in-months)
  (let ((strain-date-s (loop for strain in strains
			   when (assoc (remove-ag-sr-from-name strain) strain-isolation-date-s)
			   collect (list strain 'no-season (assoc-value-2 (remove-ag-sr-from-name strain) strain-isolation-date-s))))
	(this-step-start-date start-date)
	this-step-end-date
	while-result)
    (while (3tuple-isolation-date-<= 
	    (setq this-step-end-date 
	      (3tuple-isolation-date-increment-month this-step-start-date date-step-size-in-months))
	    end-date)
      (setq while-result
	(push-end 
	 (prog1
	   (collect 
	    (^ (strain-date)
	       (and (3tuple-isolation-date->= (nth 2 strain-date) this-step-start-date)
		    (3tuple-isolation-date-<  (nth 2 strain-date) this-step-end-date)))
	    strain-date-s)
	   (setq this-step-start-date this-step-end-date))
	 while-result)))
    while-result))

(defun likes-in-time-slices (save name-season-date-s start-date end-date month-increment like-strain-names like-radius)
  (let ((likes (transpose 
		like-strain-names
		(strains-like-antigens-in-save save like-strain-names like-radius))))
    (let ((all-strains-in-time-slice-s (split-strains-into-timeseries
					(hi-table-antigens-short (table-from-save save))
					name-season-date-s
					start-date
					end-date
					month-increment)))
      (loop for (reference like-strains) in likes collect
	    (list reference
		  (loop for like-strains-in-time-slice in (split-strains-into-timeseries 
							   like-strains
							   name-season-date-s
							   start-date
							   end-date
							   month-increment) 
		      for all-strains-in-time-slice in all-strains-in-time-slice-s
		      collect
			(list 
			 (length like-strains-in-time-slice)
			 (length all-strains-in-time-slice)
			 (if (not (zerop (length all-strains-in-time-slice)))
			     (2dp
			      (/ (length like-strains-in-time-slice)
				 (length all-strains-in-time-slice)))))))))))
