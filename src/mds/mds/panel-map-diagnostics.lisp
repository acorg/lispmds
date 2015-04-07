(in-package user)

(defun panel-map-distances (save &optional &key plot (plot-filename "/tmp/foo.png"))
  (let* ((hi-table (un-asl-hi-table-from-save save))
	 (antigen-coords (firstn (length (hi-table-antigens hi-table)) (coordss (starting-coordss-from-save save))))
	 (map-distance-matrix (all-comparisons-square antigen-coords #'e-dist))
	 (panel-table (hi-table-to-euclidean-distance-matrix hi-table))
	 (panel-distance-matrix (hi-table-values panel-table))
	 (names-distances (loop for x-name in (hi-table-antigens hi-table) 
			      for map-distance-matrix-row in map-distance-matrix
			      for panel-distance-matrix-row in panel-distance-matrix append
				(loop for y-name in (hi-table-antigens hi-table) 
				    for map-distance in map-distance-matrix-row
				    for panel-distance in panel-distance-matrix-row
				    when (and (numberp map-distance) (numberp panel-distance))
				    collect (list x-name
						  y-name
						  panel-distance
						  map-distance))))
	 (distances (mapcar (^ (l) (nthcdr 2 l)) names-distances))
	 (names     (mapcar (^ (l) (firstn 2 l)) names-distances)))
    (if plot
	(progn
	  (gnuplot-correlation 
	   distances
	   :x-title "Panel distance"
	   :y-title "Map distance"
	   :equal-axes-range t
	   :show-diagonal t)
	  (if plot-filename
	      (progn
		(sleep 1)
		(gnuplot-png 14 plot-filename 1.0)
		(sleep 1)
		(gnuplot-exit)))))
    (values
     distances
     names
     map-distance-matrix
     panel-distance-matrix)))


#|
(setq save (get-save-after-runs-reoriented "cdc" "20040820-ref-ag-merge" "20040512"))
(gnuplot-correlation (setq distances (panel-map-distances save)))

(setq saveok (get-save-after-runs-reoriented "cdc" "20040820-ref-ag-merge" "20040519"))
(gnuplot-correlation (setq distancesok (panel-map-distances saveok)))

(panel-map-distances (get-save-after-runs-reoriented "cdc" "20040820-ref-ag-merge" "20040512") :plot t)
(panel-map-distances (get-save-after-runs-reoriented "cdc" "20040820-ref-ag-merge" "20040804") :plot t)

(panel-map-distances (setq foo (get-save-after-runs-reoriented "cdc" "20040820-ref-ag-merge" "20040603") :plot t)

would be nice to see how much difference between the table by itself, and with the ref ag merge, and also to remove the ref ag megrge strains
(but ref ags does not seem to be a keyword in the early strains)
and also to look on the ref ag merge

and to retro fit on the existing runs web pages

|#


(defun panel-map-distances-for-strain-from-panel-map-info (antigen save panel-matrix map-matrix)
  (let* ((antigens (mapcar #'remove-ag-sr-from-name (collect #'ag-name-p (hi-table-antigens-table-from-save-non-expanding-hack save))))
         (p (position antigen antigens))
         (npms (transpose antigens (nth p panel-matrix) (nth p map-matrix)))
         (npms-numeric (collect (^ (npm) (and (numberp (nth 1 npm)) (numberp (nth 2 npm)))) npms)))
    (if npms-numeric
        (gnuplot-correlation 
         (mapcar #'cdr npms-numeric)
         :x-title "Panel distance" 
         :y-title "Map distance"
         :title (string antigen)
         :equal-axes-range t
         ))
    (values
     npms
     npms-numeric)))


#|
(setq save (fi-in "/home/dsmith/Desktop/foo2.save"))
(setq foox (fi-in "/home/dsmith/Desktop/foox"))
(setq names (hi-table-antigens-table-from-save-non-expanding-hack save))
(setq map-matrix (nth 2 foox))
(setq panel-matrix (nth 3 foox))
(panel-map-distances-for-strain-from-panel-map-info 'CHRISTCHURCH/223/2004 save panel-matrix map-matrix)

townsville/112/2005
townsville/118/2005
townsville/119/2005
townsville/111/2005
townsville/116/2005
townsville/120/2005
townsville/151/2005
townsville/29/2005
townsville/107/2005
townsville/125/2005

|#


(defun strains-close-to-strain-by-panel-distance (antigen save panel-matrix map-matrix)
  (let* ((antigens (mapcar #'remove-ag-sr-from-name (collect #'ag-name-p (hi-table-antigens-table-from-save-non-expanding-hack save))))
         (p (position antigen antigens))
         (npms (transpose antigens (nth p panel-matrix) (nth p map-matrix)))
         (npms-numeric (collect (^ (npm) (and (numberp (nth 1 npm)) (numberp (nth 2 npm)))) npms)))
    (sort-nth 1 npms-numeric)))

#|
(strains-close-to-strain-by-panel-distance 'townsville/112/2005 save panel-matrix map-matrix)
|#