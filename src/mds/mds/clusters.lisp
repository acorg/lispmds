/*(in-package user)  */

;;;----------------------------------------------------------------------
;;;                 misc code for dealing with clusters
;;;     (at first for measuring distances through cluster centroids)
;;;----------------------------------------------------------------------

(defun antigen-cluster (antigen clusters &optional &key cluster-path)
  (if (member antigen (nth 1 (nth 0 clusters)))
      (values (nth 0 (nth 0 clusters))
	      'this-is-first-cluster)
    (loop for (cluster-name strains) in clusters do 
	  (if (member antigen strains)
	      (return 
		(values 
		 cluster-name
		 (previous-cluster-to-this-cluster cluster-name clusters :cluster-path cluster-path))))
	finally (return 
		  (values 
		   'no-cluster
		   'no-previous-cluster-name)))))

(defun previous-cluster-to-this-cluster (this-cluster clusters &optional &key cluster-path)
  (if (eql this-cluster (nth 0 (nth 0 clusters)))
      'this-is-first-cluster
    ;; default to order of clusters in "clusters" if no cluster-path is passed
    (loop for (previous-cluster-name cluster-name) on (if cluster-path cluster-path (nths 0 clusters)) do 
	  (if (eql this-cluster cluster-name)
	      (return previous-cluster-name))
	finally (return 'no-previous-cluster-name))))



(defvar *cluster-colors*)
(setq *cluster-colors*
  (list 
   (rgb-tk-color 162   8 189)   ;;HK
   "#00ffe1" ;;"yellow"  ;; (rgb-tk-color 255 132   0)   ;;en
   (rgb-tk-color 249 208   4)   ;;VI
   (rgb-tk-color 171  76   0)   ;;TX
   (rgb-tk-color   0 255   0)   ;;BK
   (rgb-tk-color   0   0 255)   ;;SI
   (rgb-tk-color 255   0   0)   ;;BE
   (rgb-tk-color 248 148 248)   ;;BE
   (rgb-tk-color  55 128  43)   ;;WU
   (rgb-tk-color   0 175 255)   ;;SY
   ;;(rgb-tk-color 255 215   0)  ;; original fujian color
   (rgb-tk-color 255 132   0)   ;; FU (change fujian to a real orange)
   ))

(defun cluster-color-from-name (name clusters &optional &key plot-cluster-colors (not-found-action 'return-black))
  (let ((cluster-names (nths 0 clusters)))
    (if (position name cluster-names)
        (nth (position name cluster-names) (if plot-cluster-colors plot-cluster-colors *cluster-colors*))
      ;; not found action
      (case not-found-action
        (return-black "black")
        (t (error "Only 'return-black' action currently supported"))))))

(defun cluster-centroid (cluster-name centroids)
  (assoc-value-1 cluster-name centroids))



;;;----------------------------------------------------------------------
;;;           cluster spacing by dist to previous cluster
;;;----------------------------------------------------------------------

(defun distance-of-point-to-previous-cluster-centroid (point-name point-coords clusters cluster-centroids &optional &key cluster-path)
  (multiple-value-bind (cluster-name previous-cluster-name)
      (antigen-cluster point-name clusters :cluster-path cluster-path)
    (e-dist point-coords
	    (cluster-centroid 
	     (if (eql previous-cluster-name 'this-is-first-cluster)
		 cluster-name
	       previous-cluster-name)
	     cluster-centroids))))

(defun distance-from-cluster-centroid-to-root (cluster-name clusters cluster-centroids &optional &key cluster-path)
  (let ((root (nth 0 (nth 0 clusters))))
    (if (or (eql cluster-name root)
	    (eql cluster-name 'this-is-first-cluster))
	0
      (let ((previous-cluster-name (previous-cluster-to-this-cluster cluster-name clusters :cluster-path cluster-path)))
	(+ (e-dist (cluster-centroid cluster-name cluster-centroids)
		   (cluster-centroid previous-cluster-name cluster-centroids))
	   (distance-from-cluster-centroid-to-root (previous-cluster-to-this-cluster cluster-name clusters) clusters cluster-centroids :cluster-path cluster-path))))))
  
(defun distance-of-point-from-root-via-previous-cluster (point-name point-coords clusters cluster-centroids &optional &key cluster-path)
  (+ (distance-of-point-to-previous-cluster-centroid
      (remove-ag-sr-from-name point-name)
      point-coords
      clusters
      cluster-centroids
      :cluster-path cluster-path)
     (distance-from-cluster-centroid-to-root 
      (previous-cluster-to-this-cluster 
       (antigen-cluster 
	(remove-ag-sr-from-name point-name) 
	clusters
	:cluster-path cluster-path) 
       clusters :cluster-path cluster-path)
      clusters
      cluster-centroids
      :cluster-path cluster-path)))

(defun year-by-year-distances (&optional &key
					 save-or-filename
					 save-names-short
					 save-coordss
					 clusters-or-filename 
					 cluster-paths         ;; default is the order of the clusters
					 use-direct-target-distances-and-from-table
					 (start-year 1968)
					 (end-year   2020)
					 (root-strain 'bi/16190/68-ag)
					 (show-plot nil)
					 (plot-scale-x 1.0)
					 (plot-scale-y 1.0)
					 (plot-grid-step-x 1.0)
					 (plot-grid-step-y 1.0)
					 plot-cluster-colors
					 (output-stream t)    ;; can also be filename
					 (if-exists :error))

  (if (or save-names-short
	  save-coordss)
      (if (not (null save-or-filename))
	  (error "Specify either 'save-or-filename' or both of 'save-names-short' and 'save-coords', but not a mix.  'save-or-filename' is the recommended way to call, the other way because of some coding uglyness from the GUI.")))

  (if (stringp output-stream)
      (with-open-file (out output-stream :direction :output :if-exists if-exists)
	(year-by-year-distances 
	 :save-or-filename     save-or-filename
	 :save-names-short     save-names-short
	 :save-coordss         save-coordss
	 :clusters-or-filename clusters-or-filename
	 :use-direct-target-distances-and-from-table use-direct-target-distances-and-from-table
	 :start-year           start-year
	 :end-year             end-year
	 :root-strain          root-strain
	 :show-plot            show-plot
	 :plot-scale-x         plot-scale-x
	 :plot-scale-y         plot-scale-y
	 :plot-grid-step-x     plot-grid-step-x
	 :plot-grid-step-y     plot-grid-step-y
	 :plot-cluster-colors  plot-cluster-colors
	 :output-stream        out))

    (let* ((save (if (stringp save-or-filename)
		     (fi-in save-or-filename)
		   save-or-filename))
	   (hi-table-p (asl-hi-table-p (table-from-save save)))
	   (clusters (if (stringp clusters-or-filename)
			 (fi-in-s clusters-or-filename)
		       clusters-or-filename))
	   (a-save save)
	   (a-save-names-short (if save-names-short save-names-short (hi-table-antigens-short (table-from-save a-save))))
	   (a-save-coordss     (coordss (if save-coordss save-coordss (starting-coordss-from-save a-save))))
	 
	   (all-winter-seasons
	    (loop for year from start-year to end-year collect
		  (let ((low  (mod year 100))
			(high (mod (inc year) 100)))
		    (read-from-string (format nil "~2,'0d-~2,'0d" low high)))))

	   (season-years (mapcar #'first-year-of-season all-winter-seasons))

	   (a-centroids-raw
	    (loop for (name strains) in clusters collect
		  (list name
			(let ((coordss (loop for strain in strains 
					   when (member (if hi-table-p
							    (suffix-as-ag strain)
							  strain)
							a-save-names-short)  ;; allow the cluster file to have extra strain
					   collect
					     (nth (position (if hi-table-p
								(suffix-as-ag strain)
							      strain)
							    a-save-names-short) 
						  a-save-coordss))))
			  (let ((num-dims (length (car coordss))))
			    (loop for dim below num-dims collect
				  (av (nths dim coordss))))))))

	   ;; make centroid of first cluster a particular strain
	   (a-centroids-double-precision
	    (if root-strain
		(let ((cluster-of-root-strain (antigen-cluster (remove-ag-sr-from-name root-strain) clusters))
		      (position-of-root-strain (position root-strain a-save-names-short)))
		  (if (not position-of-root-strain)
		      (error "Root strain must be in save (you might not have added the suffix -ag on the strain)")
		    (if (not (equal cluster-of-root-strain (caar clusters)))
			(error "Root strain must be in first cluster (~a) but is in cluster ~a"
			       (caar clusters)
			       cluster-of-root-strain)
		      (cons (list cluster-of-root-strain
				  (nth position-of-root-strain a-save-coordss))
			    (cdr a-centroids-raw)))))
	      a-centroids-raw))
	 
	   (a-centroids
	    (loop for (name coords) in a-centroids-double-precision collect
		  (list name (mapcar (^ (x) (coerce x 'single-float)) coords))))

	   (clustered-strains (mapcar (^ (strain) (if hi-table-p (suffix-as-ag strain) strain))
				      (apply-append (nths 1 clusters))))

	   (table-from-save (table-from-save save))
	   (table-strains   (hi-table-sera table-from-save))

	   (year-strains-alist
	    (loop for year in season-years collect
		  (list year 
			(loop for antigen in (my-intersection clustered-strains a-save-names-short)
			    when (= (first-year-of-season (strain-isolation-season (remove-ag-sr-from-name antigen))) year)
			    collect (let ((antigen-cluster (antigen-cluster (remove-ag-sr-from-name antigen) clusters)))
				      (list antigen
					    (if use-direct-target-distances-and-from-table
						(hi-table-value table-from-save root-strain antigen :hi-table-sera-efficiency-hack table-strains)
					      (distance-of-point-from-root-via-previous-cluster 
					       antigen
					       (nth (position antigen a-save-names-short) a-save-coordss)
					       clusters
					       a-centroids
					       :cluster-path (assoc-value-1 antigen-cluster cluster-paths)))
					    antigen-cluster
					    ))))))

	   (year-strains-alist-cluster-grouped
	    (loop for (season l) in year-strains-alist collect
		  (list season
			(loop for group in (remove-duplicates (nths 2 l)) collect
			      (list group
				    (mapcar (^ (y) (firstn 2 y)) (collect (^ (x) (eql group (nth 2 x))) l)))))))

	   (year-cluster-distances
	    (loop for (season l) in year-strains-alist-cluster-grouped collect
		  (list season 
			(loop for (cluster-name name-distance-pairs) in l collect
			      (list cluster-name
				    (av (nths 1 name-distance-pairs))
				    (float (/ (length (nths 1 name-distance-pairs))
					      (length (apply-append (nths 1 l)))))
				    (nths 0 name-distance-pairs)      ;; for looking by hand
				    (nths 1 name-distance-pairs)))))) ;; for completness

	   (data-per-year
	    (loop for (year clusterName-avDistance-proportion-distances) in year-cluster-distances collect
		  (loop for (clusterName avDistance proportion) in clusterName-avDistance-proportion-distances collect
			(list (+ 1900 (y2k-offset year)) (coerce avDistance 'single-float) clusterName proportion))))
	 
	   (data-per-year-averaged 
	    (loop for data-for-year in data-per-year 
		when data-for-year
		collect
		  (list (caar data-for-year)
			(coerce (weighted-av (transpose (nths 1 data-for-year) (nths 3 data-for-year)))
				'single-float))))
	 
	   (data (apply-append data-per-year))
	 
	   (tkid 
	    (if show-plot
		(let* ((tk (tk-open))
		       (offset 20)
		       (scale-x (* plot-scale-x 10))
		       (scale-y (* plot-scale-y 10))
		       (width (+ (* 2 offset) (* scale-x (- end-year start-year))))
		       (height (+ (* 2 offset) (* scale-y (apply-max (nths 1 data))))))
		  (tk-put tk "canvas .c -width ~d -height ~d -bg white" width height)
		  (tk-put tk "pack .c")
		  (sleep 0.5)
		  (loop for x from 0 by (* scale-x plot-grid-step-x) below (+ width 1) do  
			(tk-put tk ".c create line ~d ~d ~d ~d -fill gray85" x 0 x height))
		  (loop for y from 0 by (* scale-y plot-grid-step-y) below (+ height 1) do 
			(tk-put tk ".c create line ~d ~d ~d ~d -fill gray85" 0 y width y))
		  (loop for (year average cluster-name proportion) in data do
			(let* ((x (* scale-x (- year start-year)))
			       (y (* scale-y average))
			       (radius (* 8 (sqrt proportion)))
			       )
			  (tk-put tk ".c create oval ~d ~d ~d ~d -fill ~a" 
				  (+ offset (- x radius))
				  (+ offset (- y radius))
				  (+ offset (+ x radius))
				  (+ offset (+ y radius))
				  (cluster-color-from-name cluster-name clusters :plot-cluster-colors plot-cluster-colors))))
		  tk))))

      (format output-stream "Cluster centroids:~%")
      (fll a-centroids :stream output-stream)
      (newline output-stream)
    
      (format output-stream "Per year, per cluster, average distance from root and proprtion in each cluster:~%")
      (fll data :stream output-stream)
      (newline output-stream)

      (format output-stream "Per year average distance from root:~%")
      (fll data-per-year-averaged :stream output-stream)
      (newline output-stream)

      (format output-stream "Per strain, distance from root:~%")
      (fll
       (loop for (year clusters) in year-strains-alist-cluster-grouped append
	     (loop for (cluster strains-data) in clusters append
		   (loop for (strain distance) in strains-data collect
			 (list strain (coerce distance 'single-float) (+ 1900 (y2k-offset year)) cluster))))
       :stream output-stream)
      (newline output-stream)
	    
      (if show-plot 
	  tkid
	t))))
    
    
#|
(setq save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod27.save")) 
(setq clusters (fi-in-s "mds/clusters-from-science-map.lisp"))

(year-by-year-distances 
 :save-or-filename save
 :clusters-or-filename clusters
 :root-strain 'BI/16190/68-ag
 :show-plot t)
			
(tk-put 1 ".c postscript -file mds/investigations/shape-sequence/ms-figs/season-by-season-antigenic.ps") 

(tk-close 1)
|#


(defun year-by-year-distances-from-mds-window (mds-window 
					       clusters-or-filename 
					       &optional &key
							 (start-year 1968)
							 (end-year   2020)
							 (root-strain 'bi/16190/68-ag)
							 (show-plot nil)
							 (output-stream t)    ;; can also be filename
							 (if-exists :error))
  (year-by-year-distances
   (get-save (get-table-window-for-mds-window mds-window))
   clusters-or-filename 
   :start-year    start-year
   :end-year      end-year  
   :root-strain   root-strain
   :show-plot     show-plot
   :output-stream output-stream
   :if-exists     if-exists))