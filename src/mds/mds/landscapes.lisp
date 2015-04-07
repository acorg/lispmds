(in-package user)

(defun preprocess-name-titer-s-for-landscaping (name-titer-s allowable-names)
  (loop for (name titer) in name-titer-s
      when (and (not (true-dont-care-p titer))
		(if (member name allowable-names)
		    t
		  (progn
		    (format t "~%Warning: antigen ~s from the table to lanscape is not in map on the base, excluding this antigens values.~%" name)
		    nil)))
      collect (list 
	       name
	       (if (numberp titer)
		   (inc (std-log-titer titer))  ;; so the thresholded value can be one less, and so a titer of 10 is not zero
		 (if (thresholdp titer)
		     (std-log-titer (threshold-number titer))
		   (error "did not expect to be here"))))))

(defun landscape-sera-from-table-on-save (save 
					  un-asl-hi-table
					  serum-names      ;; sera in un-asl-hi-table and without suffix
                                          &optional &key 
                                                    (landscape-fit-default "loess")
                                                    landscape-fits
                                                    landscape-colors
                                                    )
  (let ((hi-table-sera (hi-table-sera un-asl-hi-table)))
    (loop for serum-name in serum-names
	when (not (member serum-name hi-table-sera))
	do (progn
	     (format t "~2%Warning: you can only landscape sera, doing nothing with ~a~2%" serum-name)
	     ;; should pop up a tk dialog so the user sees this!
	     ))
    (setq serum-names (collect (^ (serum-name) (member serum-name hi-table-sera)) serum-names))
    (let* ((strain-colors (coords-colors-from-save save))
	   (sera-colors (nthcdr (hi-table-length un-asl-hi-table) strain-colors))
	   (serum-name-color-s (if (null (set-difference serum-names (hi-table-sera (un-asl-hi-table-from-save save))))
				   (transpose 
				    (hi-table-sera (un-asl-hi-table-from-save save))
				    (mapcar (^ (tk-color) (rgb256-into-rgb1 (tk-color-to-rgb tk-color))) sera-colors))
				 (loop for serum-name in serum-names collect
				       (list serum-name 
                                             (rgb256-into-rgb1 
                                              (if (assoc-value serum-name landscape-colors)
                                                  (tk-color-to-rgb (assoc-value-1 serum-name landscape-colors))
                                                '(0 0 255)))))))
           (serum-name-fit-s (loop for serum-name in serum-names collect
                                   (list serum-name 
                                         (if (assoc-value serum-name landscape-fits)
                                             (assoc-value-1 serum-name landscape-fits)
                                           landscape-fit-default)))))
      (if serum-names
	  (pymol-display-from-saves
	   (cons 
	    save
	    (loop for serum-name in serum-names collect
		  (set-landscape-titers-in-save
		   save
		   (preprocess-name-titer-s-for-landscaping
		    (transpose
		     (mapcar #'suffix-as-ag (hi-table-antigens un-asl-hi-table))
		     (hi-table-serum-values
		      un-asl-hi-table
		      serum-name))
		    (mapcar #'suffix-as-ag (hi-table-antigens (un-asl-hi-table-from-save save))))
		   :not-found-action :add)))
	   :python-options "
options.update({
    'axes hidden' : True,
    })

TiterPlane(3, 'titer plane', (1.0, 0.0, 0.0), 1, -10, 20, -5, 15, log=False).load()

defaultMapOptions.update({
    'surface gnuplot density' : 10,
    'titer hidden' : True,
    'titer plane hidden' : True,
    'surface triangles hidden' : True,
    'surface quads hidden' : False,
    'bounding box hidden' : True,
    })
"
	   :python-options-per-map-contents     
	   (cons 
	    "
        'cgo prefix' : '',
"
	    (loop for serum-name in serum-names collect
		  (print (apply #'format nil "
        'cgo prefix' : '~a_',
        'surface r fit' : '~a',
        'surface quads color' : (~f, ~f, ~f),
        'strains compute' : False,
        'strains names compute' : False,
        'antisera compute' : False,
        'antisera names compute' : False,
        'bounding box compute' : False,
        'procrustes names compute' : False,
        'procrustes spheres compute' : False,
        'procrustes lines compute' : False,
        'bounding box compute' : False,
        'grid compute' : False,
        'notches compute' : False,
        'dots compute' : False,
        'surface spheres compute' : False,
        'surface triangles compute' : False,
        'titer plane compute' : False,
"
			 (string-subst
			  #\/ #\_
			  (string (remove-ag-sr-from-name serum-name)))
                         (assoc-value-1 serum-name serum-name-fit-s)
			 (assoc-value-1 serum-name serum-name-color-s)
                         )))))))))

(defun landscape-sera-from-save (save 
				 serum-names)
  (landscape-sera-from-table-on-save 
   save 
   (un-asl-hi-table-from-save save)
   (loop for serum-name in serum-names collect 
	 ;; only remove -sr from sera, don't want to remove -ag from an antigen and have
	 ;; there also be a serum of that name that then could be accidently landscaped.
	 (if (serum-name-p serum-name)  
	     (remove-ag-sr-from-name serum-name) 
	   serum-name))))

(defun landscape-sera-from-mds-window (mds-window &optional &key canvas-ids)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (table (get-hi-table table-window))
	 (point-names (hi-table-antigens table))
	 (save (make-save-form
		:hi-table         table
		:starting-coordss (get-mds-coordss mds-window)
		:plot-spec        (generate-plot-spec-from-mds-window mds-window))))
    (landscape-sera-from-save
     save
     (if canvas-ids
	 (multiple-nth
	  (get-mds-point-indices-from-mds-window-canvas-ids mds-window canvas-ids)
	  point-names)
       (hi-table-sera-short table)))))

(defun landscape-table-sera-from-mds-window (mds-window table-filename &optional &key colors-filename fits-filename)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (save-table (get-hi-table table-window))
	 (save (make-save-form
		:hi-table         save-table
		:starting-coordss (get-mds-coordss mds-window)
		:plot-spec        (generate-plot-spec-from-mds-window mds-window)))
	 (table-to-landscape (un-asl-hi-table (read-hi-table-and-convert table-filename 1 0)))
         (landscape-colors (if colors-filename (fi-in-readline-to-list colors-filename)))
         (landscape-fits   (if fits-filename   (fi-in-readline-to-list fits-filename))))
    (landscape-sera-from-table-on-save
     save
     table-to-landscape
     (hi-table-sera table-to-landscape)
     :landscape-colors landscape-colors
     :landscape-fits   landscape-fits)))

