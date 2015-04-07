(in-package user)

;;;----------------------------------------------------------------------
;;;                       pymol format
;;;----------------------------------------------------------------------

(defun pymol-format (save &optional &key filename (stream (not filename)) (if-exists :error))
  (let* ((sera-color-tk "{}")
	 (sera-color-rgb "#ffffff")
	 ;;(coordss (coordss (starting-mds-coordss-coordss-no-scale-flip-y-from-save save)))
	 (coordss (coordss (starting-coordss-from-save save)))
	 (names (hi-table-antigens (table-from-save save))) ;; This gets ag and sr names
	 (working-names (coords-names-working-copy-from-save save))
	 (colors (mapcar (^ (c) (if (string= c sera-color-tk) sera-color-rgb c)) (coords-colors-from-save save)))
	 (radii (coords-dot-sizes-from-save save))
	 (shapes (coords-shapes-from-save save))
         (transparencies (coords-transparencies-from-save save))
	 (constant-stress-radial-data (constant-stress-radial-data-from-save save))
	 (blob-steps 20)
	 (blob-power 3)
	 (procrustes-data (procrustes-data-from-save save))
	 (error-connection-prediction-line-data (error-connection-prediction-line-data-from-save save))
	 (landscape-titers (landscape-titers-from-save save)))
    (if (not (and (= (length colors) (length names))
		  (= (length colors) (length coordss))))
	(error (format nil "Colors (~a), names (~a), and coords (~a) lists are not all of equal length."
		       (length colors) (length names) (length coordss)))
      (let ((s (if filename
		   (open filename :direction :output :if-exists if-exists)
		 stream)))
	(format s "self.map_data = {~%")
	(loop for i from 0 
	      for coords in coordss
  	      for color  in colors 
	      for radius in radii   ;;      to do serum size (not changing).
	      for shape  in shapes
              for transparency in transparencies
	      for name   in names   
  	      for working-name in working-names do
	      (progn
		(format s "  '~a' : {~%" name)
		(if (not (equal "" working-name)) (format s "    'text' : '~a',~%" working-name))
		(format s "    'coords' : (~a, ~a, ~a),~%" 
			(coerce (nth 0 coords) 'single-float)    ;; to avoid the d0 exponent
			(coerce (nth 1 coords) 'single-float)
			(coerce (or (nth 2 coords) 0.0) 'single-float))
		(apply #'format s "    'color' : (~f, ~f, ~f),~%" (apply #'rgb-to-r-rgb (tk-color-to-rgb color)))
                (format s "    'transparency' : ~d,~%" transparency)
		(format s "    'type' : '~a',~%" (if (sr-name-p name) "antisera" "strains"))
		(if (sr-name-p name)
		    ;;(format s "    'side length' : ~d," (* radius 2 0.075))
                    (format s "    'side length' : ~d," (* radius 2 0.0375))
		  (format s "    'radius' : ~d," (* radius (if (ag-name-p name) 0.075 0.0375))))
		(if (and (stringp shape) 
			 (> (length shape) 15)
			 (equal "(POLYGON-SCALED " (substring shape 0 15)))
		    (pymol-blob-format 
		     s
		     coords
		     color
		     :style 'simple-blob
		     :blob-steps 1
		     :locus (nth 1 (read-from-string shape)))
		  (if (and (listp (read-from-string shape))
			   (numberp (nth 1 (read-from-string shape))))
		      (pymol-blob-format 
		       s
		       coords
		       color
		       :style 'color-graded-blob
		       :constant-stress-radial-datum (nth 1 (nth i constant-stress-radial-data))
		       :blob-stress (nth 1 (read-from-string shape))
		       :index i
		       :blob-steps blob-steps 
		       :blob-power blob-power)))
		(if (assoc name procrustes-data)
		    (let ((procrustes-coords (assoc-value-1 name procrustes-data)))
		      (format s "    'procrustes coords' : (~a, ~a, ~a),~%"
			      (coerce (nth 0 procrustes-coords) 'single-float)    ;; to avoid the d0 exponent
			      (coerce (nth 1 procrustes-coords) 'single-float)
			      (coerce (or (nth 2 procrustes-coords) 0.0) 'single-float))))
		(if (assoc (if (stringp name) (read-from-string name) name) procrustes-data)
		    (apply #'format s "    'procrustes color' : (~f, ~f, ~f),~%"
			   (apply #'rgb-to-r-rgb (tk-color-to-rgb (assoc-value-2 name procrustes-data)))))
		(if (assoc name (nth 1 error-connection-prediction-line-data))
		    (progn
		      (format s "    '~a lines' : (~%"
			      (case (nth 0 error-connection-prediction-line-data)
				(error-lines-only "error")
				(connection-lines-only "connection")
				(prediction-lines-only "prediction")
				(t (error "~%Unexpected error in outputting error/connection/prediction lines~%"))))
		      (loop for (coords color) in (assoc-value-1 name (nth 1 error-connection-prediction-line-data)) do
			    (progn
			      (format s "    { 'coord' : (~a, ~a, ~a),~%"
				      (coerce (nth 0 coords) 'single-float)
				      (coerce (nth 1 coords) 'single-float)
				      (coerce (or (nth 2 coords) 0.0) 'single-float))
			      (apply #'format s "      'color' : (~a, ~a, ~a) },~%"
				     (let ((hsv (multiple-value-list (apply #'rgb-to-r-hsv (tk-color-to-rgb color)))))
				       (multiple-value-list
					(r-hsv-to-r-rgb
					 (nth 0 hsv)
					 (nth 1 hsv)
					 (if (eql (nth 0 error-connection-prediction-line-data) 'connection-lines-only)
					     ;;(- 1 (nth 2 hsv))  ;; fade to black, instead of white
					     (nth 2 hsv)  ;; fade to black, instead of white
					   (nth 2 hsv))))))))
		      (format s "    ),~%")))
		(if (assoc name landscape-titers)
		    (format s "    'titer' : ~a,~%"
			    (assoc-value-1 name landscape-titers)))
		(format s "  },~%")
		))
	(format s "  }~%")
	(if filename (close s))))))

(defun pymol-blob-format (stream
			  coords
			  color
			  &optional &key 
				    style
				    index
				    locus
				    constant-stress-radial-datum
				    blob-stress
				    (blob-steps (if (eql style 'simple-blob) 1 20)) 
				    (blob-power 3))
  (format stream   "    'blobs' : (~%")
  (loop for blob-step below blob-steps do
	(let* ((blob-color (if (equal style 'simple-blob)
			       color
			       (if (equal "{}" color)
				   color
				   (multiple-value-bind (h s v)
				       (apply #'rgb-to-r-hsv (tk-color-to-rgb color))
				     (hsv-tk-color
				      h
				      s
				      (max 0 (- v (* blob-step (/ v blob-steps)))))))))
	       (alpha (float (/ blob-step blob-steps)))
	       (blob-locus (if (equal style 'simple-blob)
			       locus
			       (calc-constant-stress-shape
				constant-stress-radial-datum
				:stress-delta (* (expt (float (/ (- blob-steps blob-step) blob-steps)) blob-power) 
						 blob-stress)))))

	  ;;(print (format nil "blob step = ~a, stress-delta = ~f, alpha = ~f" blob-step stress-delta alpha))
	  (format stream "       # Fan ~a~
                          ~%       { 'transparency' : ~f,~
                          ~%         'points' : (~
                        ~{~%           { 'coord' : (~a, ~a, ~a),~
                          ~%             'color' : (~f, ~f, ~f) },~}~
                          ~%         ),~
                          ~%       },~%"
		  blob-step
		  alpha
		  (loop for locus-coords in blob-locus append
		       (append 
			(list (coerce (+ (nth 0 coords) (nth 0 locus-coords)) 'single-float)    
			      (coerce (+ (nth 1 coords) (nth 1 locus-coords)) 'single-float)
			      (coerce (if (nth 2 locus-coords)
					  (+ (nth 2 coords) (nth 2 locus-coords))
					  0.0)
				      'single-float))
			(apply #'rgb-to-r-rgb (tk-color-to-rgb blob-color)))))))
  (format stream   "      ),~%"))

(defun pymol-options-format (&optional &key 
						 filename
						 (stream (not filename))
						 (if-exists :error)
						 python-options
						 python-options-per-map-contents
						 python-options-per-map-references
						 )
  (let ((s (if filename
	       (open filename :direction :output :if-exists if-exists)
	     stream)))
    (format s "~a" python-options)
    (loop for python-options-contents in python-options-per-map-contents
	for python-options-per-map-reference in python-options-per-map-references 
	when (and (stringp python-options-contents)
		  (not (equal "" python-options-contents))) 
	do (progn
	     (format s "perMapOptions.update({~%")
	     (format s "    '~a' : {~%" python-options-per-map-reference)
	     (format s "~a" python-options-contents)
	     (format s "        },~%")
	     (format s "    })~%")))
    (if filename (close s))))

(defun pymol-display-from-saves (saves &optional &key
						 python-options
						 (python-options-per-map-contents
						  (loop for save in saves collect (progn save nil)))
						 )
  (if (not (= (length saves) (length python-options-per-map-contents)))
      (error "~%The number of saves (~d) and number of python-options-per-map-contents (~d) must be the same.~%" 
	     (length saves) (length python-options-per-map-contents)))
  (let* ((options-filename-file (format nil "pymol-tmpfile-~d.pml" (krandom 10000000)))
	 (save-filename-files   (loop for save in saves collect 
				      (progn save (format nil "pymol-tmpfile-~d.pml" (krandom 10000000)))))
	 (filename-directory    (if (running-on-windows-p)
				    (uw-sfnr "mds/procrustes/tmp-files/" :assertIsDir t)  ;; resuse the procrustest tmp dir
				  "/tmp/"))
	 (options-filename      (string-append filename-directory options-filename-file))
	 (save-filenames        (loop for save-filename-file in save-filename-files collect 
				      (string-append filename-directory save-filename-file))))

    (pymol-options-format 
     :python-options                    python-options
     :python-options-per-map-contents   python-options-per-map-contents
     :python-options-per-map-references save-filenames
     :filename options-filename 
     :if-exists :supersede)

    (loop for save in saves 
	for save-filename in save-filenames do
	  (pymol-format save :filename save-filename :if-exists :supersede))

    (run-shell-command (format nil 
			       "~a~a -d \"run ~a\" -d \"abl optionsFile=~a~{, ~a~}\"" 
			       (if (running-on-windows-p) "" "exec ")
			       *pymol-executable-filename*
			       (if (running-on-windows-p) 
				   (make-windows-lisp-filename-passable-as-argument *pymol-map-viewer-plugin-filename*)
				 *pymol-map-viewer-plugin-filename*)
			       (if (or python-options
				       (null (car (remove-duplicates python-options-per-map-contents))))
				   (if (running-on-windows-p) 
				       (make-windows-lisp-filename-passable-as-argument options-filename)
				     options-filename)
				 "None")
			       save-filenames)
		       :wait nil 
		       :input :stream 
		       :output :stream 
		       :error-output :output)))


(defun calc-dots-spacing (coordss)
  ;; default has been 1, but Eu reports (in email on 2009-01-07) possible error in Pymol in which much memory is consumed when 
  ;; the dot spacing is set to 1 for a large volume figure, and that this error can be circumvented by increasing 
  ;; the dot spacing.
  (let* ((min-s (map-apply #'min (apply-transpose coordss)))
         (max-s (map-apply #'max (apply-transpose coordss)))
         (range-s (mapcar #'- max-s min-s))
         (volume  (apply #'* range-s)))
    (max 1 (expt 10 (ceiling (- (log volume 10) 5))))))

(defun pymol-display-from-save (save &optional &key
					       (python-options (format nil "
options.update({
    'axes compute' : False,
    'base grid compute' : False,
    })
defaultMapOptions.update({
    'dots spacing': ~d,
    })
"
                                                                       (calc-dots-spacing (coordss (starting-coordss-from-save save)))))
					       (python-options-per-map-contents "
	'cgo prefix' : '',
"))
  (pymol-display-from-saves
   (list save)
   :python-options    python-options
   :python-options-per-map-contents (list python-options-per-map-contents)))


(defun pymol-display-from-mds-window (mds-window 
				      &optional &key
					       (python-options (format nil "
options.update({
    'axes compute' : False,
    'base grid compute' : False,
    })
defaultMapOptions.update({
    'dots spacing': ~d,
    })
"
                                                                       (calc-dots-spacing (coordss (get-mds-coordss mds-window)))))
					       (python-options-per-map-contents "
	'cgo prefix' : '',
"))
  ;; pull together a save from the table, coords, names, colors
  ;; do with the save from mds window code, but don't give a filename, but return the sexp
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (pymol-display-from-save
     (make-save-form
      :hi-table         (get-hi-table table-window)
      :starting-coordss (get-mds-coordss mds-window)
      :canvas-coord-transformations (get-canvas-coord-transformations mds-window)
      :plot-spec        (generate-plot-spec-from-mds-window mds-window)
      :procrustes-data  (get-procrustes-data mds-window)
      :error-connection-prediction-line-data (get-error-connection-prediction-line-data mds-window)
      :constant-stress-radial-data (get-constant-stress-radial-data table-window))
     :python-options    python-options
     :python-options-per-map-contents python-options-per-map-contents)))
