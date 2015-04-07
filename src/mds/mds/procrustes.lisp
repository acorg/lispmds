(in-package user)

;;;----------------------------------------------------------------------
;;;                          PROCRUSTES
;;;----------------------------------------------------------------------

(defun coordss-to-single-float (coordss)
  (mapcar (^ (coords) (mapcar (^ (x) (coerce x 'short-float)) coords)) coordss))

(defun output-names-and-coordss (filename names coordss &optional &key bases adjusts)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (fll 
     (let ((name-coords-s (mapcar #'cons names (coordss-to-single-float coordss))))
       (if (or bases adjusts)
           (progn
             (if (not (and (= (length bases)   (length name-coords-s))
                           (= (length adjusts) (length name-coords-s))))
                 (error "currently must supply both bases and adjusts, that match the names-coords"))
             (mapcar (^ (name-coords-s base adjust)
                        (append name-coords-s (list (coerce base 'short-float)) (list (coerce adjust 'short-float))))
                     name-coords-s bases adjusts))
         name-coords-s))
     :stream out)))

(defun draw-arrows-on-mds-window (mds-window master-names master-coordss slave-names slave-coordss &optional &key 
													     (tag "none") 
													     (arrow-color "#333333")
                                                                                                             ;;(arrow-color "#bbbbbb")
													     no-arrowhead-if-less-than-arrow-head-length
													     (set-procrustes-data t))
  (let ((procrustes-data
	 (loop for slave-name in slave-names
	     for slave-coords in slave-coordss collect
	       (let* ((slave-position-in-master (position slave-name master-names))
		      (master-coords (nth slave-position-in-master master-coordss))
		      (canvas-master-coords (mds-to-canvas-coords mds-window master-coords))
		      (canvas-slave-coords  (mds-to-canvas-coords mds-window slave-coords))
		      (arrow-color (if (listp arrow-color) (nth slave-position-in-master arrow-color) arrow-color)))
		 (tk-put mds-window "~a ~{~f ~} ~{~f ~} {~a} ~a"
			 (if no-arrowhead-if-less-than-arrow-head-length
			     (if (< (e-dist canvas-master-coords canvas-slave-coords) 20)
				 "mkColoredLine"
			       "mkColoredArrow")
			   "mkColoredArrow")
			 canvas-master-coords
			 canvas-slave-coords
			 tag
			 arrow-color ;;"black"
			 )
		 ;; record the procrustes data (added initially for sending to pymol for 3d procrustes)
		 (list slave-name slave-coords arrow-color)
		 ))))
    (if set-procrustes-data
	(set-procrustes-data
	 mds-window
	 procrustes-data))))   

(defun map-save-on-to-mds-window (mds-window save-or-file-name-of-slave-save &optional &key 
                                                                                       scalep
                                                                                       (arrow-color "#333333")
                                                                                       ;;(arrow-color "#bbbbbb")
                                                                                       name-subset
                                                                                       name-subset-by-indices
                                                                                       ag-only
                                                                                       sr-only)
  (if (and name-subset name-subset-by-indices)
      (error "Specify subset by name or by index, not both"))
  (if (and ag-only sr-only)
      (error "Specify ag-subset or sr-subset, not both"))
  ;; save-or-file-name-of-slave-save dispatches based on the type of the arg, string indicates filename
  (let* ((master-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	 (master-identifier (format nil "~a-~d" (hi-table-name master-table) (krandom 100000)))
	 (master-names (hi-table-antigens master-table))
	 (master-coordss (coordss (get-mds-coordss mds-window)))
	 (slave-save (if (stringp save-or-file-name-of-slave-save) 
			 (fi-in-any-save-format save-or-file-name-of-slave-save)
		       save-or-file-name-of-slave-save))
	 (slave-table (extract-table-from-save slave-save))
	 (slave-identifier (format nil "~a-~d" (hi-table-name slave-table) (krandom 100000)))
	 (slave-names (hi-table-antigens slave-table))
	 (slave-coordss (coordss (starting-coordss-or-best-batch-from-save slave-save)))
	 (name-subset (if name-subset-by-indices
			  (multiple-nth name-subset-by-indices master-names)
			name-subset)))
    (multiple-value-bind (names coordss rotation scale translation dims rms av sd)
	(procrustes-slave-onto-master 
	 master-identifier
	 master-names
	 master-coordss
	 slave-identifier
	 slave-names
	 slave-coordss
	 :scalep      scalep
	 :name-subset name-subset
         :ag-only     ag-only
         :sr-only     sr-only)

      ;; check if we do the transformation here that it is the same as what is done in the c code (and it is)
#|
      (let ((slave-coordss-subset  
	     (loop for name in names collect
		   (nth (position name (hi-table-antigens slave-table)) slave-coordss))))
	(fll (loop for slave-coords-mapped-into-master-coordss in coordss 
		 for unmapped-slave-coords in slave-coordss-subset collect
		   (list slave-coords-mapped-into-master-coordss
			 (rotate-scale-translate-coords unmapped-slave-coords rotation scale translation)))))
|#

     (draw-arrows-on-mds-window 
      mds-window
      master-names
      master-coordss
      names
      coordss
      :tag slave-identifier
      :arrow-color arrow-color)
     
      (values
       names coordss rotation scale translation dims rms av sd)
       )))

(defun transform-slave-save-best-coords-by-procrustes-to-master-save-best-coordss-set-as-starting-coordss-in-new-save (slave-save master-save &optional &key scalep slave-indices name-subset)
  ;; outputs a new slave save with coords moved, if no scaling, then stress should stay the same
  ;; for now only transform the starting-coordss (do not copy the batch coords to the new save)
  ;; slave-indices is used if we do not want to use all of the slave (added originally so we could orient with a kmeans-derived cluster)
  ;;
  ;; If no overlap between the saves, then return the slave save unchanged, and issue a warning.
  ;;
  (let* ((master-table (table-from-save master-save))
	 (master-identifier (format nil "~a-~d" (hi-table-name master-table) (krandom 100000)))
	 (master-names (hi-table-antigens master-table))
	 (master-coordss (coordss (starting-coordss-or-best-batch-from-save master-save)))
	 (slave-table (table-from-save slave-save))
	 (slave-identifier (format nil "~a-~d" (hi-table-name slave-table) (krandom 100000)))
	 (slave-all-names (hi-table-antigens slave-table))
	 (slave-names (if slave-indices 
			  (loop for slave-index in slave-indices collect 
				(let ((slave-name (nth slave-index slave-all-names)))
				  (if (not slave-name)
				      (error "Requested slave index ~a is not in the slave table" slave-index))
				  slave-name))
			slave-all-names))
	 (slave-coordss-plus-more (starting-coordss-or-best-batch-from-save slave-save))
	 (slave-coordss (if slave-indices
			    (multiple-nth slave-indices (coordss slave-coordss-plus-more))
			  (coordss slave-coordss-plus-more))))
    (multiple-value-bind (names coordss rotation scale translation dims rms av sd)
	(procrustes-slave-onto-master 
	 master-identifier
	 master-names
	 master-coordss
	 slave-identifier
	 slave-names
	 slave-coordss
	 :scalep scalep
	 :name-subset name-subset)
      (values
       (if (null names)
	   (progn
	     (format t "~2%Warning:in function transform-slave-save-best-coords-by-procrustes-to-master-save-best-coordss-set-as-starting-coordss-in-new-save, the master and slave save have no points in common; returning the slave untransformed.~2%")
	     slave-save)
	 (set-starting-coordss-in-save
	  (set-batch-runs-in-save
	   slave-save
	   nil   ;; because right now we are not going to transform the batch coords as well as the starting coords (just not bothering right now)
	   :not-found-action :add)
	  (make-coordss-plus-more
	   ;; note, we transform all coords, even if we've just used a subset of the coords for the procrustes
	   (rotate-scale-translate-coordss (coordss slave-coordss-plus-more) rotation scale translation)
	   (col-bases   slave-coordss-plus-more)
	   (row-adjusts slave-coordss-plus-more))
	  :not-found-action :add))
       names coordss rotation scale translation dims rms av sd)
      )))

(defun rotate-scale-translate-coordss (coordss rotation scale translation)
  (loop for coords in coordss collect
	(rotate-scale-translate-coords coords rotation scale translation)))

#|
;; for debug
(defun rotate-scale-translate-coords (coords rotation scale translation)
  (print 'next)
  (print (translate-coords
   (print (scale-coords
    (print (rotate-coords
     (print coords)
     rotation))
    scale))
   translation)))
|#
(defun rotate-scale-translate-coords (coords rotation scale translation)
  (translate-coords
   (scale-coords
    (rotate-coords
     coords
     rotation)
    scale)
   translation))

(defun translate-coords (coords translation)
  (f-list #'+ coords translation))

(defun scale-coords (coords scale)
  (mapcar (^ (coord) (* coord scale)) coords))

(defun rotate-coords (coords basis-vectors)
  (loop for basis-vector in basis-vectors collect
	(lvector-dot-product coords basis-vector)))


(defun translate-coordss (coordss translation)
  (mapcar (^ (coords) (translate-coords coords translation)) coordss))


(defun all-ag-or-sr-names-p (names)
  (not (filter #'ag-or-sr-name-p names)))

(defun procrustes-slave-onto-master (master-identifier master-names master-coordss 
				     slave-identifier  slave-names  slave-coordss
				     &optional &key 
					       (directory (if (running-on-windows-p)
                                                              (uw-sfnr "mds/procrustes/tmp-files/" :assertIsDir t)
							    "/tmp/"))
					       scalep
					       name-subset
                                               ag-only
                                               sr-only
					       )

  ;; special case when we are comparing sequence names and hi names because of the -ag -sr stuff
  ;; when we are pcing sequence onto HI, add -ag to the seq names (the slave names)
  ;; when we are pcing HI onto sequence, remove the -sr names totally, and add -ag to the sequence names (slave names)
  (cond ((and (all-ag-or-sr-names-p master-names)
              (not (all-ag-or-sr-names-p slave-names)))
         ;; likely pc of sequence onto HI, add -ag to the seqence names
         (setq slave-names (mapcar #'suffix-as-ag slave-names)))
        ((and (all-ag-or-sr-names-p slave-names)
              (not (all-ag-or-sr-names-p master-names)))
         ;; likely pc of HI onto sequence, remove sr strains completely, and remove -ag from the ag names
         (setq slave-names (mapcar #'remove-ag-sr-from-name (collect #'ag-name-p slave-names)))))

  (setq name-subset
    (cond (ag-only (if name-subset
                       (loop for name in name-subset
                           when (ag-name-p name)
                           collect name)
                     (loop for name in master-names when (ag-name-p name) collect name)))
	  (sr-only (if name-subset
                       (loop for name in name-subset
                           when (sr-name-p name)
                           collect name)
                     (loop for name in master-names when (sr-name-p name) collect name)))
	  (t name-subset)))

  ;; subset the data, put in originally for calling on ag and sr only
  (if name-subset
      (progn
	(setq master-coordss
	  (loop for master-name in master-names for master-coords in master-coordss when (member master-name name-subset) collect master-coords))
	(setq master-names
	  (loop for master-name in master-names when (member master-name name-subset) collect master-name))
	(setq slave-coordss  ;; padded below if more master dims than slave dims
	  (loop for slave-name in slave-names for slave-coords in slave-coordss when (member slave-name name-subset) collect slave-coords))
	(setq slave-names
	  (loop for slave-name in slave-names when (member slave-name name-subset) collect slave-name))
	))

  (let* ((master-filename (format nil "~a~a" directory master-identifier))
	 (slave-filename  (format nil "~a~a" directory slave-identifier))
	 (names-coordss-output-filename (format nil "~a~a-~a.namescoordss" directory master-identifier slave-identifier))
	 (diagnostics-output-filename  (format nil "~a~a-~a.diagnostics" directory master-identifier slave-identifier))
	 (common-names-filename  (format nil "~a~a-~a.common" directory master-identifier slave-identifier))
	 (master-num-dimensions (length (car master-coordss)))
	 (slave-num-dimensions  (length (car slave-coordss)))
	 (num-dimensions (if (zerop slave-num-dimensions)
			     (if (not (zerop master-num-dimensions))
				 master-num-dimensions  ;; empty save names, might have been excluded by the name subsetting
			       (error "no coords (or all strains excluded) for master and save"))
			   (if (= master-num-dimensions slave-num-dimensions)
			       master-num-dimensions
			     (if (> master-num-dimensions slave-num-dimensions)
				 (let ((zero-pad-coordss (loop for i below (- master-num-dimensions slave-num-dimensions) collect 0)))
				   (setq slave-coordss (mapcar (^ (coordss) (append coordss zero-pad-coordss)) slave-coordss))
				   master-num-dimensions)
			       (error "for now num dimensions in master must be >= num dimensions in slave"))))))

    ;; on one test, the coords did not have to be sorted
    (output-names-and-coordss
     master-filename
     master-names
     master-coordss)
    (output-names-and-coordss
     slave-filename
     slave-names
     slave-coordss)
    (fi (reverse (intersection master-names slave-names))
	common-names-filename
	:supersede
	nil
	:write-outer-list-elements-individually t)
    (procrustes-slave-onto-master-by-filename
     master-filename
     slave-filename
     :common-names-filename common-names-filename
     :num-dimensions num-dimensions
     :names-coordss-output-filename names-coordss-output-filename 
     :diagnostics-output-filename diagnostics-output-filename
     :scalep scalep)
    (let ((mapped-names-and-coordss (fi-in-readline 
                                     #+:allegro
                                     names-coordss-output-filename
                                     #+:lispworks
                                     (forward-slash-to-back-slash-in-string names-coordss-output-filename)
                                     :line-process-f #'space-delimited-string-to-list)))
      ;; -------- sometimes procrustes returns nan nan for the coords (i think when just one, and scaled).  filter these out.
      (setq mapped-names-and-coordss
        (filter (^ (l) (equal '(nan nan) (cdr l))) mapped-names-and-coordss))
      (multiple-value-bind (rotation scale translation dims rms av sd)
	  (transformations-from-diagnostics-filename diagnostics-output-filename)
	(values
	 (mapcar #'car mapped-names-and-coordss)
	 (mapcar #'cdr mapped-names-and-coordss)
	 rotation
	 scale
	 translation
	 dims
	 rms
	 av 
	 sd)))))


(defun procrustes-slave-onto-master-saves (slave-save master-save &optional &key scalep name-subset)
  (let* ((master-table (table-from-save master-save))
	 (master-identifier (format nil "~a-~d" (hi-table-name master-table) (krandom 100000)))
	 (master-names (hi-table-antigens master-table))
	 (master-coordss (coordss (starting-coordss-or-best-batch-from-save master-save)))
	 (slave-table (table-from-save slave-save))
	 (slave-identifier (format nil "~a-~d" (hi-table-name slave-table) (krandom 100000)))
	 (slave-names (hi-table-antigens slave-table))
	 (slave-coordss (coordss (starting-coordss-or-best-batch-from-save slave-save))))
    (procrustes-slave-onto-master 
     master-identifier
     master-names
     master-coordss
     slave-identifier
     slave-names
     slave-coordss
     :scalep scalep
     :name-subset name-subset)))

#|
raw data in the diagnostics file (parsed out below):
(DATAFILE1 /TMP/NAME-NOT-SET-EXTRACT-EXTRACT-EXTRACT-F-AG-SR-F-UNAGSR-AG-SR-58782) 
(DATAFILE2 /TMP/NAME-NOT-SET-EXTRACT-EXTRACT-EXTRACT-F-AG-SR-F-UNAGSR-AG-SR-80758) 
(DIMENSION 2) 
(MATCH 116) 
(READ NUM1 116 NUM2 116 EXAMPLES NUM 116 MATCHES) 
(TRANSFORMATIONS TO CHANGE MATCHEDDATA2 INTO MATCHEDDATA1 FOLLOW) 
(OPTIMAL-ROTATION/REFLECTION-MATRIX (0.998851 0.04793) (0.04793 -0.998851)) 
(OPTIMAL-SCALE 1.0) 
(OPTIMAL-TRANSLATION 0.106583 9.291544) 
(RMS 0.011389) 
(AVEDIST 0.007951) 
(SD 0.008154) 
|#

(defun transformations-from-diagnostics-filename (diagnostics-filename)
  (let* ((raw-data (fi-in-readline diagnostics-filename
				   :line-process-f #'space-delimited-string-to-list-replacing-colon-with-dash))
	 (scale       (assoc-value-1 'optimal-scale                      raw-data))
	 (translation (assoc-value   'optimal-translation                raw-data))
	 (rotation    (apply #'transpose (assoc-value   'optimal-rotation/reflection-matrix raw-data)))
	 (dims        (assoc-value-1 'dimension                          raw-data))
	 (rms         (assoc-value-1 'rms                                raw-data))
	 (av          (assoc-value-1 'avedist                            raw-data))
	 (sd          (assoc-value-1 'sd                                 raw-data)))
    (values 
     rotation      ;; this order, rotate, scale, translate, is also the order to apply
     scale 
     translation
     dims
     rms
     av
     sd)))
    

(defun procrustes-slave-onto-master-by-filename (master-filename slave-filename &optional &key common-names-filename 
											  num-dimensions
											  names-coordss-output-filename
											  diagnostics-output-filename
											  scalep)
  (if (not common-names-filename)
      (error "need to dig out the common names from the master and slave files"))
  (if (not num-dimensions)
      (error "need to dig out the num-dimensions from the master and slave files"))
  ;; procrustes1 is now superseded by procrustes2 (which takes an extra argument for whether or not to scale)
  ;; gcc -lm -o Procrustes2-for-lisp -O Procrustes2-for-lisp.c        ;; compiles but does not gererate output w/o does not work w/o the -O
  ;; 2002-07-03 Procrustes2-for-lisp is now superseded by Procrustes3-for-lisp as per ASL's email 2002-06-29
  ;;   fixing problem when no scaling, and sundry other problems (see alan's email).  also adding output of the diagnostics
  ;;   to a file as well as to stdout and stderr (and the file output set up so it is easily parsed by lisp).  and the
  ;;   diagnoistics output filename being a new parameter one before the end (seems better to put it next to the other output
  ;;   filename
  ;; gcc -lm -o Procrustes3-for-lisp -O Procrustes3-for-lisp.c
  ;; gcc -lm -o Procrustes3-for-lisp-mac-ppc -O Procrustes3-for-lisp.c
  ;; gcc -lm -o Procrustes3-for-lisp-mac-intel -O Procrustes3-for-lisp.c
  (let ((command (format nil "~a \"~a\" \"~a\" ~d \"~a\" \"~a\" \"~a\" ~d"
			 (if (running-on-windows-p)
			     (uw-sfnr "mds/procrustes/Debug/Procrustes3-for-lisp.exe" :assertIsFile t)
			   (let ((processor (car (run-shell-command-wait-and-collect-output "uname -m")))
                                 (os        (car (run-shell-command-wait-and-collect-output "uname -s"))))
			     (cond ((equal processor "Power Macintosh")
				    (uw-sfnr "mds/procrustes/Procrustes3-for-lisp-mac-ppc" :assertIsFile t))
				   ((and (or (equal processor "i386") (equal processor "x86_64")) (equal os "Darwin"))
				    (uw-sfnr "mds/procrustes/Procrustes3-for-lisp-i386-Darwin" :assertIsFile t))
                                   ((and (or (equal processor "i686") (equal processor "x86_64")) (equal os "Linux")) 
				    (uw-sfnr "mds/procrustes/Procrustes3-for-lisp-i686-Linux" :assertIsFile t))
				   (t (error "Procrustes3-for-lisp.c needs to be compiled for your OS/processor combination")))))
			 (checkPath 
			  (if (running-on-windows-p) 
			      (make-windows-lisp-filename-passable-as-argument master-filename)
			    master-filename)
			  :assertIsFile t)

			 (checkPath 
			  (if (running-on-windows-p)
			      (make-windows-lisp-filename-passable-as-argument slave-filename)
			    slave-filename)
			  :assertIsFile t)

			 num-dimensions

			 (checkPath 
			  (if (running-on-windows-p)
			      (make-windows-lisp-filename-passable-as-argument common-names-filename)
			    common-names-filename)
			  :assertIsFile t)
			 
			 (if (running-on-windows-p)
			     (make-windows-lisp-filename-passable-as-argument names-coordss-output-filename)
			   names-coordss-output-filename)

			 (if (running-on-windows-p)
			     (make-windows-lisp-filename-passable-as-argument diagnostics-output-filename)
			   diagnostics-output-filename)

			 (bool->bit scalep)
			 )))
    #+:allegro
    (run-shell-command command)
    #+:lispworks
    (if (running-on-windows-p)
	(sys:call-system command :wait t)
      (sys:open-pipe command :direction :io))
   ))


(defun procrustes-among-runs (save &optional &key (num-runs (length (batch-runs-from-save save))) name-subset)
  (loop for i below num-runs collect
	(loop for j below num-runs collect
	      (2dp (nth-value
		    7
		    (procrustes-slave-onto-master-saves 
		     (set-nth-best-batch-as-starting-coordss-in-save j save)
		     (set-nth-best-batch-as-starting-coordss-in-save i save)
		     :name-subset name-subset))))))
