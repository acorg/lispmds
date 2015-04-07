(in-package user)

;;;----------------------------------------------------------------------
;;;                          timeseries
;;;----------------------------------------------------------------------

(defun intial-hacky-timeseries (save 
				&optional &key
					  (date-prefix "")  ;; i'm thinking this is the lab, could also be the date made
					  mds-window
					  directory
					  marked-strains
					  years month-starts month-ends
                                          (include-sera-as-reference-frame-points t))
  (let* ((table-from-save (table-from-save save))
	 (reference-frame-points (append (mapcar #'suffix-as-ag (reference-antigens-from-save save))
                                         marked-strains
					 (if include-sera-as-reference-frame-points
                                             (hi-table-sera-short table-from-save)
                                           nil)))
	 (all-epi-strains (my-set-difference (hi-table-antigens table-from-save) reference-frame-points))
         (all-image-map-data (image-map-of-mds-window-from-save save))
         (all-image-map-strain-data (cdr (butlast all-image-map-data))) ;; loose the map and \map
         (all-image-map-data-alist (transpose (hi-table-antigens table-from-save)
                                              all-image-map-strain-data)))
    (run-shell-command (format nil "mkdir ~a" directory) :wait t)
    (raise-strains-from-strain-names mds-window marked-strains)
    (tk-put mds-window "mkText \"~a ~a\" 61 60 date nw black {Times 24}" "Year" "Month")
    (loop for year in years
  	for month-start in month-starts
  	for month-end   in month-ends do
	  (loop for month from month-start to month-end do
		(let ((filename (format nil "~a/~d-~2,'0d" directory year month))
		      (this-months-strains (loop for epi-strain in all-epi-strains 
					       when (and (eql (isolation-date-year (remove-ag-sr-from-name epi-strain)) year)
							 (eql (isolation-date-month (remove-ag-sr-from-name epi-strain)) month))
					       collect epi-strain)))

		  (print (list year month))

		  ;; file names and isolation dates
		  (fll 
		   (sort-nth
		    3
		    (sort-nth 
		     0
		     (loop for strain in this-months-strains collect
			   (list (remove-ag-sr-from-name strain) 
				 (isolation-date-year  (remove-ag-sr-from-name strain))
				 (isolation-date-month (remove-ag-sr-from-name strain))
				 (isolation-date-day   (remove-ag-sr-from-name strain))))
		     #'strain-name-<))
		   :filename (format nil "~a.names" filename))
                  
                  ;; image-map data
                  (fi (loop for strain in (append reference-frame-points this-months-strains) collect
                            (assoc-value-1 strain all-image-map-data-alist))
                      (format nil "~a.imagemap" filename)
                      :error
                      t
                      :write-outer-list-elements-individually t)

		  ;; show only a time subset of epi strains
		  (move-strains-out-of-view-excluding-names 
		   mds-window
		   (append reference-frame-points
			   this-months-strains))
		  (tk-put mds-window "setTextText date \"~a ~d ~a\"" date-prefix year (month-number-to-name month))

		  ;; make an image
		  (tk-put mds-window "update")
		  (tk-put mds-window ".c postscript -file ~a.ps" filename)
		  (sleep 2)
		  (run-shell-command (format nil "mogrify -format pdf ~a ~a" 
					     (format nil "~a.ps"  filename)
					     (format nil "~a.pdf" filename))
				     :wait t)
		  (sleep 2)
                  (run-shell-command (format nil "mogrify -format png ~a ~a" 
					     (format nil "~a.ps"  filename)
					     (format nil "~a.png" filename))
				     :wait t)

		  ;; restore all strains
		  (move-strains-into-view-excluding-names 
		   mds-window
		   (append reference-frame-points
			   this-months-strains))
		  )))
    (run-shell-command (format nil "mkdir ~a/png"      directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/imagemap" directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/names"    directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/ps"       directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/pdf"       directory) :wait t)

    (run-shell-command (format nil "mv ~a/*.png      ~a/png/"      directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.imagemap ~a/imagemap/" directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.names    ~a/names/"    directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.ps       ~a/ps/"       directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.pdf      ~a/pdf/"      directory directory) :wait t)

    (run-shell-command (format nil "pushd ~a; zip -r png      png;      popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r imagemap imagemap; popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r names    names;    popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r ps       ps;       popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r pdf      pdf;      popd" directory) :wait t)
    ))





;;;----------------------------------------------------------------------
;;;              timseries on a geographic map
;;;----------------------------------------------------------------------

(defun show-strains-geographically-in-existing-tk-canvas (tk 
							  plot-spec
							  &optional &key 
								    dot-size  ;;(dot-size 0.1)
								    font-size ;;(font-size 1)
								    fine-grid-size ;; (fine-grid-size  0.05)  ;; every 10 is 3x
								    (coarse-grid-size (if fine-grid-size (* 3 fine-grid-size)))
								    (fine-grid-step 1)
								    (coarse-grid-step 10)
								    (image-scale 3)
								    (data-scale 1))

  (let ((step (if (and coarse-grid-size (not fine-grid-size)) coarse-grid-step fine-grid-step)))
    (if (or fine-grid-size coarse-grid-size)
	(progn
	  ;;vertical grid
	  (loop for x from 0 to 360 by step do
		(tk-put tk ".c create line ~d ~d ~d ~d -fill gray70 -width ~d"
			(* x image-scale)
			0
			(* x image-scale)
			(* 180 image-scale)
			(if (zerop (mod x coarse-grid-step)) coarse-grid-size fine-grid-size)
			))
	  ;; horizontal grid
	  (loop for y from 0 to 180 by step do
		(tk-put tk ".c create line ~d ~d ~d ~d -fill gray70 -width ~d"
			0
			(* y image-scale)
			(* 360 image-scale)
			(* y image-scale)
			(if (zerop (mod y coarse-grid-step)) coarse-grid-size fine-grid-size))))))

  (loop for psline in plot-spec do
	(let (;;(name     (car psline))
	      (coords        (snoop-keyword-arg :cs psline :not-found-action :error))
	      (location      (snoop-keyword-arg :lo psline :not-found-action :return-nil))
	      (color         (snoop-keyword-arg :co psline :not-found-action :return-nil))
	      (outline-color (snoop-keyword-arg :oc psline :not-found-action :return-nil)))
	  
	  (let ((x (* (nth 0 coords) image-scale data-scale))
		(y (* (nth 1 coords) image-scale data-scale)))
	    ;; dots
	    (if dot-size
		(tk-put tk ".c create oval ~d ~d ~d ~d -outline ~a -fill ~a" 
			(- x dot-size)
			(- y dot-size)
			(+ x dot-size)
			(+ y dot-size)
			(if outline-color outline-color "black")
			(if color         color         "black")))
	    ;; names
	    (if font-size
		(tk-put tk ".c create text ~d ~d -text ~s -anchor se -fill black -font {Times ~d}"
			x y
			location
			font-size)))))

  (tk-put tk "update")
  tk)


(defun make-geographic-map-image (&optional &key 
					    plot-spec
					    (label "")
					    (label-color "black")
					    (label-font-size 24)
					    background-image 
					    dot-size  ;;(dot-size 0.1)
					    font-size ;;(font-size 1)
					    fine-grid-size ;; (fine-grid-size  0.05)  ;; every 10 is 3x
					    (coarse-grid-size (if fine-grid-size (* 3 fine-grid-size)))
					    (fine-grid-step 1)
					    (coarse-grid-step 10)
					    (image-scale 3)
					    (data-scale 1)  ;; to-put-data-into-360x180
					    filename-without-suffix-for-ps-pdf-png)

  (let ((tk (tk-open)))
    (tk-put tk "canvas .c -bg white -width ~d -height ~d"
	    (* 360 image-scale)
	    (* 180 image-scale))
    (tk-put tk "pack .c")

    ;;mogrify -resize 1080x880 wrldnanb.gif
    ;;(tk-put tk "image create photo mapbackground -file mds/investigations/spatial/maps/wrldnanb-scaled.gif")
    ;;mogrify -resize 1080x552 utmworld.gif
    ;;(tk-put tk "image create photo mapbackground -file mds/investigations/spatial/maps/utmworld-scaled.gif")
    ;;mogrify -resize 1080x528 world-grid-outline.gif  765x374 should be 1080x540 (* 374 (/ 1080 765.0)) = 528.0
    ;; mogrify -format gif -resize 1130 world-grid-outline.jpg
    ;;(tk-put tk "image create photo mapbackground -file mds/investigations/spatial/maps/world-grid-outline-scaled.gif")
    ;;(tk-put tk ".c create image -9 -17 -anchor nw -image mapbackground")
    ;;(tk-put tk ".c create image 0 0 -anchor nw -image mapbackground")
    ;; Earth_satellite_plane is 1024x512
    ;;(tk-put tk "image create photo mapbackground -file mds/investigations/spatial/maps/Earth_satellite_plane.gif")

    (if background-image
	(progn
	  (tk-put tk "image create photo mapbackground -file ~a" background-image)
	  (tk-put tk ".c create image 0 0 -anchor nw -image mapbackground")))

    (sleep 6)

    (show-strains-geographically-in-existing-tk-canvas 
     tk
     plot-spec
     :dot-size         dot-size
     :font-size        font-size
     :fine-grid-size   fine-grid-size
     :coarse-grid-size coarse-grid-size
     :fine-grid-step   fine-grid-step 
     :coarse-grid-step coarse-grid-step
     :image-scale      image-scale
     :data-scale       data-scale)

    (tk-put tk ".c create text 10 5 -text ~s -anchor nw -fill ~a -font {Times ~d}" label label-color label-font-size)

    (tk-put tk "update")
    (if filename-without-suffix-for-ps-pdf-png
	(progn
	  (tk-put tk "update")
	  (sleep 10)
	  (tk-put tk ".c postscript -file ~a.ps" filename-without-suffix-for-ps-pdf-png)
	  (sleep 10)
	  ;;(ps-to-pdf (format nil "~a.ps" filename-without-suffix-for-ps-pdf-png))
	  (ps-to-png (format nil "~a.ps" filename-without-suffix-for-ps-pdf-png))
	  ))
    tk
    ))

(defun intial-hacky-geographic-timeseries (&optional &key
						     save
						     plot-spec
						     (date-prefix "")  
						     directory  ;; in which results go
						     ;;marked-strains
						     years month-starts month-ends
						     (image-scale 3)
						     (fine-grid-size  0.075)
						     (label-color "black")
						     (label-font-size 24)
						     background-image)
  (let ((all-epi-strains
	 (if save
	     (let* ((table-from-save (table-from-save save))
		    (reference-frame-points (append (mapcar #'suffix-as-ag (reference-antigens-from-save save))
						    (hi-table-sera-short table-from-save)))
		    (all-epi-strains (my-set-difference (hi-table-antigens table-from-save) reference-frame-points)))
	       all-epi-strains)
	   (if plot-spec
	       (nths 0 plot-spec)
	     (error "~%Expected either a save or plot spec and got neither"))))
	(plot-spec (if plot-spec plot-spec (plot-spec-from-save save))))

    (run-shell-command (format nil "mkdir ~a" directory) :wait t)

    (loop for year in years
  	for month-start in month-starts
  	for month-end   in month-ends do
	  (loop for month from month-start to month-end do
		(let ((filename (format nil "~a/~d-~2,'0d" directory year month))
		      (this-months-strains (loop for epi-strain in all-epi-strains 
					       when (and (eql (isolation-date-year  (remove-ag-sr-from-name epi-strain)) year)
							 (eql (isolation-date-month (remove-ag-sr-from-name epi-strain)) month))
					       collect epi-strain)))

		  (print (list year month))

		  ;; file names and isolation dates
		  (fll 
		   (sort-nth
		    3
		    (sort-nth 
		     0
		     (loop for strain in this-months-strains collect
			   (list (remove-ag-sr-from-name strain) 
				 (isolation-date-year  (remove-ag-sr-from-name strain))
				 (isolation-date-month (remove-ag-sr-from-name strain))
				 (isolation-date-day   (remove-ag-sr-from-name strain))))
		     #'strain-name-<))
		   :filename (format nil "~a.names" filename))

		  (let ((tk (make-geographic-map-image
			     :plot-spec (loop for strain in this-months-strains collect (assoc strain plot-spec))
			     :label (format nil "~a ~a ~a" 
					    date-prefix 
					    year
					    (month-number-to-name month))
			     :label-color label-color
			     :label-font-size label-font-size
			     :dot-size 1 ;;0.1
			     ;;:font-size 4 ;;1
			     :fine-grid-size  fine-grid-size
			     ;;:coarse-grid-size  0.225
			     :image-scale image-scale
			     :background-image background-image
			     :filename-without-suffix-for-ps-pdf-png filename)))
		    (tk-close tk))

		  )))

    (run-shell-command (format nil "mkdir ~a/png"   directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/names" directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/ps"    directory) :wait t)

    (run-shell-command (format nil "mv ~a/*.png   ~a/png/"   directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.names ~a/names/" directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.ps    ~a/ps/"    directory directory) :wait t)

    (run-shell-command (format nil "pushd ~a; zip -r png   png;   popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r names names; popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r ps    ps;    popd" directory) :wait t)
    ))



;;;----------------------------------------------------------------------
;;;                          web pages
;;;----------------------------------------------------------------------

(defun previous-year-month (year month)
  (if (= 1 month)
      (list (dec year) 12)
    (list year (dec month))))

(defun next-year-month (year month)
  (if (= 12 month)
      (list (inc year) 1)
    (list year (inc month))))

(defun webpage-timeseries-separate-lines-maps (timeseries-input-directories-s  ;; each list on a separate line
					       output-directory
					       &optional &key
							 (root-directory "")
							 (directory-back-offset "")
							 small-suffices-s  ;; "-small" is normal. should follow same list structure as timeseries-input-directories-s
							 (blank-small-suffices-s small-suffices-s)
							 (title "")
							 (small-legend "")
							 years month-starts month-ends
							 (if-exists-action :error))

  (run-shell-command (format nil "mkdir ~a~a" root-directory output-directory) :wait t)

  (cp (string-append (sys:getenv *MDS-ROOT-VAR*) "/src/mds/mds/overlib.js") (format nil "~a~a" root-directory output-directory))

  (loop for year in years
      for month-start in month-starts
      for month-end   in month-ends do
	(loop for month from month-start to month-end do
	      (let* ((this-year-month     (format nil "~d-~2,'0d" year month))
		     (previous-year-month (apply #'format nil "~d-~2,'0d" (previous-year-month year month)))
		     (next-year-month     (apply #'format nil "~d-~2,'0d" (next-year-month year month)))
		     (filename (format nil "~a~a/~a.html" root-directory output-directory this-year-month)))

		(with-open-file (out filename :direction :output :if-exists if-exists-action)

		  (if (not (equal "" title))
		      (format out "<H1><CENTER>~a (~a ~a)</CENTER></H1>~%" title year (month-number-to-name month)))

		  (format out "<PRE>~%")

		  (if (and (= month (car month-starts)) (= year (car years)))
		      (format out "                  ")
		    (format out "<a href=~a.html>Previous (~a)</a>" previous-year-month previous-year-month))
		  (if (and (= month (car (last month-ends))) (= year (car (last years))))
		      (format out "               ")
		    (format out " <a href=~a.html>Next (~a)</a>" next-year-month next-year-month))
                  (format out "           Jump to:")
                  (loop for year in years do
                        (format out " <a href=~a-~2,'0d.html>~a</a>" 
                                year
                                (nth (position year years) month-starts)
                                year))
		  (newline out)
		  (newline out)

		  (loop for timeseries-input-directories in timeseries-input-directories-s do
			(progn
			  (loop for timeseries-input-directory in timeseries-input-directories do
				(if (file-or-directory-exists-p (format nil "~a~a/png/~a.png"
									root-directory
									timeseries-input-directory
									this-year-month))
				    (format out "<a href=~a~a/png/~a.png><IMG src=~a~a/png/~a.png alt=~a~a/png/~a.png></a>~%"
					    directory-back-offset
					    timeseries-input-directory
					    this-year-month
					    directory-back-offset
					    timeseries-input-directory
					    this-year-month
					    directory-back-offset
					    timeseries-input-directory
					    this-year-month
					    )))
			  (newline out)))

		  (if (not (equal "" small-legend))
		      (format out small-legend))

		  (format out "</PRE>~%")))))
  
  (loop for year in years
      for month-start in month-starts
      for month-end   in month-ends do
	(loop for month from month-start to month-end do
	      (let* ((this-year-month     (format nil "~d-~2,'0d" year month))
		     (previous-year-month (apply #'format nil "~d-~2,'0d" (previous-year-month year month)))
		     (next-year-month     (apply #'format nil "~d-~2,'0d" (next-year-month year month)))
		     (filename (format nil "~a~a/small-~a.html" root-directory output-directory this-year-month)))

		(with-open-file (out filename :direction :output :if-exists if-exists-action)

		  (if (not (equal "" title))
		      (format out "<H1><CENTER>~a (~a ~a)</CENTER></H1>~%" title year (month-number-to-name month)))

		  (format out "<PRE>~%")

		  (if (and (= month (car month-starts)) (= year (car years)))
		      (format out "                  ")
		    (format out "<a href=small-~a.html>Previous (~a)</a>" previous-year-month previous-year-month))
		  (if (and (= month (car (last month-ends))) (= year (car (last years))))
		      (format out "               ")
		    (format out " <a href=small-~a.html>Next (~a)</a>" next-year-month next-year-month))
                  (format out "           Jump to:")
                  (loop for year in years do
                        (format out " <a href=small-~a-~2,'0d.html>~a</a>" 
                                year
                                (nth (position year years) month-starts)
                                year))
                  (newline out)
		  (newline out)

		  (loop for timeseries-input-directories in timeseries-input-directories-s 
		      for image-line-index from 0 
		      for small-suffices in small-suffices-s 
		      for blank-small-suffices in blank-small-suffices-s do
			(progn
			  (loop for timeseries-input-directory in timeseries-input-directories 
			      for image-on-line-index from 0 
			      for small-suffix in small-suffices 
			      for blank-small-suffix in blank-small-suffices do
				(if (file-or-directory-exists-p (format nil "~a~a/png~a/~a.png"
									root-directory
									timeseries-input-directory
									small-suffix
									this-year-month))
				    (format out "<a href=~a-line-~d-image-~d-info.html><IMG src=~a~a/png~a/~a.png alt=~a~a/png~a/~a.png></a>"

					    this-year-month
					    image-line-index
					    image-on-line-index

					    directory-back-offset
					    timeseries-input-directory
					    small-suffix
					    this-year-month

					    directory-back-offset
					    timeseries-input-directory
					    small-suffix
					    this-year-month
					    )
				  
				  (format out "<IMG src=../blank-images/antigenic-map-blank~a.png alt=../blank-images/antigenic-map-blank~a.png>" 
					  blank-small-suffix
					  blank-small-suffix
					  )))
			  (newline out)))

		  (if (not (equal "" small-legend))
		      (format out small-legend))

		  (format out "</PRE>~%")))))
  
  (loop for year in years
      for month-start in month-starts
      for month-end   in month-ends do
	(loop for month from month-start to month-end do
	      (let* ((this-year-month     (format nil "~d-~2,'0d" year month))
		     (previous-year-month (apply #'format nil "~d-~2,'0d" (previous-year-month year month)))
		     (next-year-month     (apply #'format nil "~d-~2,'0d" (next-year-month year month)))
		     )

		(loop for timeseries-input-directories in timeseries-input-directories-s 
		    for image-line-index from 0 do
		      (loop for timeseries-input-directory in timeseries-input-directories 
			  for image-on-line-index from 0 do
			    
			    (let ((filename (format nil "~a~a/~a-line-~d-image-~d-info.html" 
						    root-directory
						    output-directory
						    this-year-month
						    image-line-index
						    image-on-line-index)))

			      (with-open-file (out filename :direction :output :if-exists if-exists-action)

                                (format out "<script type=\"text/javascript\" src=\"overlib.js\"></script>~%")
                                (format out "<div id=\"overDiv\" style=\"position:absolute; visibility:hidden; z-index:1000;\"></div>~%")

                                (format out "<H1><CENTER>Details for ~a ~a</CENTER></H1>~%" year (month-number-to-name month))

				(format out "<PRE>~%")


                                ;; next and previous links
                                (if (and (= month (car month-starts)) (= year (car years)))
                                    (format out "                  ")
                                  (format out "<a href=~a-line-~d-image-~d-info.html>Previous (~a)</a>" 
                                          previous-year-month
                                          image-line-index
                                          image-on-line-index
                                          previous-year-month))
                                (if (and (= month (car (last month-ends))) (= year (car (last years))))
                                    (format out "               ")
                                  (format out " <a href=~a-line-~d-image-~d-info.html>Next (~a)</a>" 
                                          next-year-month
                                          image-line-index
                                          image-on-line-index
                                          next-year-month))
                                (format out "           Jump to:")
                                (loop for year in years do
                                      (format out " <a href=~a-~2,'0d-line-~d-image-~d-info.html>~a</a>"
                                              year
                                              (nth (position year years) month-starts)
                                              image-line-index
                                              image-on-line-index
                                              year))
                                (format out "             <a href=small-~a.html>All-labs (~a)</a>"
                                        this-year-month
                                        this-year-month)
                                (newline out)
                                (newline out)


				(if (file-or-directory-exists-p (format nil "~a~a/png/~a.png"
									root-directory
									timeseries-input-directory
									this-year-month))
				    (progn
				      (format out "<a href=small-~a.html><IMG src=~a~a/png/~a.png alt=~a~a/png/~a.png usemap=\"#map\"></a>~%"
					      this-year-month
					      directory-back-offset
					      timeseries-input-directory
					      this-year-month
					      directory-back-offset
					      timeseries-input-directory
					      this-year-month
					      )

				      (newline out)
				      (newline out)
				      
				      (format out "Abbreviated strain names, isolation dates, unabbreviated strain names, and location information:~2%")
				      (loop for line in (fi-in-readline (format nil "~a~a/names/~a.names"
										root-directory
										timeseries-input-directory
										this-year-month))
					  do
					    (let ((strain-name (car (string-to-atoms line))))
					      (format out "  ~a    ~50a ~a~%" 
						      line
						      (string-longname-from-hiabbrev strain-name)
						      (let ((location-info (location-info-from-strain-name strain-name)))
							(if location-info
							    (pretty-location-info-from-location-info location-info)
							  "")))))
                                      
                                      ;; image map
                                      (format out "<MAP name=\"map\">~%")
                                      (let* ((image-map-lines-filename (format nil "~a~a/imagemap/~a.imagemap"
                                                                               root-directory
                                                                               timeseries-input-directory
                                                                               this-year-month))
                                             (image-map-lines
                                              (if (pathname-exists-p image-map-lines-filename)
                                                  (fi-in-readline image-map-lines-filename))))
                                        (loop for line in image-map-lines 
                                            do (format out "~a~%" line)))
                                      (format out "</MAP>~%")
                                      
				      ))

				(format out "</PRE>~%")))))))))



;;;----------------------------------------------------------------------
;;;                        utilities
;;;----------------------------------------------------------------------

(defun merge-ag-map-timeseries-directories (&optional &key 
						      source-directories
						      destination-directory
						      years-s
						      month-starts-s
						      month-ends-s
						      ;;(if-exists-action :error)
						      )
  (if (file-or-directory-exists-p destination-directory)
      (error "~%Destination directory exists~%"))
  (mkdir destination-directory)

  (let* ((source-sub-directories-s
	  (loop for source-directory in source-directories collect
		(loop for source-sub-directory in (mapcar
						   (^ (s)
						      (substring s 2))
						   (cdr 
						    (run-shell-command-wait-and-collect-output 
						     (format nil "cd ~a; find . -type d" source-directory)))) collect
		      source-sub-directory)))
	 (unique-source-sub-directories (remove-duplicates (apply #'append source-sub-directories-s) :test #'equal)))
  
    (loop for unique-source-sub-directory in unique-source-sub-directories do
	  (mkdir (format nil "~a/~a" destination-directory unique-source-sub-directory)))
    
    (loop for source-directory in source-directories
	for source-sub-directories in source-sub-directories-s
      for years in years-s
      for month-starts in month-starts-s
      for month-ends   in month-ends-s 
      do
	(loop for source-sub-directory in source-sub-directories do
	      (loop for year in years
		  for month-start in month-starts
		  for month-end   in month-ends do
		    (loop for month from month-start to month-end do
			  (cp 
			   (format nil "~a/~a/~d-~2,'0d.*" source-directory source-sub-directory year month)
			   (format nil "~a/~a/" destination-directory source-sub-directory))))))))