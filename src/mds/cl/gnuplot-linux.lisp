(in-package user)


#|
GNUPLOT ERROR:
  seems like we do not get any of a line, if gnuplot tries to clip out the zero 
  

(progn
  (gnuplot 
   '(0.0 0.0 0.0 25.0 40.0)
   :y-min 0 :y-max 100
   :x-min 1 :x-max 30)         ;; we need to see the x-min 1 here to see the bug below

  (gnuplot '((0 50) (30 50))   ;; does not work
	   ;;'((1 50) (30 50))   ;; works 
	   :element-linetype 2
	   :element-name "50% use"
	   :refresh nil))
|#


(defun fork-and-pipe (command)
  (let* ((stream (run-shell-command (string-append "exec " command)
				    :wait nil 
				    :input :stream 
				    :output :stream 
				    :error-output :output))
	 (stream-number (inc (length *tk-streams*))))
    (push (list stream-number stream 'possible-object-id)
	  *tk-streams*)
    stream-number))

(defun gnuplot-fi (lines &optional 
			 (if-exists-action :append) 
			 (filename "/tmp/tmp.gnuplot"))
  (loop for line in (if (listp lines) lines (list lines)) do
	(fi line filename if-exists-action t)))


(defun ll-to-lines (ll)
  (format nil "~{~{~a ~}~%~}" ll))

(defun float-ratios (coordss)
  (mapcar (^ (coords)
	     (mapcar (^ (coord)
			(if (typep coord 'ratio)     ;; check to see if we really have a ratio, as maybe best to leave integers as integers
			    (float coord)
			  coord))
		     coords))
	  coordss))

(defun single-float-double-floats (coordss)
  (mapcar (^ (coords)
	     (mapcar (^ (coord)
			(if (typep coord 'double-float)     ;; check to see if we really have a ratio, as maybe best to leave integers as integers
			    (coerce coord 'single-float)
			  coord))
		     coords))
	  coordss))

(defvar *last-gnuplot-plot-command*)
(defvar *last-gnuplot-plot-number*)
(defvar *gnuplot-stream*)

(setq *last-gnuplot-plot-command* nil)
(setq *last-gnuplot-plot-number* 0)
(setq *gnuplot-stream* nil)

(defun gnuplot-tmp-file-name (file-number)
  (format nil "/tmp/gp.tmp.~d" file-number))

(defun gnuplot-command (command &rest args)
  (apply #'tk-put *gnuplot-stream* command args))

(defun split-3d-data-for-gnuplot-grid (data)
  (loop for ((x y z) (next-x)) on data append
	(if (eql x next-x)
	    (list (list x y z))
	  (list (list x y z)
		nil))))	  

(defvar *unit-y-size-in-pixels*)    ;; window height when size=1 (width is 1.5xheight)
(setq *unit-y-size-in-pixels* 500)  

(defvar *gnuplot-internal-screen-ratio*)  ;; if we do not have this x to y ratio, then ratio=1 is not square
(setq *gnuplot-internal-screen-ratio* 1.5)

#|
the multiplot protocol
first call is with :multiplot t
(best also to call with null coordss, then we do not fill the plot with the first plot)
everying plotted before a multiplot gets erased when we do our first multiplot with data
then subsequent calls are with no :multiplot but with refresh nil
and we can use the x-size and y-size and origin parameters to move plots around
if we call with multiplot nil then we move out of multiplot mode
then we go back to filling the screen, and we loose the previous plots (other than the last) on the next redraw
(note we also stay at the origin of last plot, all in all, switching out of multiplot mode is not recommended)
new. call :next-multiplot when we move to the next multiplot (to purge the replotting of previous part plots)
  (so we can call composite gnuplot function such as gnuplot-correlation in mgnuplot without changing gnuplot-correlation

if we want an array of plots, use mgnuplots
(mgnuplots '(((0 1) (1 2) (2 3)) 
	     ((0 2) (1 1) (2 2.5))
	     ((0 2) (1 1) (2 2.5))
	     ((0 2) (1 1) (2 2.5))
	     ((0 2) (1 1) (2 2.5))
	     ((0 2) (1 1) (2 2.5))) 
	   :row-length 3
	   :row-step 0.3
	   :col-step 0.3
	   :ratio 1 :x-size 0.3
	   :window-scale 1.3    ;; general sizing of the window (we have to scale y as well as x for gnuplot)
	   :gnuplot-function #'gnuplot-correlation)

the plots will fit on the window only if the (row-length * row-step)+row-offset < 1.5 (approx)
and similarly for col-length*col-step + col-offset < 1 (approx)
note, col-length is currently derived from the row-length and the number of data sets
note, the 1.5 in overall col width is gnuplots ugly 1.5 aspect ratio
note, does not work with :gnuplot-functon gnuplots yet
|#


(defun derive-x-y-sizes (x-size y-size ratio)
  (if ratio
      (if (and x-size y-size)
	  (error "do not specify ratio and x-size and y-size")
	(if x-size
	    (values x-size (* x-size ratio) ratio)
	  (if y-size
	      (values (/ y-size ratio) y-size ratio)
	    (values 1.0 1.0 1.0))))
    (if (and x-size y-size)  ;; and (null ratio) because of the above
	(values
	 x-size
	 y-size
	 (float (/ y-size x-size)))
    (values
     x-size
     y-size
     ratio))))

#|
(defun derive-x-y-sizes (x-size y-size ratio window-ratio)
  (multiple-value-bind (x y r)
      (if ratio
	  (if (and x-size y-size)
	      (error "do not specify ratio and x-size and y-size")
	    (if x-size
		(values x-size (* x-size ratio) ratio)
	      (if y-size
		  (values (/ y-size ratio) y-size ratio)
		(values 1.0 1.0 1.0))))
	(if (and x-size y-size)  ;; and (null ratio) because of the above
	    (values
	     x-size
	     y-size
	     (float (/ y-size x-size)))
	  (values
	   x-size
	   y-size
	   ratio)))
    (if (and x y r window-ratio)
	(values
	 (* x window-ratio)
	 y
	 (* r window-ratio))
      (values
       x
       y
       r))))
|#

(defun gnuplot (datas &rest args)
  ;;(print (list 'to-gnuplot args))
  (let ((gnuplot-function (prog1 (snoop-keyword-arg :gnuplot-function args :not-found-action :return-nil)
			    (setq args (remove-keyword-and-arg :gnuplot-function args :not-found-action :ignore)))))
    (if gnuplot-function
	(apply gnuplot-function datas args)
      (apply #'gnuplot-basic datas args))))

	
(defun gnuplot-basic (coords &key ;;(style 'line)   
			    (refresh t)
			    (element-name "")
			    ;;(element-color (random-x-color))
			    element-symbol
			    element-dashes
			    (element-style 'lines)  ;; yerr, with :bar 'small and :element-pointtype 2 is good, 'points gives scatterplot
			                            ;; box is bar chart (but cannot fill)
			    element-linewidth
			    element-linetype   ;; do test in gnuplot to see the types
			    element-pointtype  ;; 2 is a horizontal bar (good for use with element-style yerrorbars)
			    element-pointsize
			    bar                ;; small indicates no (error)bar(0.0), large is size 1.0, number can be between
			    boxwidth
			    x-min x-max y-min y-max z-min z-max
			    title
			    x-title y-title z-title
			    y2-title
			    x-log y-log z-log   ;;can be numbers to give base
			    x-tics y-tics z-tics  ;;can be label-position pairs or just labels
			    y2-tics
			    x-tic-format y-tic-format z-tic-format
			    tic-direction ;;in or out
			    ;;latex
			    (ps-filename "/home/dsmith/misc/ps/xy.ps")
			    hardcopy-only  ;;also doubles as hardcopy fontsize
			    legend-position
			    ;;(legend-mapped t)
			    ;;big-font
			    ;;tag
			    label ;;(text x y)
			    labels
			    x-size
			    y-size
			    ratio     ;; ratio of y-length to x-length (so 1 is square).  can be used in combination of 1 of x-size or y-size
			    3d-plot       ;; say this explicity, we used to infer from the number of coords, but not possible when errorbars
			    (split-3d-data-for-gnuplot-grid 3d-plot)  ;; add newline between lines to give grid 
			    dgrid3d  ;; mutually exclusive with split-3d-data-for-gnuplot-grid
			    (surface 3d-plot)
			    (hidden3d 3d-plot)
			    (ticslevel (if 3d-plot 0 nil))
			    view  ;; string or list, default is  60 rot_x, 30 rot_z, 1 scale, 1 scale_z
			    (multiplot 'not-set)
			    next-multiplot
			    origin
			    (window-scale 1)
			    ;;window-ratio
			    tmargin bmargin lmargin rmargin
			    )
  


  (multiple-value-bind (derived-x-size derived-y-size derived-ratio)
      (derive-x-y-sizes x-size y-size ratio)
    (setq x-size derived-x-size)
    (setq y-size derived-y-size)
    (setq ratio  derived-ratio))

  (let ((width  (round (* (or x-size 1) (* *gnuplot-internal-screen-ratio* *unit-y-size-in-pixels* window-scale))))
	(height (round (* (or y-size 1) (* *unit-y-size-in-pixels* window-scale)))))
    (if refresh (setq *gnuplot-stream* (fork-and-pipe (format nil "gnuplot -geometry ~dx~d" width height))))
    ;;(if refresh (setq *gnuplot-stream* (fork-and-pipe "gnuplot")))
    )
    
  (if (not refresh)  ;; otherwise we are setting the size with the -geometry above
      (if (or x-size y-size)
	  (gnuplot-command "set size ~d, ~d" x-size y-size)))  ;; derive-x-y-sizes above sets 0 or both
  
  (if ratio
      (gnuplot-command "set size ratio ~d" ratio))
  
  (if boxwidth
      (gnuplot-command "set boxwidth ~d" boxwidth))

  (if ps-filename (gnuplot-command "set out ~s" ps-filename))

  (if hardcopy-only (gnuplot-command "set te postscript eps ~a" 
				     (if (numberp hardcopy-only) hardcopy-only ""))) ;;fontsize

  (if (atom (car coords))
      (setq coords (loop for x below (length coords) for y in coords when y collect (list x y))))

  (if (eql t multiplot)
      (gnuplot-command "set multiplot")
    (if (null multiplot)
	(gnuplot-command "set nomultiplot")))
  
  (if origin 
      (progn
	(gnuplot-command "set origin ~d, ~d" (nth 0 origin) (nth 1 origin))
	(gnuplot-command "set nolabel")))  ;; because labels would otherwise appear on each plot ;; http://sparky.rice.edu/~hartigan/gnuplot.html
 
  (if tmargin (gnuplot-command "set tmargin ~d" tmargin))
  (if bmargin (gnuplot-command "set bmargin ~d" bmargin))
  (if rmargin (gnuplot-command "set rmargin ~d" rmargin))
  (if lmargin (gnuplot-command "set lmargin ~d" lmargin))
  
  (if (or x-min x-max) (gnuplot-command "set xr[~a:~a]" (if x-min x-min "") (if x-max x-max "")))
  (if (or y-min y-max) (gnuplot-command "set yr[~a:~a]" (if y-min y-min "") (if y-max y-max "")))
  (if (or z-min z-max) (gnuplot-command "set zr[~a:~a]" (if z-min z-min "") (if z-max z-max "")))
  
  (if x-log (gnuplot-command "set logscale x ~a" (if (numberp x-log) x-log "")))
  (if y-log (gnuplot-command "set logscale y ~a" (if (numberp y-log) y-log "")))
  (if z-log (gnuplot-command "set logscale z ~a" (if (numberp z-log) z-log "")))

  (if (and x-tics (listp (car x-tics)))
      (setq x-tics (mapcar (^ (label-position) (apply #'format nil "\"~a\" ~a" label-position)) x-tics)))
  (if (and y-tics (listp (car y-tics)))
      (setq y-tics (mapcar (^ (label-position) (apply #'format nil "\"~a\" ~a" label-position)) y-tics)))
  (if (and y2-tics (listp (car y2-tics)))
      (setq y2-tics (mapcar (^ (label-position) (apply #'format nil "\"~a\" ~a" label-position)) y2-tics)))
  (if (and z-tics (listp (car z-tics)))
      (setq z-tics (mapcar (^ (label-position) (apply #'format nil "\"~a\" ~a" label-position)) z-tics)))
    
  (if x-tics (gnuplot-command "set xtics (~{~a~})" (infix-operator "," x-tics)))
  (if y-tics (gnuplot-command "set ytics (~{~a~})" (infix-operator "," y-tics)))
  (if y2-tics (gnuplot-command "set y2tics (~{~a~})" (infix-operator "," y2-tics)))
  (if z-tics (gnuplot-command "set ztics (~{~a~})" (infix-operator "," z-tics)))

  (if x-tic-format (gnuplot-command "set format x ~s" x-tic-format))
  (if y-tic-format (gnuplot-command "set format y ~s" y-tic-format))
  (if z-tic-format (gnuplot-command "set format z ~s" z-tic-format))
  
  (if tic-direction (gnuplot-command "set tics ~a" (string-downcase tic-direction)))
  
  (if label (apply #'gnuplot-command "set label ~s at ~d,~d" label))
  (if labels (loop for label in labels do (apply #'gnuplot-command "set label ~s at ~d,~d" label)))
  
  (if title (gnuplot-command "set title ~s" title))

  (if x-title (gnuplot-command "set xlabel ~s 0,-1" x-title))   
  (if y-title (gnuplot-command "set ylabel ~s" y-title))
  (if y2-title (gnuplot-command "set y2label ~s" y2-title))
  (if z-title (gnuplot-command "set zlabel ~s" z-title))
  
  (if legend-position 
      (if (eql legend-position 'none)
	  (funcall #'gnuplot-command "set nokey")
	(if (numberp (car legend-position))
	    (apply #'gnuplot-command "set key ~a, ~a" legend-position)
	  (apply #'gnuplot-command "set key ~a ~a" (mapcar (^ (e) (string-downcase (string e))) legend-position)))))
    
  (if bar (gnuplot-command "set bar ~a" (string-downcase (string bar))))

  (if surface (gnuplot-command "set surface"))
  (if hidden3d (gnuplot-command "set hidden3d"))
  (if ticslevel (gnuplot-command "set ticslevel ~d" ticslevel))
  (if dgrid3d (gnuplot-command "set dgrid3d ~a" (if (listp dgrid3d) (apply #'format nil "~d,~d,~d" dgrid3d) 
						  (if (stringp dgrid3d) dgrid3d
						    ""))))
  (if view (gnuplot-command "set view ~a" 
			    (if (stringp view)
				view
			      (error "supply comma separated values in a string string for view for now"))))
			      ;;(format nil "~{~d ~}" view)  ;; just need to put a comman between the entries
			    
  (if refresh
      (progn 
	(setq *last-gnuplot-plot-command* (if 3d-plot "sp " "p "))
	(setq *last-gnuplot-plot-number* 0))
    (progn
      (if next-multiplot
	  (progn
	    (setq *last-gnuplot-plot-command* (if 3d-plot "sp " "p "))
	    ;;(setq *last-gnuplot-plot-number* 0)
	    )
	(setq *last-gnuplot-plot-command*
	  (string-append *last-gnuplot-plot-command* ", ")))
      (setq *last-gnuplot-plot-number* (inc *last-gnuplot-plot-number*))))

  (setq coords (single-float-double-floats (float-ratios coords)))  ;; because gnuplot messes up if we pass a ratio

  (gnuplot-fi (ll-to-lines 
	       (if split-3d-data-for-gnuplot-grid
		   (split-3d-data-for-gnuplot-grid coords)
		 coords))
	      :new-version (gnuplot-tmp-file-name *last-gnuplot-plot-number*))

  (if (not (null coords))
      (gnuplot-command
       (setq *last-gnuplot-plot-command*
	 (format nil "~a '~a' ~a ~a ~a"
		 *last-gnuplot-plot-command*
		 (gnuplot-tmp-file-name *last-gnuplot-plot-number*)
		 (if element-name
		     (format nil "title '~a'" element-name)
		   "")
		 (format nil "~{ ~a ~}"
			 (append (list (case element-style
					 (lines (if element-symbol "with linespoints" "with lines"))
					 (t (format nil "with ~a" (string-downcase (string element-style))))))
				 (if element-linetype
				     (list (format nil "lt ~d" element-linetype)))
				 (if element-linewidth
				     (list (format nil "lw ~d" element-linewidth)))
				 (if element-pointtype
				     (list (format nil "pt ~d" element-pointtype)))
				 (if element-pointsize
				     (list (format nil "ps ~d" element-pointsize)))
				 ))
		 (if element-dashes element-dashes "")
		 ))))
  )

;;;----------------------------------------------------------------------
;;;                          common uses
;;;----------------------------------------------------------------------

(defun gnuplot-correlation (pairs &rest plot-args)
  (let ((x-min (or (and (member :x-min plot-args) (snoop-keyword-arg :x-min plot-args))
		   (apply #'min (nths 0 pairs))))
	(x-max (or (and (member :x-max plot-args) (snoop-keyword-arg :x-max plot-args))
		   (apply #'max (nths 0 pairs))))
	(y-min (or (and (member :y-min plot-args) (snoop-keyword-arg :y-min plot-args))
		   (apply #'min (nths 1 pairs))))
	(y-max (or (and (member :y-max plot-args) (snoop-keyword-arg :y-max plot-args))
		   (apply #'max (nths 1 pairs))))
	)
    (let ((short-correlation-names (snoop-keyword-arg :short-correlation-names plot-args :not-found-action :return-nil))
	  (show-correlation-coefficient (snoop-keyword-arg :show-correlation-coefficient plot-args :not-found-action :return-t))
	  (show-diagonal (snoop-keyword-arg :show-diagonal plot-args :not-found-action :return-nil))
	  (diagonal-label (snoop-keyword-arg :diagonal-label plot-args :not-found-action :return-nil))
	  (zero-intercept (snoop-keyword-arg :zero-intercept plot-args :not-found-action :return-nil))
	  (plot-args (remove-keywords-and-args
		      '(:short-correlation-names :show-correlation-coefficient :show-diagonal :diagonal-label :zero-intercept)
		      plot-args
		      :not-found-action :ignore)))
      (multiple-value-bind (m c r)
	  (regression pairs :zero-intercept zero-intercept)
	(apply #'gnuplot 
	       pairs
	       :element-style 'points
	       :element-name (if show-correlation-coefficient
				 (format nil "~a~5,2f" 
					 (if short-correlation-names "r=" "Correlation ")
					 r)
			       "")
	       ;;:label
	       ;;(list (format nil "~a~5,2f" 
		;;	     (if short-correlation-names "r=" "Correlation ")
		;;	     r)
		 ;;    (+ x-min (* 0.1 (- x-max x-min)))
		  ;;   (- y-max (* 0.1 (- y-max y-min))))
	       :ratio 1
	       (append 
		(if (snoop-keyword-arg :x-size plot-args :not-found-action :return-nil)
		    nil
		  (list :x-size 0.75))
		plot-args
		(if (and (member :x-max plot-args) (not (member :y-max plot-args)))
		    `(:y-max ,(+ (* (snoop-keyword-arg :x-max plot-args) m) c))
		  (if (and (member :y-max plot-args) (not (member :x-max plot-args)))
		      (multiple-value-bind (my cy)
			  (regression (mapcar #'reverse (mapcar (^ (l) (firstn 2 l)) pairs)))
			`(:x-max ,(+ (* (snoop-keyword-arg :y-max plot-args) my) cy)))))))
	(gnuplot-regression-line m c x-min x-max y-min y-max :short-correlation-names short-correlation-names :refresh nil)
	(if show-diagonal
	    (gnuplot
	     (let ((min (min x-min y-min))
		   (max (max x-max y-max)))
	       (list (list min min)
		     (list max max)))
	     :element-name (if diagonal-label
			       diagonal-label
			     "")
	     :refresh nil))
	(values 
	 r
	 m c
	 pairs)))))


#|
we've had trouble for some time with regressions lines not showing.  
i now think it might have to do with clipping, this function replaced below
(defun gnuplot-regression-line (m c x-min x-max &optional &key
							  short-correlation-names
							  (refresh t))
  (gnuplot 
   (coords-from-m-c m c (+ x-min 0.0001) (- x-max 0.0001))
   :element-name (format nil "~am=~5,2f c=~5,2f" 
			 (if short-correlation-names "" "Regression ")
			 m c)
   :refresh refresh))
|#

(defun gnuplot-regression-line (m c x-min x-max y-min y-max &optional &key
								      short-correlation-names
								      (refresh t))
  (let ((y-at-min-x (+ (* m x-min) c))
	(y-at-max-x (+ (* m x-max) c))
	(x-at-min-y (/ (- y-min c) m))
	(x-at-max-y (/ (- y-max c) m)))
    (let ((min-coords (if (< y-at-min-x y-min)
			  (list x-at-min-y y-min)
			(list x-min y-at-min-x)))
	  (max-coords (if (> y-at-max-x y-max)
			  (list x-at-max-y y-max)
			(list x-max y-at-max-x))))
      (gnuplot 
       (list min-coords max-coords)
       :element-name (format nil "~am=~5,2f c=~5,2f" 
			     (if short-correlation-names "" "Regression ")
			     m c)
       :refresh refresh))))

(defun gnuplot-distribution (ys &rest plot-and-other-args)
  (let* ((round-to-nearest (if (snoop-keyword-arg :round-to-nearest plot-and-other-args :not-found-action :return-nil)
			       (snoop-keyword-arg :round-to-nearest plot-and-other-args)
			     nil))
	 (plot-args (if round-to-nearest
			(remove-keyword-and-arg :round-to-nearest plot-and-other-args)
		      plot-and-other-args)))
    (if (not (snoop-keyword-arg :ratio plot-args :not-found-action :return-nil))
	(setq plot-args (append plot-args '(:ratio square))))
    (if (and (not (snoop-keyword-arg :x-min plot-args :not-found-action :return-nil))
	     (plusp (apply #'min ys)))
	(setq plot-args (append plot-args '(:x-min 0))))
    (apply #'gnuplot 
	   (hist-to-proportion-hist 
	    (sort-hist 
	     (if round-to-nearest
		 (mapcar (^ (x) 
			    (round-to-nearest round-to-nearest x)) 
			 ys)
	       ys)))
	   :element-name (format nil "Average ~5,2f   StdDev ~5,2f" (av ys) (sd ys))
	   plot-args)
    (values
     (av ys)
     (sd ys))))

(defun funcall-function-args (params args)
  (let (last-arg)
    (loop for arg in args collect
	  (prog1
	      (if (and (functionp arg)
		       (if (eql :gnuplot-function last-arg)   ;; special case gnuplot-function.  this is already a function
			   (= 1 (length (arglist arg)))       ;; so we don't want to funcall it here, unless it is a one arg function
			 t))
		  (apply arg params)
		arg)
	    (setq last-arg arg)))))

(defun gnuplots-basic (gnuplot-f datas &rest plot-args)
  (apply gnuplot-f (cons (car datas) (funcall-function-args '(0) plot-args)))
  (let ((plot-args (if (snoop-keyword-arg :next-multiplot plot-args :not-found-action :return-nil)
		       (remove-keyword-and-arg     
			:next-multiplot
			(remove-keyword-and-arg
			 :refresh
			 plot-args
			 :not-found-action :ignore)
			:not-found-action :ignore)
		     plot-args)))
    (loop for data in (cdr datas) for i from 1 do
	  ;;(gnuplot data :refresh nil :element-linetype 1)
	  (progn
	    (if (zerop (mod i 5))
		(sleep 1))
	    (apply gnuplot-f data 
		   (append 
		    (funcall-function-args (list i) plot-args)
		    (list :refresh nil)))))))

(defun gnuplots (datas &rest plot-args)
  (apply #'gnuplots-basic #'gnuplot datas plot-args))

(defun gnuplot-correlations (datas &rest plot-args)
  (apply #'gnuplots-basic #'gnuplot-correlation datas plot-args))



(defun mgnuplots (datas &rest args) 
  ;; see documentation for recommended with the rest of the docs for multiplot
  (let ((row-length (prog1 (snoop-keyword-arg :row-length args :not-found-action :return-nil)
		      (setq args (remove-keyword-and-arg :row-length args :not-found-action :ignore))))
	(row-offset (prog1 (snoop-keyword-arg :row-offset args :not-found-action :return-nil)
		      (setq args (remove-keyword-and-arg :row-offset args :not-found-action :ignore))))
	(row-step   (prog1 (snoop-keyword-arg :row-step   args :not-found-action :return-nil)
		      (setq args (remove-keyword-and-arg :row-step   args :not-found-action :ignore))))
	;;(col-length (prog1 (snoop-keyword-arg :col-length args :not-found-action :return-nil)          
	;;              (setq args (remove-keyword-and-arg :col-length args :not-found-action :ignore))))
	(col-offset (prog1 (snoop-keyword-arg :col-offset args :not-found-action :return-nil)          
	              (setq args (remove-keyword-and-arg :col-offset args :not-found-action :ignore))))
	(col-step   (prog1 (snoop-keyword-arg :col-step   args :not-found-action :return-nil)          
	              (setq args (remove-keyword-and-arg :col-step args :not-found-action :ignore))))
	(starting-position-index (let ((arg (snoop-keyword-arg :starting-position-index args :not-found-action :return-nil)))
				   (if arg
				       (progn
					 (setq args (remove-keyword-and-arg :starting-position-index args))
					 arg)
				     0)))
	(window-scale (prog1 (snoop-keyword-arg :window-scale args :not-found-action :return-nil)          
			(setq args (remove-keyword-and-arg :window-scale args :not-found-action :ignore))))
	(refresh (let ((refresh-parameter (snoop-keyword-arg :refresh args :not-found-action :return-not-passed)))
		   (if (eql 'not-passed refresh-parameter)
		       t
		     (progn
		       (setq args (remove-keyword-and-arg :refresh args))
		       refresh-parameter))))
	;;(window-ratio (snoop-keyword-arg :window-ratio args :not-found-action :return-nil))
	;;(x-size (snoop-keyword-arg :x-size args :not-found-action :return-nil))
	;;(y-size (snoop-keyword-arg :y-size args :not-found-action :return-nil))
	;;(ratio  (snoop-keyword-arg :ratio  args :not-found-action :return-nil))
	)
    (if (not row-offset) (setq row-offset 0))
    (if (not col-offset) (setq col-offset 0))
    (if (not row-length) (setq row-length (length datas)))


    ;; we could do some clever defaulting of row-length, row-step etc, but let's just use the system for now
    ;; maybe even defaulting of x-size based on row-length
    ;; for now require the parameters
    ;;    (multiple-value-bind (derived-x-size derived-y-size)
    ;;	(derive-x-y-sizes 
    ;;	 (snoop-keyword-arg :x-size args :not-found-action :return-nil)
    ;;	 (snoop-keyword-arg :y-size args :not-found-action :return-nil)
    ;;	 (snoop-keyword-arg :ratio  args :not-found-action :return-nil))
    ;;      (if row-step
    ;;	  row-step
    ;;	(setq row-step
    ;;	  (/ 1 row-length))))      
    (if (not (and row-length row-step col-step))
	(error "supply row-length row-step col-step"))

    (setq row-step (/ row-step *gnuplot-internal-screen-ratio*)) ;; for gnuplots internal convention
    (if refresh
	(apply #'gnuplot '() :multiplot t :refresh t
	       (append 
		(if window-scale (list :window-scale window-scale))
		;;(if window-ratio (list :window-ratio window-ratio))
		;;(if x-size       (list :x-size       x-size))
		;;(if y-size       (list :y-size       y-size))
		;;(if ratio        (list :ratio        ratio))
		)))
    (loop for data in datas
	for i from starting-position-index do
	  (apply #'gnuplot data 
		 :origin (list (+ row-offset (* row-step (mod i row-length)))
			       (+ col-offset (* col-step (floor (/ i row-length)))))
		 (append 
		  (funcall-function-args (list i) args)
		  (list :next-multiplot t :refresh nil))))))


;;;----------------------------------------------------------------------
;;;                      hardcopy (ps and gif and jpg)
;;;----------------------------------------------------------------------

(defun gnuplot-hardcopy (&optional fontsize (filename "/home/dsmith/misc/ps/xy.ps") (size 1.0) (postscript-mode "eps"))
  (if (string-equal (aref filename 0) "~")
      (error "filename must not start with tilde"))
  (if filename (gnuplot-command "set out ~s" filename))
  (gnuplot-command "set size ~d,~d" size size)
  ;; NOTE DOING EPS as the postscript-mode 1/2s THE SIZE, and gives us encapsulated PS
  (gnuplot-command "set te postscript ~a color ~a" postscript-mode (if fontsize fontsize ""))
  (gnuplot-command "rep")
  (gnuplot-command "set out")
  (gnuplot-command "set te x11")
  (gnuplot-command "rep"))

(defun gnuplot-gif (&optional fontsize (filename "/home/dsmith/misc/ps/xy.ps"))
  ;; (in gnuplot 3.7, we should be able to go directly to gif)
  ;; doing landscape gives us the same aspect ratio as what we see on the screen, 
  ;; and the same size (eps 1/2s the size), but we need to make 0.75 size, so smaller gifs, 
  ;; and to rotate back to portrait with the mogrify
  (gnuplot-hardcopy fontsize filename 0.75 "landscape")
  (sleep 1)
  ;; uncomment here to make solid lines
  (progn
    (run-shell-command "mv -f /home/dsmith/misc/ps/xy.ps /home/dsmith/misc/ps/xy.ps.old")
    (run-shell-command "perl -pe \"s/Solid false/Solid true/\" /home/dsmith/misc/ps/xy.ps.old > /home/dsmith/misc/ps/xy.ps"))
  (sleep 1)
  (run-shell-command (format nil "mogrify -rotate 90 -format gif ~a" filename))
  )

(defun gnuplot-jpg (&optional fontsize (filename "/home/dsmith/misc/ps/xy.ps"))
  ;; doing landscape gives us the same aspect ratio as what we see on the screen, 
  ;; and the same size (eps 1/2s the size), but we need to make 0.75 size, so smaller gifs, 
  ;; and to rotate back to portrait with the mogrify
  (gnuplot-hardcopy fontsize filename 0.75 "landscape")
  (sleep 1)
  (run-shell-command (format nil "mogrify -rotate 90 -format jpg ~a" filename)))

(defun gnuplot-html (directory name &optional (format 'gif) fontsize)
  (let ((ps-filename (string-append directory "/" name ".ps"))
	(format-name (string-append name "." (string-downcase (string format)))))
    (funcall (if (eql format 'gif) #'gnuplot-gif
	       (if (eql format 'jpg) #'gnuplot-jpg
		 (error "unrecognized format ~a" format)))
	     fontsize
	     ps-filename)
    (format nil "<IMG SRC=~a alt=~a><HR>" format-name format-name)))


(defun gnuplot-exit ()
  (gnuplot-command "exit"))

  


#|
(gnuplot '((0 0) (1 2) (2 2) (3 1)))
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :x-min -2 :x-max 5
	 :y-min -1 :y-max 6)
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-name "foo"
	 :element-dashes 2
	 :x-title "test X title"
	 :y-title "test Y title"
	 :x-min -2 :x-max 5
	 :y-min -1 :y-max 6
	 ;;:gnuplot-y-size 0.4
	 :x-tics '(0 0.25)
	 :label '("label test" -1 4))
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 ;;:element-name "foo"
	 :element-style 'points
	 ;;:x-title "test X title"
	 ;;:y-title "test Y title"
	 ;;:x-min -2 :x-max 5
	 ;;:y-min -1 :y-max 6
	 ;;:gnuplot-y-size 0.4
	 ;;:x-tics '(0 0.25)
	 ;;:label '("label test" -1 4)
	 )
(gnuplot '((0 1) (1 1) (2 3) (3 2))
	 :element-name "bar"
	 :element-dashes 3
	 :refresh nil)
(gnuplot-hardcopy)
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-name "linestyle 0"
	 ;;:element-dashes 0
	 :x-title "test X title"
	 :y-title "test Y title"
	 :x-min 0 :x-max 3
	 :y-min 0 :y-max 24)
(loop for  i from 1 to 14 do
      (gnuplot `((0 ,(+ 1 i)) (1 ,(+ 1 i)) (2 ,(+ 3 i)) (3 ,(+ 2 i)))
	       :element-name (format nil "linestyle ~d" i)
	       ;;:element-dashes i
	       :refresh nil))

(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-style 'points)
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-style 'lines	 
	 :element-dashes 2)
(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-style 'impulses)

(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :element-style 'impulses
	 :x-title "test")

(gnuplot '((0 0) (1 2) (2 2) (3 1)) 
	 :ps-filename "/nfs/u8/dsmith/ps/xy1.ps"
	 :hardcopy-only t)

(gnuplot '((0 0) (1 1) (2 2) (3 1)) 
	 :x-tics '(("foo" 0) ("bar" 2)))
(gnuplot '((0 0) (1 1) (2 2) (3 1)) 
	 :x-tics '(0 2))
(gnuplot '((0 0) (1 1) (2 2) (3 1)) 
	 :x-tics '((200 0) (300 2)))

(gnuplot '((0 0) (1 1) (2 2) (3 1)) 
	 :x-log t)
(gnuplot '((0 0) (1 1) (2 2) (3 1)) 
	 :y-log 3)
where am i
  the xlabel -1 does not extend the bounding box.  error.
  setting line width and symbols
  doing on titers

|#

#|
(loop for i from 1 to 10 do 
      (gnuplot (loop for x from 1 to 10 collect (list x i)) 
	       :element-linetype i 
	       :element-symbol t 
	       :refresh (= i 1)))
(gnuplot-hardcopy)
|#