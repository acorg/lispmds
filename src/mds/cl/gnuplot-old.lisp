(in-package user)

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
			 (filename "~/.tmp.gnuplot"))
  (loop for line in (if (listp lines) lines (list lines)) do
	(fi line filename if-exists-action t)))


(defun ll-to-lines (ll)
  (format nil "~{~{~a ~}~%~}" ll))

(defvar *last-gnuplot-plot-command*)
(defvar *last-gnuplot-plot-number*)
(defvar *gnuplot-stream*)

(setq *last-gnuplot-plot-command* nil)
(setq *last-gnuplot-plot-number* 0)
(setq *gnuplot-stream* nil)

(defun gnuplot-tmp-file-name (file-number)
  (format nil "/home/dsmith/.gnuplot.tmp.~d" file-number))

(defun gnuplot-command (command &rest args)
  (apply #'tk-put *gnuplot-stream* command args))

;;(defun gnuplot-command (command &rest args)
;;  (fi (apply #'format nil command args) "delete" :append t))

(defun gnuplot-command-new-file (command &rest args)
  (fi (apply #'format nil command args) "delete" :supersede t))

(defun gnuplot (coords &key ;;(style 'line) 
			    (refresh t)
			    element-name
			    ;;(element-color (random-x-color))
			    element-symbol
			    element-dashes
			    (element-style 'lines)
			    element-linewidth
			    element-linetype
			    x-min x-max y-min y-max z-min z-max
			    x-title y-title z-title
			    y2-title
			    x-log y-log z-log   ;;can be numbers to give base
			    x-tics y-tics z-tics  ;;can be label-position pairs or just labels
			    y2-tics
			    x-tic-format y-tic-format z-tic-format
			    tic-direction ;;in or out
			    ;;latex
			    (ps-filename "/home/dsmith/ps/xy.ps")
			    hardcopy-only  ;;also doubles as hardcopy fontsize
			    legend-position
			    ;;(legend-mapped t)
			    ;;big-font
			    ;;tag
			    label ;;(text x y)
			    labels
			    x-size
			    y-size
			    )
  
  ;;(if refresh (setq *gnuplot-stream* (fork-and-pipe "/nfs/research/dsmith/gnuplot/gnuplot")))   ok, this is the latest and can do y2axis
  ;;(if refresh (setq *gnuplot-stream* (fork-and-pipe "/usr/local/gnu/bin/gnuplot")))
  ;;(if refresh (setq *gnuplot-stream* (fork-and-pipe "/nfs/research/dsmith/gnuplot/gnuplot")))
  
  (if refresh (setq *gnuplot-stream* (fork-and-pipe "gnuplot")))
    
  (if refresh
      (if ps-filename (gnuplot-command-new-file "set out ~s" ps-filename))
    (if ps-filename (gnuplot-command "set out ~s" ps-filename)))

  (if hardcopy-only (gnuplot-command "set te postscript eps ~a" 
				     (if (numberp hardcopy-only) hardcopy-only ""))) ;;fontsize
  
  (if (atom (car coords))
      (setq coords (loop for x below (length coords) for y in coords collect (list x y))))

  (if (or x-size y-size)
      (gnuplot-command "set size ~d, ~d" 
		       (if x-size x-size 1)
		       (if y-size y-size 1)))

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
  
  (if x-title (gnuplot-command "set xlabel ~s 0,-1" x-title))   
  (if y-title (gnuplot-command "set ylabel ~s" y-title))
  (if y2-title (gnuplot-command "set y2label ~s" y2-title))
  (if z-title (gnuplot-command "set zlabel ~s" z-title))
  
  (if legend-position 
      (if (eql legend-position 'none)
	  (gnuplot-command "set key nokey")   ;;does not work
	(apply #'gnuplot-command "set key ~a, ~a" legend-position)))
    
  (if refresh
      (progn 
	(setq *last-gnuplot-plot-command* 
	  (if (= 3 (length (car coords)))
	      (progn
		(gnuplot-command "set parametric")
		"sp ")
	    "p "))
	(setq *last-gnuplot-plot-number* 0))
    (progn
      (setq *last-gnuplot-plot-command*
	(string-append *last-gnuplot-plot-command* ", "))
      (setq *last-gnuplot-plot-number* (inc *last-gnuplot-plot-number*))))

  (gnuplot-fi (ll-to-lines coords) :new-version (gnuplot-tmp-file-name *last-gnuplot-plot-number*))

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
					    (points "with points")
					    (impulses "with impulses")
					    (t "")))
				    (if element-linetype
					(list (format nil "linetype ~d" element-linetype)))
				    (if element-linewidth
					(list (format nil "linewidth ~d" element-linewidth)))))
		    (if element-dashes element-dashes "")
		    )))
  )

(defun gnuplot-hardcopy (&optional fontsize filename)
  (if filename (gnuplot-command "set out ~s" filename))
  (gnuplot-command "set te postscript eps ~a" (if fontsize fontsize ""))
  (gnuplot-command "rep")
  (gnuplot-command "set out")
  (gnuplot-command "set te x11")
  (gnuplot-command "rep"))

(defun gnuplot-exit ()
  (gnuplot-command "exit"))

  
(defun gnuplots (datas &rest args)
  (apply #'gnuplot (cons (car datas) args))
  (loop for data in (cdr datas) do
	(gnuplot data :refresh nil)))



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
	 :ps-filename "/nfs/research/dsmith/ps/xy1.ps"
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

