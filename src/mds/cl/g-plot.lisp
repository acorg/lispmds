(in-package user)

(defvar *g-plot-window*)

(defvar *last-coords*)   ;;for update optimization

(defun g-plot (coords &key (style 'line) (refresh t)
			   (element-name (gentemp "E"))
			   (element-color (if refresh 'black (random-x-color)))
			   (element-symbol (if (eql style 'scatter) "cross" nil))
			   element-dashes
			   element-linewidth
			   x-min x-max y-min y-max
			   x-title y-title
			   title
			   (latex nil)
			   ps-filename
			   (legend-corner 'off-right)
			   (legend-mapped t)
			   big-font
			   tag
			   aspect-ratio
			   append
			   update
			   source
			   window)  ;;just for mac compatibility (error if it is not nil)
  ;;element-symbol can be any of (in """")
  ;;  square,  circle,  diamond, plus,  cross,  splus,  scross,  triangle, ""
  ;;                   (where no symbol is  drawn),  or  a  bitmap.
  ;;legend can be nw or ne
  ;;to not map an element in the legend, name its name nothing, but then how do we
  ;;  make 2 names nothing?

  (if window (error "The window argument is not supported, just here for Mac compatibility"))

  (let ((window (if refresh 
		    (progn
		      ;;(setq *g-plot-window* (tk-open "bltwish"))
		      (setq *g-plot-window* (tk-open))
		      (tk-put *g-plot-window* "package require BLT")
		      (tk-put *g-plot-window* "source cl/basic-tk.tk")
		      (tk-put *g-plot-window* "source cl/basic-graph.tk")
		      (if latex
			  (tk-put *g-plot-window* "source cl/basic-graph-latex-addendum.tk"))
		      (tk-put *g-plot-window* "source cl/basic-graph-buttons.tk")
		      *g-plot-window*)
		  *g-plot-window*)))
    
    (if source (tk-put window "source ~a" source))
    
    (if ps-filename (tk-put window "set graphFile ~s" ps-filename))
 
    (if big-font 
	(progn 
	  (tk-put window ".g configure -font *-Times-Medium-R-Normal-*-180-*")
	  (tk-put window ".g configure -bottommargin 70")
	  ;;(tk-put window ".g configure -leftmargin 70")
	  (tk-put window ".g xaxis configure -font *-Times-Medium-R-Normal-*-140-*")
	  (tk-put window ".g yaxis configure -font *-Times-Medium-R-Normal-*-140-*")
	  (tk-put window ".g legend configure -font *-Times-Medium-R-Normal-*-140-*")))

    (if tag 
	(loop for tg in (if (listp tag) tag (list tag)) do
	      (tk-put window ".g tag create text ~a" tg)))   ;;(g-plot '(0 1 0) :tag "{.1 .1} -text whiz")

    (if title (tk-put window ".g configure -title ~s" title))
    
    (if x-title (tk-put window ".g xaxis configure -title ~s" x-title))
    (if y-title (tk-put window ".g yaxis configure -title ~s" y-title))
    
    (if aspect-ratio (tk-put window ".g configure -width ~d" (+ 20 (* aspect-ratio 350))))  ;;20 is for y-axis label
    
    (if (not legend-mapped)
	(tk-put window ".g legend configure -mapped 0"))

    (cond ((equal legend-corner 'off-right) t)
	  ((equal legend-corner 'nw) (tk-put window ".g legend configure -position @80,22"))
	  ((equal legend-corner 'sw) (tk-put window ".g legend configure -position @80,200"))
	  ((equal legend-corner 'ne) (tk-put window ".g legend configure -position @437,22"))
	  ((equal legend-corner 'nee) (tk-put window ".g legend configure -anchor ne -position @530,22"))
	  ((numberp legend-corner) (tk-put window  ".g legend configure -position @~d,22" legend-corner))
	  ((listp legend-corner) (tk-put window  ".g legend configure -position @~d,~d" 
					 (nth 0 legend-corner) (nth 1 legend-corner))))

    (if x-min (tk-put window ".g xaxis configure -min ~d" x-min))
    (if x-max (tk-put window ".g xaxis configure -max ~d" x-max))
    (if y-min (tk-put window ".g yaxis configure -min ~d" y-min))
    (if y-max (tk-put window ".g yaxis configure -max ~d" y-max))

    (if (not (listp (car coords)))
	(setq coords (loop for y in coords 
			 for x below (length coords) 
			 collect (list x y))))
    (if (eql style 'comb)
	(setq coords (loop for (x y) in coords append
			   `((,x 0) (,x ,y) (,x 0)))))

    (if coords
	(cond (append
	       (loop for (x y) in coords do
		     (tk-put window "xVec_~a append ~d" element-name x)
		     (tk-put window "yVec_~a append ~d" element-name y))
	       (setq *last-coords* nil))
	      (update
	       (if (equal coords *last-coords*)
		   'update-optimized-away
		 (progn
		   (tk-put window "xVec_~a set {~{~d ~}}" element-name (nths 0 coords))
		   (tk-put window "yVec_~a set {~{~d ~}}" element-name (nths 1 coords))
		   (setq *last-coords* coords))))
	      (t ;;create
	       (setq *last-coords* coords)
	       (tk-put window "vector xVec_~a yVec_~a" element-name element-name)
	       (tk-put window "xVec_~a set {~{~d ~}}" element-name (nths 0 coords))
	       (tk-put window "yVec_~a set {~{~d ~}}" element-name (nths 1 coords))
	       (tk-put window (format nil ".g element create ~a  -xdata xVec_~a -ydata yVec_~a -color ~a"
				      element-name
				      element-name
				      element-name
				      element-color
				      ))
	       (if element-symbol 
		   (tk-put window ".g element conf ~a -symbol ~a" 
			   element-name
			   (if (eql 'random element-symbol)
			       (random-choice '(diamond scross circle plus cross square))
			     element-symbol)))
	       (if element-dashes 
		   (tk-put window ".g element conf ~a -dashes ~d" element-name element-dashes))
	       (if element-linewidth 
		   (tk-put window ".g element conf ~a -linewidth ~d" element-name element-linewidth))
	       )))
    t))


(defvar *g-plot-symbols*)
(setq *g-plot-symbols* '("square" "circle" "diamond" "plus" "cross" "splus" "scross" "triangle"))

#|
(g-plot '((0 0) (1 2) (2 2) (3 1)))
(g-plot '((0 3) (1 1) (2 2) (3 8)) :refresh nil)

(g-plot '((0 0) (1 2) (2 2) (3 1)) :style 'comb :refresh nil)
|#

;;one problem is that g-plot leaves zombies
;;another is that we do not necessarily get a window update



(defun g-plots (datas &rest args)
  (let ((first-plot-args (collect #'atom args))
	(per-plot-args  (collect (^ (x) (and (listp x) (= (length x) (length datas)))) args)))
    (apply #'g-plot (car datas) (append first-plot-args
					(flatten (nths 0 per-plot-args))))
    (loop for data in (cdr datas) for plot from 1 do
	  (apply #'g-plot data :refresh nil (flatten (nths plot per-plot-args))))))

(defun g-plot-correlate (x y &rest g-plot-args)
  (apply #'g-plot 
   (transpose x y)
   (append g-plot-args
	   (list :latex t
		 :element-symbol 'circle
		 :element-name "Correlation"
		 :element-linewidth 0
		 :legend-corner 'nee
		 :x-min 0 :y-min 0))))
