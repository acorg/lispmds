(defun tk-plot (coords &key (style 'line) (refresh t)
			   (element-name (gensym))
			   (element-color (random-x-color))
			   (element-symbol (if (eql style 'scatter) 'cross nil))
			   element-dashes
			   element-linewidth
			   x-min x-max y-min y-max
			   x-title y-title
			   title
			   (latex t)
			   ps-filename
			   (legend-corner 'off-right)
			   (legend-mapped t)
			   big-font
			   tag
			   aspect-ratio
			   window)  ;;just for mac compatibility (error if it is not nil)
  ;;element-symbol can be any of
  ;;  line, diamond, scross, circle, plus spluss, cross or square
  ;;legend can be nw or ne
  ;;to not map an element in the legend, name its name nothing, but then how do we
  ;;  make 2 names nothing?

  (if window (error "The window argument is not supported, just here for Mac compatibility"))

  (let ((window (if refresh 
		    (progn
		      (setq *g-plot-window* (tk-open "wish"))
		      (tk-put *g-plot-window* "source ~/cl/basic-graph-buttons.tk")
		      *g-plot-window*)
		  *g-plot-window*)))
    
    (if ps-filename (tk-put window "set graphFile ~s" ps-filename))
 
    (if tag 
	(loop for tg in (if (listp tag) tag (list tag)) do
	      (tk-put window ".g tag create text ~a" tg)))   ;;(g-plot '(0 1 0) :tag "{.1 .1} -text whiz")
    
    (if aspect-ratio (tk-put window ".g configure -width ~d" (+ 20 (* aspect-ratio 350))))  ;;20 is for y-axis label
    
    (if (not (listp (car coords)))
	(setq coords (loop for y in coords 
			 for x below (length coords) 
			 collect (list x y))))
    (if (eql style 'comb)
	(setq coords (loop for (x y) in coords append
			   `((,x 0) (,x ,y) (,x 0)))))
    (if coords
	(progn
	  (tk-put window (format nil ".g element create ~s ~
                                -fg ~a -bg white ~
                                -activeforeground #ff0000 -activelinewidth 2"
				 element-name
				 element-color
				 ))
	  (loop for (x y) in coords do
		(tk-put window (format nil ".g element append ~s {~12f ~12f}"
				       element-name
				       x y
				       )))
	  ;;this configure is to cause blt_wish to update (bug in blt_wish?)
	  (tk-put window (format nil ".g element configure ~s -fg ~a ~a"
				 element-name
				 element-color
				 (if (eql style 'scatter)
				     (format nil "-symbol ~a -linewidth 0" element-symbol)
				   "")
				 ))))
    t))