(in-package user)

(defun to-xmgr-stream (stream xys)
  (loop for xy in xys do
	(format stream "~d ~d~%" (nth 0 xy) (nth 1 xy))))

(defun to-xmgr-file (xys filename)
  (with-open-file (out filename :direction :output)
    (to-xmgr-stream out xys)))



(defun x-look-at (samples distance)
  (let ((datas (loop for ag below (length (car samples)) collect
		     (sort-hist (nths distance (nths ag samples))))))
    (let ((min-datas (loop for hist in datas minimize (hist-min hist)))
	   (max-datas (loop for hist in datas maximize (hist-max hist))))
      (cons 
      (loop for i from min-datas to max-datas collect
		    (list i
			  (* (length samples) 
			     (poisson-term (nth distance *expected-num-at-dists*) i))))
      datas
      ))))

#|
(setq foo (x-look-at *so-far* 5))
(loop for i below (length foo) do (to-xmgr-file (nth i foo) (format nil "tmp-~d" i)))
xmgr ~/tmp-0 ~/tmp-1 ~/tmp-2 ~/tmp-3 ~/tmp-4 ~/tmp-5 ~/tmp-6 ~/tmp-7 ~/tmp-8 ~/tmp-9 ~/tmp-10
|#




(defvar *x-plot-window*)

(defun x-plot (coords &key (style 'line) (refresh t)
			   (element-name (gensym))
			   (element-color (random-x-color))
			   element-symbol
			   element-dashes
			   element-linewidth
			   x-min x-max y-min y-max
			   x-title y-title
			   latex
			   ps-filename
			   (legend-corner 'off-right)
			   (legend-mapped t)
			   tag)
  ;;element-symbol can be any of
  ;;  line, diamond, scross, circle, plus spluss, cross or square
  ;;legend can be nw or ne
  ;;to not map an element in the legend, name its name nothing, but then how do we
  ;;  make 2 names nothing?

  (let ((window (if refresh 
		    (progn
		      (setq *x-plot-window* (tk-open "lisp_blt_wish"))
		      (tk-put *x-plot-window* "source ~/cl/basic-graph.tk")
		      (if latex
			  (tk-put *x-plot-window* "source ~/cl/basic-graph-latex-addendum.tk"))
		      (tk-put *x-plot-window* "source ~/cl/basic-graph-buttons.tk")
		      *x-plot-window*)
		  *x-plot-window*)))
    
    (if ps-filename (tk-put window "set graphFile ~s" ps-filename))
 
    (if tag (tk-put window ".g tag create text ~a" tag))   ;;(x-plot '(0 1 0) :tag "{.1 .1} -text whiz")

    (if x-title (tk-put window ".g xaxis configure -title ~s" x-title))
    (if y-title (tk-put window ".g yaxis configure -title ~s" y-title))
    
    (if (not legend-mapped)
	(tk-put window ".g legend configure -mapped 0"))

    (case legend-corner
      (off-right t)
      (nw (tk-put window ".g legend configure -position @80,22"))
      (ne (tk-put window ".g legend configure -position @437,22"))
      (nee (tk-put window ".g legend configure -anchor ne -position @530,22"))
      (t (tk-put window  ".g legend configure -position @~d,22" legend-corner)))

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
    '(tk-put window (format nil ".g element create ~a ~
                                -fg ~a ~
                                -activeforeground #ff0000 -activelinewidth 2 ~
                                -xdata {~{~d ~}} ~
                                -ydata {~{~d ~}}"
			    (gensym)
			    (random-x-color)
			    (nths 0 coords)
			    (nths 1 coords)
		     ))
    (if coords
	(progn
	  (tk-put window (format nil ".g element create ~s ~
                                -fg ~a -bg white ~
                                -activeforeground #ff0000 -activelinewidth 2"
				 element-name
				 element-color
				 ))
	  (if element-symbol 
	      (tk-put window ".g element conf ~s -symbol ~a" element-name element-symbol))
	  (if element-dashes 
	      (tk-put window ".g element conf ~s -dashes ~d" element-name element-dashes))
	  (if element-linewidth 
	      (tk-put window ".g element conf ~s -linewidth ~d" element-name element-linewidth))
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
				     "-symbol circle -linewidth 0"
				   "")
				 ))))
    t))

#|
(x-plot '((0 0) (1 2) (2 2) (3 1)))
(x-plot '((0 3) (1 1) (2 2) (3 8)) :refresh nil)

(x-plot '((0 0) (1 2) (2 2) (3 1)) :style 'comb :refresh nil)
|#
 