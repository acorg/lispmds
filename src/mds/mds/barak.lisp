(in-package user)

;;;----------------------------------------------------------------------
;;;                             BARAK
;;;----------------------------------------------------------------------

(setq hi90d-rs (enths 2 (hi-table-antigens hi90d) 0))
(setq hi90d-vs (enths 2 (hi-table-antigens hi90d) 1))

(length (combs 3 (hi-table-antigens hi90d)))

(defun hi-triangles (hi-table)
  (loop for (v1 v2 reference) in (map-append #'perms (combs 3 (hi-table-antigens hi-table))) collect
	;;lowerbound, upperbound, actual
	(list (hi-table-value hi-table v1 reference) 
	      (hi-table-value hi-table v2 reference)
	      (hi-table-value hi-table v1 v2)        ;;we can't get this unless we have ag-ag dists or symetric table
	      v1 v2 reference)))

(defun hi-upper-lower-bounds (hi-table)
  (loop for (v1 v2 reference) in (combs 3 (hi-table-antigens hi-table)) collect
	;;lowerbound, upperbound, actual
	(list (abs (- (hi-table-value hi-table v1 reference) (hi-table-value hi-table v2 reference)))
	      (+ (hi-table-value hi-table v1 reference) (hi-table-value hi-table v2 reference))
	      (hi-table-value hi-table v1 v2))))

(defun hi-deltas (hi-table v1 v2)
  (loop for reference in (hi-table-sera hi-table) collect
	(list (hi-table-value hi-table v1 v2)
	      (abs (- (hi-table-value hi-table v1 reference) (hi-table-value hi-table v2 reference)))
	      (+ (hi-table-value hi-table v1 reference) (hi-table-value hi-table v2 reference))
	      v1 v2 reference)))

(defun hi-deltas-index (hi-table v1-index v2-index)
  (hi-deltas hi-table 
	     (nth v1-index (hi-table-antigens hi-table))
	     (nth v2-index (hi-table-antigens hi-table))))

(setq triangles (hi-triangles hi90d))

(defun d-pairs (triangles)
  (remove-duplicates
   (transpose (nths 0 triangles) (nths 1 triangles))
   :test #'equal))

(defun collect-deltas (d11 d12 triangles)
  (collect (^ (triangle) (and (= d11 (nth 0 triangle))
			      (= d12 (nth 1 triangle))))
	   triangles))

#|
(defun delta12-pdf (d11 d12 triangles)
  (sort-hist (nths 2 (collect-deltas d11 d12 triangles))))

(defun delta12-pdfs (triangles)
  (loop for (d11 d12) in (d-pairs triangles) collect
	(list d11 d12 (delta12-pdf d11 d12 triangles))))
|#

(defun delta12-pdfs (triangles)
  (let ((d11-d12-alist nil))
    (loop for (d11 d12 delta12) in triangles do
	  (let ((assoc-value (assoc-value (list d11 d12) d11-d12-alist :test #'equal)))
	    (if assoc-value
		(push-end delta12 assoc-value)
	      (setq d11-d12-alist (cons (list (list d11 d12) delta12) d11-d12-alist)))))
    (reverse
     (loop for ((d11 d12) . delta12s) in d11-d12-alist collect
	   (list d11 d12 (sort-hist delta12s))))))
    

;;(ppl (delta12-pdfs (append (hi-triangles hi90d))))
;;(ppl (delta12-pdfs (append (hi-triangles hi77d) (hi-triangles hi85d) (hi-triangles hi90d) (hi-triangles hi92d))))

(defun num-occurences-to-pdf (hist)
  (let ((total (apply #'+ (nths 1 hist))))
    (loop for (x y) in hist collect
	  (list x (float (/ y total))))))
    
(defun g-plot-pdf (d11-d21-hist x-max)
  (let-list ((d11 d21 hist) d11-d21-hist)
	    (g-plot (num-occurences-to-pdf hist)
		    :style 'scatter
		    :x-title "Delta 12"
		    :title (format nil "Delta12 pdf, d11=~d, d21=~d" d11 d21)
		    :x-min 0
		    ;;:x-max (apply #'max (flatten (mapcar (^ (hist) (nths 0 hist)) (nths 2 all-d11-d21-hists))))
		    ;;:x-max (inc (apply #'max (mapcar #'+ (nths 0 all-d11-d21-hists) (nths 0 all-d11-d21-hists))))
		    :x-max x-max
		    :y-min 0
		    :y-max 1
		    :legend-mapped nil
		    :element-color 'black
		    :y-title "Pr{(delta21=x)}")
	    (g-plot (list (list (abs (- d11 d21)) 0)
			  (list (abs (- d11 d21)) 0.4))
		    :element-dashes 2
		    :element-color 'red
		    :tag (format nil "{~d ~d} -text ~a -font -Adobe-Times-Medium-R-Normal-*-120-*" (abs (- d11 d21)) 0.45 "Lower ")
		    :refresh nil)
	    (g-plot (list (list (abs (+ d11 d21)) 0)
			  (list (abs (+ d11 d21)) 0.5))
		    :element-dashes 2
		    :element-color 'red
		    :tag (format nil "{~d ~d} -text ~a -font -Adobe-Times-Medium-R-Normal-*-120-*" (abs (+ d11 d21)) 0.55 "Upper")
		    :refresh nil)
	    (g-plot (list (list (abs (l2-norm d11 d21)) 0)
			  (list (abs (l2-norm d11 d21)) 0.45))
		    :element-dashes 2
		    :element-color 'red
		    :tag (format nil "{~d ~d} -text ~a -font -Adobe-Times-Medium-R-Normal-*-120-*" (abs (l2-norm d11 d21)) 0.50 "Mode?")
		    :refresh nil)))

(defun large-pdfs (d11-d21-hists &optional (min-points-to-be-large 0))
  (collect (^ (d11-d21-hist)
	      (>= (apply #'+ (nths 1 (nth 2 d11-d21-hist)))
		  min-points-to-be-large))
	   d11-d21-hists))

(defun pdfs-in-range (d11-d21-hists lower upper)
  (collect (^ (d11-d21-hist)
	      (and (>= (apply #'+ (nths 1 (nth 2 d11-d21-hist)))
		       lower)
		   (<= (apply #'+ (nths 1 (nth 2 d11-d21-hist)))
		       upper)))
	   d11-d21-hists))

(defun plot-large-pdfs (d11-d21-hists &optional (min-points-to-be-large 0) x-max)
  (let ((large-pdfs (large-pdfs d11-d21-hists min-points-to-be-large)))
    (loop for d11-d21-hist in large-pdfs do
	(g-plot-pdf d11-d21-hist x-max))))
       
(defun plot-pdfs (d11-d21-hists x-max)
  (loop for d11-d21-hist in d11-d21-hists do
	(g-plot-pdf d11-d21-hist x-max)))



#|
links
;;  - why is d11 d21 switch not the same pdf?
  - maybe use lapedes data, where i have ag-ag and ag-sr (then i have to bin the dists as they are real numbers)

(delta12-pdfs (append (hi-triangles hi77d) (hi-triangles hi85d) (hi-triangles hi90d) (hi-triangles hi92d)))
(delta12-pdfs (append (hi-triangles hi77a) (hi-triangles hi85a) (hi-triangles hi90a) (hi-triangles hi92a)))

(delta12-pdfs (append (hi-triangles hi77d) (hi-triangles hi85d) (hi-triangles hi92d)))

(extract-hi-table-by-indices hi77d '(0 2 5 11))
|#


;;;----------------------------------------------------------------------
;;;                           BARAK PURE 
;;;----------------------------------------------------------------------

;;generate data from a euclidean unit n-torus

(defun e-dist-torus (l1 l2)
  "Euclidean distance on a unit torus on lists."
  ;;method, change L2 to be on other side if e1 and e2 are >0.5 from each other.
  (e-dist
   l1
   (loop for e1 in l1 for e2 in l2 collect
	 (cond ((<= (abs (- e1 e2)) 0.5) e2)
	       ((> e1 e2) (inc e2))
	       (t (dec e2))))))		

#|
;;this is wrong
(defun e-dist-torus (l1 l2)
  "Euclidean distance on a unit torus on lists."
  ;;method, change L2 to be on other side on per coords basis.
  (e-dist
   l1
   (loop for e1 in l1 for e2 in l2 collect
	 (cond ((same-half? e1 e2) e2)
	       ((top-half? e1) (inc e2)) ;e2 must be bottom half because we know they are not the same half
	       (t (dec e2))))))		;e2 must be in top, and e1 in bottom

(defun same-half? (e1 e2)
  (or (and (top-half? e1) (top-half? e2))
      (and (not (top-half? e1)) (not (top-half? e2)))))

(defun top-half? (e1)
  (>= e1 0.5))
|#  

;;using utils from true-versus-pannel-distances

(defun make-random-e-point (dimensions &optional (lower 0) (upper 1))
  (loop for i below dimensions collect
	(random-in-range lower upper)))

#|
(setq antigens (loop for i below 10 collect (make-random-e-point 5)))
(setq antisera (loop for i below 10 collect (make-random-e-point 5)))

(setq pannel (pannel-from-coords antigens antisera #'e-dist-torus))
(setq hi-table (make-hi-table (series 0 9) (series 0 9) pannel))

;;now a symetric one, where antigens and antisera are the same points
(setq pannel (pannel-from-coords antigens antigens #'e-dist-torus))
(setq hi-table (make-hi-table (series 0 9) (series 0 9) pannel))

(delta12-pdfs (hi-triangles hi-table))

|#


;;-------ok change, given a fixed reference points, and variable
;;---------pairs of v points, collect the 2 ds, and the delta

(defun sample-ds-and-delta (dimensions distance-f)
  (let ((reference (make-random-e-point dimensions))
	(v1 (make-random-e-point dimensions))
	(v2 (make-random-e-point dimensions)))
    (list (funcall distance-f reference v1)
	  (funcall distance-f reference v2)
	  (funcall distance-f v1 v2))))


(defun process-triangles (triangles dps)
  ;;round to some decimal points,  (to get more points on each pair)
  ;;order the d11 and d12 (to get more points on each pair)
  ;;sort on d11 and d12 (so i can see pattners better)
  (sort (loop for (a b c) in (loop for triangle in triangles collect (dps-tree triangle dps)) collect
	      (if (> a b)
		  (list b a c)
		(list a b c)))
	(^ (a b)
	   (if (= (car a) (car b))
	       (< (cadr a) (cadr b))
	     (< (car a) (car b))))))

;;(setq foo (loop for i below 1000 collect (sample-ds-and-delta 5 #'e-dist-torus)))
;;(delta12-pdfs (process-triangles foo 2))

;;(setq foox (loop for i below 100000 collect (sample-ds-and-delta 5 #'e-dist-torus)))
;;(setq pdf2s (delta12-pdfs (process-triangles foox 2)))
;;(plot-pdfs (pdfs-in-range pdf2s 120 120) 2)

;;(setq pdf1s (delta12-pdfs (process-triangles foox 1)))  ;;53 of them
;;(loop for i below 4 do (g-plot-pdf (nth (krandom (length pdf1s)) pdf1s) 2))


