(in-package user)

(defvar antisera)
(defvar antigens)

(defun random--1+1 ()
  (- (* 2 (knuth-random))
     1))

(defun make-random-point (dimensions)
  (loop for i below dimensions collect
	(random--1+1)))

(defun pannel-from-coords (antigens antisera &optional (distance-f #'e-dist))
  (loop for antigen in antigens collect
	(loop for antiserum in antisera collect
	      (funcall distance-f antigen antiserum))))

(defun true-versus-pannel-dists (antigens antisera &optional x-max y-max)
  (let* ((panel (pannel-from-coords antigens antisera #'e-dist))
	  (p-ag-dists (all-comparisons-full panel #'e-dist))
	  (a-ag-dists (all-comparisons-full antigens #'e-dist))
	  (true-versus-panel (transpose (flatten p-ag-dists) (flatten a-ag-dists))))
    (apply #'g-plot 
	   true-versus-panel 
	   :style 'scatter
	   :element-symbol 'diamond
	   :legend-mapped nil
	   :x-title "Panel Distance"
	   :y-title "True Distance"
	   :x-min 0
	   :y-min 0
	   :aspect-ratio 1
	   (append (if x-max `(:x-max ,x-max))
		   (if y-max `(:y-max ,y-max))))
    true-versus-panel))
	  

#|
(setq antisera (loop for i below 10 collect (make-random-point 5)))
(setq antigens (loop for i below 50 collect (make-random-point 5)))

(true-versus-pannel-dists antigens antisera)
|#

;;;----------------------------------------------------------------------
;;;                    NOW WITH CLOSE BY ANTIGENS
;;;----------------------------------------------------------------------

(defun scatter-euclidean-vector (vector half-step min max)
  (loop for e in vector collect
	(max min (min max (+ e (random-in-range (- half-step) half-step))))))

(defun scatter-euclidean-vectors (times seed-vector half-step min max)
  (cons 
   seed-vector 
   (loop for i below (dec times) collect
	 (setq seed-vector (scatter-euclidean-vector seed-vector half-step min max)))))

(defun serial-evolve-antigens (times seed-antigen step)
  (scatter-euclidean-vectors times seed-antigen step -1 1))
(defun parallel-evolve-antigens (times seed-antigen step)
  (loop for i below times collect
	(scatter-euclidean-vector seed-antigen step -1 1)))

#|
(scatter-euclidean-vectors 10 '(0 0 0 0 0) 0.1 -1 1)

(serial-evolve-antigens 10 '(0 0 0 0 0) 0.1)
(parallel-evolve-antigens 10 '(0 0 0 0 0) 0.1)

(setq antisera (serial-evolve-antigens 10 '(0 0 0 0 0) 0.1))
(setq antigens (loop for i from 5 to 9 append
		     (parallel-evolve-antigens 10 (nth i antisera) 0.1)))
(true-versus-pannel-dists antigens antisera)
|#

;;;----------------------------------------------------------------------
;;;                      A SIMPLE CASE
;;;----------------------------------------------------------------------

#|
(setq antisera '((0 0) (1 0) (0 1)))
(setq antigens '((0.25 0.25) (0.75 0.25) (0.75 0.75) (0.25 0.75)))
(true-versus-pannel-dists antigens antisera)

(setq antisera '((0 0) (1 0) (0 1)))
(setq antigens (loop for i below 50 collect (make-random-point 2)))
(true-versus-pannel-dists antigens antisera)

(setq antisera '((0 0) (1 0) (0 1)))
(setq antigens (loop for i below 4 collect (make-random-point 2)))
(true-versus-pannel-dists antigens antisera)

(setq antisera (loop for i below 50 collect (make-random-point 2)))
(setq antigens (loop for i below 4 collect (make-random-point 2)))
(true-versus-pannel-dists antigens antisera)

(setq antisera (loop for i below 50 collect (make-random-point 2)))
(setq antigens (loop for i below 50 collect (make-random-point 2)))
(true-versus-pannel-dists antigens antisera)
|#

;;;----------------------------------------------------------------------
;;;                      L1 versus L2 norm
;;;----------------------------------------------------------------------

#|
Nastily recompile e-dist (the L2 norm) to be the L1 norm

(defun e-dist (l1 l2)
  (loop for e1 in l1
      for e2 in l2
      sum (abs (- e1 e2))))

(setq antisera '((0 0) (1 0) (0 1)))
(setq antigens '((0.25 0.25) (0.75 0.25) (0.75 0.75) (0.25 0.75)))
(true-versus-pannel-dists antigens antisera)
|#


;;;----------------------------------------------------------------------
;;;                   STANFORD: MAKE AS SIMPLE AS POSSIBLE
;;;----------------------------------------------------------------------

#|
;;------------------------------;make true space 1D----------------------------

(setq antigens (loop for i below 50 collect (list (random-in-range 10 50))))
(setq antisera '((0)))
(true-versus-pannel-dists antigens antisera)    ;;straight line, no problem

(setq antisera '((20)))
(true-versus-pannel-dists antigens antisera)    ;;here we get into probems because our ref is inside

(setq antisera '((0) (100) (-20) (60)))         ;;now the pannel is 4d
(true-versus-pannel-dists antigens antisera)    ;;still straight line no prob  


;------------------------------;make true space 2D----------------------------

(setq antigens (loop for i below 50 collect (list (random-in-range 10 50) (random-in-range 10 50))))
(setq antisera '((0 0)))           
(true-versus-pannel-dists antigens antisera)    ;;true /= pannel, 
                                                ;;because 1d is not enuf for 2 d data

(setq antisera '((0 0) (100 100)))
(true-versus-pannel-dists antigens antisera)    ;;predict (correct) still not good, 
                                                ;;because refs are not orthogonal enuf(setq

(setq antisera '((30 0) (0 30)))
(true-versus-pannel-dists antigens antisera)    ;;predict (correct) good but not great, 
                                                ;;because now refs ortho, but not far enuf away

(setq antisera '((30 -1000) (-1000 30)))
(true-versus-pannel-dists antigens antisera)    ;;predict (correct) great, 
                                                ;;because now refs ortho and far enuf away

(setq antisera (loop for i below 2 collect (random-on-circumference 1000)))
(true-versus-pannel-dists antigens antisera)    ;;predict (correct) sometimes good sometimes bad
                                                ;;because depends how ortho our points are on circumference

(setq antisera (loop for i below 20 collect (random-on-circumference 100)))
(true-versus-pannel-dists antigens antisera)    ;;predict not sure (answer: ok)
                                                ;;not sure because depends how the bads weigh with the goods

(setq antisera (loop for i below 20 collect (random-on-circumference 1000)))
(true-versus-pannel-dists antigens antisera)    ;;predict not sure (answer: same as radius 100)

(setq antisera (loop for i below 20 collect (random-on-circumference 10000)))
(true-versus-pannel-dists antigens antisera)    ;;predict not sure (answer: same as radius 100 and 1000


(setq antisera '((0 -1000) (-500 800)))
(true-versus-pannel-dists antigens antisera)    ;;predict (correct) medium-bad
                                                ;;because not so ortho (2 points of equilateral triangle)

(setq antisera '((0 -1000) (-500 800) (500 800)))
(true-versus-pannel-dists antigens antisera)    ;;predict (wrong) good
                                                ;;predicted good because thought "it can triangulate"
                                                ;;  which is wrong thinking (MDS could)
                                                ;;actually medium because, well not sure

|#