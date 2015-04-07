(in-package user)
 
(defun pca (coordss &optional &key 
			      (num-dims (length (car coordss))) 
			      (directory (if (running-on-windows-p)
					     (uw-sfnr "mds/procrustes/tmp-files/" :assertIsDir t)  ;; for now snarf the procrustes tmp-file dir
					   "/tmp/")))
  (let* ((n (length coordss))
	 (a (all-comparisons-square coordss (^ (a b) (coerce (square (e-dist a b)) 'single-float))))
	 (matrix-filename (format nil "~a~a-~d" directory "svd" (krandom 100000))))
    (fll a
	:filename matrix-filename
	:if-exists :supersede)
    (run-shell-command
     ;; in mds/svd/c/   gcc -lm -o svd-for-lisp svd-for-lisp.c
     ;; svd-for-lisp MatrixSize Threshold DataHeader(1/0) OutputDim SquareIt MatrixFile LabelFile
     ;; output goes to MatrixFile.output
     (format nil "~a ~d 0 0 ~d 0 ~a -"
	     (if (running-on-windows-p)
		 (uw-sfnr "mds/svd/c/Debug/svd-for-lisp.exe" :assertIsFile t)
	       (uw-sfnr "mds/svd/c/svd-for-lisp" :assertIsFile t))
	     n
	     num-dims
	     matrix-filename))
    (fi-in (string-append matrix-filename ".output"))))

#|
(pca '((0 0) (1 1)))
((COORDSS (0.707107 0.0) (-0.707107 0.0)) 
 (EIGENVALUES 1.0 0.0)
 (CUMULATIVE-VARIANCE 1.0 1.0))

(pca '((0 0) (0.5 0.5) (1 1)))
((COORDSS (-0.707107 1.73e-4) (0.0 8.1999993e-5) (0.707107 1.73e-4))
 (EIGENVALUES 1.0 0.0 0.0)
 (CUMULATIVE-VARIANCE 1.0 1.0 1.0))

(pca '((0 0) (0.51 0.50) (1 1)))
((COORDSS (-0.709464 -0.002333) (0.004714 0.004716) (0.70475 -0.00238))
 (EIGENVALUES 1.000033 3.3e-5 0.0)
 (CUMULATIVE-VARIANCE 0.999967 1.0 1.0))

(pca '((0 0) (0.6 0.50) (1 1)))
((COORDSS (-0.730752 -0.021134) (0.047297 0.046983) (0.683454 -0.025848))
 (EIGENVALUES 1.003345 0.003322 0.0)
 (CUMULATIVE-VARIANCE 0.9967 1.0 1.0))

(pca '((0 0) (0.5 0.50) (1.1 1)))
((COORDSS (-0.730963 0.011758) (-0.024682 -0.022405) (0.755644 0.010643))
 (EIGENVALUES 1.105913 7.5399995e-4 0.0)
 (CUMULATIVE-VARIANCE 0.999319 1.0 1.0))

(pca (setq coordss
       (transpose
	(loop for i below 100 collect (add-gaussian-noise 10 4))
	(loop for i below 100 collect (add-gaussian-noise 5 1)))))
((COORDSS (-2.347599 0.076172) (0.429267 0.090722) (1.081144 -0.076847) (-0.384816 -0.294731) (0.963643 0.193918) (-0.220823 0.291244)
          (1.763421 0.315759) (0.187113 0.500446) (0.961855 0.160966) ...)
 (EIGENVALUES 131.54062 8.080231 7.8e-4 1.8999998e-5 3.0e-6 3.0e-6 1.9999999e-6 1.9999999e-6 1.9999999e-6 ...)
 (CUMULATIVE-VARIANCE 0.942122 0.999994 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...)
 (CUMULATIVE-SD 0.795443 0.99259 0.994527 0.99483 0.994953 0.995069 0.995179 0.995288 0.995395 ...))

we set up the x to move 4 units, and y 1 unit,
and we see in the cumulative sf that we get 80% of the sd in x and 20% in y, so that makes sense

(gnuplot
 coordss
 :x-min 0  :y-min 0
 :x-max 15 :y-max 15
 :ratio 1
 :element-style 'points)


;; now try with uniform distribution of points, not gaussian
(setq pca-results
  (pca (setq coordss
	 (transpose
	  (loop for i below 100 collect (uniform-perturb 10 4))
	  (loop for i below 100 collect (uniform-perturb 5 1))))))
((COORDSS (-2.297779 0.718787) (0.33029 0.619015) (-1.722178 -0.3967) (-2.692614 0.556239) (-3.281739 1.024587) (0.700905 0.644692)
          (3.41639 -0.726449) (3.752628 -0.155626) (2.05332 -0.733518) ...)
 (EIGENVALUES 540.9374 27.625153 0.001014 9.1e-5 7.0000005e-6 5.0e-6 3.9999998e-6 3.9999998e-6 3.0e-6 ...)
 (CUMULATIVE-VARIANCE 0.951411 0.999998 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...)
 (CUMULATIVE-SD 0.811431 0.994802 0.995913 0.996245 0.996335 0.996409 0.99648 0.996546 0.99661 ...))
;; yes, we again get the 80/20 split of the sd
(gnuplot
 coordss
 :x-min 0  :y-min 0
 :x-max 15 :y-max 15
 :ratio 1
 :element-style 'points)
(gnuplot
 (assoc-value 'coordss pca-results)
 :x-min -7  :y-min -7
 :x-max 8 :y-max 8
 :ratio 1
 :element-style 'points)


(setq pca-results
  (pca (setq coordss
	 (rotate-coords
	 (transpose
	  (loop for i below 100 collect (uniform-perturb 10 4))
	  (loop for i below 100 collect (uniform-perturb 5 1))))))
(defun un-rotate (mds-window xy)
  ;; we don't do this very often, just when we move a point with the mouse in the UI.
  ;; so its ok that we are not super efficient
  ;; this should work with swap-y on the basis vectors too (as we are now assuming nothing)
  (if (not (= 2 (length (get-canvas-basis-vector-0 mds-window))))
      (progn (format t "Moving points by the mouse only works reliably in 2D~%")
	     xy)
    (let ((anti-canvas-basis (list-array (matrix:invert-matrix (array-list (list (get-canvas-basis-vector-0 mds-window) (get-canvas-basis-vector-1 mds-window)))))))
      (let ((anti-canvas-basis-vector-0 (nth 0 anti-canvas-basis))
	    (anti-canvas-basis-vector-1 (nth 1 anti-canvas-basis)))
	(list (lvector-dot-product xy anti-canvas-basis-vector-0)
	      (lvector-dot-product xy anti-canvas-basis-vector-1))))))
(defun rotate-coordss (mds-window coordss radians center-x center-y 
		       &optional (first-dimension (get-first-dimension mds-window)) (second-dimension (get-second-dimension mds-window)))
  (let-list ((center-x center-y) (canvas-to-mds-coords mds-window (list center-x center-y)))
	    (setq center-x 0
		  center-y 0)
	    (let ((sina (sin radians))
		  (cosa (cos radians)))
	      (loop for coords in coordss collect
		    (let ((x (- (nth first-dimension coords)  center-x))
			  (y (- (nth second-dimension coords) center-y)))
		      (append (firstn first-dimension coords)
			      (list (+ center-x (* x cosa) (* y sina)))
			      (firstn (- second-dimension first-dimension 1) (nthcdr (inc first-dimension) coords))
			      (list (+ center-y (* y cosa) (- (* x sina))))
			      (nthcdr (inc second-dimension) coords)))))))
(defun set-basis-vectors-from-point-indices (mds-window coordss p0-index p1-index p2-index)
  (let ((basis-vectors (2-basis-lvectors ;;2-basis-lvectors-swap-y
			(nth p0-index coordss)
			(nth p1-index coordss)
			(nth p2-index coordss))))
    ;;(print (list (list (nth p0-index coordss) (nth p1-index coordss) (nth p2-index coordss)) basis-vectors))
    (set-basis-vectors mds-window basis-vectors)))
(defun 2-basis-lvectors (p0 p1 p2)
  (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	 (p2-p0 (lvector-difference p2 p0))
	 (basis1 (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0)))))
    (list basis0 basis1)))

|#

#|
;; for the lisp version, but the lisp numerical recipies svd is very buggy
(defun pca (coordss &optional &key (num-dims (length (car coordss))))
  (let* ((n (length coordss))
	 (a (array-list (all-comparisons-square coordss (^ (a b) (square (e-dist a b))))))
	 (w (make-array (list n) :initial-element 0.0))
	 (v (make-array (list n n) :initial-element 0.0)))
    (let ((a-row-squared-avs 
	   (make-array (list n)
		       :initial-contents 
		       (loop for i below n collect
			     (float 
			      (/ (loop for j below n sum (aref a i j))
				 n)))))
	  (a-col-squared-avs 
	   (make-array (list n)
		       :initial-contents 
		       (loop for j below n collect
			     (float 
			      (/ (loop for i below n sum (aref a i j))
				 n)))))
	  (row-col-squared-avs
	   (float
	    (/ 
	     (loop for i below n sum
		   (loop for j below n sum
			 (aref a i j)))
	     (* n n)))))
      (loop for i below n do
	    (loop for j below n do
		  (setf (aref a i j) 
		    (* 0.5
		       (+ (aref a i j)
			  (- (aref a-row-squared-avs i))
			  (- (aref a-col-squared-avs j))
			  row-col-squared-avs)))))
      (print (list-array a))
      (print (list-array w))
      (print (list-array v))
      (multiple-value-bind (a m n mp np w v)
	  (svdcmp a n n 'not-set 'not-set w v :nmax n)
	m mp np n
	(values
	 (loop for i below n collect
	       (loop for dim below num-dims collect
		     (* (aref a i dim) (sqrt (aref w dim)))))
	 a
	 w
	 v
	 'still-need-to-sort-by-eigenvalue)))))
|#


;; (pca '((0 0) (1 0) (1.5 1)))