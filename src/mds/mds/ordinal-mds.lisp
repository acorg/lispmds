(in-package user)

;;;----------------------------------------------------------------------
;;;                  ORDINAL MDS COMPARISON FUNCTIONS
;;;----------------------------------------------------------------------

(defun <*4 (x y) (< (* x 4) y))
(defun <*2 (x y) (< (* x 2) y))
(defun <+half (x y) (< (+ x 0.5) y))
(defun <+1 (x y) (< (+ x 1) y))
(defun <+2 (x y) (< (+ x 2) y))


;;;------------------------------------------------------------------------------
;;;               NOT SURE IF THE REST OF THIS FILE IS USED
;;;------------------------------------------------------------------------------

;;;------------------------------------------------------------------------------
;;;                           SCALING COORDS
;;;------------------------------------------------------------------------------

(defun scale-list (target original)
  (let* ((min-original (apply #'min original))
         (min-original-position (position min-original original))
         (max-original (apply #'max original))
         (max-original-position (position max-original original))
         (min-target (nth min-original-position target))
         (max-target (nth max-original-position target)))
    (mapcar (^ (x)
               (+ (* (- x min-target)
                     (/ (- max-original min-original)
                        (- max-target   min-target)))
                  min-original))
            target)))

(defun scale-lists (targets originals)
  (apply #'transpose (mapcar #'scale-list (apply #'transpose targets) (apply #'transpose originals))))
    

;;;----------------------------------------------------------------------
;;;                      TESTING ORDINAL MDS
;;;----------------------------------------------------------------------

(defun test-ordinal-mds-from-coordss (trial-coordss &rest hillclimbs-args)
  (let ((ans (ordinal-mds
	      (all-comparisons trial-coordss #'e-dist)
	      (loop for coord in trial-coordss collect
		    (loop for x in coord collect (knuth-random)))
	      :hillclimbs-args hillclimbs-args)))
    (ppl (transpose trial-coordss (mapcar (^ (coords) (mapcar #'2dp coords)) (scale-lists ans trial-coordss))))
    (g-plot-correlate (all-comparisons trial-coordss #'e-dist) (all-comparisons ans #'e-dist))
    (values (mapcar (^ (coords) (mapcar #'2dp coords)) (scale-lists ans trial-coordss))
            trial-coordss
            ans)))

#|
(test-ordinal-mds-from-coordss '((0) (1) (9)))
(test-ordinal-mds-from-coordss '((0) (1) (9) (10) (15) (16)))
=> ((0.3969478954) (0.39918418460000016) (0.5175842776000001) 
    (0.5217278734000002) (0.6043611520000002) (0.6193533042000001))
(mapcar (^ (x) (2dp (/ (- x (caar foo)) (- (caadr foo) (caar foo))))) (nths 0 
foo))
(0.0 1.0 53.94 55.8 92.75 99.45)

(test-ordinal-mds-from-coordss '((0) (1) (9) (10) (15) (16) (20) (24) (26) (27)))
((0.7521152712000001) (0.7453922543999999) (0.6363233526000002) 
 (0.6258103794000001) (0.5504234648000002) (0.5360204336) (0.4787634602000001) 
 (0.4230247250000002) (0.3901185078) (0.37455813920000014))
(mapcar (^ (x) (2dp (/ (- x (caar foo)) (- (caadr foo) (caar foo))))) (nths 0 
foo))
(0.0 1.0 17.22 18.79 30.0 32.14 40.66 48.95 53.84 56.16)
(mapcar (^ (x) (* x (/ 27 56.16))) *)
(0.0 0.48 8.28 9.03 14.42 15.45 19.55 23.53 25.88 27.0)    ;;better scaled, 
over whole range
(0   1    9   10    15    16    20    24    26    27)

(test-ordinal-mds-from-coordss (loop for i below 20 collect (list (krandom 20))) :max-climbs 1000)
((11 11.11)(8 8.04)(9 8.7)(5 5.08)(5 4.8)(10 10.28)(8 7.82)(13 12.86)(16 16.0)(12 11.95)
 (1 1.0)(11 11.02)(5 5.84)(12 11.74)(13 12.99)(5 5.27)(6 5.84)(2 2.3)(14 13.64)(12 10.99))

;;WOW, we really can recover metric data
;;and we have a climbable fitness function
;;and it looks like we don't have to worry about the degenerate case
;;and it will be easy to add 4-fold to this fitness function
;;and 3-way MDS for multiple tables!

;;when comparing with coords, do the above scaling over each dimension
;;yes, good, can then use that to check for "error correction"  (only in 1d)
;;error correction
;;  look at incorrect measurements (an outlier)
;;  look at coarse rule
;;  look at the x4 stuff and a coarse rule

(test-ordinal-mds-from-coordss '((0 0) (1 1) (9 9)))
((0.0 0.0) (6.49 -3.26) (9.0 9.0))
((0.0 0.0) (2.98 112.8) (9.0 9.0))
the above are ok, because it is dists, not coord values we are concerned with
|#

;;;------------------------------------------------------------------------------
;;;                               COARSE RULE
;;;------------------------------------------------------------------------------

(defun test-ordinal-mds-from-coordss-coarse-rule (trial-coordss finest-measure &rest hillclimbs-args)
  (let ((ans (ordinal-mds
	      (mapcar (^ (x) (finest-measure finest-measure x))
                            (all-comparisons trial-coordss #'e-dist))
	      (loop for coord in trial-coordss collect
		    (loop for x in coord collect (knuth-random)))
	      :hillclimbs-args hillclimbs-args)))
    (ppl (transpose trial-coordss 
		    (mapcar (^ (coords) (mapcar #'2dp coords)) (scale-lists ans trial-coordss))))
    (g-plot-correlate (all-comparisons trial-coordss #'e-dist) (all-comparisons ans #'e-dist))
    (g-plot-correlate (mapcar (^ (x) (finest-measure finest-measure x))
			      (all-comparisons trial-coordss #'e-dist))
		      (all-comparisons ans #'e-dist))
    (values (mapcar (^ (coords) (mapcar #'2dp coords)) (scale-lists ans trial-coordss))
            trial-coordss
            ans)))


#|
(test-ordinal-mds-from-coordss-coarse-rule '((0) (1) (9) (10) (15) (16) (20)) 2)
((0.0) (-0.04) (7.92) (8.11) (12.67) (15.73) (20.0))
((0) (1) (9) (10) (15) (16) (20))

(test-ordinal-mds-from-coordss-coarse-rule (nths 0 foo) 2 :max-climbs 1000)
((11) (11.22)) 
((8) (8.46)) 
((9) (9.74)) 
((5) (5.81)) 
((5) (5.72)) 
((10) (9.78)) 
((8) (8.08)) 
((13) (13.55)) 
((16) (16.0)) 
((12) (11.99)) 
((1) (1.0)) 
((11) (10.45)) 
((5) (6.92)) 
((12) (12.47)) 
((13) (13.46)) 
((5) (5.83)) 
((6) (6.32)) 
((2) (1.47)) 
((14) (14.14)) 
((12) (13.04)) 

here are some of the incorrect measurements in the input table
actually the measurements for results above
USER(51): (ppl (hist (transpose (mapcar (^ (x) (finest-measure 1 x))
                            (all-comparisons coords #'e-dist)) (mapcar (^ (x) (finest-measure 2 x))
                            (all-comparisons coords #'e-dist))) :test #'equal))

((13 12) 1) 
((14 14) 1) 
((15 16) 1) 
((12 12) 3) 
((11 12) 9) 
((7 8) 18) 
((4 4) 22) 
((8 8) 13) 
((9 8) 7) 
((0 0) 12) 
((10 10) 6) 
((5 4) 14) 
((1 0) 24) 
((6 6) 16) 
((2 2) 17) 
((3 4) 26) 

and if the finest-measure is 4
((13 12) 1) 
((14 16) 1) 
((15 16) 1) 
((12 12) 3) 
((11 12) 9) 
((7 8) 18) 
((4 4) 22) 
((8 8) 13) 
((9 8) 7) 
((0 0) 12) 
((10 8) 6) 
((5 4) 14) 
((1 0) 24) 
((6 8) 16) 
((2 0) 17) 
((3 4) 26) 

and here are the coords
((11) (10.48)) 
((8) (8.1)) 
((9) (8.67)) 
((5) (4.21)) 
((5) (4.61)) 
((10) (9.44)) 
((8) (7.95)) 
((13) (12.5)) 
((16) (16.0)) 
((12) (12.01)) 
((1) (1.0)) 
((11) (10.98)) 
((5) (4.51)) 
((12) (11.9)) 
((13) (12.53)) 
((5) (4.08)) 
((6) (4.8)) 
((2) (1.7)) 
((14) (14.22)) 
((12) (12.12)) 

now when the finest-measure is 8
((13 16) 1) 
((14 16) 1) 
((15 16) 1) 
((12 16) 3) 
((11 8) 9) 
((7 8) 18) 
((4 0) 22) 
((8 8) 13) 
((9 8) 7) 
((0 0) 12) 
((10 8) 6) 
((5 8) 14) 
((1 0) 24) 
((6 8) 16) 
((2 0) 17) 
((3 0) 26) 

the coords are 
((11) (10.03)) 
((8) (7.69)) 
((9) (8.41)) 
((5) (5.74)) 
((5) (5.02)) 
((10) (9.15)) 
((8) (7.19)) 
((13) (10.73)) 
((16) (16.0))   ;;i scaled to this
((12) (11.48)) 
((1) (1.0))     ;;i scaled to this
((11) (11.05)) 
((5) (3.96)) 
((12) (11.92)) 
((13) (14.02)) 
((5) (2.34)) 
((6) (5.78)) 
((2) (2.5)) 
((14) (13.61)) 
((12) (11.86)) 


do more extensive testing
and also testing of larger tables
(with this i can also test binary measurements)


coding:
 - prettier "shepherd plots"
science
 - record in note book these experiments (after i have sorted out what im looking at)
|#


;;;----------------------------------------------------------------------
;;;                      HISTORICAL
;;;----------------------------------------------------------------------

;(defun ordinal-mds (target-dists &rest args)
;  (apply #'mds 
;	 (^ (coordss)
;	    (loop for (mds-dist . mds-dist-rest) on (all-comparisons coordss #'e-dist)
;		  for (target-dist . target-dist-rest) on target-dists 
;		  when (not (eql 'dont-care target-dist)) sum
;		  (loop for next-mds-dist in mds-dist-rest
;		        for next-target-dist in target-dist-rest
;		        when (not (eql 'dont-care next-target-dist)) sum
;			(if (<= target-dist next-target-dist)
;			    (if (<= mds-dist next-mds-dist)
;				0
;			      (- (square (- next-mds-dist mds-dist))))
;			  (if (>= mds-dist next-mds-dist)
;			      0
;			    (- (square (- mds-dist next-mds-dist))))))))
;	 args))

#|
(defun ordinal-full-mds (target-dists stress-component-f &rest args)
  (apply #'mds 
	 (^ (coordss)
	    (loop for (mds-dist . mds-dist-rest) on (all-comparisons coordss #'e-dist)
		  for (target-dist . target-dist-rest) on target-dists 
		  when (not (eql 'dont-care target-dist)) sum
		  (loop for next-mds-dist in mds-dist-rest
		        for next-target-dist in target-dist-rest
		        when (not (eql 'dont-care next-target-dist)) sum
			(if (<= target-dist next-target-dist)
			    (funcall stress-component-f (- next-mds-dist mds-dist))
			  (funcall stress-component-f (- mds-dist next-mds-dist))))))
	 args))

|#