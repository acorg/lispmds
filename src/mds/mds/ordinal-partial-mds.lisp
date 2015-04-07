(in-package user)

(defun remove-dont-cares (index-dists)
  (filter (^ (x) (dont-care-p (nth 2 x))) index-dists))

;;simple non-optimized 
'(defun ordinal-partial-mds (target-dists stress-component-f &optional (comparison-f #'<) &rest args)
  ;;the target-dists are the upper triangle flattened
  (apply #'mds 
	 (let* ((comparisons (find-comparisons 
			      (sort-indexed-dists 
			       (remove-dont-cares 
				(index-dists target-dists)) 
			       #'<) 
			      comparison-f))
		(num-comparisons (print (length comparisons))))
	   (^ (coordss &optional old-coordss)
	      old-coordss
	      (/ (loop for ((p1 p2) (q1 q2)) in comparisons sum  ;;is it worth not destructuring?
		       (funcall stress-component-f               ;;test on naked loop example
				(- (e-dist (nth q1 coordss) (nth q2 coordss))
				   (e-dist (nth p1 coordss) (nth p2 coordss)))))
		 num-comparisons)))
	 args))

;;ordinal-partial-without call-to-call hack
'(defun ordinal-partial-mds (target-dists stress-component-f &optional (comparison-f #'<) &rest args)
  ;;the target-dists are the upper triangle flattened
  (apply #'mds 
	 (let* ((comparisons (find-comparisons 
			      (sort-indexed-dists 
			       (remove-dont-cares 
				(index-dists target-dists)) 
			       #'<) 
			      comparison-f))
		(num-comparisons (print (length comparisons)))
		(stress-components (make-array num-comparisons :initial-element 'not-set))
		(previous-raw-stress 'not-set))
	   (^ (coordss &optional old-coordss)
	      (let ((changed-coordss
			    (if old-coordss
				(loop for coords in coordss for old-coords in old-coordss for i from 0 
				    when (not (equal coords old-coords))
				    collect i)
			      'no-old-coordss)))
		;;(print changed-coordss)
		(let ((raw-stress
		       (cond ((null old-coordss)
			      (loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
				    (setf (aref stress-components i)
				      (funcall stress-component-f             
					       (- (e-dist (nth q1 coordss) (nth q2 coordss))
						  (e-dist (nth p1 coordss) (nth p2 coordss)))))))
			     ((null changed-coordss)
			      previous-raw-stress)
			     ((= 1 (length changed-coordss))
			      (let ((changed-coords (car changed-coordss)))
				(loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
				      (if (or (= changed-coords p1) (= changed-coords p2) (= changed-coords q1) (= changed-coords q2))
					  ;;(setf (aref stress-components i)
					  (funcall stress-component-f             
						   (- (e-dist (nth q1 coordss) (nth q2 coordss))
						      (e-dist (nth p1 coordss) (nth p2 coordss))))
					;;)
					(aref stress-components i)))))
			     (t (progn
				  (format t "More than one coord changed, optimization will not happen, but answer will be correct")
				  (loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
					(setf (aref stress-components i)
					  (funcall stress-component-f             
						   (- (e-dist (nth q1 coordss) (nth q2 coordss))
						      (e-dist (nth p1 coordss) (nth p2 coordss)))))))))))
		  (setq previous-raw-stress raw-stress)
		  (/ raw-stress
		     num-comparisons)))))
	 args))

(defun copy-array-contents (a b l)
  (loop for i below l do (setf (aref b i) (aref a i))))

(defun ordinal-partial-mds (target-dist-matrix stress-component-f &optional (comparison-f #'<) &rest args)
  (apply #'mds 
	 (let* ((target-dists (apply #'append (hi-table-values-upper-short-triangle target-dist-matrix)))
		(comparisons (find-comparisons 
			      (sort-indexed-dists 
			       (remove-dont-cares 
				(index-dists target-dists)) 
			       #'<) 
			      comparison-f))
		(num-comparisons (length comparisons))
		(stress-components (make-array num-comparisons :initial-element 'not-set))
		(previous-raw-stress 'not-set)
		(coordss-of-last-call 'not-set)
		(stress-of-last-call 'not-set)
		(stress-components-of-last-call (make-array num-comparisons :initial-element 'not-set)))
	   (^ (coordss &optional old-coordss)
	      (let ((partial-answer
		     (let ((changed-coordss
			    (if old-coordss
				(loop for coords in coordss for old-coords in old-coordss for i from 0 
				    when (not (equal coords old-coords))
				    collect i)
			      'no-old-coordss)))
		       ;;(print changed-coordss)
		       (let ((raw-stress
			      (cond ((null old-coordss)
				     (if (equal coordss coordss-of-last-call)
					 (progn '(print '--------------->>>>>>>>doing-the-trick)
						(copy-array-contents stress-components-of-last-call stress-components num-comparisons)
						stress-of-last-call)
				       (let ((stress (loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
							   (setf (aref stress-components i)
							     (funcall stress-component-f             
								      (- (e-dist (nth q1 coordss) (nth q2 coordss))
									 (e-dist (nth p1 coordss) (nth p2 coordss))))))))
					 ;;(print 'failed-the-trick)
					 (copy-array-contents stress-components stress-components-of-last-call num-comparisons)
					 (setq stress-of-last-call stress)
					 (setq coordss-of-last-call coordss)
					 stress)))
				    ((null changed-coordss)
				     (format t "check this is ok (in ordinal-partial-mds)")
				     (break)
				     previous-raw-stress)
				    ((= 1 (length changed-coordss))
				     (copy-array-contents stress-components stress-components-of-last-call num-comparisons)
				     (let* ((changed-coords (car changed-coordss))
					    (stress (loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
							  (if (or (= changed-coords p1) (= changed-coords p2) (= changed-coords q1) (= changed-coords q2))
							      (setf (aref stress-components-of-last-call i)
								(funcall stress-component-f             
									 (- (e-dist (nth q1 coordss) (nth q2 coordss))
									    (e-dist (nth p1 coordss) (nth p2 coordss)))))
							    (aref stress-components i)))))
				       (setq stress-of-last-call stress)
				       (setq coordss-of-last-call coordss)
				       stress))
				    (t (progn
					 (format t "More than one coord changed, optimization will not happen, check answer")
					 (loop for ((p1 p2) (q1 q2)) in comparisons for i from 0 sum
					       (setf (aref stress-components i)
						 (funcall stress-component-f             
							  (- (e-dist (nth q1 coordss) (nth q2 coordss))
							     (e-dist (nth p1 coordss) (nth p2 coordss)))))))))))
			 (setq previous-raw-stress raw-stress)
			 raw-stress))))
		'(let ((full-answer (loop for ((p1 p2) (q1 q2)) in comparisons sum
					 (funcall stress-component-f               
						  (- (e-dist (nth q1 coordss) (nth q2 coordss))
						     (e-dist (nth p1 coordss) (nth p2 coordss)))))))
		  (if (not (= partial-answer full-answer))
		      (progn
			(print (list (= full-answer partial-answer) full-answer partial-answer))
			(loop for full-eval-component in (loop for ((p1 p2) (q1 q2)) in comparisons collect
							       (funcall stress-component-f               
									(- (e-dist (nth q1 coordss) (nth q2 coordss))
									   (e-dist (nth p1 coordss) (nth p2 coordss)))))
			    for comparison in comparisons
			    for i from 0
			    when (not (= full-eval-component (aref stress-components i)))
			    do (print (list '-------- i comparison full-eval-component (aref stress-components i)))))))
		;;(setq foo (list-array stress-components))
		partial-answer)))
	 (let ((moveable-coords (snoop-moveable-coords args)))
	   (^ (coordss)
	      (let ((krandom (if (listp moveable-coords)
				 (nth (krandom (length moveable-coords)) moveable-coords)
			       (krandom (length coordss)))))
		(loop for coords in coordss for i from 0 collect
		      (if (= i krandom)
			  (uniform-perturbs coords 0.1)
			coords)))))
	 args))




(defun index-dists (dists)
  (let ((matrix-dimension-length (matrix-size-given-num-short-upper-triangle-elements (length dists))))
    (loop for dist in dists for i from 0 collect
	  (list i (2d-indices-from-1d-upper-triangle-index i matrix-dimension-length) dist))))

(defun sort-indexed-dists (indexed-dists sort-f)
  (sort indexed-dists (^ (x y) (funcall sort-f (nth 2 x) (nth 2 y)))))

(defun find-comparisons (sorted-indexed-dists &optional (difference-f #'<=))
  ;;this default #'<= is just next element
  ;;             #'<  is next different
  ;;             (^ (x y) (<= x (+ y 2)))
  (loop for (this . rest) on sorted-indexed-dists append
	;;do append so i can say nil and it goes away
	(let ((list-with-higher-values (list-with-higher-values this rest difference-f)))
	  (if list-with-higher-values
	      (loop for next in (list-with-same-values (car list-with-higher-values) list-with-higher-values difference-f) collect
		    (list (nth 1 this) (nth 1 next)    ;;this line has the comparisons that will be done during MDS
			  (nth 2 this) (nth 2 next)))  ;;this line is for viewing only, when i print the list
	    (if rest 
		;;non further down list are sufficiently different
		nil  ;;'(end-to-compare)
	      ;;that was the end of the list
	      nil)))))   

(defun list-with-higher-values (e l difference-f)
  (member e l :test (^ (a b) (funcall difference-f (nth 2 a) (nth 2 b)))))

(defun list-with-same-values (e l difference-f)
  (let ((position (position e l :test (^ (a b) (funcall difference-f (nth 2 a) (nth 2 b))))))
    (if position
	(firstn position l)
      l)))

;;;----------------------------------------------------------------------
;;;                           UTILS
;;;----------------------------------------------------------------------

(defun 2d-indices-from-1d-upper-triangle-index (index n)
  ;;this gives row then column
  (let ((count 0))
    (loop for i below (dec n) do
	  (let ((ans (loop for j from (inc i) below n do
			   (if (= count index)
			       (return (list i j))
			     (setq count (inc count))))))
	    (if ans
		(return ans)))
	  finally (error "Should not have got here"))))
		      
(defun matrix-size-given-num-short-upper-triangle-elements (l)
  (inc (round (- (sqrt (+ (* 2 l) 0.25))
		 0.5))))

#|
these points gave a local optima when running with min 0 and offset on the lapedes oMDS
and that was broken out of without the min and offset (presumably it only really required the min removal)

((0.09513871 0.5756035) (0.34193882 0.98985237) (0.20878159 0.860667) (0.8298513 0.5704126) (0.51366526 0.45873067)
 (0.7235568 0.3836671) (0.8410022 0.9689141) (0.12400404 0.3466318) (0.944917 0.8424613) (0.98760974 0.7541543))
|#


;;;----------------------------------------------------------------------
;;;                        HISTORICAL PLAY
;;;----------------------------------------------------------------------

;(defun lapedes-ordinal-mds (target-dists &rest args)
;  ;;here we offset and min to 0
;  ;;the target-dists are the upper triangle flattened
;  (apply #'mds 
;	 (let ((comparisons (pair-sorted-foo-dists (sort-foo-dists (foo-dists target-dists))))
;	       (offset-to-zero (- (log 0.5))))
;	   (^ (coordss)
;	      (loop for ((p1 p2) (q1 q2)) in comparisons sum
;		    (min 0 (+ offset-to-zero
;		       (log (squash (- (e-dist (nth q1 coordss) (nth q2 coordss))
;				       (e-dist (nth p1 coordss) (nth p2 coordss))))))))))
;	 args))

;;this has a tail-hack that does not work for other than < 
;(defun pair-sorted-foo-dists (sorted-foo-dists &optional (difference-f #'<))
;  ;;this default #'<= is just next element
;  ;;             #'<  is next different
;  ;;             (^ (x y) (<= x (+ y 2)))
;  (let (last-this-for-tail-hack)
;    (loop for (this . rest) on sorted-foo-dists append
;	  ;;do append so i can say nil and it goes away
;	  (let ((next (next-element-with-different-value this rest difference-f)))
;	    (if next 
;		(list (list (setq last-this-for-tail-hack (nth 1 this)) (nth 1 next)))
;	      (if rest ;;non further down list are sufficiently different
;		  (if last-this-for-tail-hack
;		      (list (list last-this-for-tail-hack (nth 1 (car rest))))
;		    nil)  ;;case when no elements in list at all are different enuf from any others
;		nil))))))   


#|

without rounding

(((2 5) (2 9) 10 11) ((2 9) (6 9) 11 13) ((6 9) (6 8) 13 16) ((6 8) (2 8) 16 19) ((2 8) (5 6) 19 21) ((5 6) (1 5) 21 36)
 ((1 5) (2 3) 36 38) ((2 3) (3 6) 38 40) ((3 6) (0 8) 40 42) ((0 8) (0 2) 42 50) ((0 2) (4 9) 50 52) ((4 9) (0 9) 52 53)
 ((5 7) (0 9) 52 53) ((0 9) (4 8) 53 57) ((4 8) (0 1) 57 66) ((0 1) (1 6) 66 76) ((7 9) (1 6) 66 76) ((1 6) (1 9) 76 79)
 ((1 9) (4 5) 79 80) ((4 5) (5 8) 80 93) ((5 8) (7 8) 93 102) ((7 8) (0 3) 102 113) ((0 3) (1 2) 113 140) ((1 2) (2 4) 140 145)
 ((2 4) (3 4) 145 151) ((3 4) (0 7) 151 152) ((0 7) (0 5) 152 156) ((0 5) (1 8) 156 160) ((0 6) (1 8) 156 160) ((1 8) (3 5) 160 161)
 ((3 5) (1 4) 161 164) ((1 4) (6 7) 164 183) ((6 7) (5 9) 183 202) ((5 9) (2 7) 202 215) ((2 7) (3 7) 215 224) ((3 7) (1 3) 224 247)
 ((1 3) (2 6) 247 267) ((2 6) (4 6) 267 316) ((4 6) (3 9) 316 375) ((3 9) (4 7) 375 417) ((4 7) (0 4) 417 422) ((0 4) (3 8) 422 472)
 ((3 8) (8 9) 472 481) ((8 9) (1 7) 481 492)) 


with rounding

(((2 5) (2 8) 10 20) ((2 9) (2 8) 10 20) ((6 9) (2 8) 10 20) ((2 8) (0 2) 20 40) ((5 6) (0 2) 20 40) ((6 8) (0 2) 20 40)
 ((0 2) (0 1) 40 80) ((0 8) (0 1) 40 80) ((0 9) (0 1) 40 80) ((1 5) (0 1) 40 80) ((2 3) (0 1) 40 80) ((3 6) (0 1) 40 80)
 ((4 9) (0 1) 40 80) ((5 7) (0 1) 40 80) ((0 1) (0 5) 80 160) ((0 3) (0 5) 80 160) ((1 6) (0 5) 80 160) ((1 9) (0 5) 80 160)
 ((4 5) (0 5) 80 160) ((4 8) (0 5) 80 160) ((5 8) (0 5) 80 160) ((7 8) (0 5) 80 160) ((7 9) (0 5) 80 160) ((0 5) (0 4) 160 320)
 ((0 6) (0 4) 160 320) ((0 7) (0 4) 160 320) ((1 2) (0 4) 160 320) ((1 4) (0 4) 160 320) ((1 8) (0 4) 160 320) ((2 4) (0 4) 160 320)
 ((2 7) (0 4) 160 320) ((3 4) (0 4) 160 320) ((3 5) (0 4) 160 320) ((3 7) (0 4) 160 320) ((5 9) (0 4) 160 320) ((6 7) (0 4) 160 320)
 ((0 4) (1 7) 320 640) ((1 3) (1 7) 320 640) ((2 6) (1 7) 320 640) ((3 9) (1 7) 320 640) ((4 6) (1 7) 320 640) ((4 7) (1 7) 320 640)) 

|#


;;;----------------------------------------------------------------------
;;;                         TIMING FOR LAPEDES
;;;----------------------------------------------------------------------

#|
(ordinal-mds (all-comparisons foo #'e-dist) '((0.39431077 0.69756424) (0.8606575 0.8064499) (0.079279535 0.6015672) 
					      (0.633542 0.8955652) (0.7825126 0.48757812) (0.25140613 0.3180885) 
					      (0.3579287 0.35260952) (0.32890105 0.69128746) (0.24129799 0.7369013) 
					      (0.0987132 0.501906)) 
	     :hillclimbs-args '(:max-trials 250))
|#



;;;----------------------------------------------------------------------
;;;                      HISTORICAL
;;;----------------------------------------------------------------------

#|
(defun lapedes-ordinal-mds (target-dists &rest args)
  ;;the target-dists are the upper triangle flattened
  (apply #'mds 
	 (let ((comparisons (find-comparisons (sort-indexed-dists (index-dists target-dists) #'<) #'<)))
	   (^ (coordss)
	      (loop for ((p1 p2) (q1 q2)) in comparisons sum
		    (log (squash (- (e-dist (nth q1 coordss) (nth q2 coordss))
				    (e-dist (nth p1 coordss) (nth p2 coordss))))))))
	 args))

(defun ordinal-lapedes-ordinal-mds (target-dists &rest args)
  ;;the target-dists are the upper triangle flattened
  (apply #'mds 
	 (let ((comparisons (find-comparisons (sort-indexed-dists (index-dists target-dists) #'<) #'<)))
	   (^ (coordss)
	      (loop for ((p1 p2) (q1 q2)) in comparisons sum
		    (- (square  (- (e-dist (nth q1 coordss) (nth q2 coordss))
				    (e-dist (nth p1 coordss) (nth p2 coordss))))))))
	 args))

;;test similarity by reordering the minus
'(defun lapedes-ordinal-mds (target-dists &rest args)
  ;;the target-dists are the upper triangle flattened
  (apply #'mds 
	 (let ((comparisons (print (find-comparisons (sort-indexed-dists (index-dists target-dists) #'<) #'<))))
	   (^ (coordss)
	      (loop for ((p1 p2) (q1 q2)) in comparisons sum
		    (log (squash (- (e-dist (nth p1 coordss) (nth p2 coordss))
				    (e-dist (nth q1 coordss) (nth q2 coordss))))))))
   args))

(defun next-element-with-different-value (e l difference-f)
  (find e l :test (^ (a b) (funcall difference-f (nth 2 a) (nth 2 b)))))

(defun find-comparisons (sorted-indexed-dists &optional (difference-f #'<=))
  ;;this default #'<= is just next element
  ;;             #'<  is next different
  ;;             (^ (x y) (<= x (+ y 2)))
  (loop for (this . rest) on sorted-indexed-dists append
	;;do append so i can say nil and it goes away
	(let ((next (next-element-with-different-value this rest difference-f)))
	  (if next 
	      (list (list (nth 1 this) (nth 1 next)    ;;these are the comparisons that will be done during MDS
			  (nth 2 this) (nth 2 next)))  ;;these are for viewing only, when i print the list
	    (if rest 
		;;non further down list are sufficiently different
		nil  ;;'(end-to-compare)
	      ;;that was the end of the list
	      nil)))))   


|#
