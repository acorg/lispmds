(in-package user)

;;;----------------------------------------------------------------------
;;;                          CORRELATION
;;;----------------------------------------------------------------------

(defun standard-deviation-difference (x av sd)
  ;;(if (and (zerop sd)   ;;if zero sd because all x's are the same (and thus av=x) then 
	;;   (eql x av))  ;;  say we are 0 sd's away, but if zero sd because no values, then divide by 0
  ;;    0
    (/ (- x av)
       sd))
  ;;)

(defun correlation (pairs)
  (if (null pairs)
      0.0
    (let ((xs (nths 0 pairs))
	  (ys (nths 1 pairs)))
      (multiple-value-bind (x-av x-sd)
	  (av-sd xs)
	(multiple-value-bind (y-av y-sd)
	    (av-sd ys)
	  (let ((r (cond ((and (zerop x-sd) (zerop y-sd)) 0.0)  ;;(error "what to do?")
			 ((or  (zerop x-sd) (zerop y-sd)) 1.0)
			 (t (av (mapcar (^ (x y)
					   (* (standard-deviation-difference x x-av x-sd)
					      (standard-deviation-difference y y-av y-sd)))
					xs ys))))))
	    (multiple-value-bind (loa-m loa-c)   ;; line of averages (is this right?)
		(linear-interpolation-m-c
		 x-av y-av
		 (+ x-av x-sd) (+ y-av y-sd))
	      (values r
		      x-av y-av
		      x-sd y-sd
		      (list loa-m loa-c)))))))))

(defun regression (pairs &optional &key zero-intercept)
  (if zero-intercept
      ;; use R for now
      (values (nth 1 (nth 0 (r-regression (cons '(y x) (mapcar #'reverse pairs)) :zero-intercept t)))
	      0
	      (correlation pairs))
    (multiple-value-bind (r x-av y-av x-sd y-sd)
	(correlation pairs)
      (multiple-value-bind (m c)
	  (linear-interpolation-m-c
	   x-av y-av
	   (+ x-av x-sd) (+ y-av (* r y-sd)))
	(values
	 m c
	 r
	 (* y-sd (sqrt (- 1 (square r))))
	 x-av y-av
	 x-sd y-sd)))))

(defun regression-rms-error (pairs)
  (nth-value 3 (regression pairs)))

(defun regression-intercept (pairs)
  (nth-value 1 (regression pairs)))

(defun regression-y-given-x (pairs x)
  (multiple-value-bind (m c)
      (regression pairs)
    (+ (* x m) c)))

(defun regression-residuals (pairs)
  (multiple-value-bind (m c)
      (regression pairs)
    (loop for (x y) in pairs collect
	  (- y (+ c (* m x))))))


;;;----------------------------------------------------------------------
;;;                        REGRESSION PLOT
;;;----------------------------------------------------------------------
	  
(defun regression-plot (pairs &rest plot-args)
  (multiple-value-bind (m c r rms av-x av-y sd-x sd-y)
      (regression pairs)
    (let* ((min-x (apply #'min (nths 0 pairs))) (max-x (apply #'max (nths 0 pairs)))
	   (min-y (apply #'min (nths 1 pairs))) (max-y (apply #'max (nths 1 pairs)))
	   (2min-x (double min-x)) (2max-x (double max-x))         ;; these assume the mins are -ve, should really go further from the 
	   (2min-y (double min-y)) (2max-y (double max-y)))        ;; center, or make it 0,0 if we are close to that

      ;;Scatter plot
      (apply #'g-plot pairs
	     (append plot-args
		     (list :style 'scatter 
			   :x-min min-x :x-max max-x :y-min min-y :y-max max-y
			   :element-name "Pairs" :element-color 'black
			   :tag (format nil "{~d ~d} -text ~s -anchor nw" 
					(+ min-x (/ (- max-x min-x) 20.0))
					(- max-y (/ (- max-y min-y) 20.0))
					(format nil "r=~d" (dps r 3))))))

      ;;Regression line
      (g-plot (list (list 2min-x (+ (* m 2min-x) c)) (list 2max-x (+ (* m 2max-x) c))) 
	      :element-name "Regression" :element-color 'blue :refresh nil
	      :tag (format nil "{~d ~d} -text ~s -anchor nw" 
			   (+ min-x (/ (- max-x min-x) 20.0))
			   (- max-y (double (/ (- max-y min-y) 20.0)))
			   (format nil "rms=~d" (dps rms 2))))
      (multiple-value-bind (sd-line-m sd-line-c)
	  (linear-interpolation-m-c av-x av-y (+ av-x (* (sign r) sd-x)) (+ av-y sd-y))

	;;Standard Deviation line
	(g-plot (list (list 2min-x (+ (* sd-line-m 2min-x) sd-line-c)) (list 2max-x (+ (* sd-line-m 2max-x) sd-line-c))) 
		:element-name "SD Line" :element-color 'green :refresh nil)
	
	;;average lines
	(g-plot (list (list 2min-x av-y) (list 2max-x av-y))
		:element-name "av-x" :element-color 'gray90 :refresh nil)
	(g-plot (list (list av-x 2min-y) (list av-x 2max-y))
		:element-name "av-y" :element-color 'gray90 :refresh nil)

	;;+/- x/y standard deviation lines
	(g-plot (list (list 2min-x (+ av-y sd-y)) (list 2max-x (+ av-y sd-y)))
		:element-name "+sd-y" :element-color 'gray80 :refresh nil)
	(g-plot (list (list 2min-x (- av-y sd-y)) (list 2max-x (- av-y sd-y)))
		:element-name "-sd-y" :element-color 'gray80 :refresh nil)
	(g-plot (list (list (+ av-x sd-x) 2min-y) (list (+ av-x sd-x) 2max-y))
		:element-name "+sd-x" :element-color 'gray80 :refresh nil)
	(g-plot (list (list (- av-x sd-x) 2min-y) (list (- av-x sd-x) 2max-y))
		:element-name "-sd-x" :element-color 'gray80 :refresh nil)
	))))

;;still need to fix the element colors, and putting the legend away from the data
(defun regression-gnuplot (pairs &rest plot-args)
  (multiple-value-bind (m c r rms av-x av-y sd-x sd-y)
      (regression pairs)
    (let* ((min-x (apply #'min (nths 0 pairs))) (max-x (apply #'max (nths 0 pairs)))
	   (min-y (apply #'min (nths 1 pairs))) (max-y (apply #'max (nths 1 pairs)))
	   (2min-x (double min-x)) (2max-x (double max-x))         ;; these assume the mins are -ve, should really go further from the 
	   (2min-y (double min-y)) (2max-y (double max-y)))        ;; center, or make it 0,0 if we are close to that

      ;;Scatter plot
      (apply #'gnuplot pairs
	     (append plot-args
		     (list :element-style 'points
			   :x-min min-x :x-max max-x :y-min min-y :y-max max-y
			   :element-name "Pairs" ;;:element-color 'black
			   :label (list (format nil "r=~d" (dps r 3))
					(+ min-x (/ (- max-x min-x) 20.0))
					(- max-y (/ (- max-y min-y) 20.0))))))

      ;;Regression line
      (gnuplot (list (list 2min-x (+ (* m 2min-x) c)) (list 2max-x (+ (* m 2max-x) c))) 
	       :element-name "Regression" ;;:element-color 'blue 
	       :refresh nil
	       :label (list (format nil "rms=~d" (dps rms 2))
			    (+ min-x (/ (- max-x min-x) 20.0))
			    (- max-y (double (/ (- max-y min-y) 20.0)))
			    ))
      (multiple-value-bind (sd-line-m sd-line-c)
	  (linear-interpolation-m-c av-x av-y (+ av-x (* (sign r) sd-x)) (+ av-y sd-y))

	;;Standard Deviation line
	(gnuplot (list (list 2min-x (+ (* sd-line-m 2min-x) sd-line-c)) (list 2max-x (+ (* sd-line-m 2max-x) sd-line-c))) 
		 :element-name "SD Line" ;;:element-color 'green 
		 :refresh nil)
	
	;;average lines
	(gnuplot (list (list 2min-x av-y) (list 2max-x av-y))
		 :element-name "av-x" ;;:element-color 'gray90 
		 :refresh nil)
	(gnuplot (list (list av-x 2min-y) (list av-x 2max-y))
		 :element-name "av-y" ;;:element-color 'gray90 
		 :refresh nil)

	;;+/- x/y standard deviation lines
	(gnuplot (list (list 2min-x (+ av-y sd-y)) (list 2max-x (+ av-y sd-y)))
		 :element-name "+sd-y" ;;:element-color 'gray80 
		 :refresh nil)
	(gnuplot (list (list 2min-x (- av-y sd-y)) (list 2max-x (- av-y sd-y)))
		 :element-name "-sd-y" ;;:element-color 'gray80 
		 :refresh nil)
	(gnuplot (list (list (+ av-x sd-x) 2min-y) (list (+ av-x sd-x) 2max-y))
		 :element-name "+sd-x" ;;:element-color 'gray80
		 :refresh nil)
	(gnuplot (list (list (- av-x sd-x) 2min-y) (list (- av-x sd-x) 2max-y))
		 :element-name "-sd-x" ;;:element-color 'gray80
		 :refresh nil)
	))))
      

;;;----------------------------------------------------------------------
;;;                         LINE OF AVERAGES
;;;----------------------------------------------------------------------

(defun line-of-averages (pairs)
  (sort-car 
   (loop for x in (remove-duplicates (nths 0 pairs)) collect
	 (list x (av (nths 1 (collect (^ (pair) (eql x (car pair))) pairs)))))))

(defun plot-line-of-averages (pairs)
  (multiple-value-bind (m c r rms)
      (regression pairs)
    (let* ((min-x (apply #'min (nths 0 pairs))) (max-x (apply #'max (nths 0 pairs)))
	   (min-y (apply #'min (nths 1 pairs))) (max-y (apply #'max (nths 1 pairs)))
	   (2min-x (double min-x)) (2max-x (double max-x))
	   (2min-y (double min-y)) (2max-y (double max-y)))
      (g-plot (list (list 2min-x (+ (* m 2min-x) c)) (list 2max-x (+ (* m 2max-x) c))) 
	      :x-min min-x :x-max max-x :y-min min-y :y-max max-y
	      :element-name "Regression" :element-color 'black
	      :tag (format nil "{~d ~d} -text ~s -anchor nw" 
			   (+ min-x (/ (- max-x min-x) 20.0))
			   (- max-y (double (/ (- max-y min-y) 20.0)))
			   (format nil "rms=~d" (dps rms 2))))
      (g-plot (line-of-averages pairs)
	      :element-name "Line of Avs" :element-color 'blue :element-symbol 'circle :element-linewidth 0
	      :refresh nil
	      :tag (format nil "{~d ~d} -text ~s -anchor nw" 
			   (+ min-x (/ (- max-x min-x) 20.0))
			   (- max-y (/ (- max-y min-y) 20.0))
			   (format nil "r=~d" (dps r 3))))
      )))

;;;----------------------------------------------------------------------
;;;                      01half-plot
;;;----------------------------------------------------------------------

(defun g-plot-01half (pairs &rest args)
  (apply #'g-plot pairs
	 (append args
		 (list :y-min 0 :y-max 1)))   ;;put these last so args can override
  (g-plot (list (list (apply #'min (nths 0 pairs)) 0.5)
		(list (apply #'max (nths 0 pairs)) 0.5))
	  :element-color 'gray80
	  :refresh nil))

(defun g-plot--110 (pairs &rest args)
  (apply #'g-plot pairs
	 (append args
		 (list :y-min -1 :y-max 1)))   ;;put these last so args can override
  (g-plot (list (list (apply #'min (nths 0 pairs)) 0)
		(list (apply #'max (nths 0 pairs)) 0))
	  :element-color 'gray80
	  :refresh nil))



;;;----------------------------------------------------------------------
;;;                      STATISTICAL SIGNIFICANCE
;;;----------------------------------------------------------------------

(defun z-obs (p1-hat n1 p2-hat n2)
  ;;1.65 standard deviations is 95% for one-tailed
  ;;1.97 standard deviations is 95% for two-tailed
  ;;2.58 standard deviations is 99% for one-tailed
  ;;2.82 standard deviations is 99% for two-tailed
  (let* ((p-hat (/ (+ (* p1-hat n1) (* p2-hat n2)) (+ n1 n2)))
	 (se (sqrt (* p-hat (- 1 p-hat) (+ (/ 1 n1) (/ 1 n2))))))
    (if (zerop se)
	7777777  ;;if p1-hat and p2-hat are both 0 or both 1
      (/ (- p1-hat p2-hat)
	 se))))


(defun var (l)
  (let ((av (av l)))
    (float (/ (loop for e in l sum (square (- av e))) (dec (length l))))))

(defun t-test-from-zero (l)
  ;; t is mean / se mean
  (let ((mean (av l))
	(se-mean (sqrt (/ (var l) (length l)))))
    (/ mean se-mean)))

(defun t-test-paired-differences (l)
  (t-test-from-zero (map-apply #'- l)))

(defun lisp-to-r-bool (lisp-bool)
  (if lisp-bool 'TRUE 'FALSE))

(defun r-t-test (l1 l2 &optional &key var-equal paired)
  (fi (f-elements (^ (x) (if (typep x 'long-float) (coerce x 'short-float) x)) (cons 'l1 l1))
      "/tmp/data.l1.r"
      :supersede
      t
      :write-outer-list-elements-individually t
      :write-inner-list-elements-individually t)
  (fi (f-elements (^ (x) (if (typep x 'long-float) (coerce x 'short-float) x)) (cons 'l2 l2))
      "/tmp/data.l2.r"
      :supersede
      t
      :write-outer-list-elements-individually t
      :write-inner-list-elements-individually t)
  (fi (list (format nil "L1 <- read.table(~s, header=TRUE)" "/tmp/data.l1.r")
	    (format nil "L2 <- read.table(~s, header=TRUE)" "/tmp/data.l2.r")
	    (format nil "t.test(L1$L1, L2$L2, var.equal=~a, paired=~a)" (lisp-to-r-bool var-equal) (lisp-to-r-bool paired)))
      "/tmp/script.r"
      :supersede
      t
      :write-outer-list-elements-individually t)
  (run-shell-command "R CMD BATCH /tmp/script.r /tmp/script.r.out")
  (print-file "/tmp/script.r.out"))

(defun r-var-test (l1 l2)
  (fi (f-elements (^ (x) (if (typep x 'long-float) (coerce x 'short-float) x)) (cons 'l1 l1))
      "/tmp/data.l1.r"
      :supersede
      t
      :write-outer-list-elements-individually t
      :write-inner-list-elements-individually t)
  (fi (f-elements (^ (x) (if (typep x 'long-float) (coerce x 'short-float) x)) (cons 'l2 l2))
      "/tmp/data.l2.r"
      :supersede
      t
      :write-outer-list-elements-individually t
      :write-inner-list-elements-individually t)
  (fi (list (format nil "L1 <- read.table(~s, header=TRUE)" "/tmp/data.l1.r")
	    (format nil "L2 <- read.table(~s, header=TRUE)" "/tmp/data.l2.r")
	    (format nil "var.test(L1$L1, L2$L2)"))
      "/tmp/script.r"
      :supersede
      t
      :write-outer-list-elements-individually t)
  (run-shell-command "R CMD BATCH /tmp/script.r /tmp/script.r.out")
  (print-file "/tmp/script.r.out"))

  

;;;----------------------------------------------------------------------
;;;                      confidence intervals
;;;----------------------------------------------------------------------

(defun se-of-proportion (n p)
  (sqrt (/ (* p (- 1 p)) n)))

(defun confidence-interval (n p &optional (confidence 0.95))
  (let ((se (se-of-proportion n p))
	(delta (case confidence
		 (0.95 1.96)
		 (0.99 2.58)
		 (t (error "confidence of ~d is not programmed, use 0.95, 0.99, or change the program" confidence)))))
    (list (- p (* se delta))
	  (+ p (* se delta)))))


;;;----------------------------------------------------------------------
;;;                      regression using R
;;;----------------------------------------------------------------------

#|
(> DIMNAMES (AI$COMPLETE) [1]) 
([[1]]) 
([1] "IV058" "ST047") 
(> DIMNAMES (AI$COMPLETE) [2]) 
([[1]]) 
([1] "AE163" "AS114" "AS138" "AT138" "AT167" "AV272" "DE101" "DE135" "DE190") 
([10] "DG053" "DG078" "DG275" "DN291" "EG135" "EG325" "EK156" "FS219" "GK135") 
([19] "IM067" "IR208" "IT214" "IT248" "IT262" "IV196" "IV267" "IV323" "KN092") 
([28] "KN122" "KN145" "KR050" "LQ226" "NS006" "NS096" "NS133" "NS145" "NS246") 
([37] "NS278" "PS157" "PS227" "PS289" "PT143") 
(> FOR (I IN 1-DIM (AI$COMPLETE) [1]) PRINT (AI$COMPLETE[I !IS.NA (AI$COMPLETE[I ]) ])) 
([1] 1) 
([1] 1) 
|#


(defun R-pp-vector-to-list (raw-data)
  (let ((with-headers-vector-printing
	 (not (apply #'nary-equal t (mapcar (^ (first-element)
						  (equal #\[ (aref (format nil "~a" first-element) 0)))
					       (mapcar #'car raw-data))))))
    (if (not with-headers-vector-printing)
	(list (mapcar (^ (x) (if (stringp x) (read-from-string x) x))
		      (apply-append (mapcar #'cdr raw-data))))
      (let ((no-numbered-raw-data 
	     (filter (^ (l) (equal #\[ (aref (format nil "~a" (car l)) 0))) raw-data)))
	(list (apply-append (enths 2 no-numbered-raw-data))
	      (apply-append (enths 2 no-numbered-raw-data 1)))))))
       
	    

(defun R-pp-vectors-to-list (raw-data)
  (let ((current-vector (list (car raw-data)))
	lists)
    (loop for datum in (cdr raw-data) do
	  (if (eql '[1] (car datum))
	      (progn
		(setq lists (append lists (R-pp-vector-to-list current-vector)))
		(setq current-vector (list datum)))
	    (push-end datum current-vector)))
    (append
     lists
     (R-pp-vector-to-list current-vector))))
		
#|
(R-pp-vectors-to-list
 '(
   ([1] "IV058" "ST047") 
   ([1] "AE163" "AS114" "AS138" "AT138" "AT167" "AV272" "DE101" "DE135" "DE190") 
   ([10] "DG053" "DG078" "DG275" "DN291" "EG135" "EG325" "EK156" "FS219" "GK135") 
   ([19] "IM067" "IR208" "IT214" "IT248" "IT262" "IV196" "IV267" "IV323" "KN092") 
   ([28] "KN122" "KN145" "KR050" "LQ226" "NS006" "NS096" "NS133" "NS145" "NS246") 
   ([37] "NS278" "PS157" "PS227" "PS289" "PT143") 
   ([1] 1) 
   ([1] 1)))
((IV058 ST047) (AE163 AS114 AS138 AT138 AT167 AV272 DE101 DE135 DE190 DG053 ...) (1) (1))
|#		   

(defun parse-alias-output (raw-data)
  (let* ((vectors 
	  (let ((ll (R-pp-vectors-to-list
			 (filter (^ (l) (eql '> (car l)))
				 raw-data))))
	    ;; when a col is all zero, the multiplers part of the alias output is not only vectors, but has "numeric(0)" and "character(0)"
	    ;; entries.  could get around this by parsing them, or with a better R statement to print the multipliers matrix, but for now
	    ;; barf and require no non-zero cols.  as i'm currently only calling R from lisp, it is no problem to exclude the zero cols. 11oct2003
	    (loop for l in ll
		when (or (equal l '(NUMERIC (0)))
			 (equal l '(CHARACTER (0))))
		do (error "Unhandled condition in R alias postprocessing (perhaps due to the original data having column(s) of all zeros)"))
	    ll))
	 (includeds (nth 0 vectors))
	 (excludeds (nth 1 vectors))
	 (multiplierss (mapcar #'apply-transpose (groups-of-n 2 (nthcdr 2 vectors))))
	 (collinear-relations (loop for excluded in excludeds 
				  for multipliers in multiplierss append
				    (loop for multiplier in multipliers collect
					  (append multiplier (list excluded)))))
	 (collinear-relation-names (nths 0 collinear-relations)))
    (loop for included in includeds collect
	  (let ((collinears (multiple-nth (positions included collinear-relation-names :test #'equal) collinear-relations)))
	    (cons included (mapcar #'reverse (mapcar #'cdr collinears)))))))

(defun alias-output-to-names (data)
;;  (let ((num-of-each-excluded (filter #'null (nths 0 (mapcar #'cdr data)))))
  (let ((num-of-each-excluded (hist (apply #'append (mapcar (^ (l) (nths 0 l)) (mapcar #'cdr data))))))
    (loop for datum in data collect
	  (read-from-string
	   (format nil "~a~{~a~}"
		   ;;(if (> (length datum) 1) "C." "")
		   (car datum)
		   (mapcar (^ (l) 
			      (format nil "~a~a~a~a"
				      (if (plusp (nth 1 l)) "+" "-")
				      (if (= 1 (abs (nth 1 l))) "" (abs (nth 1 l)))
				      (let ((num-times-used (assoc-value-1 (nth 0 l) num-of-each-excluded)))
					(if (= 1 num-times-used)
					    ""
					  (format nil "OF~d" num-times-used)))
				      (nth 0 l)))
			   (cdr datum)))))))

#|
(RESIDUALS-) 
(MIN 1Q MEDIAN 3Q MAX) 
(-2.8033 -0.178 -0.1764 0.2585 3.6775) 
(COEFFICIENTS- (212 NOT DEFINED BECAUSE OF SINGULARITIES)) 
(ESTIMATE STD. ERROR T VALUE PR (|>t|)) 
((INTERCEPT) 0.17643 0.04454 3.961 8.1999993e-5 ***) 
(AD124 0.87058 0.3555 2.449 0.014567 *) 
(AE163 0.78102 0.14235 5.487 5.68e-8 ***) 
(AS114 0.38805 0.31636 1.227 0.220367) 
(SV186 4.79495 0.79332 6.044 2.41e-9 ***) 
(SY137 -0.84933 1.08549 -0.782 0.434215) 
(ZAS138.KN145 -4.43366 0.52029 -8.522 < 1.9999997e-16 ***) 
(---) 
(SIGNIF. CODES- 0 `*** '0.001 `** '0.01 `* '0.05 `'0.1 `'1) 
(RESIDUAL STANDARD ERROR- 0.7585 ON 718 DEGREES OF FREEDOM) 
(MULTIPLE R-SQUARED- 0.7086 ADJUSTED R-SQUARED- 0.6724) 
(F-STATISTIC- 19.61 ON 89 AND 718 DF P-VALUE- < 2.1999998e-16) 
|#

(defun parse-r-regression-output-file (filename &optional &key 
							  (compose-collinear-columns t))
  (let* ((raw-data (fi-in-readline filename
				   :line-process-f 
				   #'space-delimited-string-to-list-replacing-colon-with-asterix-and-comma-with-space-semicolon-with-space-and-removing-free-periods-unparen-intercept))
	 (f-statistic  (let ((line (assoc 'f-statistic*             raw-data)))
			 (list (nth 1 line) (nth 3 line) (nth 5 line) (nthcdr 8 line))))
	 (adj-r-square (nth 5 (assoc 'multiple                 raw-data)))
	 (residual-se  (nth 3 (assoc 'residual                 raw-data)))
	 (residuals    (nth (+ 2 (assoc-position 'residuals*      raw-data)) raw-data))
	 (collinearp   (and compose-collinear-columns
			    (not (equal '(NULL) (nth (inc (position '(> ai$Complete) raw-data :test #'equal)) raw-data)))))
	 (names-including-collinearity-info
	  (if collinearp
	      (alias-output-to-names
	       (parse-alias-output 
		(let ((a-start (inc (position '([1] "aliasstart") raw-data :test #'equal)))
		      (a-end   (dec (position '([1] "aliasend") raw-data :test #'equal))))
		  (firstn (- a-end a-start) (nthcdr a-start raw-data))
		  )))))
	 (coefficients 
	  (let* ((c-start (+ (assoc-position 'coefficients*   raw-data) 2))
		 (c-end   (or (assoc-position '---             raw-data)   ;; not present when no variation
			      (assoc-position 'residual             raw-data)))
		 (coeffs (firstn (- c-end c-start) (nthcdr c-start raw-data))))
	    (if (equal '(intercept) (caar coeffs))
		(cons (cons 'intercept (cdar coeffs))
		      (cdr coeffs))
	      coeffs)))
	 (cooefficients-with-collinear-info-names
	  (if collinearp
	      (progn
		(if (not (= (length coefficients) (length names-including-collinearity-info)))
		    (error "unexptected condition"))
		(loop for name in names-including-collinearity-info
		    for coefficient in coefficients 
		    collect (cons name (cdr coefficient))))
	    coefficients)))
    (values
     cooefficients-with-collinear-info-names
     adj-r-square    
     residuals   
     f-statistic 
     residual-se
     )))

(defun r-regression (data &optional &key 
				    zero-intercept 
				    (compose-collinear-columns t)
				    multiplicative-model   ;; nil means +, t means * (all combinations), 
				                           ;; and lists indicate which are interaction terms eg ((a b) (a b c))
                                                           ;; gives a:b + a:b:c  as extra terms to the additive terms
				    show-t-and-p-values)
  ;; assume first line are column names
  ;; and the first col is the dependent variable, and the remaining columns are dependent variables
  ;; later we can add keyword args for specifying alternates to this
  (let ((independent-variable (car (car data)))
	(dependent-variables  (cdr (car data))))
    (fi (f-elements (^ (x) 
		       (if (typep x 'long-float)
			   (coerce x 'short-float)
			 x))
		    data)
	"/tmp/data.r"
	:supersede
	t
	:write-outer-list-elements-individually t
	:write-inner-list-elements-individually t)
    (fi (list (format nil "iandd <- read.table(~s, header=TRUE)" "/tmp/data.r")
	      (format nil "iandd.lm <- lm(~a ~a, data=iandd)" 
		      (if (eql nil multiplicative-model)
			  ;;(format nil "~a ~~ ~a" independent-variable (infix-operator '+ dependent-variables))
			  (format nil "~a ~~ . " independent-variable)
			(if (eql t multiplicative-model)
			    (format nil "~a ~~ ~a" independent-variable (infix-operator '* dependent-variables))
			  (format nil "~a ~~ ~a" 
				  independent-variable
				  (infix-operator 
				   '+ 
				   (append
				    dependent-variables
				    (loop for pairs in multiplicative-model collect
					  (infix-operator ":" pairs)))))))
		      (if zero-intercept "-1" ""))
	      "summary(iandd.lm)"
	      "ai<-alias(iandd.lm)"
	      "ai$Complete"
	      "print(\"aliasstart\")"
	      "dimnames(ai$Complete)[[2]]"
              "dimnames(ai$Complete)[[1]]"
	      "if(!is.null(ai$Complete)) for (i in 1:dim(ai$Complete)[1]) {print(dimnames(ai$Complete)[[2]][!is.na(ai$Complete[i,])]); print(ai$Complete[i,!is.na(ai$Complete[i,])])}"
	      "print(\"aliasend\")")
	"/tmp/script.r"
	:supersede
	t
	:write-outer-list-elements-individually t)
    (run-shell-command "R CMD BATCH /tmp/script.r /tmp/script.r.out")
    (multiple-value-bind (coefficients
			  adj-r-square    
			  residuals   
			  f-statistic 
			  residual-se
			  collinearities)
	(parse-r-regression-output-file "/tmp/script.r.out" :compose-collinear-columns compose-collinear-columns)
      (values 
       (mapcar (^ (l) (firstn (if show-t-and-p-values 5 3) l)) coefficients)
       coefficients
       adj-r-square    
       residuals   
       f-statistic 
       residual-se
       ;; would be good to show the sum of squares of residuals
       collinearities
       ))))

(defun r-regression-step (data &optional &key zero-intercept)
  ;; assume first line are column names
  ;; and the first col is the dependent variable, and the remaining columns are dependent variables
  ;; later we can add keyword args for specifying alternates to this
  (let ((independent-variable (car (car data))))
    (fi data
	"/tmp/data.r"
	:supersede
	t
	:write-outer-list-elements-individually t
	:write-inner-list-elements-individually t)
    (fi (list (format nil "iandd <- read.table(~s, header=TRUE)" "/tmp/data.r")
	      (format nil "iandd.lm <- lm(~a ~~ . ~a, data=iandd)" 
		      independent-variable
		      (if zero-intercept "-1" ""))
	      "summary(iandd.lm)"
	      "iandd.lm.step <- step(iandd.lm, scope=list(upper=formula(iandd.lm), lower = ~1), direction=\"backward\", trace=2)"
	      "summary(iandd.lm.step)")
	"/tmp/script.r"
	:supersede
	t
	:write-outer-list-elements-individually t)
    (run-shell-command "R CMD BATCH /tmp/script.r /tmp/script.r.out")
    "output in /tmp/script.r.out"))
       
