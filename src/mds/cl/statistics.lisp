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
	 (* y-sd (sqrt (- 1 (square r))))  ;; this really is the sqrt of the average squared error.  
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
	   (2min-x (double min-x)) (2max-x (double max-x)))
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

(defun call-R-on-script (&optional &key 
                                   (script-input-filename  "/tmp/script.r")
                                   (script-output-filename "/tmp/script.r.out"))
  #+:allegro
  (run-shell-command (format nil "R CMD BATCH ~a ~a" script-input-filename script-output-filename))
  #+:openmcl
  (run-program "/sw/bin/R" `("CMD" "BATCH" script-input-filenam script-output-filename) :wait t :output t :error :output)
  )

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
  (call-R-on-script)
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
  (call-R-on-script)
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
		
#||
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
||#		   

(defun parse-alias-output (raw-data)
  (let* ((vectors 
	  (R-pp-vectors-to-list
	   (filter (^ (l) (eql '> (car l)))
		   raw-data)))
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

#||
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
||#




(defun parse-r-regression-output-file (filename &optional &key 
							  (compose-collinear-columns t)
                                                          show-t-and-p-values)
  (let ((raw-data (fi-in-readline filename
                                  :line-process-f 
                                  #'space-delimited-string-to-list-replacing-colon-with-asterix-and-comma-with-space-semicolon-with-space-and-removing-free-periods-unparen-intercept)))
    (unless (assoc 'error raw-data)
      (let* ((f-statistic  (let ((line (assoc 'f-statistic*             raw-data)))
                             (list (nth 1 line) (nth 3 line) (nth 5 line) (nthcdr 8 line))))
             (adj-r-square (nth 5 (assoc 'multiple                 raw-data)))
             (residual-se  (nth 3 (assoc 'residual                 raw-data)))
             (residuals    (if (assoc-position 'residuals*      raw-data)
                               (nth (+ 2 (assoc-position 'residuals*      raw-data)) raw-data)))
             (collinearp   (and compose-collinear-columns
                                (member '(> ai$Complete) raw-data :test #'equal)
                                (not (equal '(NULL) (nth (inc (position '(> ai$Complete) raw-data :test #'equal)) raw-data)))))
             (names-including-collinearity-info
              (if collinearp
                  (alias-output-to-names
                   (parse-alias-output 
                    (let ((a-start (inc (position '([1] "aliasstart") raw-data :test #'equal)))
                          (a-end   (dec (position '([1] "aliasend") raw-data :test #'equal))))
                      (firstn (- a-end a-start) (nthcdr a-start raw-data))
                      )))))
             (c-start (+ (or (assoc-position 'coefficients* raw-data)
                             (assoc-position 'Parameters*   raw-data))
                         2))
             (c-end   (or (assoc-position '---             raw-data)   ;; not present when no variation
                          (assoc-position 'residual             raw-data)))
             (coefficients 
              (let* ((coeffs 
                      ;;the below for the version of R I was using prior to 1.9.1
                      ;;the filter was added as in 1.9.1 collinear columns were printed in the list of coeffs, whereas before they were not
                      ;;(firstn (- c-end c-start) (nthcdr c-start raw-data))
                      (filter (^ (l) (equal '(na na na) (multiple-nth '(1 2 3) l))) (firstn (- c-end c-start) (nthcdr c-start raw-data)))))
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
                        when (not (eql name 'NULL))
                        collect (cons name (cdr coefficient))))
                coefficients))
             (coefficients-with-NA-estimates (collect (^ (l) (equal '(na na na) (multiple-nth '(1 2 3) l))) (firstn (- c-end c-start) (nthcdr c-start raw-data)))))
        (values
         (mapcar (^ (l) (firstn (if show-t-and-p-values 5 3) l)) cooefficients-with-collinear-info-names)
         cooefficients-with-collinear-info-names
         adj-r-square    
         residuals   
         f-statistic 
         residual-se
         coefficients-with-NA-estimates
         )))))



(defun r-regression (data &optional &key 
				    zero-intercept 
				    (compose-collinear-columns t)
				    multiplicative-model   ;; nil means +, t means * (all combinations), 
				                           ;; and lists indicate which are interaction terms eg ((a b) (a b c))
                                                           ;; gives a:b + a:b:c  as extra terms to the additive terms
                                    (model "lm")     ;; alternatives are rlm, lqs
                                    weights-filename
                                    nls-ssmodel0-bias
                                    nls-ssmodel0-theta
                                    nls-ssmodel0-intercept
                                    random-number-generator 
                                    add-gaussian-noise-to-independent-variables   ;; 0.01 is a good amount for 0/1 data
				    show-t-and-p-values
                                    (script-input-filename  "/tmp/script.r")
                                    (script-output-filename "/tmp/script.r.out")
                                    regression-matrix-filename
                                    keep-regression-data-matrix-passed-to-r)
  
  (if (not keep-regression-data-matrix-passed-to-r)
      (setq regression-matrix-filename (format nil "/tmp/data.~9,'0d.r" (krandom 1000000000 *unique-seed-each-restart-rng*))))
  
  ;; assume first line are column names
  ;; and the first col is the independent variable, and the remaining columns are dependent variables
  ;; later we can add keyword args for specifying alternates to this
  (let* ((dependent-variable     (car (car data)))
         (independent-variables  (cdr (car data))))
         
    (with-open-file (out regression-matrix-filename :direction :output :if-exists :supersede)
      (format out "~{~a ~}~%" (cond ((member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal)
                                     (convert-regression-matrix-header-for-nls (car data)))
                                    ((member model '("intercept-per-hd-other-than-highest--x") :test #'equal)
                                     (convert-regression-matrix-header-for-intercept-per-hd data))
                                    (t (car data))))
      (let ((sorted-unique-hds (if (member model '("intercept-per-hd-other-than-highest") :test #'equal)
                                   (sorted-unique-hds-in-regression-matrix-data data)
                                 'not-needed)))
        (loop for row in (cdr data) do
              (format out "~{~a ~}~%" (loop for e in (if add-gaussian-noise-to-independent-variables
                                                         (cons (car row) 
                                                               (mapcar (^ (x) 
                                                                          (if (numberp x)
                                                                              (add-gaussian-noise
                                                                               x
                                                                               add-gaussian-noise-to-independent-variables
                                                                               random-number-generator)
                                                                            x))
                                                                       (cdr row)))
                                                       (if (member model '("intercept-per-hd-other-than-highest--x") :test #'equal)
                                                           (convert-regression-matrix-row-for-intercept-per-hd row sorted-unique-hds)
                                                         row))
                                          collect
                                            (if (typep e 'long-float)
                                                (coerce e 'short-float)
                                              e))))))
    (fi (append
         (list (format nil "library(\"MASS\")")
               (format nil "assignInNamespace(\"all.vars\", function (expr, functions = FALSE,  max.names = 2000, unique = TRUE) .Internal(all.names(expr, functions, max.names, unique)),  ns=.BaseNamespaceEnv)")   ;; for nls
               (format nil "iandd <- read.table(~s, header=TRUE)" regression-matrix-filename)
               (if weights-filename
                   (format nil "weightVector <- read.table(~s, header=TRUE)$WEIGHT" weights-filename)
                 "")
               (if (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal)
                   (nls-ssmodel0-start-vector-from-substitutions independent-variables 
                                                                 :bias nls-ssmodel0-bias
                                                                 :theta nls-ssmodel0-theta
                                                                 :intercept nls-ssmodel0-intercept
                                                                 :random-number-generator random-number-generator)
                 "")
               (format nil "iandd.lm <- ~a(~a ~a, data=iandd~a~a)" 
                       (cond ((member model '("nls-ssmodel0" "nls-ssmodel0i")        :test #'equal) "nls")
                             ((member model '("intercept-per-hd-other-than-highest") :test #'equal) "lm")
                             (t model))
                       (if (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal)
                           (nls-formula-from-variables dependent-variable independent-variables :model model)
                         (if (eql nil multiplicative-model)
                             ;;(format nil "~a ~~ ~a" dependent-variable (infix-operator '+ independent-variables))
                             (format nil "~a ~~ . " dependent-variable)
                           (if (eql t multiplicative-model)
                               (format nil "~a ~~ ~a" dependent-variable (infix-operator '* independent-variables))
                             (format nil "~a ~~ ~a" 
                                     dependent-variable
                                     (infix-operator 
                                      '+ 
                                      (append
                                       independent-variables
                                       (loop for pairs in multiplicative-model collect
                                             (infix-operator ":" pairs))))))))
                       (if zero-intercept "-1" "")
                       (if (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal) ", start=st, trace=T, nls.control(maxiter = 200)" "")
                       (if weights-filename (format nil ", weights=weightVector") ""))
               "summary(iandd.lm)")
         (if (not (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal))
             (list
              "ai<-alias(iandd.lm)"
              "ai$Complete"
              "print(\"aliasstart\")"
              "dimnames(ai$Complete)[[2]]"
              "dimnames(ai$Complete)[[1]]"
              "if(!is.null(ai$Complete)) for (i in 1:dim(ai$Complete)[1]) if(length(ai$Complete[i,ai$Complete[i,]!=0])>0) {print(dimnames(ai$Complete)[[2]][ai$Complete[i,]!=0]); print(ai$Complete[i,ai$Complete[i,]!=0])}"
              "print(\"aliasend\")"
              "print(\"start non-aliased singularities\")"
              "if(!is.null(ai$Complete)) for (i in 1:dim(ai$Complete)[1]) if(length(ai$Complete[i,ai$Complete[i,]!=0])==0) print(dimnames(ai$Complete)[[1]][i])"
              "print(\"end non-aliased singularities\")"
              "anova(iandd.lm)"

              "print(\"R calculation of composite collinear names\")"
              "if (!is.null(ai$Complete))"
              "  for (i in 1:dim(ai$Complete)[2]) {"
              "    parameterEstimateName <- dimnames(ai$Complete)[[2]][i]"
              "    compositeParameterEstimateName <- parameterEstimateName"
              "    collinearCoeffs <- ai$Complete[(ai$Complete[,i]!=0),i]"
              "    if (length(collinearCoeffs)>0) {"
              "      for (j in 1:length(collinearCoeffs)) {"
              "        collinearCoeff <- collinearCoeffs[j]"
              "        absCollinearCoeffEq1 <- ((abs(collinearCoeff) > 0.9999999) && (abs(collinearCoeff) < 1.0000001))"
              "        collinearName <- dimnames(ai$Complete)[[1]][ai$Complete[,i]!=0][j]"
              "        collinearNameCoeffs <- ai$Complete[c(collinearName),]"
              "        collinearOfNumber <- length(collinearNameCoeffs[collinearNameCoeffs!=0])"
              "        compositeParameterEstimateName <- paste(compositeParameterEstimateName,"
              "                                                paste(if (collinearCoeff < 0) \"-\" else \"+\","
              "                                                      if (absCollinearCoeffEq1) \"\" else abs(collinearCoeff),"
              "                                                      if (collinearOfNumber == 1) \"\" else paste(\"OF\", collinearOfNumber, sep = \"\"),"
              "                                                      collinearName,"
              "                                                      sep = \"\"),"
              "                                                sep = \"\")"
              "      }"
              "    }"
              "    print(compositeParameterEstimateName)"
              "  }"
              )))
	script-input-filename
	:supersede
	t
	:write-outer-list-elements-individually t)
    (call-R-on-script 
     :script-input-filename  script-input-filename
     :script-output-filename script-output-filename)
    (if (not keep-regression-data-matrix-passed-to-r) (rm regression-matrix-filename))
    (ignore-errors
     (parse-r-regression-output-file script-output-filename 
                                     :compose-collinear-columns compose-collinear-columns
                                     :show-t-and-p-values       show-t-and-p-values))))


;;;----------------------------------------------------------------------
;;;                      nls regression support
;;;----------------------------------------------------------------------

(defun nls-ssmodel0-start-vector-from-substitutions (substitutions 
                                                     &optional &key
                                                               random-number-generator
                                                               starts
                                                               (bias 1.0)
                                                               (theta 2.0)
                                                               intercept)
  (if (null starts)
      (setq starts (loop for subst in substitutions collect (progn subst (+ 1 (knuth-random random-number-generator))))))
  (let ((beta-starts
         (apply 
          #'string-append
          (infix-operator
           ", "
           (loop for subst in substitutions 
                 for start in starts collect
                 (format nil "~a=~d" subst (coerce start 'single-float)))))))
    (format nil "st <- c(~abias=~d, theta=~d, ~a)" 
            (if intercept (format nil "intercept=~d, " (coerce intercept 'single-float)) "")
            (coerce bias 'single-float)
            (coerce theta 'single-float)
            beta-starts)))

(defun nls-formula-from-variables (dependent-variable independent-variables &optional &key model)
  (let ((beta-with-independent-variable-s (apply
                                           #'string-append
                                           (infix-operator 
                                            " + " 
                                            (loop for independent-variable in independent-variables collect
                                                  (format nil "~a*~aME" independent-variable independent-variable))))))
    (format nil "~a ~~ ~a~a + bias*2^(-(~a)/theta)" 
            dependent-variable
            (cond ((equal model "nls-ssmodel0" )  "")
                  ((equal model "nls-ssmodel0i") (format nil "intercept + "))
                  (t (error "Unexpected model ~a" model)))
            beta-with-independent-variable-s
            beta-with-independent-variable-s)))

  
(defun convert-regression-matrix-header-for-nls (regression-matrix-header)
  (cons (car regression-matrix-header)
        (loop for independent-variable in (cdr regression-matrix-header) collect
              (read-from-string (format nil "~aME" independent-variable)))))



;;;----------------------------------------------------------------------
;;;               intercept per hd regression support
;;;----------------------------------------------------------------------

(defun sorted-unique-hds-in-regression-matrix-data (regression-input-data)
  (my-sort 
   (collect 
    #'numberp 
    (remove-duplicates 
     (mapcar (^ (row) (assoc-value-1 1 (hist (cdr row)))) (cdr regression-input-data))))))

(defun convert-regression-matrix-header-for-intercept-per-hd (regression-input-data)
  (let ((sorted-unique-hds (sorted-unique-hds-in-regression-matrix-data regression-input-data)))
    (append (mapcar (^ (i) (read-from-string (format nil "InterceptWhenHD~d" i))) (butlast sorted-unique-hds)) 
            (car regression-input-data))))

(defun convert-regression-matrix-row-for-intercept-per-hd (regression-input-data-row unique-sorted-hds)
  (cons 
   (car regression-input-data-row)
   (append
    (loop for i in (butlast unique-sorted-hds) collect
          (bool->bit (= i (assoc-value-1 1 (hist (cdr regression-input-data-row))))))
    (cdr regression-input-data-row))))


;;;----------------------------------------------------------------------
;;;                      misc r-regression utils
;;;----------------------------------------------------------------------

(defun collinear-composed-name-p (name)
  (> (length (string name)) 5))


;;;----------------------------------------------------------------------
;;;                            step
;;;----------------------------------------------------------------------

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
    (call-R-on-script)
    "output in /tmp/script.r.out"))


;;;----------------------------------------------------------------------
;;;                          stepAIC
;;;----------------------------------------------------------------------

(defun split-r-stepAIC-output-file-into-stepAIC-and-summary (input-filename 
                                                             &optional &key 
                                                                       stepAIC-output-filename
                                                                       (stepAIC-output-filename-if-exists-action :error)
                                                                       summary-output-filename
                                                                       (summary-output-filename-if-exists-action :error))
  (if (not stepAIC-output-filename)
      (setq stepAIC-output-filename
        (string-append (reverse (substring-after-char #\. (reverse input-filename)))
                       "-stepAIC-component.txt")))
  
  (if (not summary-output-filename)
      (setq summary-output-filename
        (string-append (reverse (substring-after-char #\. (reverse input-filename)))
                       "-summary-component.txt")))

  (let* ((raw-data (fi-in-readline input-filename))

         (poststepAICsummary-start (let ((p (position "> print(\"poststepAICsummarystart\")" raw-data :test #'equal))) (if p (+ p 2) nil)))
         (poststepAICsummary-end   (let ((p (position "> print(\"poststepAICsummaryend\")"   raw-data :test #'equal))) (if p p nil)))
         (poststepAICsummary-strings (if (and poststepAICsummary-start poststepAICsummary-end)
                                         (firstn (- poststepAICsummary-end poststepAICsummary-start) (nthcdr poststepAICsummary-start raw-data))
                                       '("stepAIC summary could not be extracted, did the stepAIC run fail?")))
         
         (stepAIC-start (let ((p (position "> print(\"stepAICstart\")" raw-data :test #'equal))) (if p (+ p 2) nil)))
         (stepAIC-end   (let ((p (position "> print(\"stepAICend\")"   raw-data :test #'equal))) (if p p nil)))
         (stepAIC-strings (if (and stepAIC-start stepAIC-end)
                              (firstn (- stepAIC-end stepAIC-start) (nthcdr stepAIC-start raw-data))
                            '("stepAIC output could not be extracted, did the stepAIC run fail?"))))

    (fi stepAIC-strings
        stepAIC-output-filename
        stepAIC-output-filename-if-exists-action
        t
        :write-outer-list-elements-individually t)
    
    (fi poststepAICsummary-strings
        summary-output-filename
        summary-output-filename-if-exists-action
        t
        :write-outer-list-elements-individually t)
    
    (values
     (and stepAIC-start 
          stepAIC-end
          poststepAICsummary-start 
          poststepAICsummary-end)
     stepAIC-output-filename
     summary-output-filename)))


(defun parse-r-stepAIC-output-file (step-output-directory
                                    regression-source-directory
                                    &optional &key 
                                              (stepAIC-input-filename  
                                               (string-append 
                                                step-output-directory
                                                "/script-output-for-stepAIC.txt"))
                                              (stepAIC-output-filename 
                                               (string-append 
                                                step-output-directory
                                                "/script-output-for-stepAIC-stepAIC-component.txt"))
                                              (stepAIC-output-filename-if-exists-action :error)
                                              (summary-output-filename 
                                               (string-append 
                                                step-output-directory
                                                "/script-output-for-stepAIC-summary-component.txt"))
                                              (summary-output-filename-if-exists-action :error)
                                              (generate-parameter-estimates-for-each-step-p t)
                                              (regression-data-matrix-filename
                                               (string-append 
                                                regression-source-directory
                                                "/regression-data-matrix-passed-to-r-collinears-removed.txt"))
                                              (collinear-set-representatives-filename
                                               (string-append 
                                                regression-source-directory
                                                "/collinear-set-representatives.txt"))
                                              (generate-parameter-estimates-for-each-step-directory 
                                               (string-append 
                                                step-output-directory
                                                "/intermediate-estimates"))
                                              (generate-step-comparison-graphic-p generate-parameter-estimates-for-each-step-p))
  
  (let ((split-parse-success-p
         (split-r-stepAIC-output-file-into-stepAIC-and-summary
          stepAIC-input-filename
          :stepAIC-output-filename                  stepAIC-output-filename
          :stepAIC-output-filename-if-exists-action stepAIC-output-filename-if-exists-action
          :summary-output-filename                  summary-output-filename
          :summary-output-filename-if-exists-action summary-output-filename-if-exists-action)))

    (if split-parse-success-p
        (if generate-parameter-estimates-for-each-step-p
            (let ((parameter-estimate-filenames-for-each-step
                   (generate-parameter-estimates-for-each-step 
                    stepAIC-output-filename
                    regression-data-matrix-filename
                    generate-parameter-estimates-for-each-step-directory)))
              (if generate-step-comparison-graphic-p
                  (routine-regression-comparison 
                   generate-parameter-estimates-for-each-step-directory
                   parameter-estimate-filenames-for-each-step
                   :substs-representing-collinear-terms (fi-in-s collinear-set-representatives-filename)
                   :vertical-no-sort t
                   :pop-up-web-page  nil))
              parameter-estimate-filenames-for-each-step)
          split-parse-success-p))))


(defun generate-parameter-estimates-for-each-step (stepAIC-output-filename regression-data-matrix-filename directory-for-results)

  (if (file-or-directory-exists-p directory-for-results)
      (if (not (directoryp directory-for-results))
	  (error "Directory (~a) specified for output is not a directory, but an existing file." directory-for-results))
    (mkdir directory-for-results))

  (let ((raw-data (fi-in-readline stepAIC-output-filename
                                  :line-process-f 
                                  #'space-delimited-string-to-list-replacing-colon-with-asterix-and-comma-with-space-semicolon-with-space-and-removing-free-periods-unparen-intercept))
        output-formulas
        output-filenames)

    (loop for i from 0 collect
          (let* ((formula-start (let ((p (or (position 'start* (nths 0 raw-data))
                                             (position 'step*  (nths 0 raw-data))))) (if p (+ p 1) nil)))
                 (formula-end   (let ((p (position '(DF SUM OF SQ RSS AIC) raw-data :test #'equal))) (if p p nil)))
                 (formula       (if (and formula-start formula-end)
                                    (char-substs-in-string 
                                     (format nil "~{~s ~}" 
                                             (apply-append (firstn (- formula-end formula-start) (nthcdr formula-start raw-data))))
                                     '((#\* #\:)))
                                  '("formula could not be extracted, did the stepAIC run fail?")))
                 (script-input-filename  (format nil "~a/intermediate-~3,'0d-input.txt" directory-for-results i))
                 (script-output-filename (format nil "~a/intermediate-~3,'0d-output.txt" directory-for-results i)))
            (if (null formula-end)
                (return (values (reverse output-filenames)
                                (reverse output-formulas))))
            (setq raw-data (nthcdr (inc formula-end) raw-data))
            (push script-output-filename output-filenames)
            (push formula                output-formulas)
            (fi (list (format nil "iandd <- read.table(~s, header=TRUE)" regression-data-matrix-filename)
                      (format nil "iandd.lm <- lm(~a, data=iandd)" formula)
                      (format nil "summary(iandd.lm)"))
                script-input-filename
                :supersede
                t
                :write-outer-list-elements-individually t)

            (call-R-on-script 
             :script-input-filename  script-input-filename
             :script-output-filename script-output-filename)))))
  

#||

(parse-r-stepAIC-output-file 
 "/tmp/step-test-29"
 "/home/dsmith/mds/src/mds/mds/investigations/shape-sequence/step/35yr-culled-1-20")

||# 


(defun num-non-zero-entries-in-regression-matrix (regression-data-array i j &optional &key 
                                                                                      (regression-data-length (array-dimension regression-data-array 0)))
  (loop for y from 1 below regression-data-length
      when (and (not (zerop (aref regression-data-array y i)))
                (not (zerop (aref regression-data-array y j))))
      sum 1))

(defun interaction-terms-with-gte-n-observations (regression-data &optional &key
                                                                            (exclude-interaction-terms-with-lt-n-mutations 5))
  ;; look for whether there is any (or >x) data in an interaction term
  (let* ((regression-data-width  (length (nth  0 regression-data)))
         (regression-data-length (length (nths 0 regression-data)))
         (regression-data-array (make-array (list regression-data-length regression-data-width) :initial-contents regression-data))
         all-interaction-terms
         interaction-terms-with-any-non-zero-entries
         interaction-terms-with-gte-n-mutations)
    (loop for i from 1 below (dec regression-data-width) do
          (loop for j from (inc i) below regression-data-width do
                (let ((num-non-zero-entries (num-non-zero-entries-in-regression-matrix regression-data-array i j))
                      (interaction-pair (list (aref regression-data-array 0 i) (aref regression-data-array 0 j))))
                  (push interaction-pair all-interaction-terms)
                  (if (> num-non-zero-entries 0) (push interaction-pair interaction-terms-with-any-non-zero-entries))
                  (if (> num-non-zero-entries (dec exclude-interaction-terms-with-lt-n-mutations)) (push interaction-pair interaction-terms-with-gte-n-mutations)))))
    (values
     (reverse interaction-terms-with-gte-n-mutations)
     (reverse interaction-terms-with-any-non-zero-entries)
     (reverse all-interaction-terms)
     (length interaction-terms-with-gte-n-mutations)
     (length interaction-terms-with-any-non-zero-entries)
     (length all-interaction-terms))))


(defun exclude-interaction-terms-with-gt-x-strain-number (regression-data-matrix
                                                          interaction-terms
                                                          exclude-interaction-terms-with-gt-x-strain-number)
  (loop for interaction-term in interaction-terms
      when (<= (nth 1 
                    (car 
                     (strains-involved-in-subst-info 
                      (mapcar (^ (l) (firstn 4 l)) 
                              (substitution-info 
                               regression-data-matrix
                               interaction-term
                               nil)))))
              exclude-interaction-terms-with-gt-x-strain-number)
      collect interaction-term))

#|
the below gets around error in the above, but without paging back in to know if correct
(defun exclude-interaction-terms-with-gt-x-strain-number (regression-data-matrix
                                                          interaction-terms
                                                          exclude-interaction-terms-with-gt-x-strain-number)
  (loop for interaction-term in interaction-terms
      when (let ((foo (nth 1 
                           (car 
                            (strains-involved-in-subst-info 
                             (mapcar (^ (l) (firstn 4 l)) 
                                     (substitution-info 
                                      regression-data-matrix
                                      interaction-term
                                      nil)))))))
             (if foo
                 (<= foo exclude-interaction-terms-with-gt-x-strain-number)
               nil))
      collect interaction-term))
|#

(defun make-collinear-removed-regression-data-matrix-in-directory (directory)
  (let* ((columns-to-remove-to-avoid-collinearity (nths 0 (nth-value 6 (parse-r-regression-output-file 
                                                                        (format nil "~a/script-output.txt" directory)
                                                                        :compose-collinear-columns nil
                                                                        :show-t-and-p-values       nil))))

         (regression-data-matrix-passed-to-r (fi-in-readline-to-list (format nil "~a/regression-data-matrix-passed-to-r.txt" directory)))
         (positions-of-columns-to-remove (loop for i from 0
                                             for col-name in (car regression-data-matrix-passed-to-r)
                                             when (member col-name columns-to-remove-to-avoid-collinearity)
                                             collect i))
         (regression-data-matrix-passed-to-r-collinears-removed (mapcar (^ (l) (multiple-butnth positions-of-columns-to-remove l)) regression-data-matrix-passed-to-r))
         
         (regression-parameter-estimates (nths 0 (parse-r-regression-output-file 
                                                  (format nil "~a/script-output.txt" directory)
                                                  :compose-collinear-columns t
                                                  :show-t-and-p-values       nil)))
         (collinear-sets                      (remove 'intercept (filter  (^ (e) (= 5 (length (string e)))) regression-parameter-estimates)))
         (collinear-set-representative-substs (mapcar (^ (e) (read-from-string (substring (string e) 0 4))) collinear-sets)))
    
    (fi collinear-sets                      (format nil "~a/collinear-sets.txt" directory) :error nil :write-outer-list-elements-individually t)
    (fi collinear-set-representative-substs (format nil "~a/collinear-set-representatives.txt" directory) :error nil :write-outer-list-elements-individually t)
    (fll regression-data-matrix-passed-to-r-collinears-removed :filename (format nil "~a/regression-data-matrix-passed-to-r-collinears-removed.txt" directory))))



(defun generate-step-formualae (start lower upper substs interactions)
  (let* ((substs-string (let ((substs-string-with-plus-on-end 
                               (format nil "~{~a + ~}" substs)))
                          (substring substs-string-with-plus-on-end 0 (- (length substs-string-with-plus-on-end) 4))))
         (interaction-string (let ((interaction-string-with-plus-on-end 
                                    (format nil "~{~a + ~}" (mapcar (^ (l) (format nil "~a:~a" (nth 0 l) (nth 1 l))) interactions)))) 
                                 (substring interaction-string-with-plus-on-end 0 (- (length interaction-string-with-plus-on-end) 4))))
         (substs-plus-interaction-string (string-append substs-string " + " interaction-string))
           
         (start-string (cond ((stringp start)             start)
                             ((eql start 'original-model) substs-string)
                             (t                           (error "Unknown start option ~a" start))))
         (lower-string (cond ((stringp lower)             lower)
                             ((eql start 'original-model) substs-string)
                             (t                           (error "Unknown lower option ~a" lower))))
         (upper-string (cond ((stringp upper)             upper)
                             ((eql upper 'original-model) substs-string)
                             ((eql upper 'original-model-plus-interaction-terms) substs-plus-interaction-string)
                             (t                           (error "Unknown upper option ~a" upper)))))
    (values
     start-string
     lower-string
     upper-string)))



(defun write-stepAIC-page (directory 
                           &optional &key 
                                     (if-exists-action :error)
                                     (experiment-title "")
                                     regression-source-directory
                                     step-output-directory 
                                     start
                                     lower
                                     upper
                                     exclude-interaction-terms-with-lt-n-mutations
                                     exclude-interaction-terms-with-gt-x-strain-number
                                     k)

  (with-open-file (out (format nil "~a/index.html" directory) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>~a<br><FONT SIZE=-2>(StepAIC automated diagnostics version 0.0)</FONT></CENTER></H1>" experiment-title)
    (newline out)
    (format out "<PRE>")
    (newline out)
    (format out "
<hr>
<h3>Input parameters</h3>
Regression run from which stepAIC started (and in which a collinear-removed data file was made for stepAIC): ~a

Output directory for all stepAIC results: ~a

Source: ~a
Lower:  ~a
Upper:  ~a

Interaction terms that were collinear or had less than ~a mutations and stain number greater than ~a were excluded

Stop criteria: (k=~a)


<hr>
<h3>Results</h3>
<A href=\"script-input-for-collinear-test.txt\">Script input for collinear test on interaction terms</A>
<A href=\"script-output-for-collinear-test.txt\">Script output for collinear test on interaction terms</A>

<A href=\"script-input-for-stepAIC.txt\">Script input for stepAIC</A>
<A href=\"script-output-for-stepAIC.txt\">Script output for stepAIC</A>

Split out from the link above:
  <A href=\"script-output-for-stepAIC-summary-component.txt\">StepAIC final parameter estimates</A>

  <A href=\"script-output-for-stepAIC-stepAIC-component.txt\">StepAIC verbose step output</A> 
  <A href=\"intermediate-estimates\">Directory parameter estimates for intermediate stepAIC results</A> 

  Comparion of intermediate stepAIC results:
    <A href=\"intermediate-estimates/vertical-no-sort.ps\">sorted no-sort ps</A>         
    <A href=\"intermediate-estimates/vertical-no-sort.pdf\">sorted no-sort pdf</A>       
  <IMG src=intermediate-estimates/vertical-no-sort.png alt=intermediate-estimates/vertical-no-sort.png>       
"
            regression-source-directory
            step-output-directory 
            start
            lower
            upper
            exclude-interaction-terms-with-lt-n-mutations
            exclude-interaction-terms-with-gt-x-strain-number
            k
            )
    (newline out)
    (format out "</PRE>")
    (newline out)))    


(defun stepAIC (regression-source-directory
                step-output-directory 
                &optional &key
                          (start 'original-model)
                          (lower "1")
                          (upper 'original-model-plus-interaction-terms)
                          (exclude-interaction-terms-with-lt-n-mutations 5)
                          (exclude-interaction-terms-with-gt-x-strain-number 1)
                          k
                          (experiment-title "")
                          (pop-up-web-page t))

  (if (file-or-directory-exists-p step-output-directory)
      (if (not (directoryp step-output-directory))
	  (error "Directory (~a) specified for output is not a directory, but an existing file." step-output-directory))
    (mkdir step-output-directory))

  (let* ((regression-matrix-filename          (format nil "~a/regression-data-matrix-passed-to-r.txt" regression-source-directory))
         (extended-regression-matrix-filename (format nil "~a/extended-regression-data-matrix.csv"    regression-source-directory)))

    (if (not (pathname-exists-p regression-source-directory))
        (error "regression source directory does not exist"))

    (if (not (pathname-exists-p regression-matrix-filename))
        (error "Regression input matrix not in regression directory -- rerun regression wtih with option :keep-regression-data-matrix-passed-to-r set to t"))
    (if (not (pathname-exists-p extended-regression-matrix-filename))
        (error "Extended regression input matrix not in regression directory -- rerun regression wtih with option :keep-extended-regression-data-matrix set to t"))
    
    (if (not (pathname-exists-p (format nil "~a/regression-data-matrix-passed-to-r-collinears-removed.txt" regression-source-directory)))
        (make-collinear-removed-regression-data-matrix-in-directory regression-source-directory))

    (let* ((script-input-filename-for-collinear-test  (string-append step-output-directory "/script-input-for-collinear-test.txt"))
           (script-output-filename-for-collinear-test (string-append step-output-directory "/script-output-for-collinear-test.txt"))
           
           (script-input-filename-for-step  (string-append step-output-directory "/script-input-for-stepAIC.txt"))
           (script-output-filename-for-step (string-append step-output-directory "/script-output-for-stepAIC.txt"))
           
           (extended-regression-data-matrix (read-csv-file-into-ll extended-regression-matrix-filename))
           (regression-data-matrix          (fi-in-readline-to-list (format nil "~a/regression-data-matrix-passed-to-r-collinears-removed.txt" regression-source-directory)))
           (regression-data-matrix-header   (car regression-data-matrix))

           (substs (cdr regression-data-matrix-header))
           (interaction-terms-with-gte-n-observations 
            (interaction-terms-with-gte-n-observations
             regression-data-matrix
             :exclude-interaction-terms-with-lt-n-mutations exclude-interaction-terms-with-lt-n-mutations))
           (interaction-terms-with-gte-n-observations-and-strain-num-lte-x
            (if exclude-interaction-terms-with-gt-x-strain-number
                (exclude-interaction-terms-with-gt-x-strain-number
                 extended-regression-data-matrix
                 interaction-terms-with-gte-n-observations 
                 exclude-interaction-terms-with-gt-x-strain-number)
              interaction-terms-with-gte-n-observations))
           (first-pass-interaction-terms interaction-terms-with-gte-n-observations-and-strain-num-lte-x))
            
      (multiple-value-bind (start-string lower-string upper-string)
          (generate-step-formualae start lower upper substs first-pass-interaction-terms)
        
        ;; run linear model with upper to determine which are collinear
        start-string  ;; to stop compiler warning
        lower-string  ;; to stop compiler warning
        (fi (list (format nil "library(\"MASS\")")
                  (format nil "iandd <- read.table(~s, header=TRUE)" regression-matrix-filename)
                  (format nil "iandd.lm <- lm(~a ~~ ~a, data=iandd)" (car regression-data-matrix-header) upper-string)
                  (format nil "summary(iandd.lm)")
                  )
            script-input-filename-for-collinear-test
            :supersede
            t
            :write-outer-list-elements-individually t)
        (call-R-on-script 
         :script-input-filename  script-input-filename-for-collinear-test
         :script-output-filename script-output-filename-for-collinear-test)
        (let* ((collinear-test-interaction-terms-to-remove-because-collinear
                (mapcar (^ (term) (mapcar #'read-from-string (explode-string #\* (string term))))
                        (nths 0 (nth-value 6 (parse-r-regression-output-file script-output-filename-for-collinear-test
                                                                             :compose-collinear-columns t
                                                                             :show-t-and-p-values       nil))))))
          
          (multiple-value-bind (start-string lower-string upper-string)
              (generate-step-formualae start lower upper substs (my-set-difference first-pass-interaction-terms
                                                                                   collinear-test-interaction-terms-to-remove-because-collinear
                                                                                   :test #'equal))
            ;; run stepAIC
            (fi (list (format nil "library(\"MASS\")")
                      (format nil "iandd <- read.table(~s, header=TRUE)" (format nil "~a/regression-data-matrix-passed-to-r-collinears-removed.txt" regression-source-directory))
                      (format nil "iandd.lm <- lm(~a ~~ ~a, data=iandd)" (car regression-data-matrix-header) start-string)
                      (format nil "summary(iandd.lm)")
                      (format nil "print(\"stepAICstart\")")
                      (format nil "iandd.lm.step <- stepAIC(iandd.lm, scope = list(lower = ~~ ~a, upper = ~~ ~a)~a)" 
                              lower-string
                              upper-string
                              (if k (format nil ", k=~d" k) ""))
                      (format nil "print(\"stepAICend\")")
                      (format nil "print(\"poststepAICsummarystart\")")
                      (format nil "summary(iandd.lm.step)")
                      (format nil "print(\"poststepAICsummaryend\")")
                      )
                script-input-filename-for-step
                :supersede
                t
                :write-outer-list-elements-individually t)

            (call-R-on-script 
             :script-input-filename  script-input-filename-for-step
             :script-output-filename script-output-filename-for-step)
            
            (parse-r-stepAIC-output-file step-output-directory regression-source-directory)
            
            (write-stepAIC-page
             step-output-directory
             :experiment-title experiment-title
             :regression-source-directory regression-source-directory
             :step-output-directory step-output-directory 
             :start start
             :lower lower
             :upper upper
             :exclude-interaction-terms-with-lt-n-mutations     exclude-interaction-terms-with-lt-n-mutations
             :exclude-interaction-terms-with-gt-x-strain-number exclude-interaction-terms-with-gt-x-strain-number
             :k k)

            (let ((open-command (format nil "open ~a/index.html" step-output-directory)))
              (if pop-up-web-page (run-shell-command open-command))
              `(run-shell-command ,open-command))))))))


#||
Test the stepAIC

(stepAIC "/home/dsmith/mds/src/mds/mds/investigations/shape-sequence/step/35yr-culled-1-20"
         "/tmp/step-test-27"
         :start 'original-model
         :lower 'original-model
         :upper 'original-model-plus-interaction-terms
         :exclude-interaction-terms-with-lt-n-mutations 20)

(stepAIC "/home/dsmith/mds/src/mds/mds/investigations/shape-sequence/step/35yr-culled-1-20"
         "/tmp/step-test-29"
         :start 'original-model
         :lower 'original-model
         :upper 'original-model-plus-interaction-terms
         :exclude-interaction-terms-with-lt-n-mutations 20
         :k 50)

(stepAIC "/home/dsmith/mds/src/mds/mds/investigations/shape-sequence/step/35yr-culled-1-20"
         "/tmp/step-test-41"
         :start 'original-model
         :lower 'original-model
         :upper 'original-model-plus-interaction-terms
         :exclude-interaction-terms-with-lt-n-mutations 20
         :k 50)

;;compare to hand processing in ../step/step-again-35yr-culled.html, and step results ../step/35yr-culled-forward.txt
||#



;;;----------------------------------------------------------------------
;;;              misc regression analysis utilities
;;;----------------------------------------------------------------------

(defun approx-binary-collinear-check-text (dataset subst1 subst2)
  (let* ((subst1-observations (cdr (nths (position subst1 (car dataset)) dataset)))
         (subst2-observations (cdr (nths (position subst2 (car dataset)) dataset)))
         (subst1-subst2-s     (transpose subst1-observations subst2-observations)))
    (let* ((observations                              (length dataset))
           (observations-with-subst1                  (apply #'+ subst1-observations))
           (observations-with-subst2                  (apply #'+ subst2-observations))
           (observations-with-subst1-and-subst2       (length (collect (^ (l) (= 2 (apply #'+ l))) subst1-subst2-s)))
           (observations-with-subst1-or-subst2        (length (collect (^ (l) (not (= 0 (apply #'+ l)))) subst1-subst2-s)))
           (observations-with-subst1-and-not-subst2   (length (collect (^ (l) (equal '(1 0) l)) subst1-subst2-s)))
           (observations-with-subst2-and-not-subst1   (length (collect (^ (l) (equal '(0 1) l)) subst1-subst2-s)))
           (observations-with-subst1-and-not-subst2-pc-of-subst1    (round (%age observations-with-subst1-and-not-subst2 observations-with-subst1)))
           (observations-with-subst2-and-not-subst1-pc-of-subst2    (round (%age observations-with-subst2-and-not-subst1 observations-with-subst2)))
           (observations-with-subst1-and-subst2-pc-subst1-or-subst2 (round (%age observations-with-subst1-and-subst2 observations-with-subst1-or-subst2)))
           )
      (format nil "
Observations with ~a and not ~a ~5d  (~3d% of ~a)
Observations with ~a and not ~a ~5d  (~3d% of ~a)
Observations with ~a and     ~a ~5d  (~3d% of ~a or ~a)
Observations with ~a or      ~a ~5d
Observations with ~a               ~5d
Observations with ~a               ~5d
Observations in dataset               ~5d
"
              subst1 subst2 observations-with-subst1-and-not-subst2  observations-with-subst1-and-not-subst2-pc-of-subst1    subst1
              subst2 subst1 observations-with-subst2-and-not-subst1  observations-with-subst2-and-not-subst1-pc-of-subst2    subst2
              subst1 subst2 observations-with-subst1-and-subst2      observations-with-subst1-and-subst2-pc-subst1-or-subst2 subst1 subst2
              subst1 subst2 observations-with-subst1-or-subst2 
              subst1        observations-with-subst1
              subst2        observations-with-subst2
              observations
              ))))
