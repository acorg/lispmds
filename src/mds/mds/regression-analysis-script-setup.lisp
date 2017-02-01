(in-package user)

;;;----------------------------------------------------------------------
;;;                      subetting the data      
;;;----------------------------------------------------------------------

(defun dependent-and-independents-only  (data)
  (loop for l in data collect
	(cons (nth 2 l)
	      (nthcdr 4 l))))

(defun exclude-zero-genetic-distances (data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (not (zerop (nth 3 l)))
	    collect l)))

(defun exclude-zero-genetic-distances-when-self-self (data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (not (and (zerop (nth 3 l))
                           (and (eql (nth 0 l)
                                     (nth 1 l)))))
	    collect l)))

(defun exclude-zero-genetic-distances-with-non-zero-antigenic-distances (data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (not (and (zerop (nth 3 l))
			   (not (zerop (nth 2 l)))))
	    collect l)))

(defun exclude-self-self-measurements (data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (not (and (eql (nth 0 l)
				(nth 1 l))))
	    collect l)))

(defun exclude-when-genetic-distances-gt-n (n data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (>= n (nth 3 l))
	    collect l)))

(defun exclude-when-genetic-distances-lt-n (n data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (<= n (nth 3 l))
	    collect l)))

(defun exclude-when-genetic-distances-not-n (n data)
  (cons (car data)
	(loop for l in (cdr data) 
	    when (= n (nth 3 l))
	    collect l)))

;;;----------------------------------------------------------------------
;;;             splitting data into train and test sets
;;;----------------------------------------------------------------------

(defun split-into-train-and-test (proportion-test data &optional &key random-number-generator)
  (let (train 
	test)
    (loop for datum in (cdr data) do
	  (if (< (knuth-random random-number-generator) proportion-test)
	      (push datum test)
	    (push datum train)))
    (list (cons (car data) (reverse train))    ;; car of data is the variable names
	  (cons (car data) (reverse test)))))

(defun regression-train-set (train-test-set-proportion differences &optional &key (seed-random 467739585))
  (if seed-random
      (seed-random seed-random))
  (nth 0 (split-into-train-and-test 
          (- 1 train-test-set-proportion)
          differences)))

(defun regression-test-set (train-test-set-proportion differences &optional &key (seed-random 467739585))
  (if seed-random
      (seed-random seed-random))
  (nth 1 (split-into-train-and-test 
          (- 1 train-test-set-proportion)
          differences)))


;;;----------------------------------------------------------------------
;;;                   different zero treatments
;;;----------------------------------------------------------------------

(defun compare-coefficients (coefficients1 coefficients2)
  (loop for name in (my-intersection (nths 0 coefficients1) (nths 0 coefficients2)) collect
	(list name (assoc-value-1 name coefficients1) (assoc-value-1 name coefficients2))))

(defun adjust-parameter-estimates-for-intercept (coefficients)
  (if (not (eql 'intercept (caar coefficients)))
      (error "expected coefficients with intercept line"))
  (let ((intercept (nth 1 (car coefficients))))
    (loop for (name parameter-estimate se) in (cdr coefficients) collect
	  (list name 
		(+ parameter-estimate intercept)
		se))))

(defun with-and-without-zero-intercept-raw-output (data)
  (let ((with-zero-intercept    (multiple-value-list (r-regression data :zero-intercept t)))
	(without-zero-intercept (multiple-value-list (r-regression data :zero-intercept nil))))
    (list with-zero-intercept 
	  without-zero-intercept)))

(defun with-and-without-zero-intercept-process-raw-output (raw-output)
  (let ((with-zero-intercept    (nth 0 raw-output))
	(without-zero-intercept (nth 1 raw-output)))
    (let* ((with-zero-intercept-short-coeffs    (nth 0 with-zero-intercept))
	   (without-zero-intercept-short-coeffs (nth 0 without-zero-intercept))
	   (paired-coeffs (compare-coefficients
			   with-zero-intercept-short-coeffs
			   (adjust-parameter-estimates-for-intercept without-zero-intercept-short-coeffs))))
      (gnuplot-correlation
       (mapcar #'cdr paired-coeffs)
       :x-title "With forced zero intercept"
       :y-title "Without forced zero intercept"
       :element-pointsize 1
       :show-diagonal t
       :diagonal-label (format nil "Adj Rsquares ~d and ~d" (nth 2 with-zero-intercept) (nth 2 without-zero-intercept))
       :legend-position '(right bottom))
      )))


;;;----------------------------------------------------------------------
;;;                      prediction
;;;----------------------------------------------------------------------

(defun sum-squares (l)
  (loop for e in l sum (square e)))


(defun regression-prediction-errors (coefficients data &optional &key (include-intercept t) model)
  ;; this function used to take (dependents-and-independents-only data) now is passed data (so we can know the strain names)
  (let* ((independent-variable-names (nthcdr 4 (car data)))
	 (prediction-output-verbose
	  (loop for (straina strainb actual-dependent-variable hd . independent-variable-values) in (cdr data) collect
		(let* ((predicted-dependent-variable-as-list
			(loop for independent-variable-value in independent-variable-values
			    for independent-variable-name in independent-variable-names 
                            when (not (zerop independent-variable-value))
                            collect
			      (let ((independent-variable-coefficient (assoc-value-1 independent-variable-name coefficients)))
                                hd  ;; to stop compiler complaining
                                (list independent-variable-name
                                      independent-variable-value
                                      independent-variable-coefficient))))
                       (predicted-dependent-variable-as-list-including-intercept
                        (if (and include-intercept (assoc-value-1 'intercept coefficients))
                            (cons (list 'intercept 1 (assoc-value-1 'intercept coefficients))
                                  predicted-dependent-variable-as-list)
                          predicted-dependent-variable-as-list))
		       (predicted-dependent-variable
                        (list straina 
                              strainb
                              actual-dependent-variable
                              (if (member nil (nths 2 predicted-dependent-variable-as-list-including-intercept))
                                  'coeffs-missing
                                (let ((sum-of-terms 
                                       (apply #'+ (map-apply #'* (mapcar #'cdr predicted-dependent-variable-as-list-including-intercept)))))
                                  (if (not (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal))
                                      sum-of-terms
                                    (if (not include-intercept)
                                        sum-of-terms
                                      (+ sum-of-terms 
                                         (* (assoc-value-1 'bias coefficients)
                                            (expt 2.0 (- (/ sum-of-terms (assoc-value-1 'theta coefficients))))))))))
                              predicted-dependent-variable-as-list-including-intercept)))
		  predicted-dependent-variable)))
         (prediction-output (mapcar (^ (l) (list (nth 2 l) (nth 3 l))) prediction-output-verbose))
	 (predictions       (collect (^ (l) (numberp (nth 1 l))) prediction-output))
	 (unable-to-predict (filter  (^ (l) (numberp (nth 1 l))) prediction-output))
	 (prediction-errors (map-apply #'- predictions)))
    (if prediction-errors
        (values
         (av (mapcar #'abs prediction-errors))
         (av prediction-errors)
         (sd prediction-errors)
         (list (sum-squares prediction-errors) (sqrt (/ (sum-squares prediction-errors) (length predictions))))  ;; 1st of these is SSE, 2nd is rms
         (list (apply-min prediction-errors)
               (apply #'median prediction-errors)
               (apply-max prediction-errors))
         (format nil "~d predictions, ~d unable to predict because of collinearities" (length predictions) (length unable-to-predict))
         predictions
         prediction-output-verbose)
      (values
       7777777
       7777777
       7777777
       (list 7777777 7777777 7777777)  ;; 1st of these is SSE, 2nd is rms
       (list 7777777
             7777777
             7777777)
       (format nil "No predictions")
       predictions
       prediction-output-verbose))))
      


;;;----------------------------------------------------------------------
;;;                      unbaising
;;;----------------------------------------------------------------------

(defvar *3-mutations-for-one-2-fold-bias*)
(setq *3-mutations-for-one-2-fold-bias*
  '((0    1.1409837)
    (1    0.8317964)
    (2   0.58999085)
    (3   0.40453047)
    (4    0.2654243)
    (5   0.17128909)
    (6   0.09544886)
    (7  0.052324425)
    (8  0.032216467)
    (9  0.012254533)))

(defun unbias-ag-dist (ag-dist hd unbias-list)
  (if (functionp unbias-list)
      (funcall unbias-list ag-dist hd)
    (- ag-dist (if (assoc-value-1 hd unbias-list)
                   (assoc-value-1 hd unbias-list)
                 0))))

(defun unbias-ag-dists (data unbias-list)
  (cons
   (car data)
   (loop for (name1 name2 ag-dist hd . rest) in (cdr data) collect
	 (append
	  (list name1 
		name2
		(unbias-ag-dist ag-dist hd unbias-list)
		hd)
	  rest))))



#|
;; this was my eyeballing reading off of values from Ken's plot
;; superseded by values below directly from ken
(defun 2d-mle-distance-unbias (ag-dist hd)
  hd ;; to stop the compiler warning
  (cond ((<  ag-dist 0) (error "did not expect negative ag-dist"))
        ((<  ag-dist 0.5) 1.4)
        ((<  ag-dist 1.0) 1.5)
        ((<  ag-dist 1.5) 1.7)
        ((>= ag-dist 1.5) (+ ag-dist 0.3))))
|#

(defun 2d-mle-distance-unbias (ag-dist &optional hd)  ;; optional hd as this is what unbiasing calls with for sequence-based unbiasing
  (let ((observed-2dmle-pairs (groups-of-n
                               2
                               '(0.0000    1.4125  ;; derek added
                                 0.1000    1.4125
                                 0.2000    1.4250
                                 0.3000    1.4250
                                 0.4000    1.4375
                                 0.5000    1.4625
                                 0.6000    1.4750
                                 0.7000    1.5000
                                 0.8000    1.5375
                                 0.9000    1.5750
                                 1.0000    1.6125
                                 1.1000    1.6625
                                 1.2000    1.7125
                                 1.3000    1.7750
                                 1.4000    1.8375
                                 1.5000    1.9125
                                 1.6000    1.9875
                                 1.7000    2.0750
                                 1.8000    2.1625
                                 1.9000    2.2500
                                 2.0000    2.3500
                                 2.1000    2.4375
                                 2.2000    2.5375
                                 2.3000    2.6250
                                 2.4000    2.7250
                                 2.5000    2.8125
                                 2.6000    2.9125
                                 2.7000    3.0000
                                 2.8000    3.1000
                                 2.9000    3.1875
                                 3.0000    3.2875
                                 3.1000    3.3750
                                 3.2000    3.4750
                                 3.3000    3.5625
                                 3.4000    3.6625
                                 3.5000    3.7500
                                 3.6000    3.8500
                                 3.7000    3.9375
                                 3.8000    4.0375
                                 3.9000    4.1375
                                 4.0000    4.2250
                                 4.1000    4.3250
                                 4.2000    4.4250
                                 4.3000    4.5125
                                 4.4000    4.6125
                                 4.5000    4.7125
                                 4.6000    4.8000
                                 4.7000    4.9000
                                 4.8000    5.0000
                                 ;;4.9000    5.0000  ;; derek removed
                                 ;;5.0000    5.0000  ;; derek removed
                                 ))))
    hd ;; to stop the compiler warning
    (if (minusp ag-dist)
        (error "Did not expect negative antigenic distance"))
    (linear-interpolation-from-series ag-dist observed-2dmle-pairs :extrapolate-beyond-series t)))

#|
(gnuplot (fll (loop for i from -1 to 6.79 by 0.01 collect
                    (list i (linear-interpolation-from-series i observed-mle :extrapolate-beyond-series t)))))

(gnuplot (fll (loop for i from -1 to 6.79 by 0.01 collect
                    (list i (2d-mle-distance-unbias i)))))
|#

;;;----------------------------------------------------------------------
;;;                        changes only 
;;;----------------------------------------------------------------------

(defun pairs-containing-mutations (pairs mutations)
  (let ((mutation-locations (loop for mutation in mutations collect (position mutation (car pairs)))))
    (cons (car pairs)
	  (loop for pair in (cdr pairs) 
	      when (not (zerop (apply #'+ (multiple-nth mutation-locations pair))))
	      collect pair))))
		      
(defun remove-all-zero-cols (pairs)
  (let ((mutation-positions
         (my-sort 
          (remove-duplicates 
           (loop for pair in (cdr pairs) append
                 (positions 1 (nthcdr 4 pair)))))))
    (loop for pair in pairs collect
	  (append (firstn 4 pair)
		  (multiple-nth mutation-positions (nthcdr 4 pair))))))

(defun remove-all-cols-with-lt-n-mutations (n data &optional &key (keep-pairs-that-have-cols-removed t))
  (let* ((cols-to-delete (loop for i from 4 below (length (car data))
			     when (< (length (filter #'zerop (nths i (cdr data)))) n)
			     collect i))
	 (cols-to-keep (loop for i below (length (car data))
			   when (not (member i cols-to-delete))
			   collect i)))
    (loop for l in data 
        for i from 0 
        when (or keep-pairs-that-have-cols-removed
                 (zerop i)
                 (not (filter #'zerop (multiple-nth cols-to-delete l))))
        collect
	  (multiple-nth cols-to-keep l))))



(defun remove-subst-from-regression-dataset (subst regression-dataset &optional &key keep-pairs-that-have-cols-removed)
  (let* ((position-of-subst (position subst (car regression-dataset))))
    (cons
     (butnth position-of-subst (car regression-dataset))
     (loop for l in (cdr regression-dataset)
         when (or keep-pairs-that-have-cols-removed
                  (zerop (nth position-of-subst l)))
        collect
	  (butnth position-of-subst l)))))
	  
	  

;;;----------------------------------------------------------------------
;;;                       multicolinearity
;;;----------------------------------------------------------------------

(defun exclude-zero-pairs (ll)
  (filter (^ (l) (and (zerop (nth 0 l)) (zerop (nth 1 l)))) ll))

(defun covariances-removing-zeroes (data)
  (let ((mutation-data-t (nthcdr 4 (apply-transpose (cdr data)))))
    (all-comparisons-square mutation-data-t
			    (^ (a b)
			       (2dp 
				(correlation
				 (exclude-zero-pairs 
				  (transpose a b))))))))

(defun merge-equal-columns (data)
  (let* ((mutation-col-names (nthcdr 4 (car data)))
	 (data-t (apply-transpose (cdr data)))
	 (mutation-data-t (nthcdr 4 data-t))
	 (col-name-subsequent-colinear-col-names-s
	  (loop for (col . remaining-cols) on mutation-data-t
	      for (mutation . remaining-mutations) on mutation-col-names
	      collect (cons mutation
			    (loop for other-col in remaining-cols
				for other-mutation in remaining-mutations
				when (equal col other-col)
				collect other-mutation))))
	 (col-names-to-remove (apply #'append (mapcar #'cdr col-name-subsequent-colinear-col-names-s)))
	 (col-names-to-keep (loop for col-name-subsequent-colinear-col-names in col-name-subsequent-colinear-col-names-s
				when (not (member (car col-name-subsequent-colinear-col-names) col-names-to-remove))
				collect col-name-subsequent-colinear-col-names))
	 (col-positions-to-keep (loop for col-name in (mapcar #'car col-names-to-keep) collect
				      (position col-name mutation-col-names)))
	 (new-col-names (loop for (col-name . colinear-col-names) in col-names-to-keep collect
			      (if colinear-col-names
				  (read-from-string (format nil "C.~a~{.~a~}" col-name colinear-col-names))
				col-name))))
    (cons (append (firstn 4 (car data)) new-col-names)
	  (apply-transpose
	   (multiple-nth (append '(0 1 2 3) (mapcar (^ (x) (+ x 4)) col-positions-to-keep)) data-t)))))



;; ---------------- data generation helper function ------------------------

(defun sequence-differences-alpha-sorted-aa-and-location (s1 s2)
  (loop for aa1 in s1 for aa2 in s2 for i from 1
      when (not (eql aa1 aa2))
      collect (let ((ordered-difference (sort-alpha (list aa1 aa2))))
		(read-from-string (format nil "~a~a~3,'0d" (nth 0 ordered-difference) (nth 1 ordered-difference) i)))))



;;;----------------------------------------------------------------------
;;;                 for regression routine diagnostics
;;;----------------------------------------------------------------------

(defun collinear-name-p (name)
  (> (length (string name)) 5))

(defun plus-minus-determinant-collinear-name-p (name)
  (or (char-member #\+ (string name))    ;; not perfect, as name might have a +, or especially a - in it
      (char-member #\- (string name))))


(defun pp-regression-output (ll &optional &key new-line-test-f additional-line-info-f (stream t) filename (if-exists :error))
  (if filename
      (setq stream (open filename :direction :output :if-exists if-exists)))
  (let ((max-independent-variable-name-length
	 (apply-max (mapcar #'length (mapcar #'string (nths 0 ll))))))
    (loop for ((independent-variable parameter-estimate se) 
	       (next-independent-variable next-parameter-estimate next-se)) on ll do
	  (format stream
		  (format nil "~a~d~a"
			  "~%~"
			  max-independent-variable-name-length
			  "a~7,2f~7,2f")
		  independent-variable parameter-estimate se)
	  (if additional-line-info-f
	      (funcall additional-line-info-f independent-variable parameter-estimate se :stream stream))
	  (if new-line-test-f
	      (if (funcall new-line-test-f 
			   independent-variable parameter-estimate se
			   next-independent-variable next-parameter-estimate next-se)
		  (newline stream)))))
  (if filename
      (close stream))
  ll)


(defun subst-location-location (name &optional &key (regression-unit 'substitutions-and-locations))
  (if (not (>= (length (string name)) 5))
      'no-location
    (case regression-unit 
      (substitutions-and-locations (read-from-string (substring (string name) 2 4)))
      (substitutions-only          0)
      (locations-only              (read-from-string (substring (string name) 1 3)))
      (t                           (error "Unexpected regression-unit ~a" regression-unit)))))

(defun subst-location-subst (name &optional &key (regression-unit 'substitutions-and-locations))
  (case regression-unit 
    (substitutions-and-locations (read-from-string (substring (string name) 0 1)))
    (substitutions-only          (read-from-string (substring (string name) 0 1)))
    (locations-only              'aa)
    (t                           (error "Unexpected regression-unit ~a" regression-unit))))

(defun substitution-info (dataset substitution clusters)
  ;; if substitution is a list, then is interaction term
  (if (not (listp substitution))
      (setq substitution (list substitution)))
  (let* ((positions (loop for subst in substitution collect (position subst (car dataset)))))
    (loop for line in (cdr dataset)
	when (not (collect #'zerop (multiple-nth positions line)))
	collect (append (firstn 4 line)
                        (list (loop for subst in (nthcdr 4 (car dataset))
                                  for e in (nthcdr 4 line)
                                  when (not (zerop e))
                                  collect (list subst e)))
                        (if clusters
                            (list (glue-up
                                   (sort-alpha-substring
                                    (list
                                     (antigen-cluster (nth 0 line) clusters)
                                     (antigen-cluster (nth 1 line) clusters))
                                    2 3))))))))

(defun substitution-num-unique-pairs-annotation (dataset substitution)
  (let* ((position (position substitution (car dataset)))
	 (max-hd (apply-max (nths 3 (cdr dataset))))
	 (subst-info (make-array (inc max-hd)))
	 ;;(subst-info-summary (make-array (inc max-hd)))
	 )
    (loop for line in (cdr dataset)
	when (and position (not (eql 0 (nth position line))))
	do (push (firstn 2 line) (aref subst-info (nth 3 line))))
    (loop for hd from 1 to max-hd collect
	  (let ((pairs (aref subst-info hd)))
	    (list hd 
		  ;; the next is not perfect, should be 5 for nk145 hd=1, but is 4
		  (length (remove-duplicates (reverse pairs) :test #'my-intersection))
		  ;; this makes the same numbers as above
		  ;;(loop for (pair . rest) on (reverse pairs) 
		  ;;      when (loop for r in rest do
		  ;;		 (if (my-intersection pair r)
		  ;;		     (return nil))
		  ;;	       finally (return t))
		  ;;  collect pair)
		  (length pairs)
		  )))))

(defun substitution-clusters-involoved (dataset substitution clusters)
  (let* ((position (position substitution (car dataset)))
	 (max-hd (apply-max (nths 3 (cdr dataset))))
	 (subst-info (make-array (inc max-hd)))
	 (subst-info-clusters (make-array (inc max-hd)))
	 )
    (loop for line in (cdr dataset)
	when (and position (not (eql 0 (nth position line))))
	do (push (firstn 2 line) (aref subst-info (nth 3 line))))
    (loop for hd from 1 to max-hd do
	  (let ((pairs (reverse (aref subst-info hd))))
	    (setf (aref subst-info-clusters hd)
	      (remove-duplicates
	       (loop for (name1 name2) in pairs collect
		     (sort-alpha
		      (list 
		       (antigen-cluster name1 clusters)
		       (antigen-cluster name2 clusters))))
	       :test #'equal))))
    (loop for hd from 1 to max-hd collect
	  (cons hd (aref subst-info-clusters hd)))))

;; similar to above, but not split by HD and with numbers
(defun substitution-all-clusters (dataset substitution clusters)
  (let* ((position (position substitution (car dataset)))
	 (subst-info-clusters (loop for line in (cdr dataset)
                                  when (and position (not (eql 0 (nth position line))))
                                  collect (sort-alpha-substring
                                           (list
                                            (antigen-cluster (nth 0 line) clusters)
                                            (antigen-cluster (nth 1 line) clusters))
                                           2 3)))
         (subst-info-clusters-hist (mapcar (^ (l)
                                              (list (glue-up (nth 0 l)) (nth 1 l)))
                                           (reverse (sort-nth 1 (hist subst-info-clusters :test #'equal))))))
    (values
     subst-info-clusters-hist
     (hist-to-proportion-hist subst-info-clusters-hist :dps 2))))


;;;----------------------------------------------------------------------
;;;                      generating the data
;;;----------------------------------------------------------------------

#|
;;  --------------- SUPERCEDED BELOW WITH NEW SAVE ----------------------
(progn
  (setq t8-save (fi-in "mds/investigations/merge-hi-tables/seq-t8-halved.save"))
  (setq t8-coordss (coordss (starting-coordss-from-save t8-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t8-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2002-10/373-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '((AS138 KN145 zAS138-KN145) (KN145 LR261 zKN145-LR261)))
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag *sequenced-strains*)
			      (member mate *sequenced-strains*)
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 10)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t8-coordss) (nth j t8-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t8-halved-positional-upto-10-mutations-with-interactions.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
|#

#|
(progn
  (setq t9-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod12.save"))
  (setq t9-coordss (coordss (starting-coordss-from-save t9-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t9-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2003-04/dutch-only-for-ms-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '((AS138 KN145 zAS138-KN145) (KN145 LR261 zKN145-LR261)))
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag *sequenced-strains*)
			      (member mate *sequenced-strains*)
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 10)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t9-coordss) (nth j t9-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod12-positional-upto-10-mutations-with-interactions.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
|#


#|
(progn
  (setq t9-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod13.save"))
  (setq t9-coordss (coordss (starting-coordss-from-save t9-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t9-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2003-04/dutch-only-for-ms-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '((AS138 KN145 zAS138-KN145) (KN145 LR261 zKN145-LR261)))
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag *sequenced-strains*)
			      (member mate *sequenced-strains*)
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 10)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t9-coordss) (nth j t9-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-positional-upto-10-mutations-with-interactions.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
|#


#|
;; same as above, oct 2003, no interactions
(progn
  (setq t9-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod13.save"))
  (setq t9-coordss (coordss (starting-coordss-from-save t9-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t9-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2003-04/dutch-only-for-ms-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '())
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag *sequenced-strains*)
			      (member mate *sequenced-strains*)
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 10)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t9-coordss) (nth j t9-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a lisp and csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-and-positions-upto-10-mutations.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
|#

#|
;; same as above, feb 2004, no interactions
(progn
  (setq t9-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod27.save"))
  (setq t9-coordss (coordss (starting-coordss-from-save t9-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t9-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2004-02/dutch-only-and-ag-map-only-for-ms-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '())
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag *sequenced-strains*)
			      (member mate *sequenced-strains*)
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 10)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t9-coordss) (nth j t9-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a lisp and csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-10-mutations.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
|#

#|
;; same as above, also mod27, upto 25 mutations
(progn
  (setq t9-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a-mod27.save"))
  (setq t9-coordss (coordss (starting-coordss-from-save t9-save)))
  (setq hi-antigens (hi-table-antigens (un-asl-hi-table (table-from-save t9-save))))

  (setq name-dna (fi-in-readline "mds/data/all-seq/2004-02/dutch-only-and-ag-map-only-for-ms-dna" :line-process-f #'space-delimited-string-to-list))
  (setq name-pro (mapcar (^ (l) (list (nth 0 l) (explode-symbol (dna-to-pro (string (nth 1 l)))))) name-dna)))

(setq differences
  (let* ((interaction-terms
	  '())
	 (shape-sequence-differences
	  (loop for (ag . rest) on hi-antigens for i from 0 append
		(loop for mate in (cons ag rest) for j from i 
		    when (and (member ag (nths 0 name-pro))
			      (member mate (nths 0 name-pro))
			      (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
				(<= num-mutations 25)))
		    collect
		      (let* ((ag-sequence (assoc-value-1 ag name-pro))
			     (mate-sequence (assoc-value-1 mate name-pro))
			     (sequence-differences (sequence-differences-alpha-sorted-aa-and-location ag-sequence mate-sequence))
			     (interactions 
			      (apply #'append 
				     (loop for (a b id) in interaction-terms collect
					   (if (and (member a sequence-differences)
						    (member b sequence-differences))
					       (list id)
					     nil)))))
			(list ag mate
			      (dps (e-dist (nth i t9-coordss) (nth j t9-coordss)) 6)
			      (length sequence-differences)
			      (append 
			       sequence-differences
			       interactions))))))
	 (all-mutations
	  (sort-alpha (remove-duplicates (apply #'append (nths 4 shape-sequence-differences))))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) all-mutations)
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
	   (append (list aa1 aa2 ag-dist num-mutations)
		   (loop for possible-mutation in all-mutations collect
			 (if (member possible-mutation mutations)
			     1
			   0)))))))

;; write as a lisp and csv file, suitable for reading into excel and spss
(fi
 differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)
(fi
 new-differences
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-ba179-fixed.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)

(fll
 differences
 :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations")
|#


;;;----------------------------------------------------------------------
;;;                      loading the data
;;;----------------------------------------------------------------------

#|
2004 SCIENCE MS UPTO DEC 2004
(progn
  (setq differences
    (fi-in-s 
     ;;"/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t8-halved-positional-upto-10-mutations-with-interactions.lisp"
     ;;"/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-positional-upto-10-mutations-with-interactions.lisp"
     ;;"/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-and-positions-upto-10-mutations.lisp"
     "mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-10-mutations.lisp"
     ;;"/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-ba179-fixed.lisp"
     ))
  (setq train-and-test-differences
    (progn
      (seed-random 467739585)
      (split-into-train-and-test 0.1 differences)))
  (setq train-differences (nth 0 train-and-test-differences))
  (setq test-differences  (nth 1 train-and-test-differences))
  (setq unbiased-train-differences (unbias-ag-dists train-differences *3-mutations-for-one-2-fold-bias*))
  (setq unbiased-test-differences  (unbias-ag-dists test-differences  *3-mutations-for-one-2-fold-bias*)))

(progn
  (mkdir "/home/dsmith/m/investigations/shape-sequence/to-ana")
  (fll unbiased-train-differences :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-10-mutations-unbiased-train")
  (fll unbiased-test-differences :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-10-mutations-unbiased-test"))
|#


;; (fll unbiased-train-differences :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-unbiased-train")
;; 
;;(progn
;;  (fll unbiased-train-differences :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-unbiased-train-ba179-fixed")
;;  (fll train-differences :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-train-ba179-fixed"))

#|
;;  just remove the low ag dist stuff
(setq train-differences-ad-gte-1
  (cons (car train-differences)
	(loop for train-difference in (cdr train-differences) 
	    when (>= (nth 2 train-difference) 1)
	    collect train-difference)))
(setq train-differences-ad-gte-2
  (cons (car train-differences)
	(loop for train-difference in (cdr train-differences) 
	    when (>= (nth 2 train-difference) 2)
	    collect train-difference)))
(setq train-differences-ad-gte-3
  (cons (car train-differences)
	(loop for train-difference in (cdr train-differences) 
	    when (>= (nth 2 train-difference) 3)
	    collect train-difference)))

(fll train-differences-ad-gte-1 :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-ag-gte-2-train")
(fll train-differences-ad-gte-2 :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-ag-gte-2-train")
(fll train-differences-ad-gte-3 :filename "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-25-mutations-ag-gte-3-train")


|#

#|
;;;----------------------------------------------------------------------
;;;               anonymized data for statistician
;;;----------------------------------------------------------------------

(setq unbiased-differences (unbias-ag-dists differences *3-mutations-for-one-2-fold-bias*))

(length
 (setq anonymized-unbiased-differences
       (cons (cons 'Y
		   (loop for i below (- (length (car unbiased-differences)) 4) collect
			 (read-from-string (format nil "X~d" i))))
	     (loop for (ag1 ag2 agdist hd . mutations) in (cdr unbiased-differences) collect
		   (cons agdist mutations)))))

(length
 (fll
  anonymized-unbiased-differences
  :filename "/home/dsmith/mds/investigations/shape-sequence/anonymized-unbiased-differences-t9a-mod13"))

sent to Yingcun, 26dec2003

|#


#|
;;;----------------------------------------------------------------------
;;;                 mutations only, no positions
;;;----------------------------------------------------------------------

;; ------------------- generating -----------------------

(setq differences-mutations-only
  (let* ((mutations (mapcar (^ (s) (read-from-string (substring (string s) 0 1))) (nthcdr 4 (car differences))))
	 (unique-mutations (remove-duplicates mutations))
	 (unique-mutation-positions-s
	  (loop for unique-mutation in unique-mutations collect
		(positions unique-mutation mutations))))
    (cons 
     (append (firstn 4 (car differences)) unique-mutations)
     (loop for dataline in (cdr differences) collect
	   (append (firstn 4 dataline)
		   (loop for unique-mutation-positions in unique-mutation-positions-s collect
			 (loop for unique-mutation-position in unique-mutation-positions sum
			       (nth (+ 4 unique-mutation-position) dataline))))))))

(fi
 differences-mutations-only
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-upto-10-mutations.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)


;; ----------------- reading back in ----------------------

(progn
  (setq differences-mutations-only
    (fi-in-s 
     "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-upto-10-mutations.lisp"))
  (setq train-and-test-differences-mutations-only
    (progn
      (seed-random 467739585)
      (split-into-train-and-test 0.1 differences-mutations-only)))
  (setq train-differences-mutations-only (nth 0 train-and-test-differences-mutations-only))
  (setq test-differences-mutations-only  (nth 1 train-and-test-differences-mutations-only))
  (setq unbiased-train-differences-mutations-only (unbias-ag-dists train-differences-mutations-only *3-mutations-for-one-2-fold-bias*))
  (setq unbiased-test-differences-mutations-only  (unbias-ag-dists test-differences-mutations-only  *3-mutations-for-one-2-fold-bias*)))


;;---------------------------------------- the raw data ------------------------------------------

(setq ten-r
  (r-regression 
   (dependent-and-independents-only 
    (remove-all-zero-cols
      unbiased-train-differences-mutations-only))
   :zero-intercept t))


;; -------------------------- with some of my clean ups to reduce SE ------------------------------

(setq ten-r
  (r-regression 
   (dependent-and-independents-only 
    (remove-all-cols-with-lt-n-mutations 
     5
     (remove-all-zero-cols
      (exclude-zero-genetic-distances
       (exclude-when-genetic-distances-gt-n
	10
	unbiased-train-differences-mutations-only)))))
   :zero-intercept t))


output <A href="../../../projects/dan/mutations-only.html">here</A>


;;;----------------------------------------------------------------------
;;;                 locations only, no mutations
;;;----------------------------------------------------------------------

;; ------------------- generating -----------------------

(setq differences-locations-only
  (let* ((locations (mapcar (^ (s) (read-from-string (format nil "L~a" (substring (string s) 2 4)))) (nthcdr 4 (car differences))))
	 (unique-locations (sort-alpha (remove-duplicates locations)))
	 (unique-location-positions-s
	  (loop for unique-location in unique-locations collect
		(positions unique-location locations))))
    (cons 
     (append (firstn 4 (car differences)) unique-locations)
     (loop for dataline in (cdr differences) collect
	   (append (firstn 4 dataline)
		   (loop for unique-location-positions in unique-location-positions-s collect
			 (loop for unique-location-position in unique-location-positions sum
			       (nth (+ 4 unique-location-position) dataline))))))))

(fi
 differences-locations-only
 "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-upto-10-locations.lisp"
 :error
 nil
 :write-outer-list-elements-individually t)


;; ----------------- reading back in ----------------------

(progn
  (setq differences-locations-only
    (fi-in-s 
     "/home/dsmith/mds/investigations/shape-sequence/antigenic-genetic-t9a-mod13-substitutions-upto-10-locations.lisp"))
  (setq train-and-test-differences-locations-only
    (progn
      (seed-random 467739585)
      (split-into-train-and-test 0.1 differences-locations-only)))
  (setq train-differences-locations-only (nth 0 train-and-test-differences-locations-only))
  (setq test-differences-locations-only  (nth 1 train-and-test-differences-locations-only))
  (setq unbiased-train-differences-locations-only (unbias-ag-dists train-differences-locations-only *3-mutations-for-one-2-fold-bias*))
  (setq unbiased-test-differences-locations-only  (unbias-ag-dists test-differences-locations-only  *3-mutations-for-one-2-fold-bias*)))


;; or directly, without a saved file
(progn
  differences-locations-only
  (setq train-and-test-differences-locations-only
    (progn
      (seed-random 467739585)
      (split-into-train-and-test 0.1 differences-locations-only)))
  (setq train-differences-locations-only (nth 0 train-and-test-differences-locations-only))
  (setq test-differences-locations-only  (nth 1 train-and-test-differences-locations-only))
  (setq unbiased-train-differences-locations-only (unbias-ag-dists train-differences-locations-only *3-mutations-for-one-2-fold-bias*))
  (setq unbiased-test-differences-locations-only  (unbias-ag-dists test-differences-locations-only  *3-mutations-for-one-2-fold-bias*)))

(progn
  (fll unbiased-train-differences-locations-only :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-positions-upto-10-mutations-unbiased-train")
  (fll unbiased-test-differences-locations-only :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-positions-upto-10-mutations-unbiased-test"))



;;;----------------------------------------------------------------------
;;;                 mutations only, no locations
;;;----------------------------------------------------------------------

;; ------------------- generating -----------------------

(setq differences-mutations-only
  (let* ((mutations (mapcar (^ (s) (read-from-string (format nil "~a" (substring (string s) 0 1)))) (nthcdr 4 (car differences))))
	 (unique-mutations (sort-alpha (remove-duplicates mutations)))
	 (unique-mutation-positions-s
	  (loop for unique-mutation in unique-mutations collect
		(positions unique-mutation mutations))))
    (cons 
     (append (firstn 4 (car differences)) unique-mutations)
     (loop for dataline in (cdr differences) collect
	   (append (firstn 4 dataline)
		   (loop for unique-mutation-positions in unique-mutation-positions-s collect
			 (loop for unique-mutation-position in unique-mutation-positions sum
			       (nth (+ 4 unique-mutation-position) dataline))))))))

(progn
  (setq train-and-test-differences-mutatins-only
    (progn
      (seed-random 467739585)
      (split-into-train-and-test 0.1 differences-mutations-only)))
  (setq train-differences-mutatins-only (nth 0 train-and-test-differences-mutatins-only))
  (setq test-differences-mutatins-only  (nth 1 train-and-test-differences-mutatins-only))
  (setq unbiased-train-differences-mutatins-only (unbias-ag-dists train-differences-mutatins-only *3-mutations-for-one-2-fold-bias*))
  (setq unbiased-test-differences-mutatins-only  (unbias-ag-dists test-differences-mutatins-only  *3-mutations-for-one-2-fold-bias*)))

(progn
  (fll unbiased-train-differences-mutatins-only :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-mutations-upto-10-mutations-unbiased-train")
  (fll unbiased-test-differences-mutatins-only :filename "/home/dsmith/m/investigations/shape-sequence/to-ana/antigenic-genetic-t9a-mod27-mutations-upto-10-mutations-unbiased-test"))



;;---------------------------------------- the raw data ------------------------------------------

(setq ten-r
  (r-regression 
   (dependent-and-independents-only 
    (remove-all-zero-cols
      unbiased-train-differences-locations-only))
   :zero-intercept t))


;; -------------------------- with some of my clean ups to reduce SE ------------------------------

(setq ten-r
  (r-regression 
   (dependent-and-independents-only 
    (remove-all-cols-with-lt-n-mutations
     5
     (remove-all-zero-cols
      (exclude-zero-genetic-distances
       (exclude-when-genetic-distances-gt-n
	10
	unbiased-train-differences-locations-only)))))
   :zero-intercept t))

output <A href="../../../projects/dan/locations-only.html">here</A>

|#



;;;----------------------------------------------------------------------
;;;              individual mutations, for comparison
;;;----------------------------------------------------------------------

(defun group-mutations (n-mutation-list)
  (let (mutation-instances  ;; ((kn145 ps123) (1.23 1.83 1.32 1.45))
	(mutation-names (nthcdr 4 (car n-mutation-list))))
    (loop for (strain1 strain2 antigenic-distance hamming-distance . mutations) in (cdr n-mutation-list) do
	  (progn
	    strain1 strain2 hamming-distance  ;; to keep the compiler from complaining
	    (let ((mutations-names (loop for mutation-name in mutation-names
				       for mutation in mutations
				       when (not (zerop mutation))
				       collect mutation-name)))
	      (if (assoc mutations-names mutation-instances :test #'equal)
		  (push-end antigenic-distance (cadr (assoc mutations-names mutation-instances :test #'equal)))
		(push-end (list mutations-names (list antigenic-distance)) mutation-instances)))))
    (my-sort
     (loop for (mutations-names antigenic-distances) in mutation-instances collect
	   (list (sort-alpha mutations-names)
		 (av antigenic-distances)
		 (sd antigenic-distances)
		 (length antigenic-distances)))
     (^ (a b) (string< (string (glue-up (car a)))
		       (string (glue-up (car b))))))))