(in-package user)


;;;----------------------------------------------------------------------
;;;                generating data for regression
;;;----------------------------------------------------------------------

;; ----------------- by location and substitution -----------------------

(defun regression-renumber-mutations (all-mutations position-renumbering-list)
  (loop for aannn in all-mutations collect
        (read-from-string
         (format nil "~a~3,'0d"
                 (substring (string aannn) 0 1)
                 (nth (dec (string->number (substring (string aannn) 2 4))) position-renumbering-list)))))
;;(regression-renumber-mutations '(ab001 cd002) '(3 4))
;;(AB003 CD004)

(defun make-shape-sequence-regression-data (antigenic-save name-dna-pairs
					    &optional &key
						      (upto-n-mutations 10)
						      interaction-terms
                                                      lists-to-compare
                                                      position-renumbering-list)
  
  (setq name-dna-pairs
    (loop for (name sequence) in name-dna-pairs 
        when (let ((sequence-complete (not (string-member "-" (string sequence)))))
               (if (not sequence-complete)
                   (format t "~%  Excluding sequence for ~a as it contains at least one '-'  (sequence ~a)" name sequence))
               sequence-complete)
        collect (list name sequence)))
  (let* ((pro-data-p  (set-difference (remove-duplicates (explode-symbol (nth 1 (car name-dna-pairs)))) '(a c g t x)))
	 (name-pro    (mapcar 
		       (^ (l) 
			  (list (nth 0 l) 
				(explode-symbol 
				 (funcall
				  (if pro-data-p
				      #'identity
				    #'dna-to-pro) 
				  (string (nth 1 l))))))
		       name-dna-pairs))
	 (sequenced-strains (nths 0 name-pro))
         (hi-antigens-all   (hi-table-antigens-unasl-table-from-save-non-expanding-hack antigenic-save))
	 (coordss       (firstn (length hi-antigens-all) (coordss (starting-coordss-from-save antigenic-save))))
         (name-coords-s (loop for hi-antigen in hi-antigens-all
                            for coords in coordss
                            when (member hi-antigen sequenced-strains)
                            collect (list hi-antigen coords)))
         (hi-antigens (nths 0 name-coords-s))
         (pairs (if lists-to-compare
                    (let ((list-1 (my-intersection (nth 0 lists-to-compare) hi-antigens))
                          (list-2 (my-intersection (nth 1 lists-to-compare) hi-antigens)))
                      (append
                       (loop for ag in (append list-1 list-2) collect
                             (list ag ag))
                       (loop for ag in list-1 append
                             (loop for mate in list-2 collect
                                   (list ag mate)))))
                  (loop for (ag . rest) on hi-antigens append
                        (loop for mate in (cons ag rest) collect
                              (list ag mate)))))
	 (shape-sequence-differences
	  (loop for (ag mate) in pairs
              when (and (member ag sequenced-strains)
                        (member mate sequenced-strains)
                        (or (null upto-n-mutations)
                            (let ((num-mutations (hd (assoc-value-1 ag name-pro) (assoc-value-1 mate name-pro))))
                              (<= num-mutations upto-n-mutations))))
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
                        (dps (e-dist (assoc-value-1 ag name-coords-s) (assoc-value-1 mate name-coords-s)) 6)
                        (length sequence-differences)
                        (append 
                         sequence-differences
                         interactions)))))
	 (all-mutations (sort-alpha 
                         ;; (remove-duplicates (apply-append (nths 4 shape-sequence-differences))) replaced by below for efficiency
                         (let* ((mutations-s (nths 4 shape-sequence-differences))
                                (unique-mutations-so-far (car mutations-s)))
                           (loop for mutations in (cdr mutations-s) do
                                 (setq unique-mutations-so-far (my-union unique-mutations-so-far mutations)))
                           (if (not (equal (sort-alpha unique-mutations-so-far)
                                           (sort-alpha (remove-duplicates unique-mutations-so-far))))
                               (error "unexpected condition"))
                           unique-mutations-so-far)
                         ))
          (all-mutations-renumbered
            (if position-renumbering-list
                (regression-renumber-mutations all-mutations position-renumbering-list))))
    (cons
     (append (list 'ag1 'ag2 'agdist 'hd) (if position-renumbering-list all-mutations-renumbered all-mutations))
     (loop for (aa1 aa2 ag-dist num-mutations mutations) in (sort-nth 3 (sort-nth 2 shape-sequence-differences)) collect
           (append (list aa1 aa2 ag-dist num-mutations)
                   (loop for possible-mutation in all-mutations collect
                         (if (member possible-mutation mutations)
                             1
                           0)))))))

(defun make-shape-sequence-regression-data-filenames (antigenic-save-filename
						      name-sequence-pairs-filename
						      differences-output-filename
						      &optional &key
								(if-exists-action :error)
								(upto-n-mutations 10)
								interaction-terms-filename
                                                                lists-to-compare
                                                                position-renumbering-list)
  
  ;; write as csv so it can be read into excel
  (csvll
   (make-shape-sequence-regression-data
    (fi-in antigenic-save-filename)
    (if (fas-file-p name-sequence-pairs-filename)
	(read-fas-format name-sequence-pairs-filename)
      (fi-in-readline name-sequence-pairs-filename
		      :line-process-f #'space-delimited-string-to-list))
    :upto-n-mutations  upto-n-mutations
    :interaction-terms (if (or (null interaction-terms-filename)
                               (equal "" interaction-terms-filename))
                           nil 
                         (fi-in interaction-terms-filename))
    :lists-to-compare  lists-to-compare
    :position-renumbering-list position-renumbering-list)
   :filename differences-output-filename
   :if-exists if-exists-action))
  


;; -------------------- by location only -----------------------

(defun postprocess-mutation-location-shape-sequence-regression-data-to-mutation-only-data (differences)
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

(defun postprocess-mutation-location-shape-sequence-regression-data-to-mutation-only-data-by-filename (mutation-location-filename output-filename)
  (csvll
   (postprocess-mutation-location-shape-sequence-regression-data-to-mutation-only-data
    (read-csv-file-into-ll mutation-location-filename))
   :filename output-filename))

;; -------------------- by substitution only -----------------------

(defun postprocess-mutation-location-shape-sequence-regression-data-to-location-only-data (differences)
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

(defun postprocess-mutation-location-shape-sequence-regression-data-to-location-only-data-by-filename (mutation-location-filename output-filename)
  (csvll
   (postprocess-mutation-location-shape-sequence-regression-data-to-location-only-data
    (read-csv-file-into-ll mutation-location-filename))
   :filename output-filename))


;;;----------------------------------------------------------------------
;;;            expainding the dataset by cluster or strain
;;;----------------------------------------------------------------------

(defun expand-subst-by-clusterpair-strain-or-mutations (subst mutation-dataset regression-run-directory &optional &key 
                                                                                                                  (starting-location-for-expansion 900)
                                                                                                                  cluster-pairs
                                                                                                                  strains
                                                                                                                  mutations)
  ;; put the two together, even though both together not programmed because they could go together, and because each individually shares most of the code
  (if (and strains cluster-pairs mutations)
      (error "Supply only strain, cluster pairs, or mutations, more than one ould make sense, but not yet programmed"))
  
  (if (not (or (null strains) (= 1 (length strains))))
      (error "Only programmed for one strain right now (as not sure what to do when more than one strain is in pair)"))

  (if (not (or (null mutations) (= 1 (length mutations))))
      (error "Only programmed for one mutation right now"))

  (if (not (pathname-exists-p (format nil "~a/substs-details/~a.txt" regression-run-directory subst)))
      mutation-dataset
    (let* ((string-subst (string subst))
           (postion-of-subst-in-mutations (position subst (car mutation-dataset)))
           (num-extra-columns (+ (length strains) (length cluster-pairs) (length mutations)))  ;; could be * not + if we allow more than one, but we don't, this + is short hand
           (strains-clusterPair-mutations-s-for-subst    ;; in format (((strain-a strain-b) cluster-cluster mutations) ((strain-a strain-b) cluster-cluster mutations) ((strain-a strain-b) cluster-cluster mutations) ...)
            (mapcar (^ (l) (list 
                            (list (nth 0 l) (nth 1 l))
                            (nth 5 l)
                            (nths 0 (nth 4 l))))
                    (fi-in-readline-to-list (format nil "~a/substs-details/~a.txt" regression-run-directory subst)))))
      (cons 
       (append (car mutation-dataset) 
               (loop for location from starting-location-for-expansion below (+ starting-location-for-expansion num-extra-columns) collect
                     (read-from-string (format nil "~a~d" (substring string-subst 0 1) location))))
       (let ((line-suffix-zeros (zero-base num-extra-columns)))
         (loop for line in (cdr mutation-dataset) collect
               (let ((matrix-entry (nth postion-of-subst-in-mutations line)))
                 (if (= 0 matrix-entry)
                     (append line line-suffix-zeros)
                   (if (= 1 matrix-entry)
                       ;; in the cond below, use the subst-details data to find the clusterPair-s for each line
                       ;;   for strains we can just look at the start of the line
                       ;;   for mutations, we could look for that mutation on the line, but we can also look in the strains-clusterPair-mutations-s-for-subst, we do the latter as easy
                       (let ((position (cond (cluster-pairs (position (assoc-value-1 (firstn 2 line) strains-clusterPair-mutations-s-for-subst :test #'equal) cluster-pairs))
                                             (strains       (if (member (nth 0 strains) (firstn 2 line)) 0 nil))  ;; can do this way because we only allow one strain
                                             (mutations     (if (member (nth 0 mutations) (assoc-value-2 (firstn 2 line) strains-clusterPair-mutations-s-for-subst :test #'equal)) 0 nil))
                                             (t nil))))
                         (if position
                             (append (replace-nth postion-of-subst-in-mutations 0 line)
                                     (replace-nth position 1 line-suffix-zeros))
                           (append line 
                                   line-suffix-zeros)))
                     (error "~%Expected only 0 or 1 entries in mutation-dataset (as working currently with subst-location datasets) but found ~a" matrix-entry))))))))))



;;;----------------------------------------------------------------------
;;;               utility for intercept-per-hd model
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;               intercept per hd regression support
;;;----------------------------------------------------------------------

(defun sorted-unique-hds-in-differences-matrix-data (differences-input-data)
  (my-sort 
   (collect 
    #'numberp 
    (remove-duplicates 
     (mapcar (^ (row) (assoc-value-1 1 (hist (nthcdr 4 row)))) (cdr differences-input-data))))))

(defun convert-differences-matrix-header-for-intercept-per-hd (differences-input-data)
  (let ((sorted-unique-hds (sorted-unique-hds-in-differences-matrix-data differences-input-data)))
    (append (firstn 4 (car differences-input-data))
            (mapcar (^ (i) (read-from-string (format nil "InterceptWhenHD~d" i))) (butlast sorted-unique-hds)) 
            (nthcdr 4 (car differences-input-data)))))

(defun convert-differences-matrix-row-for-intercept-per-hd (differences-input-data-row unique-sorted-hds)
  (append
   (firstn 4 differences-input-data-row)
   (let ((hd (assoc-value-1 1 (hist (nthcdr 4 differences-input-data-row)))))
     (loop for i in (butlast unique-sorted-hds) collect (bool->bit (= i hd))))
   (nthcdr 4 differences-input-data-row)))

(defun convert-differences-matrix-for-intercept-per-hd (differences)
  (cons
   (convert-differences-matrix-header-for-intercept-per-hd differences)
   (let ((sorted-unique-hds (sorted-unique-hds-in-differences-matrix-data differences)))
     (loop for row in (cdr differences) collect
           (convert-differences-matrix-row-for-intercept-per-hd row sorted-unique-hds)))))



;;;----------------------------------------------------------------------
;;;                running regression and diagnoses
;;;----------------------------------------------------------------------

(defun regression-routine-diagnostics (differences-or-differences-filename
				       directory
				       &optional &key
                                                 (experiment-title "")
						 (remove-all-cols-with-lt-n-mutations 5)
						 (exclude-when-genetic-distances-gt-n 10)
						 exclude-when-genetic-distances-lt-n
                                                 exclude-when-genetic-distances-not-n
						 (low-se-threshold                    0.35)
						 clusters
						 (regression-run 'not-passed)
						 (train-test-set-proportion 1.0) ;; 1.0 indicates no split
                                                 (train-test-random-seed 467739585)
                                                 (train-test-random-number-generator 9)
						 unbias-alist
						 (exclude-zero-genetic-distances 'not-passed)  ;; will be default, but processing below
						 (exclude-zero-genetic-distances-when-self-self 'not-passed)
                                                 data-filter-function
						 (zero-intercept t)
                                                 add-gaussian-noise-to-independent-variables 
                                                 (model "lm")
                                                 random-number-seed
                                                 random-number-generator   ;; note train-test split is on a different generator
                                                 nls-ssmodel0-bias  
                                                 nls-ssmodel0-theta
                                                 nls-ssmodel0-intercept                       ;; nls-ssmodel0 has no intercept nls-ssmodel0i does
                                                 add-gaussian-noise-to-nls-ssmodel0-bias  
                                                 add-gaussian-noise-to-nls-ssmodel0-theta
                                                 add-gaussian-noise-to-nls-ssmodel0-intercept ;; nls-ssmodel0 has no intercept nls-ssmodel0i does
                                                 (regression-random-iteration-limit 1)
                                                 (regression-unit 'substitutions-and-locations)   ;; can also be substitutions-only and locations-only
                                                 (write-substs-details t)
                                                 (generate-graphics t)
                                                 (remove-strain-1p0-substs-from-some-figures t)
                                                 inverse-weight-blob-error
                                                 inverse-weight-blob-error-save
                                                 (pop-up-web-page t)
                                                 zip-results-dir
                                                 keep-regression-data-matrix-passed-to-r
                                                 keep-extended-regression-data-matrix     ;; in addition to the data passed to r, also includes the strain names and hamming distance
                                                 )

  (if random-number-seed
      (if random-number-generator
          (seed-random random-number-seed random-number-generator)
        (seed-random random-number-seed)))

  (if (and nls-ssmodel0-intercept
           (not (equal model "nls-ssmodel0i")))
      (error "nls-ssmodel0-intercept specified but model ~a does not have an nls intercept" model))
  
  (if (and add-gaussian-noise-to-nls-ssmodel0-intercept
           (not (equal model "nls-ssmodel0i")))
      (error "add-gaussian-noise-to-nls-ssmodel0-intercept specified but model ~a does not have an nls intercept" model))

  (if (and exclude-when-genetic-distances-gt-n
           exclude-when-genetic-distances-not-n)
      (error "Only one of exclude-when-genetic-distances-gt-n or exclude-when-genetic-distances-not-n can have a non-nil value, but both do"))

  (if (and pop-up-web-page zip-results-dir)
      (error "if you zip the results dir, then popping up the web page will not work, set pop-up-web-page to nil"))

  ;; the below complexisty for zero-genetic-distance to keep old default of exclude-zero-genetic-distances, 
  ;; but allow new option of exclude-zero-genetic-distances-when-self-self
  (if (and (equal exclude-zero-genetic-distances                t)
           (equal exclude-zero-genetic-distances-when-self-self t))
      (error "Set just one, not both, of exclude-zero-genetic-distances and exclude-zero-genetic-distances-when-self-self"))
  (if (equal exclude-zero-genetic-distances                'not-passed) (setq exclude-zero-genetic-distances t))
  (if (equal exclude-zero-genetic-distances-when-self-self 'not-passed) (setq exclude-zero-genetic-distances-when-self-self nil))
    
  (if inverse-weight-blob-error
      (if (not (numberp inverse-weight-blob-error))
          (error ":inverse-weight-blob-error must be a number, 0.5 is usual default for HI flu data")
        (if (not inverse-weight-blob-error-save)
            (error "Must supply a save, with blobs, when turning on :inverse-weight-blob-error")
          (if (not (constant-stress-radial-data-from-save inverse-weight-blob-error-save))
              (error "inverse-weight-blob-error-save must contain constant-force-locii information")))))

  (setq unbias-alist
    (cond ((and (stringp unbias-alist) (equal "default" (string-downcase unbias-alist))) *3-mutations-for-one-2-fold-bias*)
	  ((equal "" unbias-alist) nil)
	  ((stringp unbias-alist) (fi-in unbias-alist))
	  (t unbias-alist)))

  (setq clusters
    (cond ((equal "" clusters) nil)
	  ((stringp clusters) (fi-in clusters))
	  (t clusters)))

  (if (file-or-directory-exists-p directory)
      (if (not (directoryp directory))
	  (error "Directory (~a) specified for output is not a directory, but an existing file." directory))
    (mkdir directory))

  (let* ((differences (if (stringp differences-or-differences-filename)
                          (read-csv-file-into-ll differences-or-differences-filename :simple-csv t)
                        differences-or-differences-filename))
         (original-full-differences differences)
         test-differences
         differences-before-unbiasing
         test-differences-before-unbiasing
         (weights-filename (string-append directory "/weights.txt")))
    
    (if (not (eql 1.0 train-test-set-proportion))
        (let ((train-test-split (progn
                                  (seed-random train-test-random-seed train-test-random-number-generator)
                                  (split-into-train-and-test (- 1 train-test-set-proportion) differences
                                                             :random-number-generator train-test-random-number-generator))))
          (setq differences      (nth 0 train-test-split))
          (setq test-differences (nth 1 train-test-split))))
    
    (if data-filter-function
        (progn
          (setq differences      (funcall data-filter-function differences))
          (setq test-differences (funcall data-filter-function test-differences))))
    
    (if exclude-when-genetic-distances-gt-n 
        (progn
          (setq differences      (exclude-when-genetic-distances-gt-n exclude-when-genetic-distances-gt-n differences))
          (setq test-differences (exclude-when-genetic-distances-gt-n exclude-when-genetic-distances-gt-n test-differences))))
    
    (if exclude-when-genetic-distances-lt-n 
        (progn
          (setq differences      (exclude-when-genetic-distances-lt-n exclude-when-genetic-distances-lt-n differences))
          (setq test-differences (exclude-when-genetic-distances-lt-n exclude-when-genetic-distances-lt-n test-differences))))
    
    (if exclude-when-genetic-distances-not-n 
        (progn
          (setq differences      (exclude-when-genetic-distances-not-n exclude-when-genetic-distances-not-n differences))
          (setq test-differences (exclude-when-genetic-distances-not-n exclude-when-genetic-distances-not-n test-differences))))
    
    (if exclude-zero-genetic-distances
        (progn
          (setq differences      (exclude-zero-genetic-distances differences))
          (setq test-differences (exclude-zero-genetic-distances test-differences))))
    
    (if exclude-zero-genetic-distances-when-self-self
        (progn
          (setq differences      (exclude-zero-genetic-distances-when-self-self differences))
          (setq test-differences (exclude-zero-genetic-distances-when-self-self test-differences))))
    
    (if remove-all-cols-with-lt-n-mutations 
        (progn
          (setq differences      (remove-all-cols-with-lt-n-mutations remove-all-cols-with-lt-n-mutations differences 
                                                                      :keep-pairs-that-have-cols-removed nil))
          (setq test-differences (remove-all-cols-with-lt-n-mutations remove-all-cols-with-lt-n-mutations test-differences
                                                                      :keep-pairs-that-have-cols-removed nil))))
    (if unbias-alist
        (progn
          (setq differences-before-unbiasing      differences)
          (setq test-differences-before-unbiasing test-differences)
          (setq differences      (unbias-ag-dists differences      unbias-alist))
          (setq test-differences (unbias-ag-dists test-differences unbias-alist)))
      (progn  
        (setq differences-before-unbiasing      differences)
        (setq test-differences-before-unbiasing test-differences)
        ))

    (setq differences      (remove-all-zero-cols differences     )
          test-differences (remove-all-zero-cols test-differences))

    (if (member model '("intercept-per-hd-other-than-highest") :test #'equal)
        (setq differences      (convert-differences-matrix-for-intercept-per-hd differences)
              test-differences (convert-differences-matrix-for-intercept-per-hd test-differences)))

    (if keep-extended-regression-data-matrix
        (progn
          (csvll 
           differences
           :filename (string-append directory "/extended-regression-data-matrix.csv"))
          (if (= 1.0 train-test-set-proportion)
              (with-open-file (out (string-append directory "/extended-test-regression-data-matrix.csv") :direction :output)
                (format out "Extended-test-regression-data-matrix-was-not-saved-because-the-train-proportion-was-0.0"))
            (csvll 
             test-differences
             :filename (string-append directory "/extended-test-regression-data-matrix.csv")))))

    (if inverse-weight-blob-error
        (let ((blob-error-data
               (nth-value 2 (calc-inverse-weight-blob-error-vector inverse-weight-blob-error-save differences :blob-error inverse-weight-blob-error))))
          (fll (cons
                '(Strain-a strain-b error-a error-b rms-error weight)
                blob-error-data)
               :filename weights-filename))
      (with-open-file (out weights-filename :direction :output)
        (format out "Ran-unweighted-regression")))

    (let (regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
          was-error)

      (when (eql regression-run 'not-passed)
        (setq regression-run

          (loop for iteration below regression-random-iteration-limit do
                (let ((return-value
                       (r-regression 
                        (dependent-and-independents-only 
                         differences)
                        :zero-intercept zero-intercept 
                        :add-gaussian-noise-to-independent-variables add-gaussian-noise-to-independent-variables 
                        :model model
                        :weights-filename (if inverse-weight-blob-error weights-filename)
                        :random-number-generator random-number-generator   ;; used for gaussian noise, and nls start parameters (if needed)
                        :nls-ssmodel0-bias  (if add-gaussian-noise-to-nls-ssmodel0-bias
                                                (add-gaussian-noise nls-ssmodel0-bias add-gaussian-noise-to-nls-ssmodel0-bias)
                                              nls-ssmodel0-bias)
                        :nls-ssmodel0-theta (if add-gaussian-noise-to-nls-ssmodel0-theta
                                                (add-gaussian-noise nls-ssmodel0-theta add-gaussian-noise-to-nls-ssmodel0-theta)
                                              nls-ssmodel0-theta)
                        :nls-ssmodel0-intercept (if add-gaussian-noise-to-nls-ssmodel0-intercept
                                                    (add-gaussian-noise nls-ssmodel0-intercept add-gaussian-noise-to-nls-ssmodel0-intercept)
                                                  nls-ssmodel0-intercept)
                        :script-input-filename  (string-append directory "/script-input.txt")
                        :script-output-filename (string-append directory "/script-output.txt")
                        :regression-matrix-filename (if keep-regression-data-matrix-passed-to-r (string-append directory "/regression-data-matrix-passed-to-r.txt"))
                        :keep-regression-data-matrix-passed-to-r keep-regression-data-matrix-passed-to-r)))
                  (if return-value
                      (progn
                        (if (> regression-random-iteration-limit 1)
                            (format t "~%Regression iteration ~2d succeeded." iteration))
                        (return return-value))
                    (if (> regression-random-iteration-limit 1)
                        (format t "~%Regression iteration ~2d failed, ~a" 
                                iteration 
                                (if (= iteration (dec regression-random-iteration-limit))
                                    "giving up, reached max-num-tries."
                                  "doing another.")))))))

        (if (null regression-run) 
            (setq was-error t)
          ))
      
      (if (null regression-run) 
          (if was-error
              (format t "~%ERROR: R Regression resulted in an error.  See output file.")
            (format t "~%ERROR: Null regression-run was passed."))
        (progn
          (pp-regression-output
           regression-run
           :filename (string-append directory "/raw-regression.txt"))

          ;; --------------------- add an informative comment when the regression data matrix was not saved -------------------------
          (if (not keep-regression-data-matrix-passed-to-r)
              (with-open-file (out (string-append directory "/regression-data-matrix-passed-to-r.txt") :direction :output)
                (format out "Regression-data-matrix-was-not-saved")))

          (if (not keep-extended-regression-data-matrix)
              (progn
                (with-open-file (out (string-append directory "/extended-regression-data-matrix.csv") :direction :output)
                  (format out "Extended-regression-data-matrix-was-not-saved"))
                (with-open-file (out (string-append directory "/extended-test-regression-data-matrix.csv") :direction :output)
                  (format out "Extended-test-regression-data-matrix-was-not-saved"))))

          ;; ---------------------------- low se ---------------------------------
          (pp-regression-output 
           (collect (^ (l) (< (nth 2 l) low-se-threshold)) regression-run)
           :filename (string-append directory "/low-se.txt"))

          ;; --------------------- low se, non-collinear -------------------------
          (pp-regression-output
           (setq regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
             (filter (^ (l) (collinear-name-p (car l)))
                     (collect (^ (l) (< (nth 2 l) low-se-threshold)) regression-run)))
           :filename (string-append directory "/low-se-non-collinear.txt"))

          ;; ----------------- low se, non-collinear, >1 strain ------------------
          
          ;; Derivative of above, but is furhter down in this function, after calc all-substs-info
          

          ;; ------------------ sort by greatest ag effect -----------------------
          ;; simple
          (pp-regression-output
           (reverse
            (sort-nth 
             1
             regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear))
           :filename (string-append directory "/low-se-non-collinear-sort-by-ag-effect-simple.txt"))

          ;; detailed
          (pp-regression-output
           (reverse
            (sort-nth 
             1
             regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear))
           :additional-line-info-f
           (^ (substitution ag-effect se &optional &key stream)
              ag-effect se
              (let ((hd-sets (substitution-num-unique-pairs-annotation differences substitution)))
                (format stream "     ")
                (loop for hd-set in hd-sets do
                      (apply #'format stream "   ~2d ~2d ~3d" hd-set))))
           :filename (string-append directory "/low-se-non-collinear-sort-by-ag-effect-detailed.txt"))

          ;; this one needs clusters
          (if clusters
              (pp-regression-output
               (reverse
                (sort-nth 
                 1
                 regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear))
               :additional-line-info-f
               (^ (substitution ag-effect se &optional &key stream)
                  ag-effect se
                  (let ((hd-sets (substitution-clusters-involoved differences substitution clusters)))
                    (format stream "      ~a" hd-sets)))
               :filename (string-append directory "/low-se-non-collinear-sort-by-ag-effect-clustered.txt")))


          ;; --------------------- sort by location -----------------------
          ;; simple
          (pp-regression-output
           (my-sort
            regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
            (^ (a b) 
               (string< (anything->string (subst-location-location (car a) :regression-unit regression-unit))
                        (anything->string (subst-location-location (car b) :regression-unit regression-unit)))))
           :new-line-test-f
           (^ (subst effect se next-subst next-effect next-se)
              effect se next-effect next-se
              (not (eql (subst-location-location subst :regression-unit regression-unit)
                        (and next-subst (subst-location-location next-subst :regression-unit regression-unit)))))
           :filename (string-append directory "/low-se-non-collinear-sort-by-location-simple.txt"))


          ;; detailed
          (pp-regression-output
           (my-sort
            regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
            (^ (a b) 
               (string< (anything->string (subst-location-location (car a) :regression-unit regression-unit))
                        (anything->string (subst-location-location (car b) :regression-unit regression-unit)))))
           :new-line-test-f
           (^ (subst effect se next-subst next-effect next-se)
              effect se next-effect next-se
              (not (eql (subst-location-location subst :regression-unit regression-unit)
                        (and next-subst (subst-location-location next-subst :regression-unit regression-unit)))))
           :additional-line-info-f
           (^ (substitution ag-effect se &optional &key stream)
              ag-effect se
              (let ((hd-sets (substitution-num-unique-pairs-annotation differences substitution)))
                (format stream "     ")
                (loop for hd-set in hd-sets do
                      (apply #'format stream "   ~2d ~2d ~3d" hd-set))))
           :filename (string-append directory "/low-se-non-collinear-sort-by-location-detailed.txt"))

          ;; needs cluster info
          (if clusters
              (pp-regression-output
               (my-sort
                regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
                (^ (a b) 
                   (string< (anything->string (subst-location-location (car a) :regression-unit regression-unit))
                            (anything->string (subst-location-location (car b) :regression-unit regression-unit)))))
               :new-line-test-f
               (^ (subst effect se next-subst next-effect next-se)
                  effect se next-effect next-se
                  (not (eql (subst-location-location subst :regression-unit regression-unit)
                            (and next-subst (subst-location-location next-subst :regression-unit regression-unit)))))
               :additional-line-info-f
               (^ (substitution ag-effect se &optional &key stream)
                  ag-effect se
                  (let ((hd-sets (substitution-clusters-involoved differences substitution clusters)))
                    (format stream "      ~a" hd-sets)))
               :filename (string-append directory "/low-se-non-collinear-sort-by-location-clustered.txt")))


          ;;-----------------------  sort by aa subst  --------------------------
          ;; simple
          (pp-regression-output
           (my-sort
            regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
            (^ (a b) 
               (string< (string (subst-location-subst (car a) :regression-unit regression-unit))
                        (string (subst-location-subst (car b) :regression-unit regression-unit)))))
           :new-line-test-f
           (^ (subst effect se next-subst next-effect next-se)
              effect se next-effect next-se
              (not (equal (subst-location-subst subst :regression-unit regression-unit)
                          (and next-subst (subst-location-subst next-subst :regression-unit regression-unit)))))
           :filename (string-append directory "/low-se-non-collinear-sort-by-subst-simple.txt"))


          ;; detailed
          (pp-regression-output
           (my-sort
            regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
            (^ (a b) 
               (string< (string (subst-location-subst (car a) :regression-unit regression-unit))
                        (string (subst-location-subst (car b) :regression-unit regression-unit)))))
           :new-line-test-f
           (^ (subst effect se next-subst next-effect next-se)
              effect se next-effect next-se
              (not (equal (subst-location-subst subst :regression-unit regression-unit)
                          (and next-subst (subst-location-subst next-subst :regression-unit regression-unit)))))
           :additional-line-info-f
           (^ (substitution ag-effect se &optional &key stream)
              ag-effect se
              (let ((hd-sets (substitution-num-unique-pairs-annotation differences substitution)))
                (format stream "     ")
                (loop for hd-set in hd-sets do
                      (apply #'format stream "   ~2d ~2d ~3d" hd-set))))
           :filename (string-append directory "/low-se-non-collinear-sort-by-subst-detailed.txt"))


          ;; needs cluster info  (so check if cluster info supplied as parameter)
          (if clusters
              (pp-regression-output
               (my-sort
                regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
                (^ (a b) 
                   (string< (string (subst-location-subst (car a) :regression-unit regression-unit))
                            (string (subst-location-subst (car b) :regression-unit regression-unit)))))
               :new-line-test-f
               (^ (subst effect se next-subst next-effect next-se)
                  effect se next-effect next-se
                  (not (equal (subst-location-subst subst :regression-unit regression-unit)
                              (and next-subst (subst-location-subst next-subst :regression-unit regression-unit)))))
               :additional-line-info-f
               (^ (substitution ag-effect se &optional &key stream)
                  ag-effect se
                  (let ((hd-sets (substitution-clusters-involoved differences substitution clusters)))
                    (format stream "      ~a" hd-sets)))
               :filename (string-append directory "/low-se-non-collinear-sort-by-subst-clustered.txt")))


          ;; ---------- predictions
          (regression-prediction-summary 
           differences
           regression-run
           directory
           :id "train"
           :generate-graphics generate-graphics
           :substs-details-directory "substs-details/"
           :model model)
          (if test-differences 
              (regression-prediction-summary
               test-differences
               regression-run
               directory
               :id "test"
               :generate-graphics generate-graphics
               :substs-details-directory "substs-details/"
               :model model))

          ;; the below for testing without unbiasing, see also a substraction of an intercept (calculated with just the below)
          ;; to get better, not sure what that intercept is.  maybe have these steps in the regression too
          ;;(regression-prediction-summary differences-before-unbiasing regression-run directory :id "train")
          ;;(if test-differences 
          ;;    (regression-prediction-summary test-differences-before-unbiasing regression-run directory :id "test"))
          differences-before-unbiasing        ;; to stop compiler warning
          test-differences-before-unbiasing   ;; to stop compiler warning
    
          ;; ---------- detail info on each subst, strains involved with each substitution
          (let* ((substs-details-directory "substs-details/")
                 (substs-details-directory-full-name (string-append directory "/" substs-details-directory)))
            (mkdir substs-details-directory-full-name)
            (let ((all-substs-info (gather-all-subst-info differences 
                                                          clusters
                                                          :write-substs-details write-substs-details 
                                                          :substs-details-directory-full-name substs-details-directory-full-name)
                                   ))
              
              ;; -- write the substs info
              (fll
               all-substs-info
               :filename (string-append directory "/" "substs-strain-summary-details.txt"))
              
              ;; this belongs aboove (comment "low se, non-collinear, >1 strain" above, but is here as had to be after we calc all-substs-info)
              (pp-regression-output
               (setq regression-run-exclude-low-copy-number-substitutions-low-se-non-collinear
                 (filter-singletons 
                  (filter (^ (l) (collinear-name-p (car l)))
                          (collect (^ (l) (< (nth 2 l) low-se-threshold)) regression-run))
                  all-substs-info))
               :filename (string-append directory "/low-se-non-collinear-gt1strain.txt"))


              (if generate-graphics
                  (progn
                    ;; ---------- regression parameter estimate plots
                    ;; all
                    (regression-coefficients-plot
                     (reverse (sort-nth 1 regression-run))
                     nil
                     :plot-filename-without-suffix (string-append directory "/" "all-estimates-sorted"))


                    ;; remove collinear
                    (if (reverse (filter-collinears (sort-nth 1 regression-run)))
                        (progn
                          (regression-coefficients-plot
                           (reverse (filter-collinears (sort-nth 1 regression-run)))
                           all-substs-info
                           :plot-filename-without-suffix (string-append directory "/" "non-collinear-estimates-sorted"))

                          ;; remove collinear and singletons
                          (regression-coefficients-plot
                           (reverse (if remove-strain-1p0-substs-from-some-figures
                                        (filter-singletons (filter-collinears (sort-nth 1 regression-run)) all-substs-info)
                                      (filter-collinears (sort-nth 1 regression-run))))
                           all-substs-info
                           :plot-filename-without-suffix (string-append directory "/" "non-collinear-gt1-strain-estimates-sorted"))

                          ;; remove collinear and singletons, sort by subst
                          (let ((data (reverse (sort-regression-run-by-subst 
                                                (if remove-strain-1p0-substs-from-some-figures
                                                    (filter-singletons (filter-collinears (sort-nth 1 regression-run)) all-substs-info)
                                                  (filter-collinears (sort-nth 1 regression-run)))
                                                :regression-unit regression-unit))))
                            (regression-coefficients-plot
                             data
                             all-substs-info
                             :lines (loop for (this next) on (nths 0 data)
                                        for i from 1
                                        when (and next
                                                  (not (equal (subst-location-subst this :regression-unit regression-unit)
                                                              (subst-location-subst next :regression-unit regression-unit))))
                                        collect i)
                             :plot-filename-without-suffix (string-append directory "/" "by-subst-estimates")))

                          ;; remove collinear and singletons, sort by location
                          (let ((data (reverse (sort-regression-run-by-location 
                                                (if remove-strain-1p0-substs-from-some-figures
                                                    (filter-singletons (filter-collinears (sort-nth 1 regression-run)) all-substs-info)
                                                  (filter-collinears (sort-nth 1 regression-run)))
                                                :regression-unit regression-unit))))
                            (regression-coefficients-plot
                             data
                             all-substs-info
                             :lines (loop for (this next) on (nths 0 data)
                                        for i from 1
                                        when (and
                                              next
                                              (not (equal (subst-location-location this :regression-unit regression-unit)
                                                          (subst-location-location next :regression-unit regression-unit))))
                                        collect i)
                             :plot-filename-without-suffix (string-append directory "/" "by-location-estimates")))
                          ))
                    ))

              (write-regression-diagnostics-pages
               directory
               substs-details-directory
               (nthcdr 4 (car differences))
               :regression-dataset-or-filename differences-or-differences-filename
               :low-se-threshold low-se-threshold
               :experiment-title experiment-title
               :remove-strain-1p0-substs-from-some-figures remove-strain-1p0-substs-from-some-figures
               :model model)))
          

          ;; ----------------- single and double mutants -------------------
          (single-and-double-mutants-summary original-full-differences regression-run directory :generate-graphics generate-graphics :model model)


          ;; ----------------- intercept model summary -------------------
          (intercept-model-summary regression-run directory :generate-graphics generate-graphics :model model)


          ;; ----------------- optionally zip results dir -------------------
          (if zip-results-dir
              (print "zip-results-dir not implemented yet"))


          ;; ----------------- return results and optionally pop up html window -------------------
          (values
           regression-run
           (let ((open-command (format nil "open ~a/index.html" directory)))
             (if pop-up-web-page (run-shell-command open-command))
             `(run-shell-command ,open-command)))
          
          )))))

(defun gather-all-subst-info (differences clusters &optional &key write-substs-details dont-write-file substs-details-directory-full-name)
  (filter 
   (^ (l) (null (nth 2 l))) ;; substs with no pairs (happens when not in test data)
   (loop for subst in (nthcdr 4 (car differences)) collect
         (let* ((subst-info (substitution-info differences subst clusters))
                (strains-involved-in-subst-info (strains-involved-in-subst-info (mapcar (^ (l) (firstn 4 l)) subst-info)))
                (substs-involved-in-subst-info  (substs-involved-in-subst-info  subst subst-info))
                (clusters-involved-in-subst     (if clusters (nth-value 1 (substitution-all-clusters differences subst clusters)))))
           (unless dont-write-file
             (fll 
              (if write-substs-details
                  (let* ((unique-substs (remove-duplicates (map-append (^ (l) (nths 0 l)) (nths 4 subst-info))))
                         ;;(sorted-unique-substs (sort-alpha unique-substs))
                         (sorted-unique-substs (reverse
                                                (nths 
                                                 0
                                                 (sort-nth 
                                                  1
                                                  (sort-alpha-nth
                                                   0
                                                   (loop for subst in unique-substs collect
                                                         (list subst (loop for subst-info-line in subst-info
                                                                         when (assoc subst (nth 4 subst-info-line))
                                                                         sum 1)))))))))
                    (loop for subst-info-line in subst-info collect
                          (append
                           (firstn 4 subst-info-line)
                           (last subst-info-line)
                           (loop for (subst . rest-substs) on sorted-unique-substs collect
                                 (if (assoc subst (nth 4 subst-info-line))
                                     (assoc subst (nth 4 subst-info-line))
                                   (loop for rest-subst in rest-substs
                                       when (assoc rest-subst (nth 4 subst-info-line))
                                       do (return ".")
                                       finally (return "")))))))
                '((no details to save disk space)))
              :preamble "<PRE>"
              :postamble "</PRE>"
              :filename (string-append substs-details-directory-full-name "/" (string subst) ".html")))
           (append
            (list subst)
            (list 
             (if (and (nth 1 (nth 0 strains-involved-in-subst-info))
                      (= 1 (nth 1 (nth 0 strains-involved-in-subst-info))))
                 '-
               '*))
            (list (nth 3 (nth 0 subst-info)))
            (list (length subst-info))
            (apply-append (firstn 2 strains-involved-in-subst-info))
            (if (= 2 (length (firstn 2 substs-involved-in-subst-info)))
                (apply-append (firstn 2 substs-involved-in-subst-info))
              (if (= 1 (length (firstn 2 substs-involved-in-subst-info)))
                  (append (nth 0 substs-involved-in-subst-info) '(no-more 0.0))
                '(none 0.0 none 0.0)))
            (if (= 2 (length (firstn 2 clusters-involved-in-subst)))
                (apply-append (firstn 2 clusters-involved-in-subst))
              (if (= 1 (length (firstn 2 clusters-involved-in-subst)))
                  (append (nth 0 clusters-involved-in-subst) '(no-more 0.0))
                '(none 0.0 none 0.0)))
            ))
         )))

(defun regression-prediction-summary (differences regression-run directory &optional &key 
                                                                                     (id "")
                                                                                     (generate-graphics t)
                                                                                     substs-details-directory
                                                                                     model)
  (multiple-value-bind
      (av-absolute-prediction-error
       av-prediction-error
       sd-prediction-error
       sse-rms-prediction-error
       min-median-max-prediction-errors
       num-predictions-text
       prediction-actual-s
       prediction-actual-verbose-s)
      (regression-prediction-errors 
       regression-run
       differences
       :model model
       :include-intercept t)
    (let ((prediction-actual-s-no-intercept (nth-value 6 (regression-prediction-errors 
                                                          regression-run
                                                          differences
                                                          :model model
                                                          :include-intercept nil)))
          (misc-prediction-data (list av-absolute-prediction-error
                                      av-prediction-error
                                      sd-prediction-error
                                      sse-rms-prediction-error
                                      min-median-max-prediction-errors
                                      num-predictions-text)))
      (fi misc-prediction-data (format nil "~a/~a-misc-prediction-data.lisp" directory id))
      (fll prediction-actual-s              :filename (format nil "~a/~a-prediction-actual-s.txt"              directory id))
      (fll prediction-actual-s-no-intercept :filename (format nil "~a/~a-prediction-actual-s-no-intercept.txt" directory id))
      (fll prediction-actual-verbose-s :filename (format nil "~a/~a-prediction-actual-s-verbose-for-programs.txt" directory id))
      (with-open-file (out (format nil "~a/~a-prediction-actual-s-verbose-for-eye.html" directory id) :direction :output)
        (format out "<H1><CENTER>Regression diagnostics for map and predicted distances (version 0.0)</CENTER></H1>")
        (newline out)
        (format out "<PRE>")
        (newline out)
        (fll 
         (append
          (list
           (list "Strain 1"    "Strain 2"     "Map distance"      "Predicted distance"        "Components of predicted distance")
           (list "" "" "" "" ""))
          (loop for (strain-a strain-b map-distance predicted-distance components-of-predicted-distance) in prediction-actual-verbose-s collect
                (list strain-a strain-b map-distance predicted-distance 
                      (glue-up-to-string 
                       " + "
                       (loop for (name multiplier estimate) in components-of-predicted-distance collect
                             (format nil "(~a ~a)"
                                     (if (equal 'intercept name)
                                         "Intercept"
                                       (format nil "<A href=~a/~a.html>~a</A>" substs-details-directory name name))
                                     (if (numberp estimate)
                                         (if (equal 1 multiplier)
                                             estimate
                                           (format nil "~a * ~a = ~a" multiplier estimate (* multiplier estimate)))
                                       "missing")))))))
         :stream out)
        (newline out)
        (format out "</PRE>")
        (newline out))
      (if (not (running-on-windows-p))
          (if (> (length (remove-duplicates prediction-actual-s :test #'equal)) 1)

              (let ((pairs              (mapcar #'reverse prediction-actual-s))
                    (pairs-no-intercept (mapcar #'reverse prediction-actual-s-no-intercept)))
                (fi (multiple-value-bind (m c r)
                        (regression pairs)
                      (list r m c pairs))
                    (format nil "~a/~a-misc-prediction-data-2.lisp" directory id))
                (fi (multiple-value-bind (m c r)
                        (regression pairs-no-intercept)
                      (list r m c pairs-no-intercept))
                    (format nil "~a/~a-misc-prediction-data-2-no-intercept.lisp" directory id))
                
                (if generate-graphics
                    (progn
                      (progn
                        (gnuplot-correlation 
                         pairs
                         :equal-axes-range t
                         :show-diagonal t 
                         :x-title "Predicted"
                         :y-title "Actual"
                         :x-min (floor (apply-min (cons 0 (apply-append (append prediction-actual-s prediction-actual-s-no-intercept)))))
                         :y-min (floor (apply-min (cons 0 (apply-append (append prediction-actual-s prediction-actual-s-no-intercept)))))
                         :hardcopy-only t 
                         :size 1.5
                         :ps-filename (format nil "~a/~a-prediction-actual-s.ps" directory id) :hardcopy-fontsize 20)
                        (sleep 1.5)
                        (ps-to-png (format nil "~a/~a-prediction-actual-s.ps" directory id) "")
                        (sleep 1.5)
                        (ps-to-pdf (format nil "~a/~a-prediction-actual-s.ps" directory id) 
                                   (format nil "~a/~a-prediction-actual-s.pdf" directory id))
                        (sleep 1.5)
                        (gnuplot-exit)
                        (sleep 1.5))
                      (progn
                        (gnuplot-correlation 
                         pairs-no-intercept
                         :equal-axes-range t
                         :show-diagonal t 
                         :x-title "Predicted (without using intercept)"
                         :y-title "Actual"
                         :x-min (floor (apply-min (cons 0 (apply-append (append prediction-actual-s prediction-actual-s-no-intercept)))))
                         :y-min (floor (apply-min (cons 0 (apply-append (append prediction-actual-s prediction-actual-s-no-intercept)))))
                         :hardcopy-only t 
                         :size 1.5
                         :ps-filename (format nil "~a/~a-prediction-actual-s-no-intercept.ps" directory id) :hardcopy-fontsize 20)
                        (sleep 1.5)
                        (ps-to-png (format nil "~a/~a-prediction-actual-s-no-intercept.ps" directory id) "")
                        (sleep 1.5)
                        (ps-to-pdf (format nil "~a/~a-prediction-actual-s-no-intercept.ps" directory id)
                                   (format nil "~a/~a-prediction-actual-s-no-intercept.pdf" directory id))
                        (sleep 1.5)
                        (gnuplot-exit)
                        (sleep 1.5)))))
            (fi '(0.0 0.0 0.0 ((0.0 0.0) (0.0 0.0) (0.0 0.0) (0.0 0.0) (0.0)))
                (format nil "~a/~a-misc-prediction-data-2.lisp" directory id)))))))


(defun parameter-estimate-including-intercpet (name parameter-estimates model)
  (let ((parameter-estimate (assoc-value-1 name parameter-estimates)))
    (if (member model '("nls-ssmodel0" "nls-ssmodel0i") :test #'equal)
        (if (member name '(bias theta intercept))
            parameter-estimate
          (let ((bias          (assoc-value-1 'bias      parameter-estimates))
                (theta         (assoc-value-1 'theta     parameter-estimates))
                (intercept (or (assoc-value-1 'intercept parameter-estimates) 0)))
            (+ intercept
               parameter-estimate
               (* bias
                  (expt 2.0 (- (/ parameter-estimate theta)))))))
      (let ((intercept (or (assoc-value-1 'intercept parameter-estimates) 0)))
        (if (member name '(intercept))
            parameter-estimate
          (+ intercept 
             parameter-estimate))))))


(defun single-and-double-mutants-summary (differences regression-run directory &optional &key (generate-graphics t) model)
  (let* ((1-mutations (cons (car differences)
			    (collect (^ (l) (= 1 (nth 3 l))) (cdr differences))))
	 (2-mutations (cons (car differences)
			    (collect (^ (l) (= 2 (nth 3 l))) (cdr differences))))
	 (1-mutations-summary (group-mutations 1-mutations))
	 (2-mutations-summary (group-mutations 2-mutations)))
	  
    ;; ---------------------- 1 mutation -----------------------
    (fll 1-mutations-summary
	 :filename (string-append directory "/1-mutations-simple.txt"))

    (fll
     (cons '("Name" "av-in-obs" "sd-in-obs" "num-obs" "reg-pe" "reg-sd" "obs-reg" "reg-inc-int" "obs-minus-reg-inc-int")
           (loop for (mutation-names av sd n) in 1-mutations-summary 
               when (assoc (car mutation-names) regression-run)
               collect
                 (let ((regression-entry (assoc (car mutation-names) regression-run))
                       (regression-entry-including-intercpet (parameter-estimate-including-intercpet (car mutation-names) regression-run model)))
                   (list mutation-names av sd n 
                         (nth 1 regression-entry)
                         (nth 2 regression-entry)
                         (- av (nth 1 regression-entry))
                         regression-entry-including-intercpet
                         (- av regression-entry-including-intercpet)
                         ))))
     :filename (string-append directory "/1-mutations-with-regression.txt"))

    (fll
     (cons '("Name" "av-in-obs" "sd-in-obs" "num-obs" "reg-pe" "reg-sd" "obs-reg" "reg-inc-int" "obs-minus-reg-inc-int")
           (loop for (mutation-names av sd n) in 1-mutations-summary 
               when (and (>= n 2)
                         (assoc (car mutation-names) regression-run))
               collect (let ((regression-entry (assoc (car mutation-names) regression-run))
                             (regression-entry-including-intercpet (parameter-estimate-including-intercpet (car mutation-names) regression-run model)))
                         (list mutation-names av sd n 
                               (nth 1 regression-entry)
                               (nth 2 regression-entry)
                               (- av (nth 1 regression-entry))
                               regression-entry-including-intercpet
                               (- av regression-entry-including-intercpet)))))
     :filename (string-append directory "/1-mutations-with-regression-gte-2-instances.txt"))

    (fll
     (cons '("Name" "av-in-obs" "sd-in-obs" "num-obs" "reg-pe" "reg-sd" "obs-reg" "reg-inc-int" "obs-minus-reg-inc-int")
           (loop for (mutation-names av sd n) in 1-mutations-summary 
               when (and (>= n 5)
                         (assoc (car mutation-names) regression-run))
               collect (let ((regression-entry (assoc (car mutation-names) regression-run))
                             (regression-entry-including-intercpet (parameter-estimate-including-intercpet (car mutation-names) regression-run model)))
                         (list mutation-names av sd n 
                               (nth 1 regression-entry)
                               (nth 2 regression-entry)
                               (- av (nth 1 regression-entry))
                               regression-entry-including-intercpet
                               (- av regression-entry-including-intercpet)))))
     :filename (string-append directory "/1-mutations-with-regression-gte-5-instances.txt"))


    ;; ---------------------- 2 mutations -----------------------
    (fll 2-mutations-summary
	 :filename (string-append directory "/2-mutations-simple.txt"))

    (fll
     (loop for (mutant-names av sd n) in 2-mutations-summary collect
	   (list mutant-names av sd n
		 (loop for mutant-name in mutant-names 
		     when (assoc (list mutant-name) 1-mutations-summary :test #'equal)
		     collect (assoc (list mutant-name) 1-mutations-summary :test #'equal))))
     :filename (string-append directory "/2-mutations-and-1-mutations.txt"))

    (fll
     (loop for (mutant-names av sd n) in 2-mutations-summary 
	 when (>= n 2)
	 collect (list mutant-names av sd n
		       (loop for mutant-name in mutant-names 
			   when (assoc (list mutant-name) 1-mutations-summary :test #'equal)
			   collect (assoc (list mutant-name) 1-mutations-summary :test #'equal))))
     :filename (string-append directory "/2-mutations-and-1-mutations-gte-2-instances.txt"))

    (fll
     (loop for (mutant-names av sd n) in 2-mutations-summary 
	 when (>= n 5)
	 collect (list mutant-names av sd n
		       (loop for mutant-name in mutant-names 
			   when (assoc (list mutant-name) 1-mutations-summary :test #'equal)
			   collect (assoc (list mutant-name) 1-mutations-summary :test #'equal))))
     :filename (string-append directory "/2-mutations-and-1-mutations-gte-5-instances.txt"))

      
    ;; ---------- plots of the 1 mutations correlation with regression -----------------

    (if (not (running-on-windows-p))
        (if generate-graphics
            (progn
              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (assoc (car mutation-names) regression-run)
                              collect
                                (progn
                                  sd n ;; to stop compiler bitching
                                  (list 
                                   av
                                   (let ((regression-entry (assoc (car mutation-names) regression-run)))
                                     (nth 1 regression-entry)))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "Any num instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-1-instances.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1.5)
                      (ps-to-png (string-append directory "/1-mutations-at-least-1-instances.ps") "")
                      (sleep 1.5)
                      (gnuplot-exit)
                      (sleep 1.5))))

              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (and (>= n 2)
                                        (assoc (car mutation-names) regression-run))
                              collect (progn
                                        sd n ;; to stop the compiler bitching
                                        (list 
                                         av
                                         (let ((regression-entry (assoc (car mutation-names) regression-run)))
                                           (nth 1 regression-entry)))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "At least 2 instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-2-instances.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1)
                      (ps-to-png (string-append directory "/1-mutations-at-least-2-instances.ps") "")
                      (sleep 1)
                      (gnuplot-exit)
                      (sleep 1))))

              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (and (>= n 5)
                                        (assoc (car mutation-names) regression-run))
                              collect (progn
                                        sd n ;; to stop the compiler bitching
                                        (list 
                                         av
                                         (let ((regression-entry (assoc (car mutation-names) regression-run)))
                                           (nth 1 regression-entry)))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "At least 5 instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-5-instances.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1)
                      (ps-to-png (string-append directory "/1-mutations-at-least-5-instances.ps") "")
                      (sleep 1)
                      (gnuplot-exit)
                      (sleep 1))))
              
              
          
              ;; same again, this time adding in intercept
              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (assoc (car mutation-names) regression-run)
                              collect
                                (progn
                                  sd n ;; to stop compiler bitching
                                  (list 
                                   av
                                   (parameter-estimate-including-intercpet (car mutation-names) regression-run model))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "Any num instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-1-instances-with-intercept.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1.5)
                      (ps-to-png (string-append directory "/1-mutations-at-least-1-instances-with-intercept.ps") "")
                      (sleep 1.5)
                      (gnuplot-exit)
                      (sleep 1.5))))

              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (and (>= n 2)
                                        (assoc (car mutation-names) regression-run))
                              collect (progn
                                        sd n ;; to stop the compiler bitching
                                        (list 
                                         av
                                         (parameter-estimate-including-intercpet (car mutation-names) regression-run model))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "At least 2 instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-2-instances-with-intercept.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1)
                      (ps-to-png (string-append directory "/1-mutations-at-least-2-instances-with-intercept.ps") "")
                      (sleep 1)
                      (gnuplot-exit)
                      (sleep 1))))

              (let ((data (loop for (mutation-names av sd n) in 1-mutations-summary 
                              when (and (>= n 5)
                                        (assoc (car mutation-names) regression-run))
                              collect (progn
                                        sd n ;; to stop the compiler bitching
                                        (list 
                                         av
                                         (parameter-estimate-including-intercpet (car mutation-names) regression-run model))))))
                (if (> (length data) 1)
                    (progn
                      (gnuplot-correlation
                       data
                       :x-min -1 :x-max 5
                       :y-min -1 :y-max 5
                       :x-title "Single-mutant-pairs"
                       :y-title "Regression"
                       :show-diagonal t
                       :title "At least 5 instances of each mutation"
                       :hardcopy-only t 
                       :size 1.5
                       :ps-filename (string-append directory "/1-mutations-at-least-5-instances-with-intercept.ps")
                       :hardcopy-fontsize 20)
                      (sleep 1)
                      (ps-to-png (string-append directory "/1-mutations-at-least-5-instances-with-intercept.ps") "")
                      (sleep 1)
                      (gnuplot-exit)
                      (sleep 1))))
              )))))


;;;----------------------------------------------------------------------
;;;                   intercept model summary
;;;----------------------------------------------------------------------

(defun intercept-model-summary (regression-run directory &optional &key (generate-graphics t) model)

  (let* ((intercept-verbose (assoc 'intercept regression-run))
         (bias-verbose      (assoc 'bias      regression-run))
         (theta-verbose     (assoc 'theta     regression-run))

         (intercept         (nth 1 intercept-verbose))
         (bias              (nth 1 bias-verbose     ))
         (theta             (nth 1 theta-verbose    )))

    (fll (append 
          (if intercept (list intercept-verbose))
          (if bias      (list bias-verbose     ))
          (if theta     (list theta-verbose    )))
	 :filename (string-append directory "/intercept-model-summary.txt"))

    (if generate-graphics
        (progn
          (gnuplot-correlation
           (loop for x from -2 to 10 by 0.01 collect
                 (list 
                  (add-gaussian-noise x 0.1)
                  (cond ((equal model "nls-ssmodel0")   (+           x (* bias (expt 2 (- (/ x theta))))))
                        ((equal model "nls-ssmodel0i")  (+ intercept x (* bias (expt 2 (- (/ x theta))))))
                        ((not (null intercept))         (+ intercept x))
                        (t x))))
           :show-diagonal t
           :x-min -2
           :y-min -2
           :x-max 10
           :y-max 10
           :x-title "Without intercept"
           :y-title (format nil "Intercept for model ~a (~a)"
                            model
                            (apply 
                             #'string-append 
                             (infix-operator 
                              ", "
                              (append
                               (if intercept (list (format nil "intercept =~d" intercept)))
                               (if bias      (list (format nil "bias =~d"      bias     )))
                               (if theta     (list (format nil "theta =~d"     theta    )))))))
           :hardcopy-only t 
           :size 1.5
           :ps-filename (string-append directory "/intercept-model-summary.ps")
           :hardcopy-fontsize 20)
          (sleep 1)
          (ps-to-png (string-append directory "/intercept-model-summary.ps") "")
          (sleep 1)
          (gnuplot-exit)
          (sleep 1)))))


;;;----------------------------------------------------------------------
;;;                  plotting regression coeffes 
;;;----------------------------------------------------------------------

(defun regression-coefficients-plot (regression-run 
                                     all-substs-info
                                     &optional &key
                                               lines
                                               plot-filename-without-suffix
                                               (x-size (if (< (length regression-run) 20) 2 4))
                                               (top-element-axis nil)
                                               label
                                               max-x
                                               min-x
                                               ratio
                                               value-to-ignore-when-calculating-parameter-values-range
                                               (element-linewidth 1)
                                               (hardcopy-fontsize 22)
                                               (between-plot-sleep-time 1.5))
  (if (not regression-run)
      (format t "~%No data passed to regression-coefficients-plot~%")
    (let* ((parameter-estimates-for-range-calcs (if value-to-ignore-when-calculating-parameter-values-range
                                                    (remove value-to-ignore-when-calculating-parameter-values-range (nths 1 regression-run))
                                                  (nths 1 regression-run)))
           (abs-x-max (if (remove nil parameter-estimates-for-range-calcs)
                          (apply #'max (mapcar #'abs (remove nil parameter-estimates-for-range-calcs)))
                        1))
           (x-min (or min-x (- (- (ceiling abs-x-max)) 1)))  ;; the -1 to leave room for annoations
           (x-max (or max-x (ceiling abs-x-max)))
           (axis-pos (if top-element-axis (second (find-if #'second regression-run :from-end t)))) ;;Note: FIRST result CANNOT be null !!
           (ps-filename  (format nil "~a.ps" plot-filename-without-suffix)))
      (apply #'gnuplot
       (loop for (name pe se) in regression-run
           for i from 0 
           when (not (null pe)) collect
             (progn
               name  ;; to stop compiler warning
               (list pe (+ i 0.5) se)))
       ;;:x-title "aa substitution by order of effect"
       ;;:y-title "Extimated antigenic effect of aa substitution"
       ;;:ratio ratio
       :y-min 0
       :y-max (length regression-run)
       :x-min x-min
       :x-max x-max
       :y-tics (if lines lines (loop for i below (length regression-run) by 5 collect i))
       :grid t
       ;;:no-y-tics t  ;; would prefer no y-tics, but don't know how to get grid if we turn off tic labels
       :element-style 'xerr
       :bar 'small
       ;;:element-pointtype 2
       :labels (loop for i from 0 
                   for (name pe se) in regression-run 
                   when (not (null (and pe se)))
                   collect
                     (list 
                      (if (assoc name all-substs-info)
                          (apply #'format nil "~a ~a ~2d ~4d ~4,2f ~4,2f | ~4,2f ~4,2f | ~4,2f ~4,2f" 
                                 (multiple-butnth '(4 6 8 10 12 14) (assoc name all-substs-info)))
                        (string name))
                      (+ x-min 0.1) (+ i 0.5)
                      ;;:font-size 14
                      :label-font "Courier"
                      ;;:rotate t
                      ))
       ;;:element-name "ml unbias"
       :element-linewidth (if plot-filename-without-suffix 
                              (* 4 element-linewidth)
                            element-linewidth)
       :element-linetype 3
       (append
        (if ratio 
            (list :ratio ratio))
        (if plot-filename-without-suffix 
            (list :hardcopy-only     t 
                  :ps-filename       ps-filename
                  :hardcopy-fontsize hardcopy-fontsize
                  :size              x-size))
        ))

      (sleep between-plot-sleep-time)

      (apply #'gnuplot 
             `((0 ,(length regression-run)) (0 0))
             :element-linetype -1
             :refresh nil
             (if plot-filename-without-suffix 
                 (append
                  (if ratio 
                      (list :ratio ratio))
                  (list :hardcopy-only     t 
                        :ps-filename       ps-filename
                        :hardcopy-fontsize hardcopy-fontsize
                        :size              x-size))))  
      (cond ((and axis-pos label)
             (apply #'gnuplot 
                    `((,axis-pos ,(length regression-run)) (,axis-pos 0))
                    :element-linewidth 2
                    :element-linetype 1
                    :label (list label 0.8 0.1)
                    :refresh nil
                    (if plot-filename-without-suffix 
                        (append
                         (if ratio 
                             (list :ratio ratio))
                         (list :hardcopy-only     t 
                               :ps-filename       ps-filename
                               :hardcopy-fontsize hardcopy-fontsize
                               :size              x-size)))))
            (label 
             (apply #'gnuplot
                    nil 
                    :label (list label 0.8 0.1) 
                    :refresh nil
                    (if plot-filename-without-suffix 
                        (append
                         (if ratio 
                             (list :ratio ratio))
                         (list :hardcopy-only     t 
                               :ps-filename       ps-filename
                               :hardcopy-fontsize hardcopy-fontsize
                               :size              x-size)))))
            (axis-pos
             (apply #'gnuplot 
                    `((,axis-pos ,(length regression-run)) (,axis-pos 0))
                    :element-linewidth 2
                    :element-linetype 1
                    :refresh nil
                    (if plot-filename-without-suffix 
                        (append
                         (if ratio 
                             (list :ratio ratio))
                         (list :hardcopy-only     t 
                               :ps-filename       ps-filename
                               :hardcopy-fontsize hardcopy-fontsize
                               :size              x-size))))) )
     (if plot-filename-without-suffix
         (let ((pdf-filename (string-append plot-filename-without-suffix ".pdf")))
            (sleep between-plot-sleep-time)
            (run-shell-command-and-wait (format nil "mv -f ~a ~a.old;perl -pe \"s/Solid false/Solid true/\" ~a.old > ~a" 
                                                ps-filename ps-filename ps-filename ps-filename))
            (sleep between-plot-sleep-time)
            (ps-to-png ps-filename "")
            (sleep between-plot-sleep-time)
            (ps-to-pdf ps-filename pdf-filename)  ;; does not get whole plot
            (gnuplot-exit)
            (sleep between-plot-sleep-time))))))

;;;----------------------------------------------------------------------
;;;                      num strains involved
;;;----------------------------------------------------------------------

(defun one-strain-involved-p (subst-info)
  (let ((unique-strains (remove-duplicates (append (nths 0 subst-info) (nths 1 subst-info))))
        (num-pairs (length subst-info)))
    (loop for unique-strain in unique-strains do
          (let ((num-pairs-with-strain 
                 (loop for (strain-a strain-b) in subst-info
                     when (or (eql unique-strain strain-a)
                              (eql unique-strain strain-b))
                     sum 1)))
            (if (= num-pairs-with-strain num-pairs)
                (return unique-strain))))))

(defun num-pairs-including-strain (strain subst-info)
  (loop for (strain-a strain-b) in subst-info
      when (or (eql strain strain-a)
               (eql strain strain-b))
      sum 1))

(defun num-pairs-including-subst (subst full-subst-info)
  (loop for l in full-subst-info
      when (member subst (nths 0 (nth 4 l)))
      sum 1))

(defun strains-involved-in-subst-info (subst-info)
  (let ((unique-strains (remove-duplicates (append (nths 0 subst-info) (nths 1 subst-info))))
        (num-pairs (length subst-info)))
    (reverse
     (sort-nth 
      1
      (loop for strain in unique-strains collect
            (list 
             strain
             (2dp 
              (float
               (/ (num-pairs-including-strain strain subst-info)
                  num-pairs)))))))))

(defun substs-involved-in-subst-info (subst full-subst-info)
  (let ((unique-substs (remove subst (remove-duplicates (map-append (^ (l) (nths 0 (nth 4 l))) full-subst-info))))
        (num-pairs (length full-subst-info)))
    (reverse
     (sort-nth 
      1
      (loop for subst in unique-substs collect
            (list 
             subst
             (2dp 
              (float
               (/ (num-pairs-including-subst subst full-subst-info)
                  num-pairs)))))))))
                
(defun filter-collinears (data)
  (filter
   (^ (l)
      (or (char-member #\+ (string (nth 0 l)))    ;; not perfect, as name might have a +, or especially a - in it
          (char-member #\- (string (nth 0 l)))))
   data))

(defun filter-singletons (data all-substs-info)
  (collect
   (^ (l)
      (equal '* (assoc-value-1 (car l) all-substs-info)))
   data))


;;;----------------------------------------------------------------------
;;;                  sorting regression runs
;;;----------------------------------------------------------------------

(defun sort-regression-run-by-subst (regression-run &optional &key (regression-unit 'substitutions-and-locations))
  (let ((location-start (case regression-unit
                          (substitutions-and-locations 2)
                          (substitutions-only          0)
                          (locations-only              1)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (location-end   (case regression-unit
                          (substitutions-and-locations 4)
                          (substitutions-only          0)
                          (locations-only              3)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (subst-start    (case regression-unit
                          (substitutions-and-locations 0)
                          (substitutions-only          0)
                          (locations-only              0)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (subst-end      (case regression-unit
                          (substitutions-and-locations 1)
                          (substitutions-only          1)
                          (locations-only              0)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit)))))
    (sort-alpha-substring-nth 0 (sort-alpha-substring-nth 0 regression-run location-start location-end) subst-start subst-end)))

(defun sort-regression-run-by-location (regression-run &optional &key (regression-unit 'substitutions-and-locations))
  (let ((location-start (case regression-unit
                          (substitutions-and-locations 2)
                          (substitutions-only          0)
                          (locations-only              1)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (location-end   (case regression-unit
                          (substitutions-and-locations 4)
                          (substitutions-only          0)
                          (locations-only              3)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (subst-start    (case regression-unit
                          (substitutions-and-locations 0)
                          (substitutions-only          0)
                          (locations-only              0)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit))))
        (subst-end      (case regression-unit
                          (substitutions-and-locations 1)
                          (substitutions-only          1)
                          (locations-only              0)
                          (t                           (error "Unexpected regression-unit ~a" regression-unit)))))
    (sort-alpha-substring-nth 0 (sort-alpha-substring-nth 0 regression-run subst-start subst-end) location-start location-end)))


;;;----------------------------------------------------------------------
;;;               writing the summary web pages
;;;----------------------------------------------------------------------

(defun write-regression-diagnostics-main-page (directory 
                                               &optional &key 
                                                         (experiment-title "")
                                                         (if-exists-action :error)
                                                         (low-se-threshold 0.35)
                                                         regression-dataset-or-filename
                                                         (remove-strain-1p0-substs-from-some-figures t)
                                                         model)
  (with-open-file (out (format nil "~a/index.html" directory) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>~a<br><FONT SIZE=-2>(Regression automated diagnostics version 0.6)</FONT></CENTER></H1>" experiment-title)
    (newline out)
    (format out "<PRE>")
    (newline out)

    (format out "
Script with which regression was run <A href=\"script-input.txt\">here</A>
~a
Regression data matrix passed to R <A href=\"regression-data-matrix-passed-to-r.txt\">here</A>
Extended regression data matrix <A href=\"extended-regression-data-matrix.csv\">here</A>  (same as above, but also contains the strain names and Hamming distance)
Extended test regression data matrix <A href=\"extended-test-regression-data-matrix.csv\">here</A>  (the test-set proportion of the above)
Weights (if using weighted regression) <A href=\"weights.txt\">here</A>

Raw regression output <A href=\"script-output.txt\">here</A>

Processed regression output raw <A href=\"raw-regression.txt\">here</A>, same but removing high standard error only <A href=\"low-se.txt\">here</A>, same removing non-collinear <A href=\"low-se-non-collinear.txt\">here</A>, same removing when one strain involved in all pairs <A href=\"low-se-non-collinear-gt1strain.txt\">here</A>
   (substitution, estimate of antigenic effect, SE)

Pairs involved in each substitution <A href=\"substs-details.html\">here</A>
   (strain1, strain2, ag-dist-between-strains, hd-between-strains)

Strains involved in each substitution <A href=\"substs-strain-summary-details.txt\">here</A>

Only substs with se<~d and non-collinear:

  by effect   <A href=\"low-se-non-collinear-sort-by-ag-effect-simple.txt\">here</A> (uniques per HD <A href=\"low-se-non-collinear-sort-by-ag-effect-detailed.txt\">here</A>) (clusters per HD <A href=\"low-se-non-collinear-sort-by-ag-effect-clustered.txt\">here</A>) (\"heavy to light hitters\")
  by location <A href=\"low-se-non-collinear-sort-by-location-simple.txt\">here</A> (uniques per HD <A href=\"low-se-non-collinear-sort-by-location-detailed.txt\">here</A>) (clusters per HD <A href=\"low-se-non-collinear-sort-by-location-clustered.txt\">here</A>)
  by subst    <A href=\"low-se-non-collinear-sort-by-subst-simple.txt\">here</A> (uniques per HD <A href=\"low-se-non-collinear-sort-by-subst-detailed.txt\">here</A>) (clusters per HD <A href=\"low-se-non-collinear-sort-by-subst-clustered.txt\">here</A>)

  (note, for the \"uniques\" files above, each triple is HD, num unique pairs, num total pairs)
  (NOTE: num uniques is not correct, it is likely an understimate, but is close enough for us to be able to exclude situations where everything is due to one strain)

  (note, for the \"cluster\" files above, the clusters are per HD (shown), then unique clusters involved)
~a
<hr><h3>All parameter estimates</h3>
<IMG src=all-estimates-sorted.png alt=all-estimates-sorted.png>
<hr><h3>Non-collinear parameter estimates</h3>
<IMG src=non-collinear-estimates-sorted.png alt=non-collinear-estimates-sorted.png>
<hr><h3>Non-collinear ~aparameter estimates</h3>
<IMG src=non-collinear-gt1-strain-estimates-sorted.png alt=non-collinear-gt1-strain-estimates-sorted.png>
<hr><h3>Non-collinear ~asorted by substitution</h3>
<IMG src=by-subst-estimates.png alt=by-subst-estimates.png>
<hr><h3>Non-collinear ~asorted by location</h3>
<IMG src=by-location-estimates.png alt=by-location-estimates.png>
<hr><h2>Single and double mutants alone and some comparison with regression</h2>

<IMG src=1-mutations-at-least-1-instances.png alt=1-mutations-at-least-1-instances.png>   <IMG src=1-mutations-at-least-2-instances.png alt=1-mutations-at-least-2-instances.png>  <IMG src=1-mutations-at-least-5-instances.png alt=1-mutations-at-least-5-instances.png>

Left to right:
  1. Single mutant pairs correlated with regression estimates
  2. same as 1 but when there are at least 2 instances of each pair
  3. same as 1 but when there are at least 5 instances of each pair

<IMG src=1-mutations-at-least-1-instances-with-intercept.png alt=1-mutations-at-least-1-instances-with-intercept.png>   <IMG src=1-mutations-at-least-2-instances-with-intercept.png alt=1-mutations-at-least-2-instances-with-intercept.png>  <IMG src=1-mutations-at-least-5-instances-with-intercept.png alt=1-mutations-at-least-5-instances-with-intercept.png>

Same as line of plots above, but this time adding in an intercept (or more complex intercept if non-linear regression)

Left to right:
  1. Single mutant pairs correlated with regression estimates
  2. same as 1 but when there are at least 2 instances of each pair
  3. same as 1 but when there are at least 5 instances of each pair

   
Single mutants <A href=\"1-mutations-simple.txt\">alone here</A>, <A href=\"1-mutations-with-regression.txt\">with regression</A>, <A href=\"1-mutations-with-regression-gte-2-instances.txt\">with regression at least 2 instances</A>, <A href=\"1-mutations-with-regression-gte-5-instances.txt\">with regression at least 5 instances</A> 

   Double mutants <A href=\"2-mutations-simple.txt\">alone here</A>, <A href=\"2-mutations-and-1-mutations.txt\">with 1 mutants</A>, <A href=\"2-mutations-and-1-mutations-gte-2-instances.txt\">with 1 mutants at least 2 instances</A>, <A href=\"2-mutations-and-1-mutations-gte-5-instances.txt\">with 1 mutants at least 5 instances</A>

<HR>
"
            (if (stringp regression-dataset-or-filename)
                (format nil "Dataset filename passed to r-regression:     \"~a\"" regression-dataset-or-filename)
              "")
            (2dp low-se-threshold)
            (generate-regression-diagnostics-predictions-string directory :model model)
            (if remove-strain-1p0-substs-from-some-figures "and >1 strain involved " "and NOT REMOVE >1 strain involved ")
            (if remove-strain-1p0-substs-from-some-figures "and >1 strain involved " "and NOT REMOVE >1 strain involved ")
            (if remove-strain-1p0-substs-from-some-figures "and >1 strain involved " "and NOT REMOVE >1 strain involved ")
            )

    (newline out)
    (format out "</PRE>")
    (newline out)))





(defun generate-regression-diagnostics-predictions-string (directory &optional &key model)
  (format nil "~a
~a"
          (generate-regression-diagnostics-predictions-string-part directory :id "train" :model model)
          (if (file-or-directory-exists-p (format nil "~a/test-prediction-actual-s.txt" directory))
              (generate-regression-diagnostics-predictions-string-part directory :id "test" :model model)
            "")))
  
(defun generate-regression-diagnostics-predictions-string-part (directory &optional &key (id "") model)
  (let* ((misc-prediction-data      (fi-in (format nil "~a/~a-misc-prediction-data.lisp" directory id)))
         (av-abs-pred-error         (nth 0 misc-prediction-data))
         (av-pred-error             (nth 1 misc-prediction-data))
         (sd-pred-error             (nth 2 misc-prediction-data))
         (sum-squared-pred-error    (nth 0 (nth 3 misc-prediction-data)))
         (av-sum-squared-pred-error (nth 1 (nth 3 misc-prediction-data)))
         (min-pred-error            (nth 0 (nth 4 misc-prediction-data)))
         (median-pred-error         (nth 1 (nth 4 misc-prediction-data)))
         (max-pred-error            (nth 2 (nth 4 misc-prediction-data)))
         (num-testable-string       (nth 5 misc-prediction-data))
         (misc-prediction-data-2    (fi-in (format nil "~a/~a-misc-prediction-data-2.lisp" directory id)))
         (predicition-actual-correlation (nth 0 misc-prediction-data-2))
         (predicition-actual-slope       (nth 1 misc-prediction-data-2))
         (predicition-actual-intercept   (nth 2 misc-prediction-data-2)))
    (format nil "
   <hr><h3>~a set predictions</h3>
<IMG src=~a-prediction-actual-s.png alt=~a-prediction-actual-s.png><IMG src=~a-prediction-actual-s-no-intercept.png alt=~a-prediction-actual-s-no-intercept.png><IMG src=intercept-model-summary.png alt=intercept-model-summary.png>

Left to right:
  1. Predictions vs actuals (pdf <A href=\"~a-prediction-actual-s.pdf\">here</A>)

       Average absolute prediction error ~d
       Average prediction error          ~d
       SD prediction error               ~d
       Sum squared prediction error      ~d (average ~d)
       Min/Median/Max prediction error   ~d/~d/~d
       Individual predictions and actuals <A href=\"~a-prediction-actual-s.txt\">here</A> <A href=\"~a-prediction-actual-s-verbose-for-eye.html\">verbose (for eye)</A>  <A href=\"~a-prediction-actual-s-verbose-for-programs.txt\">verbose (for program)</A>  (~a)

       Prediction-actual correlation     r ~d, r^2 ~d
       Prediction-actual slope           ~d
       Prediction-actual intercept       ~d

  2. Predictions vs actuals (pdf <A href=\"~a-prediction-actual-s-no-intercept.pdf\">here</A>) without including intercept in prediciton
       Individual predictions and actuals <A href=\"~a-prediction-actual-s-no-intercept.txt\">here</A>

  3. Intercept for model ~a (intercept paramters <A href=\"intercept-model-summary.txt\">here</A>)
"
            (string-capitalize id)
            id
            id
            id
            id
            id
            av-abs-pred-error
            av-pred-error
            sd-pred-error
            sum-squared-pred-error
            av-sum-squared-pred-error
            min-pred-error
            median-pred-error
            max-pred-error
            id
            id
            id
            num-testable-string
            predicition-actual-correlation
            (square predicition-actual-correlation)
            predicition-actual-slope
            predicition-actual-intercept

            id
            id

            model
            )))


(defun write-regression-diagnostics-subst-page (directory 
						substs-details-directory
						substs
						&optional &key 
							  (if-exists-action :error))
  (with-open-file (out (format nil "~a/substs-details.html" directory) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Regression diagnostics for each substitution (version 0.0)</CENTER></H1>")
    (newline out)
    (format out "<PRE>")
    (newline out)
    (loop for subst in substs do
	  (format out "<A href=~a/~a.html>~a</A>" substs-details-directory subst subst)
	  (newline out))
    (newline out)
    (format out "</PRE>")
    (newline out)))

(defun write-regression-diagnostics-pages (directory substs-details-directory substs &optional &key 
                                                                                               (experiment-title "")
                                                                                               regression-dataset-or-filename 
                                                                                               (low-se-threshold 0.35)
                                                                                               (remove-strain-1p0-substs-from-some-figures t)
                                                                                               model
                                                                                               (if-exists-action :error))
  (write-regression-diagnostics-main-page 
   directory
   :experiment-title experiment-title
   :regression-dataset-or-filename regression-dataset-or-filename 
   :low-se-threshold low-se-threshold
   :remove-strain-1p0-substs-from-some-figures remove-strain-1p0-substs-from-some-figures
   :model model
   :if-exists-action if-exists-action)
  (write-regression-diagnostics-subst-page 
   directory
   substs-details-directory
   substs
   :if-exists-action if-exists-action))



;;;----------------------------------------------------------------------
;;;                         blob error
;;;----------------------------------------------------------------------

(defun calc-inverse-weight-blob-error-vector (blob-error-save differences &optional &key (blob-error 0.5) (blob-step 0.1) (inverse-f (^ (x) (/ 1.0 x))))
  (let* ((strain-pairs (multiple-nths '(0 1) (cdr differences)))
         (save-antigens-short (hi-table-antigens-unasl-table-from-save-non-expanding-hack blob-error-save))
         (save-coordss (coordss (starting-coordss-from-save blob-error-save)))
         (save-coordss-short (firstn (length save-antigens-short) save-coordss))
         (constant-stress-radial-data (constant-stress-radial-data-from-save blob-error-save))
         (constant-stress-radial-data-short (loop for ag in save-antigens-short collect
                                                  (assoc-value-1 ag constant-stress-radial-data :test (^ (a b) (if (ag-name-p (nth 1 b))
                                                                                                                   (eql a (remove-ag-sr-from-name (nth 1 b)))
                                                                                                                 nil)))))
         ;; (firstn (length save-antigens-short) constant-stress-radial-data))
         (antigen-coords-radialData-alist (transpose save-antigens-short save-coordss-short constant-stress-radial-data-short))
         (blob-error-data
          (loop for (strain-a strain-b) in strain-pairs collect
                (let* ((a-to-b-angle (angle-from-coords-to-coords (assoc-value-1 strain-a antigen-coords-radialData-alist) (assoc-value-1 strain-b antigen-coords-radialData-alist)))
                       (b-to-a-angle (angle-from-coords-to-coords (assoc-value-1 strain-b antigen-coords-radialData-alist) (assoc-value-1 strain-a antigen-coords-radialData-alist)))
                       (a-to-b-spoke (nth (mod (round a-to-b-angle 10) 36) (assoc-value-2 strain-a antigen-coords-radialData-alist)))
                       (b-to-a-spoke (nth (mod (round b-to-a-angle 10) 36) (assoc-value-2 strain-b antigen-coords-radialData-alist)))
                       (a-to-b-blob-radius (* blob-step (interpolate-x-position-from-y-in-ys blob-error a-to-b-spoke)))
                       (b-to-a-blob-radius (* blob-step (interpolate-x-position-from-y-in-ys blob-error b-to-a-spoke)))
                       (rms-error (rms (list a-to-b-blob-radius b-to-a-blob-radius))))
                  (list strain-a strain-b
                        a-to-b-blob-radius b-to-a-blob-radius
                        rms-error
                        (funcall inverse-f rms-error))))))
    (values
     (nths 5 blob-error-data)
     (nths 4 blob-error-data)
     blob-error-data)))

#|
(fll 
 (nth-value
  2
  (calc-inverse-weight-blob-error-vector 
   (fi-in "mds/investigations/shape-sequence/tab1-blobs.save")
   (read-csv-file-into-ll "mds/investigations/shape-sequence/tab1-substitutions-and-positions-upto-10-mutations.csv"))))

CL-USER(66): (nth (position (apply #'min (nths 4 foo)) (nths 4 foo)) foo)
(BI/16398/68 BI/21438/71 251.74905499883914d0 71.74905499883914d0 0.32914814 0.4463013)
CL-USER(67): (nth (position (apply #'max (nths 4 foo)) (nths 4 foo)) foo)
(BI/21438/71 BI/908/69 120.8834067824418d0 300.8834067824418d0 0.70028794 0.4602326)

|#

;;;----------------------------------------------------------------------
;;;                         calling 
;;;----------------------------------------------------------------------


#||
(make-shape-sequence-regression-data-filenames
 "mds/investigations/merge-hi-tables/seq-t9a-mod27.save"
 "mds/data/all-seq/2004-02/dutch-only-and-ag-map-only-for-ms-dna.fas"
 "/tmp/redo2")

;;(cp "mds/investigations/merge-hi-tables/seq-t9a-mod27.save" "/tmp/save")
;;(cp "mds/data/all-seq/2004-02/dutch-only-and-ag-map-only-for-ms-dna.fas" "/tmp/fas")


(regression-routine-diagnostics 
 "mds/investigations/shape-sequence/antigenic-genetic-t9a-mod27-substitutions-and-positions-upto-10-mutations.lisp"
 "/tmp/regression4"
 :train-test-set-proportion 0.9
 :unbias-alist *3-mutations-for-one-2-fold-bias*)

and put into the gui (almost, the below completes)
        ### todo: get other parameters (listed below) into the selection window
        ###       make the selection window stay up until we say OK (look in demos for how to so this)
        ###       get running on windows too (needs the / and \ in filenames, and R)

could also put the save and fas we were called with into the routine diagnostics page, and also the cvs file for excel

|# 




;;;----------------------------------------------------------------------
;;;                    routine regression comparison
;;;----------------------------------------------------------------------

#||
;; replaced by the below, making figures without poping up figure on the screen
(defun routine-regression-comparison-lineplots (data 
                                                directory 
                                                &optional &key
                                                          include-se
                                                          (titles    (mapcar #'anything->string (series 1 (length data))))
                                                          (linetypes (series 1 (length data)))
                                                          (hardcopy-fontsize 16)
                                                          plot-filename-without-suffix
                                                          (between-plot-sleep-time 1.5))
  (if data
      (let ((y-min (floor (apply-min (apply-append (enths 3 data 1))))))
        (gnuplot
         (list (list 0 0)
               (list (dec (length (car data))) 0))
         :element-linetype -1
         :x-max (length (car data))
         :y-min y-min
         :x-tics (loop for i below (length (car data)) by 10 collect i))
        (sleep between-plot-sleep-time)
        (gnuplots 
         (append (enths 3 data 1)                  ;; parameter estimates
                 (if include-se (enths 3 data 2))) ;; se
 
         ;;:x-title "aa substitution by order of effect"
         ;;:y-title "Extimated antigenic effect of aa substitution"
         :grid t
         :element-linewidth 4
         ;;:element-style 'points
         :labels (loop for i from 0 
                     for name in (car data) collect
                       ;;(list (string name) i -2.9 :font-size 7 :rotate t)
                       (list (string name) i (+ y-min 0.1) :rotate t))
         :element-name     (^ (i) (nth i (append titles (mapcar (^ (title) (string-append title " SE")) titles))))
         :element-linetype (^ (i) (nth i (append linetypes linetypes)))
         :refresh nil)

        (sleep between-plot-sleep-time)
        (if include-se (sleep between-plot-sleep-time))
        (gnuplot-hardcopy hardcopy-fontsize (format nil "~a/~a.ps" directory plot-filename-without-suffix) 5.0)
        (sleep between-plot-sleep-time)
        (ps-to-png (format nil "~a/~a.ps" directory plot-filename-without-suffix) "")
        (sleep between-plot-sleep-time)
        (gnuplot-exit)
        (sleep between-plot-sleep-time))))
||#

(defun routine-regression-comparison-lineplots (data 
                                                directory 
                                                &optional &key
                                                          include-se
                                                          (titles    (mapcar #'anything->string (series 1 (length data))))
                                                          (linetypes (series 1 (length data)))
                                                          (hardcopy-fontsize 16)
                                                          (plot-scale 1)
                                                          plot-filename-without-suffix
                                                          (between-plot-sleep-time 1.5))
  (let ((ps-filename (format nil "~a/~a.ps" directory plot-filename-without-suffix)))
    (if data
        (let ((y-min (floor (apply-min (apply-append (enths 3 data 1))))))
          (gnuplot
           (list (list 0 0)
                 (list (dec (length (car data))) 0))
           :element-linetype -1
           :x-max (length (car data))
           :y-min y-min
           :x-tics (loop for i below (length (car data)) by 10 collect i)
           :size plot-scale
           :hardcopy-only t :ps-filename ps-filename :hardcopy-fontsize hardcopy-fontsize)
          (sleep between-plot-sleep-time)
          (gnuplots 
           (append (enths 3 data 1)                  ;; parameter estimates
                   (if include-se (enths 3 data 2))) ;; se
 
           ;;:x-title "aa substitution by order of effect"
           ;;:y-title "Extimated antigenic effect of aa substitution"
           :grid t
           :element-linewidth 4
           ;;:element-style 'points
           :labels (loop for i from 0 
                       for name in (car data) collect
                         ;;(list (string name) i -2.9 :font-size 7 :rotate t)
                         (list (string name) i (+ y-min 0.1) :rotate t))
           :element-name     (^ (i) (nth i (append titles (mapcar (^ (title) (string-append title " SE")) titles))))
           :element-linetype (^ (i) (nth i (append linetypes linetypes)))
           :size plot-scale
           :hardcopy-only t :ps-filename ps-filename :hardcopy-fontsize hardcopy-fontsize
           :refresh nil)
          (sleep between-plot-sleep-time)
          (ps-to-png ps-filename "")
          (sleep between-plot-sleep-time)
          (gnuplot-exit)
          (sleep between-plot-sleep-time)
          ))))




(defun routine-regression-comparison-vertical-plots-preprocess (regression-runs 
                                                                &optional &key 
                                                                          substs-representing-collinear-terms
                                                                          (num-spacer-lines-between-sets 4)
                                                                          (sort-method 'alpha-5-char-substs-first)
                                                                          )
  (let* ((unique-entries (my-remove-duplicates (nths 0 (apply-append regression-runs))))
         (sorted-unique-entries
          (case sort-method 
            (alpha-5-char-substs-first
             (cons 'intercept
                   (append 
                    (sort-alpha (collect (^ (e) (= 5 (length (string e)))) unique-entries))
                    (remove 'intercept (sort-alpha (filter  (^ (e) (= 5 (length (string e)))) unique-entries))))))
            (numeric
             (nths 0 (sort-nth 1 (loop for unique-entry in unique-entries collect
                                       (list unique-entry
                                             (car (collect #'numberp 
                                                           (mapcar 
                                                            (^ (regression) (nth 1 (assoc unique-entry regression)))
                                                            regression-runs))))))))
            (none
             (nths 0 (loop for unique-entry in unique-entries collect
                           (list unique-entry
                                 (car (collect #'numberp 
                                               (mapcar 
                                                (^ (regression) (nth 1 (assoc unique-entry regression)))
                                                regression-runs)))))))
            (t (error "Unknown sort-method passed"))))
         (middle-regression-run-index (floor (/ (length regression-runs) 2))))
    (loop for entry in sorted-unique-entries append
          (append
           (loop for regression-run in regression-runs for i from 1 collect
                 (cons (if (= i middle-regression-run-index)
                           (if (member entry substs-representing-collinear-terms :test (^ (a b) (string-member (string b) (string a))))
                               ;; append -c to indicate subst represents a collinear set
                               (read-from-string (string-append (string entry) "_C"))
                             entry)
                         ".")
                       (if (assoc entry regression-run)
                           (cdr (assoc entry regression-run))
                         '(-10 0.001))))
                   
           (loop for i below num-spacer-lines-between-sets collect
                 '("" -10 0.001)))
          )))


(defun write-regression-comparison-page (directory 
                                         &optional &key 
                                                   (experiment-title "")
                                                   (if-exists-action :error))
  (with-open-file (out (format nil "~a/index.html" directory) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>~a<br><FONT SIZE=-2>(Regression automated comparison diagnostics version 0.0)</FONT></CENTER></H1>" experiment-title)
    (newline out)
    (format out "<PRE>")
    (newline out)

    (format out "
<h3>Comparison of all parameter estimates</h3>
<A href=\"vertical-alpha.ps\">sorted alpha ps</A>  
<A href=\"vertical-alpha.pdf\">sorted alpha pdf</A>
<IMG src=vertical-alpha.png alt=vertical-alpha.png>

<A href=\"vertical-numeric.ps\">sorted numeric ps</A>         
<A href=\"vertical-numeric.pdf\">sorted numeric pdf</A>       
<IMG src=vertical-numeric.png alt=vertical-numeric.png>       

<A href=\"vertical-no-sort.ps\">sorted no-sort ps</A>         
<A href=\"vertical-no-sort.pdf\">sorted no-sort pdf</A>       
<IMG src=vertical-no-sort.png alt=vertical-no-sort.png>       


<hr><h3>Comparison of parameter estimates common to all regressions</h3>
<A href=\"common-vertical-alpha.ps\">sorted alpha ps</A>  
<A href=\"common-vertical-alpha.pdf\">sorted alpha pdf</A>
<IMG src=common-vertical-alpha.png alt=vertical-alpha.png>

<A href=\"common-vertical-numeric.ps\">sorted numeric ps</A>         
<A href=\"common-vertical-numeric.pdf\">sorted numeric pdf</A>       
<IMG src=common-vertical-numeric.png alt=vertical-numeric.png>       

<A href=\"common-vertical-no-sort.ps\">sorted no-sort ps</A>         
<A href=\"common-vertical-no-sort.pdf\">sorted no-sort pdf</A>       
<IMG src=common-vertical-no-sort.png alt=vertical-no-sort.png>       

<IMG src=lineplot.png alt=lineplot.png>   <IMG src=lineplot-se.png alt=lineplot-se.png>
<hr><h3>Same as above, but excluding collinear terms</h3>
<IMG src=lineplot-non-collinear.png alt=lineplot-non-collinear.png>   <IMG src=lineplot-non-collinear-se.png alt=lineplot-non-collinear-se.png>
"
            )

    (newline out)
    (format out "</PRE>")
    (newline out)))


(defun infer-model-from-regression-run-output (regression-run)
  (let ((names (nths 0 regression-run)))
    (cond ((equal '(intercept bias theta) (my-intersection '(intercept bias theta) names)) "nls-ssmodel0i")
          ((equal '(          bias theta) (my-intersection '(          bias theta) names)) "nls-ssmodel0")
          (t "lm"))))


(defun routine-regression-comparison (directory 
                                      regressions
                                      &optional &key
                                                substs-representing-collinear-terms
                                                (experiment-title "")
                                                (titles    (mapcar #'anything->string (series 1 (length regressions))))
                                                (linetypes (series 1 (length regressions)))
                                                (num-spacer-lines-between-sets 2)
                                                (hardcopy-fontsize 16)
                                                (between-plot-sleep-time 1.0)
                                                (pop-up-web-page t)
                                                exclude-collinear-estimates
                                                models
                                                include-intercept
                                                ;; of none of the below are specificed, then make all plots
                                                vertical-alpha
                                                vertical-numeric
                                                vertical-no-sort
                                                common-vertical-alpha
                                                common-vertical-numeric
                                                common-vertical-no-sort
                                                lineplot
                                                lineplot-se
                                                lineplot-non-collinear
                                                lineplot-non-collinear-se)

  (let ((make-all-plots (not (or vertical-alpha
                                 vertical-numeric
                                 vertical-no-sort
                                 common-vertical-alpha
                                 common-vertical-numeric
                                 common-vertical-no-sort
                                 lineplot
                                 lineplot-se
                                 lineplot-non-collinear
                                 lineplot-non-collinear-se))))

    ;; TODO
    ;;   - add averages

    ;;----------------- parameter checks/preamble -------------------------------------

    (if titles
        (if (not (= (length regressions) (length titles)))
            (error "The number of regressions (~d) and number of titles (~d) must match, but does not" (length regressions) (length titles))))
  
    (if (file-or-directory-exists-p directory)
        (if (not (directoryp directory))
            (error "Directory (~a) specified for output is not a directory, but an existing file." directory))
      (mkdir directory))
  
    (setq regressions
      (loop for regression in regressions collect
            (cond ((listp regression)    regression)
                  ((stringp regression)  (parse-r-regression-output-file 
                                          regression
                                          :compose-collinear-columns t
                                          :show-t-and-p-values       nil))
                  (t                     (error "Expected regression parameter to be a list of regression parameter estimates, or an lm summary filename"))
                  )))


  ;; infer models
    (setq models
      (if (null models)
          (mapcar #'infer-model-from-regression-run-output regressions)))

    (if include-intercept
        (setq regressions
          (loop for regression in regressions 
              for model in models collect
                (loop for (name estimate se) in regression collect
                      (progn
                        estimate  ;; to stop compiler warning
                        (list name 
                              (parameter-estimate-including-intercpet name regression model)
                              se))))))  ;; the se should be adjusted, don't however know how to adjust for the non-linear models
                          
    (if exclude-collinear-estimates
        (setq regressions
          (loop for regression in regressions collect
                (loop for pe-line in regression
                    when (not (plus-minus-determinant-collinear-name-p (nth 0 pe-line)))
                    collect pe-line))))



    ;;----------------------- plot all data in ways that do not require the data to be fully paired ---------------------------


    ;;----- subset the data so it is fully paired for easier plots to compare, and easier comparison of overlapping data  -----


    ;; the average standard errors:
    ;;(mapcar #'av (mapcar (^ (l) (nths 2 l)) raws))
    ;;(0.2612919 0.26708135 0.2272727) 

    ;;(0.33681822 0.3439091 0.30113637)





    (if (or make-all-plots vertical-alpha)
        (let* ((processed-regression-runs (reverse 
                                           (routine-regression-comparison-vertical-plots-preprocess
                                            regressions
                                            :sort-method 'alpha-5-char-substs-first
                                            :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                            :substs-representing-collinear-terms substs-representing-collinear-terms))))
          (regression-coefficients-plot 
           processed-regression-runs
           nil 
           :value-to-ignore-when-calculating-parameter-values-range -10
           ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
           ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
           :plot-filename-without-suffix (format nil "~a/~a" directory "vertical-alpha")
           :x-size 6
           :ratio 5
           :element-linewidth 0.25
           :hardcopy-fontsize 10)
          (sleep between-plot-sleep-time)))

    
    (if (or make-all-plots vertical-numeric)
        (let* ((processed-regression-runs (reverse 
                                           (routine-regression-comparison-vertical-plots-preprocess
                                            regressions
                                            :sort-method 'numeric
                                            :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                            :substs-representing-collinear-terms substs-representing-collinear-terms))))
          (regression-coefficients-plot 
           processed-regression-runs
           nil 
           :value-to-ignore-when-calculating-parameter-values-range -10
           ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
           ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
           :plot-filename-without-suffix (format nil "~a/~a" directory "vertical-numeric")
           :x-size 6
           :ratio 5
           :element-linewidth 0.25
           :hardcopy-fontsize 10)
          (sleep between-plot-sleep-time)))
    
    
    (if (or make-all-plots vertical-no-sort)
        (let* ((processed-regression-runs (reverse 
                                           (routine-regression-comparison-vertical-plots-preprocess
                                            regressions
                                            :sort-method 'none
                                            :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                            :substs-representing-collinear-terms substs-representing-collinear-terms))))
          (regression-coefficients-plot 
           processed-regression-runs
           nil 
           :value-to-ignore-when-calculating-parameter-values-range -10
           ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
           ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
           :plot-filename-without-suffix (format nil "~a/~a" directory "vertical-no-sort")
           :x-size 6
           :ratio 5
           :element-linewidth 0.25
           :hardcopy-fontsize 10)
          (sleep between-plot-sleep-time)))



    ;; ----------------------- match parameter estimates -------------------------------

    (let* ((all-estimates    (apply #'nary-union        (mapcar (^ (regression) (nths 0 regression)) regressions)))
           (common-estimates (apply #'nary-intersection (mapcar (^ (regression) (nths 0 regression)) regressions)))
           (common-estimate-regression-subsets (loop for regression in regressions collect
                                                     (loop for common-estimate in common-estimates collect
                                                           (assoc common-estimate regression)))))

      all-estimates
    
      ;; plot sorted by effect size, requires all parameter estimates to be matched.
      (if (or make-all-plots lineplot)
          (routine-regression-comparison-lineplots
           (apply-transpose (sort-nth 1 (mapcar #'apply-append (apply-transpose common-estimate-regression-subsets))))
           directory 
           :include-se                   nil
           :titles                       titles
           :linetypes                    linetypes
           :hardcopy-fontsize            hardcopy-fontsize
           :plot-filename-without-suffix "lineplot"
           :plot-scale                   4
           :between-plot-sleep-time      between-plot-sleep-time))
  
      (if (or make-all-plots lineplot-se)
          (routine-regression-comparison-lineplots
           (apply-transpose (sort-nth 1 (mapcar #'apply-append (apply-transpose common-estimate-regression-subsets))))
           directory 
           :include-se                   t
           :titles                       titles
           :linetypes                    linetypes
           :hardcopy-fontsize            hardcopy-fontsize
           :plot-filename-without-suffix "lineplot-se"
           :plot-scale                   4
           :between-plot-sleep-time      between-plot-sleep-time))


      (if (or make-all-plots lineplot-non-collinear)
          (routine-regression-comparison-lineplots
           (apply-transpose 
            (collect
             (^ (l)
                (= 5 (length (string (car l)))))
             (sort-nth 1 (mapcar #'apply-append (apply-transpose common-estimate-regression-subsets)))))
           directory 
           :include-se                   nil
           :titles                       titles
           :linetypes                    linetypes
           :hardcopy-fontsize            hardcopy-fontsize
           :plot-filename-without-suffix "lineplot-non-collinear"
           :plot-scale                   4
           :between-plot-sleep-time      between-plot-sleep-time))
  
      (if (or make-all-plots lineplot-non-collinear-se)
          (routine-regression-comparison-lineplots
           (apply-transpose 
            (collect
             (^ (l)
                (= 5 (length (string (car l)))))
             (sort-nth 1 (mapcar #'apply-append (apply-transpose common-estimate-regression-subsets)))))
           directory 
           :include-se                   t
           :titles                       titles
           :linetypes                    linetypes
           :hardcopy-fontsize            hardcopy-fontsize
           :plot-filename-without-suffix "lineplot-non-collinear-se"
           :plot-scale                   4
           :between-plot-sleep-time      between-plot-sleep-time))


      (if (or make-all-plots common-vertical-alpha)
          (let* ((processed-regression-runs (reverse 
                                             (routine-regression-comparison-vertical-plots-preprocess
                                              common-estimate-regression-subsets
                                              :sort-method 'alpha-5-char-substs-first
                                              :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                              :substs-representing-collinear-terms substs-representing-collinear-terms))))
            (regression-coefficients-plot 
             processed-regression-runs
             nil 
             :value-to-ignore-when-calculating-parameter-values-range -10
             ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
             ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
             :plot-filename-without-suffix (format nil "~a/~a" directory "common-vertical-alpha")
             :x-size 6
             :ratio 5
             :element-linewidth 0.25
             :hardcopy-fontsize 10)
            (sleep between-plot-sleep-time)))
    
      (if (or make-all-plots common-vertical-numeric)
          (let* ((processed-regression-runs (reverse 
                                             (routine-regression-comparison-vertical-plots-preprocess
                                              common-estimate-regression-subsets
                                              :sort-method 'numeric
                                              :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                              :substs-representing-collinear-terms substs-representing-collinear-terms))))
            (regression-coefficients-plot 
             processed-regression-runs
             nil 
             :value-to-ignore-when-calculating-parameter-values-range -10
             ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
             ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
             :plot-filename-without-suffix (format nil "~a/~a" directory "common-vertical-numeric")
             :x-size 6
             :ratio 5
             :element-linewidth 0.25
             :hardcopy-fontsize 10)
            (sleep between-plot-sleep-time)))
    
      (if (or make-all-plots common-vertical-no-sort)
          (let* ((processed-regression-runs (reverse 
                                             (routine-regression-comparison-vertical-plots-preprocess
                                              common-estimate-regression-subsets
                                              :sort-method 'none
                                              :num-spacer-lines-between-sets num-spacer-lines-between-sets
                                              :substs-representing-collinear-terms substs-representing-collinear-terms))))
            (regression-coefficients-plot 
             processed-regression-runs
             nil 
             :value-to-ignore-when-calculating-parameter-values-range -10
             ;; would like the below to work, but is not drawing the grid lines when there is a largish number, i think, rely on standard grid for now
             ;;:lines (print (series 0 (dec (length processed-regression-runs)) (+ (length regressions) num-spacer-lines-between-sets)))
             :plot-filename-without-suffix (format nil "~a/~a" directory "common-vertical-no-sort")
             :x-size 6
             :ratio 5
             :element-linewidth 0.25
             :hardcopy-fontsize 10)
            (sleep between-plot-sleep-time)))
        
      )



    (write-regression-comparison-page
     directory
     :experiment-title experiment-title)

    (let ((open-command (format nil "open ~a/index.html" directory)))
      (if pop-up-web-page (run-shell-command open-command))
      `(run-shell-command ,open-command))
    ))


  

#|
(routine-regression-comparison 
 "/tmp/test"
 (list
  "~/mds/src/mds/mds/investigations/shape-sequence/no-distance-unbias/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/sequence-based-distance-unbias/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/2d-mle-distance-unbias/script-output.txt")
 :experiment-title "test"
 :titles (list "x" "y" "z"))

(routine-regression-comparison 
 "/tmp/test10"
 (list
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-forward-step-all-non-collinear-summary.txt")
 :experiment-title "test"
 :titles (list "baseline" "step all non-collinears"))

(routine-regression-comparison 
 "/tmp/test22"
 (list
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-forward-step-all-non-collinear-summary.txt")
 :experiment-title "test"
 :titles (list "step all non-collinears"))


(routine-regression-comparison 
 "/tmp/testxxx"
 (list
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed-culled-0-lt-n-cols-removed/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed-culled-1-lt-n-cols-removed/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed-culled-2-lt-n-cols-removed/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed-culled-3-lt-n-cols-removed/script-output.txt"
  "~/mds/src/mds/mds/investigations/shape-sequence/step/35yr-collinear-removed-culled-4-lt-n-cols-removed/script-output.txt")
 :experiment-title "test"
 :titles (list "Culled 0"
               "Culled 1"
               "Culled 2"
               "Culled 3"
               "Culled 4"))


|#
