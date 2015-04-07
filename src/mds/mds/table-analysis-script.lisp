(in-package user)

;;;----------------------------------------------------------------------
;;;                    Automated table reporting
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;                      Color coding of maps  
;;;----------------------------------------------------------------------

(defun color-code-cdc-save-blue-reference-ags (save)
  (if (plot-spec-from-save save)
      (error "did not expect plot spec in save form, have not coded for it yet")
    (let* ((table (table-from-save save))
	   (names (hi-table-antigens table))
	   (reference-antigens (reference-antigens-from-save save)))
      (set-plot-spec-in-save
       save
       (loop for name in names collect
	     (append (list name)
		     (cond ((serum-name-p name) 
			    '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray50"))
			   ((member (remove-ag-sr-from-name name) reference-antigens)
			    '(:ds 6 :wn "" :ns 12 :nc "black" :co "blue"))
			   (t 
			    '(:ds 4 :wn "" :ns 10 :nc "black" :co "gray90"  :oc "gray40")))))
       :not-found-action :add))))

(defun color-code-reference-antigen-save (save)
  (if (plot-spec-from-save save)
      (error "did not expect plot spec in save form, have not coded for it yet")
    (let* ((table (table-from-save save))
	   (names (hi-table-antigens table))
	   (reference-antigens (reference-antigens-from-save save)))
      (set-plot-spec-in-save
       save
       (loop for name in names collect
	     (append (list name)
		     (cond ((serum-name-p name) 
			    '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray50"))
			   ((member (remove-ag-sr-from-name name) reference-antigens)
			    '(:ds 6 :wn "" :ns 12 :nc "black"))
			   (t 
			    '(:ds 4 :wn "" :ns 10 :nc "black" :co "gray90"  :oc "gray40")))))
       :not-found-action :add))))

(defun color-code-epi-strong-reference-background-save (save &optional &key (reference-strains 'not-passed))
  (let* ((table (table-from-save save))
	 (names (hi-table-antigens table))
	 (reference-antigens (if (eql 'not-passed reference-strains)
				 (reference-antigens-from-save save)
			       reference-strains)))
    ;; ignore the existing plotspec
    (set-plot-spec-in-save
     save
     (loop for name in names collect
	   (append (list name)
		   (cond ((serum-name-p name) 
			  '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70"))
			 ((member (remove-ag-sr-from-name name) reference-antigens)
			  '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70"))
			 (t ;; assume is epi strain
			  '(:ds 4 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
     :not-found-action :add)))

(defun color-code-epi-strong-reference-background-save-2 (save &optional &key (reference-strains 'not-passed))
  (let* ((table (table-from-save save))
	 (names (hi-table-antigens table))
	 (reference-antigens (if (eql 'not-passed reference-strains)
				 (reference-antigens-from-save save)
			       reference-strains)))
    ;; ignore the existing plotspec
    (set-plot-spec-in-save
     save
     (loop for name in names collect
	   (append (list name)
		   (cond ((serum-name-p name) 
			  '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray60"))
			 ((member (remove-ag-sr-from-name name) reference-antigens)
			  '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray30"))
			 (t ;; assume is epi strain
			  '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
     :not-found-action :add)))

(defun color-code-epi-strong-reference-background-save-3 (save &optional &key (reference-strains 'not-passed))
  (let* ((table (table-from-save save))
	 (names (hi-table-antigens table))
	 (reference-antigens (if (eql 'not-passed reference-strains)
				 (reference-antigens-from-save save)
			       reference-strains)))
    ;; ignore the existing plotspec
    (set-plot-spec-in-save
     save
     (loop for psline in (plot-spec-from-save save) collect
	   (cond ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		       (serum-name-p (car psline)))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray60")))
		 ((equal "gray70" (snoop-keyword-arg :oc psline))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray30")))
		 (t ;; assume is epi strain
		  (cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
     :not-found-action :add)))

(defun color-code-epi-strong-reference-background-save-4 (save &optional &key (reference-strains 'not-passed))
  (let* ((table (table-from-save save))
	 (names (hi-table-antigens table))
	 (reference-antigens (if (eql 'not-passed reference-strains)
				 (reference-antigens-from-save save)
			       reference-strains)))
    ;; ignore the existing plotspec
    (set-plot-spec-in-save
     save
     (loop for psline in (plot-spec-from-save save) collect
	   (cond ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		       (serum-name-p (car psline)))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "black")))
		 ((equal "gray70" (snoop-keyword-arg :oc psline))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "black")))
		 (t ;; assume is epi strain
		  (cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
     :not-found-action :add)))

;; ~/mds/investigations/strain-selection-meeting/database/csl/investigations/overlay-merge/20040912-ref-ag-merge/overlay-merge-part-2-save-raw.save

(defvar *wellingtons*)
(setq *wellingtons* '(WE/1/04-AG            ;; 20040609 many times 
		      WE/1-AC-AG01/04-AG    ;; 20040609 only once
		      WE/1-AC-AG04/04-AG    ;; 20040826 only once
		      WE/1-AC-AG05/04-AG))  ;; 20040826 only once


(defvar *n145*)
(setq *n145* 
  '(hy/219/03-ag
    nz/635/04-ag
    ba/24/04-ag
    ml/1235/04-ag
    ml/1344/04-ag
    dw/1/04-ag
    sp/38/04-ag
    ml/1522/04-ag
    cc/10/04-ag
    pj/1411/04-ag

    cn-rv578/04
    hk/15/04
    hn/204/04
    md-v84003
    sp/21/04
    tw-1554
    va-w56472
    ))

(defun color-code-reference-background-strongest-marked-color-spec (save &optional &key 
										   marked 
										   color-spec)
  (let ((color-spec-names (nths 0 color-spec)))
    (set-plot-spec-in-save
     save
     (loop for psline in (plot-spec-from-save save) collect
	   (cond ((member (car psline) marked)
		  (cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
		 ((and (equal "gray70" (snoop-keyword-arg :oc psline :not-found-action :return-nil))
		       (serum-name-p (car psline)))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
		 ((equal "gray70" (snoop-keyword-arg :oc psline :not-found-action :return-nil))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
		 ;; this explicitly goes after the reference (gray70) strains, otherwise, the reference strains hang
		 ;; around in a timeseries, and are there colored
		 ((member (car psline) color-spec-names)
		  (cons (car psline) (list :ds 5 :wn "" :ns 12 :nc "black" :co (assoc-value-1 (car psline) color-spec) :oc "black")))
		 (t ;; assume is epi strain, and has no color
		  (cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "{}"  :oc "gray70")))))
     :not-found-action :add)))


(defun color-code-epi-strong-reference-background-save-4-N145 (save &optional &key (reference-strains 'not-passed))
  (let* ((table (table-from-save save))
	 (names (hi-table-antigens table))
	 (reference-antigens (if (eql 'not-passed reference-strains)
				 (reference-antigens-from-save save)
			       reference-strains)))
    ;; ignore the existing plotspec
    (set-plot-spec-in-save
     save
     (loop for psline in (plot-spec-from-save save) collect
	   (cond ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		       (serum-name-p (car psline)))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "black")))
		 ((member (car psline) '(WE/1/04-AG))
		  (cons (car psline) '(:ds 6 :wn "" :ns 14 :nc "black" :co "BLUE" :oc "black" :wn "WE/1/04")))
		 ((member (car psline) '(CC/28/03-AG))
		  (cons (car psline) '(:ds 6 :wn "" :ns 14 :nc "black" :co "ORANGE" :oc "black" :wn "CC/28/03")))
		 ((member (car psline) *wellingtons*)
		  (cons (car psline) '(:ds 4 :wn "" :ns 12 :nc "black" :co "BLUE" :oc "black")))
		 ((member (car psline) *n145*)
		  (cons (car psline) '(:ds 6 :wn "" :ns 14 :nc "black" :co "red" :oc "black")))
		 ((equal "gray70" (snoop-keyword-arg :oc psline))
		  (cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "black")))
		 (t ;; assume is epi strain
		  (cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
     :not-found-action :add)))

(defun color-code-epi-strong-reference-background-strongest-marked (save &optional &key 
										   (reference-strains 'not-passed) 
										   marked 
										   intermediate-marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
	       ((member (car psline) intermediate-marked)
		(cons (car psline) (list :ds 8 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 14 :nc "black" :co "RED" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
   :not-found-action :add))

(defun color-code-epi-medium-reference-background-strongest-marked (save &optional &key (reference-strains 'not-passed) marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "#80ff80"  :oc "black")))))
   :not-found-action :add))

(defun color-code-epi-strong-reference-background-strongest-marked-larger (save &optional &key (reference-strains 'not-passed) marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 12 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 28 :nc "black" :co "BLUE" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
   :not-found-action :add))

(defun color-code-epi-strong-4-reference-background-strongest-marked-larger (save &optional &key (reference-strains 'not-passed) marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 12 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 28 :nc "black" :co "BLUE" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(cons (car psline) '(:ds 4 :wn "" :ns 12 :nc "black" :co "green"  :oc "black")))))
   :not-found-action :add))

(defun color-symbols-in-save (save &optional &key name-color-pairs (not-found-action :error))
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) (nths 0 name-color-pairs))
		(cons (car psline)
		      (subst-keyword-arg :co (cdr psline) (assoc-value-1 (car psline) name-color-pairs) 
					 :not-found-action not-found-action)))
	       ((assoc 'default name-color-pairs)
		(cons (car psline)
		      (subst-keyword-arg :co (cdr psline) (assoc-value-1 'default name-color-pairs) 
					 :not-found-action not-found-action)))
	       (t psline)))))

(defun color-names-in-save (save &optional &key name-color-pairs (not-found-action :error))
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) (nths 0 name-color-pairs))
		(cons (car psline)
		      (subst-keyword-arg :nc (cdr psline) (assoc-value-1 (car psline) name-color-pairs) 
					 :not-found-action not-found-action)))
	       (t psline)))))

(defun color-symbol-outlines-in-save (save &optional &key name-color-pairs (not-found-action :error))
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) (nths 0 name-color-pairs))
		(cons (car psline)
		      (subst-keyword-arg :oc (cdr psline) (assoc-value-1 (car psline) name-color-pairs) 
					 :not-found-action not-found-action)))
	       (t psline)))))

(defun turn-on-names-in-save (save antigens-to-name)
  (set-plot-spec-in-save
   save
   (loop for plot-spec-line in (plot-spec-from-save save) collect
	 (let ((name (car plot-spec-line)))
	   (cond ((member name antigens-to-name)
		  (subst-keyword-arg :wn plot-spec-line name))
		 (t 
		  plot-spec-line))))
   :not-found-action :add))

(defun turn-on-reference-names-in-save (save)
  (turn-on-names-in-save save (mapcar #'suffix-as-ag (reference-antigens-from-save save))))

(defun rename-working-names-in-save (save &optional &key name-workingname-pairs (not-found-action :error))
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) (nths 0 name-workingname-pairs))
		(cons (car psline)
		      (subst-keyword-arg :wn (cdr psline) (assoc-value-1 (car psline) name-workingname-pairs) 
					 :not-found-action not-found-action)))
	       (t psline)))))

(defun outline-color-names-in-save (save names color-or-colors)
  (let ((colors (if (atom color-or-colors)
		    (loop for ignore in names collect (progn ignore color-or-colors))
		  color-or-colors)))
    (if (not (= (length colors) (length names)))
	(error "there must be the same number of names and colors provided to outline-color-names-in-save~%"))
    (set-plot-spec-in-save
     save
     (loop for plot-spec-line in (plot-spec-from-save save) collect
	   (let ((name (car plot-spec-line)))
	     (cond ((member name names)
		    (subst-keyword-arg :oc plot-spec-line (nth (position name names) colors)))
		   (t 
		    plot-spec-line))))
     :not-found-action :add)))

(defun dark-outline-reference-antigen-names-in-save (save &optional (color "gray30"))
  (outline-color-names-in-save save (mapcar #'suffix-as-ag (reference-antigens-from-save save)) color))


;;;----------------------------------------------------------------------
;;;        processing saves, marking, and displaying
;;;----------------------------------------------------------------------

(defun reorient-and-color-save (save master-save &optional &key marked-strains)
  (let ((save 
         (set-reference-antigens-in-save
          (orient-slave-save-onto-master-save
           save
           master-save)
          (reference-antigens-from-save master-save)
          :not-found-action :add)))
    (let* ((colored-save (rename-working-names-in-save
                          (color-names-in-save
                           (color-symbols-in-save
                            (color-code-epi-strong-4-reference-background-strongest-marked-larger
                             (color-code-epi-strong-reference-background-save
                              save)
                             :marked marked-strains)
                            :name-color-pairs '(;;(OY/29/05-ag "yellow")
                                                ))
                           :name-color-pairs '(;;(OY/29/05-ag "{}")
                                               ))   
                          :name-workingname-pairs '(;;(HF/52/05-ag HR/52/05)
                                                    )))
           (strains-to-lower (loop for psline in (plot-spec-from-save colored-save)
                                 when (equal (snoop-keyword-arg :co psline) "gray50")
                                 collect (car psline))))
      (blank-save
       (set-raise-points-in-save 
        (set-lower-points-in-save
         colored-save
         strains-to-lower :not-found-action :add)
        marked-strains :not-found-action :add)))))


(defun show-save (save)
  (let ((tk (eval (blank-save save))))

    (sleep 1)

    (mds-hi-table 
     tk
     'metric-mds-global-norm-conjugant-gradient
     'square
     nil
     :existing-mds-window nil
     :num-trials 0
     :num-climbs 0)
  
    save))


;;;----------------------------------------------------------------------
;;;                      overlay merge support
;;;----------------------------------------------------------------------

(defun overlay-merge-save-ssm (saves
                               previous-overlay-merge-save-filename
                               this-overlay-merge-save-directory)
  (let* ((previous-overlay-merge-save (fi-in previous-overlay-merge-save-filename))
	 (new-overlay-merge-save
	  (color-difference-between-saves
	   (color-code-epi-strong-reference-background-save 
	    (set-coordss-in-save-that-intersect-with-other-save
	     (overlay-merge-saves
	      saves
	      :save-to-orient-to previous-overlay-merge-save
	      :table-output-filename (format nil "~a/overlay-merge-diagnotics.txt" this-overlay-merge-save-directory))
	     previous-overlay-merge-save))
	   previous-overlay-merge-save)))
    (write-save-form
     new-overlay-merge-save
     (format nil "~a/ompc.save" this-overlay-merge-save-directory))  ;; overlay-merge-previous-coordss
    (fi
     (my-set-difference 
      (hi-table-antigens 
       (table-from-save 
	new-overlay-merge-save))
      (hi-table-antigens
       (table-from-save 
	previous-overlay-merge-save)))
     (format nil "~a/new-strain-names" this-overlay-merge-save-directory))))


;;;----------------------------------------------------------------------
;;;                        saving results
;;;----------------------------------------------------------------------

(defun store-save-of-table-only (save directory) 
  (write-save-form 
   save ;; (make-save-form :hi-table (table-from-save save))
   (string-append directory "/save-of-table-only.save")))
(defun store-save-after-runs (save directory)
  (write-save-form 
   save
   (string-append directory "/save-after-runs.save")))

(defun store-sfi-format-of-table-only (save directory)
  (save-to-lapedes-file 
   (make-save-form :hi-table (table-from-save save))
   (string-append directory "/sfi-of-table-only.sfi")))
(defun store-sfi-format-after-runs (save directory) 
  (save-to-lapedes-file 
   save
   (string-append directory "/sfi-after-runs.sfi")))

(defun store-pretty-print-of-table (save directory)
  (pp-hi-table 
   (let ((table (table-from-save save)))
     (if (ag-sr-table-p table)
	 (un-asl-hi-table table)
       table))
   'short
   4
   nil
   :filename (string-append directory "/pp-table.txt")))
   
(defun store-pretty-print-of-stresses (save directory)
  (fll 
   (mapcar #'list (mapcar (^ (x) (if (numberp x) (dps x 4) x)) (batch-run-stresses-from-save save)))  ;; might not be a number if it was a dropped gridware run
   :filename (string-append directory "/pp-stresses.txt")))

(defun store-average-procrustes-differences (save directory &optional &key num-solutions-to-procrustes)
  (fll 
   (procrustes-among-runs save :num-runs num-solutions-to-procrustes)
   :filename (string-append directory "/average-procrustes-differences.txt")))

(defun store-save-after-runs-reoriented (save 
					 directory
					 master-orientation-save)
  (write-save-form 
   (orient-slave-save-onto-master-save save master-orientation-save)
   (string-append directory "/save-after-runs-reoriented.save")))

(defun store-lowest-stress-run-images (save
				       directory
				       master-orientation-save
				       &optional &key 
						 num-runs
						 suffix)
  (loop for i below num-runs do
	(orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
	 (blank-save (set-nth-best-batch-as-starting-coordss-in-save i save))
	 master-orientation-save
	 (format nil "~a/stress-~d~a" directory i suffix))))

(defun store-lowest-stress-run-images-with-procrustes-of-lowest-stress-run (save
									    directory
									    master-orientation-save
									    &optional &key 
										      num-runs
										      (arrow-color "#333333"))
  (loop for i below num-runs do
	(orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
	 (blank-save (set-nth-best-batch-as-starting-coordss-in-save i save))
	 master-orientation-save
	 (format nil "~a/stress-~d-pc-lowest-stress-run" directory i)
	 :map-window-hook (^ (mds-window) (map-save-on-to-mds-window mds-window save :arrow-color arrow-color)))))

(defun store-lowest-stress-run-images-with-procrustes-of-master-save (save
								      directory
								      master-orientation-save
								      &optional &key 
										num-runs
										(arrow-color "#333333"))
  (loop for i below num-runs do
	(orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
	 (blank-save (set-nth-best-batch-as-starting-coordss-in-save i save))
	 master-orientation-save
	 (format nil "~a/stress-~d-pc-master" directory i)
	 :map-window-hook 
	 (^ (mds-window) (map-save-on-to-mds-window mds-window master-orientation-save :arrow-color arrow-color)))))

(defun store-lowest-stress-run-images-with-procrustes-of-other-save (save
								     directory
								     other-save
								     other-save-id
								     &optional &key 
									       num-runs
									       (arrow-color "#333333"))
  (loop for i below num-runs do
	(orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
	 (blank-save (set-nth-best-batch-as-starting-coordss-in-save i save))
	 other-save
	 (format nil "~a/stress-~d-pc-~a" directory i other-save-id)
	 :map-window-hook 
	 (^ (mds-window) (map-save-on-to-mds-window mds-window other-save :arrow-color arrow-color)))))

(defun store-run-image-with-procrustes-of-other-save (save
						      directory
						      other-save
						      other-save-id
						      &optional &key
								name-subset    ;; can be 'ag-only or sr-only
								(arrow-color "#333333"))
  (orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
   save
   other-save
   (format nil "~a/pc-~a" directory other-save-id)
   :map-window-hook 
   (^ (mds-window) (map-save-on-to-mds-window mds-window other-save :name-subset name-subset :arrow-color arrow-color))
   :name-subset name-subset))



;;;----------------------------------------------------------------------
;;;                       batch script
;;;----------------------------------------------------------------------

(defun initial-table-analsysis (save directory &optional &key
							(num-runs 10)
							(num-dimensions 2)
							(dim-anneal-starting-coords 5)
							(num-solutions-to-procrustes 5)
							(master-orientation-save-filename 
							 "mds/investigations/strain-selection-meeting/master-save-detail.save")
							)

  (store-save-of-table-only save directory)
  (store-sfi-format-of-table-only save directory)
  (store-pretty-print-of-table save directory)

  (let ((save-after-runs (first-or-more-batch-mds-runs-from-save-return-save
			  num-runs
			  save
			  :starting-coordss              dim-anneal-starting-coords
			  :dim-anneal-f                  #'lisp-dim-anneal-two-phase
			  :dim-anneal-starting-dimension dim-anneal-starting-coords
			  :dim-anneal-ending-dimension   num-dimensions)))
    
    (store-pretty-print-of-stresses save-after-runs directory)
    (store-save-after-runs save-after-runs directory)
    (store-sfi-format-after-runs save-after-runs directory)

    (store-average-procrustes-differences save-after-runs directory :num-solutions-to-procrustes num-solutions-to-procrustes)
    
    (let ((master-orientation-save (fi-in master-orientation-save-filename)))
      (store-lowest-stress-run-images-with-procrustes-of-lowest-stress-run 
       save-after-runs
       directory
       master-orientation-save 
       :num-runs num-solutions-to-procrustes)
      (store-lowest-stress-run-images-with-procrustes-of-master-save
       save-after-runs
       directory
       master-orientation-save
       :num-runs num-solutions-to-procrustes))

    ))


#|
;; ------------------------ testing  --------------------------

(pp-hi-table (un-asl-hi-table (table-from-save (fi-in "/tmp/foo.save"))) 'short 4 nil :filename "/tmp/foo.pp2")
(fll (mapcar #'list (mapcar (^ (x) (dps x 4)) (batch-run-stresses-from-save (fi-in "/tmp/foo2.save")))))

(initial-table-analsysis 
 (make-save-form :hi-table (read-hi-table-and-convert "mds/data/HI-2001-08/TAB88.txt" 1 0))
 "/tmp/script-test/")

(initial-table-analsysis 
 (make-save-form :hi-table (read-hi-table-and-convert "mds/data/HI-2001-08/TAB88.txt" 1 0))
 "/tmp/script-test2/"
 :num-runs 3
 :num-solutions-to-procrustes 2)

(initial-table-analsysis 
 (make-save-form :hi-table (read-hi-table-and-convert "mds/data/HI-2001-08/TAB88.txt" 1 0))
 "/tmp/script-test3/"
 :num-runs 10
 :num-solutions-to-procrustes 5)

|#

#|
TODO
  DONE plot spec so reference antigens are large, and colored differently from test ags (all colored same)     
  DONE no names colors maybe red and grey? no save that for sera, use something else
  then a plot with names, a plot with just reference ags have names, a plot with all names    ;; turn-on-reference-names-in-save
  a plot with scale out, so we can see any other outliers 
  a plot of the master save, with the new map on it (all in a particular color, and size)

  el and cl plots, and the dot stress colors (for ron)

  later we can run incrementally more stuff, and just update the index page

  absolute value on connection line shades, and dot stressess

  link to html of the excel sheet, and to the original excel sheet too
     (can all be canned in the .html, and can be put in the dir when the html is)

  maybe first time around we do 3 runs, and 2 displayed, just so we can make a pass, then do 25?

  i am later going to have to look for the low avididty (and might as well high too) strains

|#