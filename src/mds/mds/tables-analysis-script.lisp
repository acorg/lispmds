(in-package user)

;;;----------------------------------------------------------------------
;;;                        ugly hack
;;;----------------------------------------------------------------------

(defvar *ugly-hack-to-convert-tcl-to-ps*)
(setq   *ugly-hack-to-convert-tcl-to-ps* nil)

(defvar *ugly-hack-to-write-to-file-instead-of-wish*)
(setq *ugly-hack-to-write-to-file-instead-of-wish* nil)


;;;----------------------------------------------------------------------
;;;                      rest of code 
;;;----------------------------------------------------------------------

(defun table-analsysis (save directory &optional &key
						 (num-runs 10)
						 (num-dimensions 2)
						 (dim-anneal-starting-coords 5)
						 run-at-sfi-p
						 (bundled-runs-if-run-at-sfi-p t)
						 recover-batch-runs-from-sfi
						 (num-solutions-to-show 5)
						 (master-orientation-save-filename 
						  "mds/investigations/strain-selection-meeting/master-save-detail.save")
						 (save-raw-data t)
						 dont-save-save-of-table-only
						 (batch-runs t)
						 (save-batch-runs t)
						 (batch-runs-graphics-diagnostics t)
						 (batch-runs-graphics-diagnostics-gnuplot t)
						 (panel-map-correlation-plot batch-runs-graphics-diagnostics-gnuplot)
						 (shepard-plot batch-runs-graphics-diagnostics-gnuplot)               
						 )

  (if (and save-raw-data
	   (not dont-save-save-of-table-only))
      (store-save-of-table-only save directory))

  (if save-raw-data
      (progn
	(store-sfi-format-of-table-only save directory)
	(store-pretty-print-of-table save directory)))

  (if (and batch-runs (not save-batch-runs))
      (format t "~%SWarning: doing batch runs, but not saving the results.~%"))

  (let ((save-after-runs (if batch-runs 
			     (apply #'first-or-more-batch-mds-runs-from-save-return-save
				    (if (and run-at-sfi-p
					     bundled-runs-if-run-at-sfi-p)
					1
				      num-runs)
				    save
				    :starting-coordss              dim-anneal-starting-coords
				    :dim-anneal-f                  (if run-at-sfi-p
								       #'lisp-dim-anneal-two-phase-gridware
								     #'lisp-dim-anneal-two-phase)
				    :dim-anneal-starting-dimension dim-anneal-starting-coords
				    :dim-anneal-ending-dimension   num-dimensions
				    (if (and run-at-sfi-p
					     bundled-runs-if-run-at-sfi-p)
					(list :num-runs num-runs)
				      nil))
			   (if recover-batch-runs-from-sfi
			       (recover-gridware-runs-from-save save)			       
			     save))))
    
    (if save-batch-runs
	(if run-at-sfi-p
	    (let ((pending-directory (format nil "~a/pending/" directory)))
	      (if (not (file-or-directory-exists-p pending-directory))
		  (run-shell-command (format nil "mkdir ~a" pending-directory) :wait t))
	      (store-save-after-runs save-after-runs pending-directory))
	  (progn
	    (store-pretty-print-of-stresses save-after-runs directory)
	    (store-save-after-runs save-after-runs directory)
	    (store-sfi-format-after-runs save-after-runs directory)

	    (store-average-procrustes-differences save-after-runs directory :num-solutions-to-procrustes num-solutions-to-show))))
    
    (if (not run-at-sfi-p)   ;; assume we stop the rest of the processing if we submit to sfi (so we don't have to explicitly switch off the rest when we do)

	(progn

	  (if batch-runs-graphics-diagnostics

	      (let ((master-orientation-save (if (not *ugly-hack-to-convert-tcl-to-ps*) 
						 (if master-orientation-save-filename
						     (if (stringp master-orientation-save-filename)
							 (fi-in master-orientation-save-filename)
						       master-orientation-save-filename)  ;; assume is save not filename
						   ;; if no master orientaion supplied, use self 
						   ;; so that lower stress runs get oriented to the lowest stress run.
						   save-after-runs))))

		(if (not *ugly-hack-to-convert-tcl-to-ps*)
		    (store-save-after-runs-reoriented
		     save-after-runs
		     directory
		     master-orientation-save))

		(store-lowest-stress-run-images
		 save-after-runs
		 directory
		 master-orientation-save 
		 :num-runs num-solutions-to-show
		 :suffix "-bland")
      
		(store-lowest-stress-run-images-with-procrustes-of-lowest-stress-run 
		 (turn-on-reference-names-in-save save-after-runs)
		 directory
		 master-orientation-save 
		 :num-runs num-solutions-to-show)

		(store-lowest-stress-run-images-with-procrustes-of-master-save
		 (turn-on-reference-names-in-save save-after-runs)
		 directory
		 master-orientation-save
		 :num-runs num-solutions-to-show)
		
		(if (not *ugly-hack-to-write-to-file-instead-of-wish*)
		    (if (and panel-map-correlation-plot
			     (hi-table-in-save-p save-after-runs))
			(panel-map-distances 
			 save-after-runs
			 :plot t
			 :plot-filename (format nil "~a/panel-map-correlation-plot.png" directory))))
		(if (not *ugly-hack-to-write-to-file-instead-of-wish*)
		    (if (and shepard-plot
			     (hi-table-in-save-p save-after-runs))
			(shepard-plot-data-from-save
			 save-after-runs
			 :plot t
			 :plot-filename (format nil "~a/shepard-plot.png" directory))))

		))
	  
	  ))

    (string-append directory "/index.html")

    ))



;;;----------------------------------------------------------------------
;;;                      running on each table
;;;----------------------------------------------------------------------

(defun process-hi-tables (&optional &key 
				    run-from-table
				    run-to-table
				    tables-to-exclude
				    (save-filename "make-save-form.msf")
				    (save-color-code-f #'color-code-reference-antigen-save)

				    (unpack-database-output t)
				    (make-internal-directory-structure t)
				    (save-raw-data t)
				    save-format-pre-save-f
				    dont-save-save-of-table-only
				    (batch-runs t)
				    (save-batch-runs t)

				    (batch-runs-graphics-diagnostics t)
				    (zip-directory  "/tmp/")
				    (zip-name "data.zip")
				    (zip-unpack-name "data")
				    (root-directory "mds/investigations/strain-selection-meeting/database/LAB/runs/")
				    (name     (numeric-sortable-date))
				    (suffix   "")

				    (num-runs 10)
				    (num-dimensions 2)
				    (dim-anneal-starting-coords 5)

				    run-at-sfi-p
				    (bundled-runs-if-run-at-sfi-p t)

				    recover-batch-runs-from-sfi

				    (num-solutions-to-show 5)
				    (master-orientation-save-filename "mds/investigations/strain-selection-meeting/master-save-detail.save"))

  (if unpack-database-output 
      (unpack-database-output :zip-directory zip-directory :zip-name zip-name :zip-unpack-name zip-unpack-name :root-directory root-directory :name name :suffix suffix))
  (if make-internal-directory-structure
      (make-internal-directory-structure (format nil "~a/~a~a" root-directory name suffix)))
  (table-analyses (format nil "~a/~a~a/batch-processing/" root-directory name suffix)
		  :run-from-table run-from-table
		  :run-to-table   run-to-table
		  :tables-to-exclude tables-to-exclude
		  :save-filename save-filename
		  :save-color-code-f save-color-code-f
		  :num-runs num-runs
		  :num-dimensions num-dimensions
		  :dim-anneal-starting-coords dim-anneal-starting-coords
		  :run-at-sfi-p run-at-sfi-p
		  :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p
		  :recover-batch-runs-from-sfi recover-batch-runs-from-sfi
		  :num-solutions-to-show num-solutions-to-show
		  :master-orientation-save-filename master-orientation-save-filename
		  :save-raw-data save-raw-data
		  :save-format-pre-save-f save-format-pre-save-f
		  :dont-save-save-of-table-only dont-save-save-of-table-only
		  :batch-runs batch-runs   
		  :save-batch-runs save-batch-runs
		  :batch-runs-graphics-diagnostics batch-runs-graphics-diagnostics))

		      
      

(defun unpack-database-output (&optional &key 
					     zip-directory
					     zip-name
					     zip-unpack-name
					     root-directory
					     name
					     suffix)
  (run-shell-command (format nil "unzip ~a/~a -d ~a" zip-directory zip-name zip-directory)
		     :wait t)
  (run-shell-command (format nil "mkdir ~a/~a~a" root-directory name suffix)
		     :wait t)
  (run-shell-command (format nil "mv ~a/~a ~a/~a~a/initial-data" zip-directory zip-unpack-name root-directory name suffix)
		     :wait t))
  

(defun make-internal-directory-structure (directory)
  (run-shell-command (format nil "mkdir ~a/batch-processing" directory) 
		     :wait t)
  (let* ((filenames (progn (run-shell-command (format nil "ls ~a/initial-data/*.msf > /tmp/foo-for-lisp-shell-commands" directory)
					      :wait t)
			   (fi-in-readline "/tmp/foo-for-lisp-shell-commands")))
	 (table-names (mapcar (^ (filename) (substring-before-char #\. (reverse (substring-before-char #\/ (reverse filename))))) filenames)))
    (run-shell-command (format nil "cp -a ~a/../../../index-templates/runs-index.html ~a/" directory directory))
    (write-tables-index-page (string-append directory "/index.html") table-names)
    (loop for table-name in table-names do
	  (progn
	    (run-shell-command (format nil "mkdir ~a/batch-processing/~a" directory table-name)
			       :wait t)
	    (run-shell-command (format nil "cp -a ~a/../../../index-templates/individual-table-index-template.html ~a/batch-processing/~a/index.html" directory directory table-name)
			       :wait t)
	    (run-shell-command (format nil "cp -a ~a/initial-data/~a.html ~a/batch-processing/~a/table.html" directory table-name directory table-name)
			       :wait t)
	    (run-shell-command (format nil "cp -a ~a/initial-data/~a.htm ~a/batch-processing/~a/table.htm" directory table-name directory table-name)
			       :wait t)
	    (run-shell-command (format nil "cp -a ~a/initial-data/~a.msf ~a/batch-processing/~a/make-save-form.msf" directory table-name directory table-name)
			       :wait t)
	    ))))


(defun replicate-internal-directory-structure (root old-run new-run)
  (let* ((runs-directory (runs-directory-from-root root))
	 (old-run-directory (string-append runs-directory "/" old-run))
	 (new-run-directory (string-append runs-directory "/" new-run)))
    (run-shell-command (format nil "mkdir ~a" new-run-directory) :wait t)
    (run-shell-command (format nil "cp ~a/index.html      ~a/index.html"      old-run-directory new-run-directory) :wait t)
    (run-shell-command (format nil "cp ~a/runs-index.html ~a/runs-index.html" old-run-directory new-run-directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/batch-processing" new-run-directory) :wait t)
    (let* ((table-names (get-tables-in-run-directory root old-run)))
      (loop for table-name in table-names do
	    (progn
	      (run-shell-command (format nil "mkdir ~a/batch-processing/~a" new-run-directory table-name)
				 :wait t)
	      (run-shell-command (format nil "cp -a ~a/../../../index-templates/individual-table-index-template.html ~a/batch-processing/~a/index.html" 
					 new-run-directory new-run-directory table-name)
				 :wait t)
	      (run-shell-command (format nil "cp ~a/batch-processing/~a/make-save-form.msf ~a/batch-processing/~a/make-save-form.msf" 
					 old-run-directory table-name new-run-directory table-name)
		:wait t)
	      (run-shell-command (format nil "cp ~a/batch-processing/~a/table.html ~a/batch-processing/~a/table.html" 
					 old-run-directory table-name new-run-directory table-name)
		:wait t)
	      (run-shell-command (format nil "cp ~a/batch-processing/~a/table.htm ~a/batch-processing/~a/table.htm" 
					 old-run-directory table-name new-run-directory table-name)
				 :wait t)
	      )))))
			  

(defun table-analyses (directory
		       &optional &key
				 run-from-table
				 run-to-table
				 tables-to-exclude
				 (save-filename "make-save-form.msf")
				 (save-color-code-f #'color-code-reference-antigen-save)
				 (num-runs 10)
				 (num-dimensions 2)
				 (dim-anneal-starting-coords 5)
				 run-at-sfi-p
				 (bundled-runs-if-run-at-sfi-p t)
				 recover-batch-runs-from-sfi
				 (num-solutions-to-show 5)
				 (master-orientation-save-filename "mds/investigations/strain-selection-meeting/master-save-detail.save")
				 (batch-runs t)
				 (save-raw-data t)
				 save-format-pre-save-f
				 dont-save-save-of-table-only
				 (save-batch-runs t)
				 (batch-runs-graphics-diagnostics t))

  (let ((table-names (get-tables-in-directory directory)))

    (if (or (member run-from-table tables-to-exclude :test #'equal)
	    (member run-to-table   tables-to-exclude :test #'equal))
	(format t "Either the run-from-table or the run-to-table are also tables-to-exclude~%"))  ;; will break below (as i want it to)

    (if tables-to-exclude
	(progn
	  (loop for table in tables-to-exclude do
		(if (not (member table table-names :test #'equal))
		    (format t "Table to exclude, ~a, is not in the list of tables which would be processed ~a~%")))
	  (setq table-names (my-set-difference table-names tables-to-exclude :test #'equal))))

    (if run-from-table
	(if (member run-from-table table-names :test #'equal)
	    (setq table-names
	      (member run-from-table table-names :test #'equal))
	  (error "The table passed to run from (run-from-table) ~a is not in list of tables ~a~%" run-from-table table-names)))
    
    (if run-to-table
	(if (position run-to-table table-names :test #'equal)
	    (setq table-names
	      (firstn (inc (position run-to-table table-names :test #'equal)) table-names))
	  (error "The table passed to run to (run-to-table) ~a is not in list of tables ~a~%" run-to-table table-names)))
    
    (loop for table-name in table-names do
	  (progn
	    (format t "~2%Starting to process table ~a at ~a~%" table-name (time-and-date))
	    (let ((save (let ((s (fi-in (format nil "~a/~a/~a" directory table-name save-filename))))
			  (if (make-save-form-p s)
			      (eval s)
			    s))))
	      (table-analsysis 
	       (funcall save-color-code-f 
			(if save-format-pre-save-f
			    (funcall save-format-pre-save-f save :diagnostics-filename (format nil "~a/~a/pre-save-f-diagnostics" directory table-name))
			  save))
	       (format nil "~a/~a/" directory table-name)
	       :num-runs                         num-runs
	       :num-dimensions                   num-dimensions
	       :dim-anneal-starting-coords       dim-anneal-starting-coords
	       :run-at-sfi-p                     run-at-sfi-p
	       :bundled-runs-if-run-at-sfi-p     bundled-runs-if-run-at-sfi-p
	       :recover-batch-runs-from-sfi      recover-batch-runs-from-sfi
	       :num-solutions-to-show            num-solutions-to-show
	       :master-orientation-save-filename master-orientation-save-filename
	       :save-raw-data                    save-raw-data
	       :dont-save-save-of-table-only     dont-save-save-of-table-only
	       :batch-runs                       batch-runs
	       :save-batch-runs                  save-batch-runs
	       :batch-runs-graphics-diagnostics  batch-runs-graphics-diagnostics))))))


#|
(process-hi-tables :suffix "-2"
		       :unpack-database-output t
		       :make-internal-directory-structure t
		       :batch-runs nil)

(process-hi-tables :suffix "-2"
		       :unpack-database-output nil
		       :make-internal-directory-structure nil
		       :batch-runs t
		       :num-runs                         3
		       :num-solutions-to-show            2
		       )

(process-hi-tables :suffix "-2"
		       :unpack-database-output t
		       :make-internal-directory-structure t
		       :batch-runs t
		       :num-runs                         3
		       :num-solutions-to-show            2
		       )


;; nimr data

(process-hi-tables :suffix "-2"
		       :zip-name "nimr.zip"
		       :zip-unpack-name "nimr"
		       :root-directory "mds/investigations/strain-selection-meeting/database/nimr/runs/"
		       :unpack-database-output t
		       :make-internal-directory-structure t
		       :batch-runs t
		       :num-runs                         3
		       :num-solutions-to-show            2
		       )



;; csl
(process-hi-tables :suffix ""
		   :zip-name "csl-from-t-20040907.zip"
		   :zip-unpack-name "csl"
		   :zip-directory  "mds/investigations/strain-selection-meeting/database/csl/raw-data"
		   :root-directory "mds/investigations/strain-selection-meeting/database/csl/runs/"
		   :unpack-database-output t
		   :make-internal-directory-structure t
		   :save-raw-data t
		   :batch-runs nil
		   :save-batch-runs nil
		   :batch-runs-graphics-diagnostics nil
		   )

|#


;;;----------------------------------------------------------------------
;;;                      setting up html pages
;;;----------------------------------------------------------------------

(defun write-tables-index-page (filename table-ids &optional &key 
							     (if-exists-action :error) 
							     tables-to-mark-as-using-earlier-ref-ag-merge
							     )
  (with-open-file (out filename :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Standard table diagnostics</CENTER></H1>~%")
    (newline out)
    (format out "<PRE>~%")
    (newline out)
    (loop for table-id in table-ids do
	  (if (member table-id tables-to-mark-as-using-earlier-ref-ag-merge :test #'equal)
	      (format out "<A href=batch-processing/~a/index.html>~a</A> (using earlier ref-ag-merge)~%" table-id table-id)
	    (format out "<A href=batch-processing/~a/index.html>~a</A>~%" table-id table-id)))
    (newline out)
    (format out "</PRE>~%")))

;; (write-tables-index-page "/tmp/fooxx" "CDC" (loop for i below 3 collect (string-append "foo" (anything->string i))))



(defun write-ref-ag-merge-summary-index-page (filename lab table-ids &optional &key 
									       (if-exists-action :error)
									       tables-to-mark-as-using-earlier-ref-ag-merge)
  ;; (write-ref-ag-merge-summary-index-page "/tmp/foox" "CDC" (loop for i below 3 collect (string-append "foo" (anything->string i))))
  (with-open-file (out filename :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Each ~a table reoptimized from a random start</CENTER></H1>~%" (string-upcase lab))
    (newline out)
    (format out "<PRE>~%")
    (newline out)
    (loop for table-id in table-ids do
	  (format out "<HR><h2>~a ~a</H2>~%" (string-upcase lab) table-id)
	  (apply #'format out "<IMG src=batch-processing/~a/stress-0-bland.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-bland.gif.labeled.gif.bordered.gif>                          <IMG src=batch-processing/~a/stress-1-pc-lowest-stress-run.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-1-pc-lowest-stress-run.gif.labeled.gif.bordered.gif> <IMG src=batch-processing/~a/stress-2-pc-lowest-stress-run.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-2-pc-lowest-stress-run.gif.labeled.gif.bordered.gif>                      <IMG src=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif>                          <IMG src=batch-processing/~a/stress-0-bland.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-bland.gif.labeled.gif.bordered.gif>                  <IMG src=batch-processing/~a/stress-0-pc-original-table-run.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-original-table-run.gif.labeled.gif.bordered.gif>~%"
		 (loop for i below 18 collect (progn i table-id)))
	  (if (member table-id tables-to-mark-as-using-earlier-ref-ag-merge :test #'equal)
	      (format out "(using earlier ref-ag-merge) Lowest stress (ref ag grey, epi green)                                                                    2nd lowest stress, pc lowest stress                                                                            3rd lowest stress, pc lowest stress                                                                                                 Lowest stress with pc ref ag merge                                                                                                     Orginal table map                                                                                                              Lowest stress with pc original table~%")
	    (format out "Lowest stress (ref ag grey, epi green)                                                                                                 2nd lowest stress, pc lowest stress                                                                            3rd lowest stress, pc lowest stress                                                                                                 Lowest stress with pc ref ag merge                                                                                                     Orginal table map                                                                                                              Lowest stress with pc original table~%"))
	  (apply #'format out "Detailed diagnostics page for this table <A href=batch-processing/~a/index.html>here</A>, Original HI table <A href=batch-processing/~a/table.htm>here</A>, processed HI table <A href=batch-processing/~a/table.html>here</A>, lisp processed HI table <A href=batch-processing/~a/pp-table.txt>here</A>, stresses <A href=batch-processing/~a/pp-stresses.txt>here</A>, average procrustes differences <A href=batch-processing/~a/average-procrustes-differences.txt>here</A>, save <A href=batch-processing/~a/save-after-runs-reoriented.save>here</A>.  SFI format before runs <A href=batch-processing/~a/sfi-of-table-only.sfi>here</A>, and after runs <A href=batch-processing/~a/sfi-after-runs.sfi>here</A>. ~a ~a~%"
		 (append (loop for i below 9 collect (progn i table-id))
			 (list (string-upcase lab))
			 (list table-id)))
	  (newline out))
    (newline out)
    (format out "</PRE>~%")))


(defun write-ref-ag-merge-summary-index-page-small (filename lab table-ids &optional &key 
										     (if-exists-action :error)
										     tables-to-mark-as-using-earlier-ref-ag-merge)
  (with-open-file (out filename :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Each ~a table reoptimized from a random start</CENTER></H1>~%" (string-upcase lab))
    (newline out)
    (format out "<PRE>~%")
    (newline out)
    (loop for table-id in table-ids do
	  ;;(format out "<HR><h2>~a ~a</H2>~%" lab table-id)
	  (apply #'format out "<a href=batch-processing/~a/stress-0-bland.gif><IMG src=batch-processing/~a/stress-0-bland.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-bland.gif.300x300.gif.labeled.gif.bordered.gif></a>                          <a href=batch-processing/~a/stress-1-pc-lowest-stress-run.gif><IMG src=batch-processing/~a/stress-1-pc-lowest-stress-run.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-1-pc-lowest-stress-run.gif.300x300.gif.labeled.gif.bordered.gif></a> <a href=batch-processing/~a/stress-2-pc-lowest-stress-run.gif><IMG src=batch-processing/~a/stress-2-pc-lowest-stress-run.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-2-pc-lowest-stress-run.gif.300x300.gif.labeled.gif.bordered.gif></a>                      <a href=batch-processing/~a/stress-0-pc-master.gif><IMG src=batch-processing/~a/stress-0-pc-master.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-master.gif.300x300.gif.labeled.gif.bordered.gif></a>                          <a href=batch-processing/~a/stress-0-bland.gif><IMG src=batch-processing/~a/stress-0-bland.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-bland.gif.300x300.gif.labeled.gif.bordered.gif></a>                  <a href=batch-processing/~a/stress-0-pc-original-table-run.gif><IMG src=batch-processing/~a/stress-0-pc-original-table-run.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-original-table-run.gif.300x300.gif.labeled.gif.bordered.gif></a>~%"
		 (loop for i below 18 collect (progn i table-id)))
	  (if (member table-id tables-to-mark-as-using-earlier-ref-ag-merge :test #'equal)
	      (format out "(using earlier ref-ag-merge) Lowest stress (ref ag grey, epi green)  2nd lowest stress, pc lowest stress         3rd lowest stress, pc lowest stress                               Lowest stress with pc ref ag merge                                   Orginal table map                                             Lowest stress with pc original table~%")
	    (format out "Lowest stress (ref ag grey, epi green)                               2nd lowest stress, pc lowest stress         3rd lowest stress, pc lowest stress                               Lowest stress with pc ref ag merge                                   Orginal table map                                             Lowest stress with pc original table~%"))
	  '(apply #'format out "Detailed diagnostics page for this table <A href=batch-processing/~a/index.html>here</A>, HI table <A href=batch-processing/~a/pp-table.txt>here</A>, stresses <A href=batch-processing/~a/pp-stresses.txt>here</A>, average procrustes differences <A href=batch-processing/~a/average-procrustes-differences.txt>here</A>, save <A href=batch-processing/~a/save-after-runs-reoriented.save>here</A>.  SFI format before runs <A href=batch-processing/~a/sfi-of-table-only.sfi>here</A>, and after runs <A href=batch-processing/~a/sfi-after-runs.sfi>here</A>. ~a ~a~%"
		 (append (loop for i below 7 collect (progn i table-id))
			 (list lab)
			 (list table-id)))
	  (apply #'format out "Detailed diagnostics page for this table <A href=batch-processing/~a/index.html>here</A>, Original HI table <A href=batch-processing/~a/table.htm>here</A>, processed HI table <A href=batch-processing/~a/table.html>here</A>, lisp processed HI table <A href=batch-processing/~a/pp-table.txt>here</A>, stresses <A href=batch-processing/~a/pp-stresses.txt>here</A>, average procrustes differences <A href=batch-processing/~a/average-procrustes-differences.txt>here</A>, save <A href=batch-processing/~a/save-after-runs-reoriented.save>here</A>.  SFI format before runs <A href=batch-processing/~a/sfi-of-table-only.sfi>here</A>, and after runs <A href=batch-processing/~a/sfi-after-runs.sfi>here</A>. ~a ~a~%"
		 (append (loop for i below 9 collect (progn i table-id))
			 (list (string-upcase lab))
			 (list table-id)))
	  (newline out))
    (newline out)
    (format out "</PRE>~%")))



(defun write-ref-ag-with-epi-summary-page (filename table-ids &optional &key (if-exists-action :error) (maps-per-line 1))
  (with-open-file (out filename :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Each epi table, aligned with reference antigen frame</CENTER></H1>")
    (newline out)
    (format out "<PRE>")
    (newline out)
    (loop for table-id in table-ids for i from 1 do
	  (format out "<a href=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif><IMG src=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif></a> " table-id table-id table-id)
	  (if (zerop (mod i maps-per-line))
	      (newline out)))
    (newline out)
    (format out "</PRE>~%")))

(defun write-ref-ag-with-epi-summary-page-small (filename table-ids &optional &key (if-exists-action :error) (maps-per-line 1))
  (with-open-file (out filename :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Each epi table, aligned with reference antigen frame</CENTER></H1>")
    (newline out)
    (format out "<PRE>")
    (newline out)
    (loop for table-id in table-ids for i from 1 do
	  (format out "<a href=batch-processing/~a/stress-0-pc-master.gif.labeled.gif.bordered.gif><IMG src=batch-processing/~a/stress-0-pc-master.gif.300x300.gif.labeled.gif.bordered.gif alt=batch-processing/~a/stress-0-pc-master.gif.300x300.gif.labeled.gif.bordered.gif></a>" table-id table-id table-id)
	  (if (zerop (mod i maps-per-line))
	      (newline out)))
    (newline out)
    (format out "</PRE>~%")))



(defun write-ref-ag-with-epi-index-pages (root run &optional &key 
							     exclude-tables
							     tables-to-mark-as-using-earlier-ref-ag-merge 
							     (lab-pretty-id (string-upcase root))
							     (if-exists-action :error))

  (write-ref-ag-merge-summary-index-page
   (format nil "~a/~a/all-tables-summary-index.html" (runs-directory-from-root root) run)
   lab-pretty-id
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :tables-to-mark-as-using-earlier-ref-ag-merge tables-to-mark-as-using-earlier-ref-ag-merge 
   :if-exists-action if-exists-action)
  (write-ref-ag-merge-summary-index-page-small
   (format nil "~a/~a/all-tables-summary-index-small.html" (runs-directory-from-root root) run)
   lab-pretty-id
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :tables-to-mark-as-using-earlier-ref-ag-merge tables-to-mark-as-using-earlier-ref-ag-merge 
   :if-exists-action if-exists-action)

  (write-ref-ag-with-epi-summary-page
   (format nil "~a/~a/all-ref-ag-epi-merge-tables-summary-index-1-per-line.html" (runs-directory-from-root root) run)
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :if-exists-action if-exists-action
   :maps-per-line 1)
  (write-ref-ag-with-epi-summary-page
   (format nil "~a/~a/all-ref-ag-epi-merge-tables-summary-index-2-per-line.html" (runs-directory-from-root root) run)
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :if-exists-action if-exists-action
   :maps-per-line 2)

  (write-ref-ag-with-epi-summary-page-small
   (format nil "~a/~a/all-ref-ag-epi-merge-tables-summary-index-small-1-per-line.html" (runs-directory-from-root root) run)
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :if-exists-action if-exists-action
   :maps-per-line 1)
  (write-ref-ag-with-epi-summary-page-small
   (format nil "~a/~a/all-ref-ag-epi-merge-tables-summary-index-small-3-per-line.html" (runs-directory-from-root root) run)
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :if-exists-action if-exists-action
   :maps-per-line 3)
  (write-ref-ag-with-epi-summary-page-small
   (format nil "~a/~a/all-ref-ag-epi-merge-tables-summary-index-small-6-per-line.html" (runs-directory-from-root root) run)
   (get-tables-in-run-directory root run :exclude-tables exclude-tables)
   :if-exists-action if-exists-action
   :maps-per-line 6))



;;;----------------------------------------------------------------------
;;;           updating an incremental run into an existing run
;;;----------------------------------------------------------------------

(defun incremental-update-of-run-with-new-tables (root new-run old-run &optional &key
										 run-from-table
										 run-to-table
										 tables-to-exclude
										 (num-runs 10)
										 run-at-sfi-p
										 (bundled-runs-if-run-at-sfi-p t)
										 change-gt-to-dc-in-tables
										 group-names
										 (master-orientation-root root)
										 (master-orientation-run  old-run))

  (if (and (equal root "melb")
	   (not change-gt-to-dc-in-tables))
      (cerror "Continue anyway?"
	      "You are running MELB data, but did not specify to change greater-than to dont-care"))

  (let ((new-run-ref-ag-merge (format nil "~a-ref-ag-merge" new-run)))

    (unpack-root-run-data 
     root new-run
     :run-from-table    run-from-table
     :run-to-table      run-to-table
     :tables-to-exclude tables-to-exclude
     :save-format-pre-save-f (if group-names
				 (^ (save &optional &key diagnostics-filename) 
				    (save-change-names-remove-dont-care-rows-and-cols 
				     save
				     group-names
				     :table-output-filename diagnostics-filename))
			       ))

    (if change-gt-to-dc-in-tables
	(change-gt-to-dc-in-tables
	 root new-run
	 :tables-to-exclude tables-to-exclude
	 :run-from-table    run-from-table
	 :run-to-table      run-to-table))

    (make-table-runs
     root new-run
     :num-runs num-runs
     :run-at-sfi-p run-at-sfi-p :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (create-and-populate-ref-ag-merge-directory-structure
     root new-run :reference-map-run old-run
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (setup-data-output-before-runs
     root new-run-ref-ag-merge
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (make-table-runs
     root new-run-ref-ag-merge
     :num-runs num-runs
     :run-at-sfi-p run-at-sfi-p :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (if (not run-at-sfi-p)
	(finish-incremental-update-of-run-with-new-tables
	 root new-run old-run
	 :run-from-table           run-from-table
	 :run-to-table             run-to-table
	 :tables-to-exclude        tables-to-exclude
	 :master-orientation-root  master-orientation-root 
	 :master-orientation-run   master-orientation-run))))

(defun finish-incremental-update-of-run-with-new-tables (root new-run old-run &optional &key
											run-from-table
											run-to-table
											tables-to-exclude
											(master-orientation-root root)
											(master-orientation-run  old-run))
  (let ((new-run-ref-ag-merge (format nil "~a-ref-ag-merge" new-run))
	(old-run-ref-ag-merge (format nil "~a-ref-ag-merge" old-run)))

    (make-table-run-graphics-in-parts-avoiding-wish-from-lisp
     root new-run
     :master-orientation-root master-orientation-root
     :master-orientation-run  master-orientation-run 
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (make-table-run-graphics-in-parts-avoiding-wish-from-lisp
     root new-run-ref-ag-merge
     :master-orientation-root master-orientation-root
     :master-orientation-run  master-orientation-run 
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (incremental-update-of-root-run-from-root-run 
     root new-run old-run 
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (incremental-update-of-root-run-from-root-run 
     root new-run-ref-ag-merge old-run-ref-ag-merge
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)))


(defun incremental-update-of-run-with-new-tables-recover-from-sfi-and-continue (root new-run old-run &optional &key
													       run-from-table
													       run-to-table
													       tables-to-exclude
													       (master-orientation-root root)
													       (master-orientation-run  old-run))
  (let ((new-run-ref-ag-merge (format nil "~a-ref-ag-merge" new-run)))

    (recover-batch-runs-from-sfi 
     root new-run
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)

    (recover-batch-runs-from-sfi 
     root new-run-ref-ag-merge
     :run-from-table run-from-table :run-to-table run-to-table :tables-to-exclude tables-to-exclude)
    
    (finish-incremental-update-of-run-with-new-tables
     root new-run old-run
     :run-from-table           run-from-table
     :run-to-table             run-to-table
     :tables-to-exclude        tables-to-exclude
     :master-orientation-root  master-orientation-root 
     :master-orientation-run   master-orientation-run)))


(defun incremental-update-of-root-run-from-root-run (root run-with-new-data run-to-update
						     &optional &key
							       run-from-table
							       run-to-table
							       tables-to-exclude)
  (let ((tables (get-tables-in-run-directory root run-with-new-data 
					     :run-from-table run-from-table
					     :run-to-table   run-to-table
					     :exclude-tables tables-to-exclude)))
    (loop for table in tables do
	  (mv (directory-from-root-run-table root run-with-new-data table) 
	      (directory-from-root-run       root run-to-update)))
    (write-tables-index-page 
     (string-append (root-run-directory root run-to-update) "index.html")
     (get-tables-in-run-directory root run-to-update)
     :tables-to-mark-as-using-earlier-ref-ag-merge tables
     :if-exists-action :supersede)
    (write-ref-ag-with-epi-index-pages 
     root
     run-to-update
     :tables-to-mark-as-using-earlier-ref-ag-merge tables
     :if-exists-action :supersede)))


;;;----------------------------------------------------------------------
;;;          merge epi strains with the lab-ref-ag-map
;;;----------------------------------------------------------------------

#|
;; this is merge using merging of tables, and we miss the reference antigens in the save
;; below we merge the saves (but don't bother merging the coords
(defun create-and-populate-ref-ag-merge-directory-structure (root run)
  (let ((lab-ref-ag-save (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/routine-diagnostics/save-after-runs-reoriented.save" root)))
	(new-run (string-append run "-ref-ag-merge")))
    (replicate-internal-directory-structure root run new-run)
    (loop for table-id in (get-tables-in-run-directory root new-run) do
	  (write-save-form
	   (merge-tables 
	    (list (un-asl-hi-table (table-from-save lab-ref-ag-save))
		  (un-asl-hi-table (get-table root run table-id)))
	    :filename (string-append (directory-from-root-run-table root new-run table-id) "/merge.txt"))
	   (string-append (directory-from-root-run-table root new-run table-id) "/save-of-table-only.save")))))
|#

(defun create-and-populate-ref-ag-merge-directory-structure (root run &optional &key 
										(reference-map-run run)
										(new-run (string-append run "-ref-ag-merge"))
										tables-to-exclude
										run-from-table
										run-to-table
										(multiple-values-f #'average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold)
										)
  (let ((lab-ref-ag-save (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" root reference-map-run))))
    (replicate-internal-directory-structure root run new-run)
    (loop for table-id in (get-tables-in-run-directory root new-run 
						       :exclude-tables tables-to-exclude
						       :run-from-table run-from-table
						       :run-to-table   run-to-table
						       ) do
	  (progn
	    (format t "~2%Starting to process table ~a at ~a~%" table-id (time-and-date))
	    (write-save-form
	     (merge-saves-no-coordss
	      (list lab-ref-ag-save
		    (get-save-of-table-only root run table-id))
	      :multiple-values-f multiple-values-f
	      :table-output-filename (string-append (directory-from-root-run-table root new-run table-id) "/merge.txt"))
	     (string-append (directory-from-root-run-table root new-run table-id) "/save-of-table-only.save"))))))

;; (create-and-populate-ref-ag-merge-directory-structure "niid" "20040909a")


;;;----------------------------------------------------------------------
;;;                  accessing directory structure
;;;----------------------------------------------------------------------


(defun directory-from-root (root)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/" root))

(defun runs-directory-from-root (root)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/runs/" root))

(defun data-directory-from-root (root)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/raw-data/" root))

(defun root-run-directory (root run)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/runs/~a/" root run))
(defun root-run-directory-without-final-slash (root run)
  ;; ugly!
  (format nil "mds/investigations/strain-selection-meeting/database/~a/runs/~a" root run))

(defun directory-from-root-run (root run)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/runs/~a/batch-processing/" root run))

(defun directory-from-root-run-table (root run table)
  (format nil "mds/investigations/strain-selection-meeting/database/~a/runs/~a/batch-processing/~a/" root run table))


(defun overlay-merge-root-run (root run) (format nil "~a/investigations/overlay-merge/~a/" (directory-from-root root) run))


(defun reference-antigen-root-run (root run) (format nil "~a/investigations/reference-antigen-map/~a/" (directory-from-root root) run))
(defun reference-antigen-root-run-routine-diagnostics (root run) (format nil "~a/routine-diagnostics/" (reference-antigen-root-run root run)))
(defun reference-antigen-group-names-root (root run) (format nil "~a/group-names/" (reference-antigen-root-run root run)))

(defun get-reference-antigen-root-run-save-after-runs-reoriented (root run) 
  (fi-in (format nil "~a/save-after-runs-reoriented.save" (reference-antigen-root-run-routine-diagnostics root run))))

(defun get-reference-antigen-root-run-save-after-runs-reoriented-filename (root run) 
  (format nil "~a/save-after-runs-reoriented.save" (reference-antigen-root-run-routine-diagnostics root run)))



(defun get-reference-antigen-map-antigen-names (root run) (fi-in-s (format nil "~a/merge-antigens.txt" (reference-antigen-root-run root run))))
(defun get-reference-antigen-map-sera-names    (root run) (fi-in-s (format nil "~a/merge-sera.txt" (reference-antigen-root-run root run))))

(defun get-reference-antigen-map-antigen-and-sera-names (root run) 
  (hi-table-antigens (table-from-save (get-reference-antigen-root-run-save-after-runs-reoriented root run))))

(defun get-tables-in-directory (directory &optional &key 
						    exclude-tables
						    run-from-table
						    run-to-table)
  (let* ((filenames (progn (run-shell-command (format nil "ls ~a > /tmp/tmp-file-for-lisp-shell-commands" directory) :wait t)
			   (fi-in-readline "/tmp/tmp-file-for-lisp-shell-commands"))))

    (if run-from-table
	(if (member run-from-table filenames :test #'equal)
	    (setq filenames
	      (member run-from-table filenames :test #'equal))
	  (error "The table passed to run from (run-from-table) ~a is not in list of tables ~a~%" run-from-table filenames)))
    
    (if run-to-table
	(if (position run-to-table filenames :test #'equal)
	    (setq filenames
	      (firstn (inc (position run-to-table filenames :test #'equal)) filenames))
	  (error "The table passed to run to (run-to-table) ~a is not in list of tables ~a~%" run-to-table filenames)))
    
    (loop for table in filenames
	when (not (member table exclude-tables :test #'equal))
	collect table)))
  
(defun get-tables-in-run-directory (root run &optional &key 
						       exclude-tables
						       run-from-table
						       run-to-table)
  (get-tables-in-directory (directory-from-root-run root run) 
			   :exclude-tables exclude-tables
			   :run-from-table     run-from-table
			   :run-to-table       run-to-table))

(define across-all-tables-in-run
    (^ (f) 
       (^ (root run &optional &key exclude-tables)
	  (loop for table in (get-tables-in-run-directory root run)
	      when (not (member table exclude-tables :test #'equal))
	      collect (funcall f root run table)))))

(defun make-get-file-from-root-run-table-f (file) 
  (^ (root run table)
     (fi-in (format nil "~a/~a" (directory-from-root-run-table root run table) file))))

(define get-save-after-runs    (make-get-file-from-root-run-table-f "save-after-runs.save"   ))
(define get-save-of-table-only (make-get-file-from-root-run-table-f "save-of-table-only.save"))
(define get-make-save-form     (make-get-file-from-root-run-table-f "make-save-form.msf" ))

(defun get-reference-antigens-in-table (root run table) (eval (snoop-keyword-arg :reference-antigens (cdr (get-make-save-form root run table)))))
(defun get-table (root run table) (table-from-save (get-save-of-table-only root run table)))
(defun get-table-antigens (root run table) (hi-table-antigens (un-asl-hi-table (table-from-save (get-save-of-table-only root run table)))))
(defun get-table-sera     (root run table) (hi-table-sera     (un-asl-hi-table (table-from-save (get-save-of-table-only root run table)))))
(defun get-reference-antigen-table (root run table)
  (extract-hi-table (un-asl-hi-table (get-table root run table)) (reference-antigens-from-save (get-save-of-table-only root run table))))
;;(get-reference-antigen-table "cdc" "20040820-10-runs" "20021219")   

(defun get-save-after-runs-reoriented (root run table)
  (fi-in (format nil "~a/save-after-runs-reoriented.save" (directory-from-root-run-table root run table))))

(defun get-msf-filename (root run table)
  (format nil "~a/make-save-form.msf" (directory-from-root-run-table root run table)))
(defun get-msf (root run table)
  (fi-in (format nil "~a/make-save-form.msf" (directory-from-root-run-table root run table))))

(defun save-after-runs-reoriented-exists-p (root run table)
  (pathname-exists-p (format nil "~a/save-after-runs-reoriented.save" (directory-from-root-run-table root run table))))

(defun get-save-after-runs-reoriented-hand-unhemisphered-and-relaxed (root run table)
  (fi-in (format nil "~a/save-after-runs-reoriented.save.hand-unhemisphered-and-relaxed.save" 
		 (directory-from-root-run-table root run table))))

(define get-reference-antigen-table-s (across-all-tables-in-run #'get-reference-antigen-table))


(defun get-equivalent-row-names (root run table)
  (eval (snoop-keyword-arg :equivalent-row-names (cdr (get-make-save-form root run table)))))
(defun get-equivalent-column-names (root run table)
  (eval (snoop-keyword-arg :equivalent-column-names (cdr (get-make-save-form root run table)))))

(define get-equivalent-row-names-s (across-all-tables-in-run #'get-equivalent-row-names))
(define get-equivalent-column-names-s (across-all-tables-in-run #'get-equivalent-column-names))

(define get-table-antigens-s (across-all-tables-in-run #'get-table-antigens))
(define get-table-sera-s     (across-all-tables-in-run #'get-table-sera))

(define get-table-s          (across-all-tables-in-run #'get-table))

(define directory-from-root-run-table-s (across-all-tables-in-run #'directory-from-root-run-table))




;;;----------------------------------------------------------------------
;;;                      image manipulation
;;;----------------------------------------------------------------------

(defun label-gifs-in-root-run (root run
			       &optional &key
					 exclude-tables
					 run-from-table
					 run-to-table)
  (loop for table in (get-tables-in-run-directory root run :exclude-tables exclude-tables :run-from-table run-from-table :run-to-table run-to-table) do
	(run-shell-command (print (format nil "find ~a/~a/ -name '*.gif' -exec convert -font helvetica -draw \"text 7,43 ~a-~a\" {} {}.labeled.gif \\;" 
					  (directory-from-root-run root run)
					  table
					  (string-upcase root)
					  table
					  )))))

(defun border-gifs-in-root-run (root run
				&optional &key 
					  exclude-tables
					  run-from-table
					  run-to-table)
  (loop for table in (get-tables-in-run-directory root run :exclude-tables exclude-tables :run-from-table run-from-table :run-to-table run-to-table) do
	(run-shell-command (print (format nil "find ~a/~a/ -name '*.gif' -exec convert -border 1x1 -bordercolor black {} {}.bordered.gif \\;" 
					  (directory-from-root-run root run)
					  table
					  (string-upcase root)
					  table
					  )))))

(defun make-300x300-gifs-in-root-run (root run
				      &optional &key 
						exclude-tables
						run-from-table
						run-to-table)
  (loop for table in (get-tables-in-run-directory root run :exclude-tables exclude-tables :run-from-table run-from-table :run-to-table run-to-table) do
	(run-shell-command (print (format nil "find ~a/~a/ -name '*.gif' -exec convert -resize 300x300 {} {}.300x300.gif \\;" 
					  (directory-from-root-run root run)
					  table)))))

(defun label-300x300-gifs-in-root-run (root run
				       &optional &key 
						 exclude-tables
						 run-from-table
						 run-to-table)
  (loop for table in (get-tables-in-run-directory root run :exclude-tables exclude-tables :run-from-table run-from-table :run-to-table run-to-table) do
	(run-shell-command (print (format nil "find ~a/~a/ -name '*.300x300.gif' -exec convert -font helvetica -draw \"text 3,23 ~a-~a\" {} {}.labeled.gif \\;" 
					  (directory-from-root-run root run)
					  table
					  (string-upcase root)
					  table
					  )))))


;; ----------------------------- more generic resizing/labeling/bordering ------------------------------

(defun make-300x300-and-labeled-bordered-gif (directory image-name 
					      &optional &key 
							(label (substring-before-char #\. image-name)))
  (let ((large-filename (string-append directory image-name))
	(small-filename (string-append directory image-name ".300x300.gif")))
    (run-shell-command (format nil "convert -resize 300x300 ~a ~a" large-filename small-filename))
    (run-shell-command (format nil "convert -font helvetica -draw \"text 7,43 '~a'\" -border 1x1 -bordercolor black ~a ~a" 
			     label
			     large-filename
			     (string-append large-filename ".labeled.gif.bordered.gif")))
    (run-shell-command (format nil "convert -font helvetica -draw \"text 3,23 '~a'\" -border 1x1 -bordercolor black ~a ~a" 
			     label
			     small-filename
			     (string-append small-filename ".labeled.gif.bordered.gif")))))

;; -------------------------------------- for make ----------------------------------------------------

(defun label-gifs-in-routine-diagnostics-directory (routine-diagnostics-directory label)
  (run-shell-command (format nil "find ~a/ -name '*.gif' -exec convert -font helvetica -draw \"text 7,43 ~a\" {} {}.labeled.gif \\;" 
			     routine-diagnostics-directory
			     label
			     )))

(defun border-gifs-in-routine-diagnostics-directory (routine-diagnostics-directory)
  (run-shell-command (format nil "find ~a/ -name '*.gif' -exec convert -border 1x1 -bordercolor black {} {}.bordered.gif \\;" 
			     routine-diagnostics-directory
			     )))

(defun make-300x300-gifs-in-routine-diagnostics-directory (routine-diagnostics-directory)
  (run-shell-command (format nil "find ~a/ -name '*.gif' -exec convert -resize 300x300 {} {}.300x300.gif \\;" 
			     routine-diagnostics-directory)))

(defun label-300x300-gifs-in-routine-diagnostics-directory (routine-diagnostics-directory label)
  (run-shell-command (format nil "find ~a/ -name '*.300x300.gif' -exec convert -font helvetica -draw \"text 3,23 ~a\" {} {}.labeled.gif \\;" 
			     routine-diagnostics-directory
			     label
			     )))


;;;----------------------------------------------------------------------
;;;                  procrustes reference antigen maps
;;;----------------------------------------------------------------------

(defun reference-antigen-map-comparison-ag-sr-all (master-lab master-run slave-lab slave-run &optional &key ag-map-only-p)
  (loop for id in (mapcar (^ (id) (format nil "-~a" id)) (if ag-map-only-p '("ref-ag-map-ag-only") '("ref-ag-map-ag-only" "ref-ag-map-sr-only" "ref-ag-map-ag-and-sr")))
      for name-subset in '(ag-only sr-only nil) do
	(if (not (pathname-exists-p (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs/" master-lab master-run)))
	    (run-shell-command (format nil "mkdir mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs/" master-lab master-run) :wait t))
	(if (not (pathname-exists-p (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs/pc-~a-~a/" master-lab master-run slave-lab slave-run)))
	    (run-shell-command (format nil "mkdir ~a" (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs/pc-~a-~a/" master-lab master-run slave-lab slave-run)) :wait t))
	(store-run-image-with-procrustes-of-other-save
	 (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" master-lab master-run))
	 (format nil        "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs/pc-~a-~a/" master-lab master-run slave-lab slave-run)
	 (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" slave-lab slave-run))
	 id
	 :name-subset name-subset)))

(defun reference-antigen-map-comparison-ag-sr-all-2 (master-lab master-run slave-lab slave-run)
  (if (not (pathname-exists-p (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs2/" master-lab master-run)))
      (run-shell-command (format nil "mkdir mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs2/" master-lab master-run) :wait t))
  (if (not (pathname-exists-p (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs2/pc-~a-~a/" master-lab master-run slave-lab slave-run)))
      (run-shell-command (format nil "mkdir ~a" (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs2/pc-~a-~a/" master-lab master-run slave-lab slave-run)) :wait t))

  (save-pc-save-and-write-ps-pdf-and-gif
   ;;(dark-outline-reference-antigen-names-in-save
    ;;(color-code-epi-strong-reference-background-save 
     (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" master-lab master-run))
     ;;)
    ;;"black")
   (fi-in (format nil "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" slave-lab slave-run))
   (format nil        "mds/investigations/strain-selection-meeting/database/~a/investigations/reference-antigen-map/~a/pc-other-labs2/pc-~a-~a/pc--ref-ag-map-ag-only" master-lab master-run slave-lab slave-run)
   :arrow-color "black"
   :name-subset 'ag-only
   ))

(defun write-ref-ag-comparison-with-other-labs (all-labs all-runs &optional &key (id (numeric-sortable-date)) (if-exists-action :error))
  (let ((filename (format nil "mds/investigations/strain-selection-meeting/database/combined/reference-maps/pc-among-labs/~a/index.html" id)))
    (with-open-file (out filename :direction :output :if-exists if-exists-action)
      (format out "<H1><CENTER>Reference map comparisons among labs</CENTER></H1>")
      (newline out)
      (format out "<PRE>")
      (newline out)
      
      (loop for lab in all-labs
	  for run in all-runs do
	    (progn
	      (format out "<HR><h2>~a compared with ~{~a ~}</h2>~%" (string-upcase lab) (mapcar #'string-upcase (butnth (position lab all-labs :test #'equal) all-labs)))
	      (loop for other-lab in (butnth (position lab all-labs :test #'equal) all-labs)
		  for other-run in (butnth (position lab all-labs :test #'equal) all-runs) do
		    (progn
		      (print (list lab run other-lab other-run))
		      (let ((map-filename (format nil "../../../../~a/investigations/reference-antigen-map/~a/pc-other-labs/pc-~a-~a/pc--ref-ag-map-ag-only.gif" lab run other-lab other-run)))
			(format out "<IMG src=~a alt=~a>      " map-filename map-filename))))
	      (newline out)))
	    
      (newline out)
      (format out "</PRE>~%"))))

(defun write-ref-ag-comparison-with-other-labs-2 (all-labs all-runs &optional &key (id (numeric-sortable-date)) (if-exists-action :error))
  (let ((filename (format nil "mds/investigations/strain-selection-meeting/database/combined/reference-maps/pc-among-labs/~a/index2.html" id)))
    (with-open-file (out filename :direction :output :if-exists if-exists-action)
      (format out "<H1><CENTER>Reference map comparisons among labs</CENTER></H1>")
      (newline out)
      (format out "<PRE>")
      (newline out)
      
      (loop for lab in all-labs
	  for run in all-runs do
	    (progn
	      (format out "<HR><h2>~a compared with ~{~a ~}</h2>~%" (string-upcase lab) (mapcar #'string-upcase (butnth (position lab all-labs :test #'equal) all-labs)))
	      (loop for other-lab in (butnth (position lab all-labs :test #'equal) all-labs)
		  for other-run in (butnth (position lab all-labs :test #'equal) all-runs) do
		    (progn
		      (print (list lab run other-lab other-run))
		      (let ((map-filename (format nil "../../../../~a/investigations/reference-antigen-map/~a/pc-other-labs2/pc-~a-~a/pc--ref-ag-map-ag-only.gif" lab run other-lab other-run)))
			(format out "<IMG src=~a alt=~a>      " map-filename map-filename))))
	      (newline out)))
	    
      (newline out)
      (format out "</PRE>~%"))))



;;;----------------------------------------------------------------------
;;;                      cross-procrustes 
;;;----------------------------------------------------------------------

(defun sss-square-pc (master-root master-run master-run-from-table
		      slave-root  slave-run  slave-run-from-table
		      &optional &key 
				(dir (format nil "~a/misc-by-hand/cross-pc" (overlay-merge-root-run master-root master-run))))
  (loop for master-table-name in (get-tables-in-run-directory master-root master-run :run-from-table master-run-from-table) do
	(loop for slave-table-name in (get-tables-in-run-directory slave-root slave-run :run-from-table slave-run-from-table) do
	      (let ((master-save (get-save-after-runs-reoriented master-root master-run master-table-name))
		    (slave-save  (get-save-after-runs-reoriented slave-root  slave-run  slave-table-name)))
		(orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
		 Master-save
		 slave-save
		 (print (format nil "~a/~a-pc-~a" dir master-table-name slave-table-name))
		 :map-window-hook (^ (mds-window) (map-save-on-to-mds-window mds-window slave-save)))))))

(defun sss-square-pc-web-page (master-root master-run master-run-from-table
			       slave-root  slave-run  slave-run-from-table
			       &optional &key 
					 (dir (format nil "~a/misc-by-hand/cross-pc" (overlay-merge-root-run master-root master-run)))
					 (html-filename (format nil "~a/square.html" dir))
					 (if-exists :error))
  (with-open-file (out html-filename :direction :output :if-exists if-exists)
    (format out "<H1><CENTER>Cross procrustes</CENTER></H1>~%")
    (newline out)
    (format out "<PRE>~%")
    (newline out)
    (let ((master-tables (get-tables-in-run-directory master-root master-run :run-from-table master-run-from-table))
	  (slave-tables  (get-tables-in-run-directory slave-root slave-run :run-from-table slave-run-from-table)))
      (loop for master-table-name in master-tables do
	    (progn
	      (loop for slave-table-name in slave-tables do
		    (format out "<a href=~a-pc-~a.gif.labeled.gif.bordered.gif><IMG src=~a-pc-~a.gif.300x300.gif.labeled.gif.bordered.gif alt=~a-pc-~a.gif.300x300.gif.labeled.gif.bordered.gif></a> " 
			    master-table-name slave-table-name
			    master-table-name slave-table-name
			    master-table-name slave-table-name))
	      (newline out))))
    (newline out)
    (format out "</PRE>~%")))

(defun sss-square-pc-small-label-border (master-root master-run master-run-from-table
					 slave-root  slave-run  slave-run-from-table
					 &optional &key 
						   (dir (format nil "~a/misc-by-hand/cross-pc/" (overlay-merge-root-run master-root master-run))))
  (loop for master-table-name in (get-tables-in-run-directory master-root master-run :run-from-table master-run-from-table) do
	(loop for slave-table-name in (get-tables-in-run-directory slave-root slave-run :run-from-table slave-run-from-table) do
	      (make-300x300-and-labeled-bordered-gif 
	       dir
	       (print (format nil "~a-pc-~a.gif" master-table-name slave-table-name))
	       ))))

(defun sss-square-pc-rich (master-root master-run master-run-from-table
			   slave-root  slave-run  slave-run-from-table
			   &optional &key 
				     (dir (format nil "~a/misc-by-hand/cross-pc" (overlay-merge-root-run master-root master-run)))
				     (html-filename (format nil "~a/square.html" dir))
				     (if-exists :error))

  "Rich as in also do the labels and borders and small images.  sss = strain selection structure"

  (sss-square-pc
   master-root master-run master-run-from-table
   slave-root  slave-run  slave-run-from-table
   :dir dir)

  (sss-square-pc-small-label-border
   master-root master-run master-run-from-table
   slave-root  slave-run  slave-run-from-table
   :dir dir)

  (sss-square-pc-web-page
   master-root master-run master-run-from-table
   slave-root  slave-run  slave-run-from-table
   :dir dir
   :html-filename html-filename
   :if-exists if-exists))


#|
(sss-square-pc-rich
 "cdc" "20060127-grouped-names-ref-ag-merge" "20050920"
 "cdc" "20060127-grouped-names-ref-ag-merge" "20050920")
|#


;;;----------------------------------------------------------------------
;;;        get data from terry, and distribute to cc directories
;;;----------------------------------------------------------------------

(defun get-all-hi-data-from-acorg (&optional &key (name (numeric-sortable-date)) (incomming-from-terry-root "mds/investigations/strain-selection-meeting/database/incomming/from-terry/"))
  (run-shell-command (format nil "scp acorg:../terry/data.zip ~a/hi/~a.zip" incomming-from-terry-root name) :wait t))

;; (get-all-hi-data-from-acorg)

(defun distribute-all-hi-data-to-cc-directories (&optional &key (name (numeric-sortable-date)) (incomming-from-terry-root "mds/investigations/strain-selection-meeting/database/incomming/from-terry/"))
  (let ((root (format nil "~a/hi/~a" incomming-from-terry-root name)))
    (run-shell-command (print (format nil "mkdir ~a" root)) :wait t)
    (run-shell-command (print (format nil "cp ~a.zip ~a" root root)) :wait t)
    (run-shell-command (print (format nil "cd ~a/; unzip ~a.zip" root name)) :wait t)
    (loop for lab in '("cdc" "csl" "niid" "nimr") do
	  (run-shell-command (print (format nil "cd ~a/; zip -r ~a-~a ~a" root lab name lab)) :wait t)
	  (run-shell-command (print (format nil "cp ~a/~a-~a.zip ~a/../../../../~a/raw-data/" root lab name root lab)) :wait t)
	  )
    ))


;; ----------------------- format 2 -------------------------------

(defun distribute-all-format2-hi-data-to-cc-directories (&optional &key (name (numeric-sortable-date)) (incomming-from-terry-root "mds/investigations/strain-selection-meeting/database/incomming/from-terry/format2") (labs '("cdc" "melb" "niid" "nimr")))
  (let ((full-root (format nil "~a/~a" incomming-from-terry-root name))
	(hi-root   (format nil "~a/hi/~a" incomming-from-terry-root name)))
    (run-shell-command (print (format nil "mkdir ~a" full-root)) :wait t)
    (run-shell-command (print (format nil "mkdir ~a" hi-root  )) :wait t)
    (loop for lab in labs do
	  (run-shell-command (print (format nil "mkdir ~a/~a" hi-root lab)) :wait t))
    (run-shell-command (print (format nil "cp ~a.zip ~a" full-root full-root)) :wait t)
    (run-shell-command (print (format nil "cd ~a/; unzip ~a.zip" full-root name)) :wait t)
    (loop for lab in labs do
	  (run-shell-command (print (format nil "cd ~a/~a; cp */*.{html,htm,msf} ../../hi/~a/~a/" full-root lab name lab)) :wait t)
	  (run-shell-command (print (format nil "cd ~a/; zip -r ~a-~a ~a" hi-root lab name lab)) :wait t)
	  (run-shell-command (print (format nil "cp ~a/~a-~a.zip ~a/../../../../../~a/raw-data/" hi-root lab name hi-root lab)) :wait t)
	  )
    ))

;;(distribute-all-format2-hi-data-to-cc-directories :name "20050128")

;; format2 goes to a different place in the incoming tree, and also at acorg has a date
(defun get-all-format2-hi-data-from-acorg (&optional &key (name (numeric-sortable-date)) (incomming-from-terry-root "mds/investigations/strain-selection-meeting/database/incomming/from-terry/format2/"))
  (run-shell-command (format nil "scp acorg:../terry/html/ac.org/who/~a/~a.zip ~a/~a.zip" name name incomming-from-terry-root name) :wait t))



;;;----------------------------------------------------------------------
;;;                 status of gridware queue at sfi
;;;----------------------------------------------------------------------

(defun qstat ()
  (run-shell-command "ssh sfi ssh vulture qstat"))


;;;----------------------------------------------------------------------
;;;                      for each lab
;;;----------------------------------------------------------------------

(defun unpack-root-run-data (root run
			     &optional &key
				       run-from-table
				       run-to-table
				       tables-to-exclude
				       save-format-pre-save-f)
  (process-hi-tables :suffix ""
		     :name run
		     :zip-name (format nil "~a-~a.zip" root run)
		     :zip-unpack-name root
		     :zip-directory  (data-directory-from-root root)
		     :root-directory (runs-directory-from-root root)
		     :unpack-database-output t
		     :make-internal-directory-structure t
		     :save-raw-data t
		     :save-format-pre-save-f save-format-pre-save-f
		     :batch-runs nil
		     :save-batch-runs nil
		     :batch-runs-graphics-diagnostics nil
		     :run-from-table run-from-table
		     :run-to-table run-to-table
		     :tables-to-exclude tables-to-exclude
		     :save-color-code-f #'color-code-epi-strong-reference-background-save
		     ))

(defun make-reference-antigen-directory-structure (root run)
  (let* ((reference-antigen-root (format nil "~a/investigations/reference-antigen-map/~a/" (directory-from-root root) run)))
    (run-shell-command (print (format nil "mkdir ~a" reference-antigen-root)) :wait t)
    (run-shell-command (print (format nil "cp ~a/../../../../index-templates/reference-antigen-index.html ~a/index.html" reference-antigen-root reference-antigen-root)) :wait t)
    (run-shell-command (print (format nil "perl -pi -e s/RUNNAME/~a/g ~a/index.html" run reference-antigen-root)) :wait t)))


(defun make-reference-antigen-merge (root run
				     &optional &key
					       exclude-tables
					       (multiple-values-f #'average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold))
  (let* ((reference-antigen-root (format nil "~a/investigations/reference-antigen-map/~a/" (directory-from-root root) run))
	 (reference-antigen-tables (get-reference-antigen-table-s root run :exclude-tables exclude-tables))
	 (merge-save (merge-tables 
		      reference-antigen-tables
		      :multiple-values-f multiple-values-f
		      :filename (format nil "~a/merge-table.txt" reference-antigen-root)
		      :if-exists-action :supersede)))
    (write-save-form 
     merge-save
     (format nil "~a/merge.save" reference-antigen-root))
 
    (fi (hi-table-antigens (un-as-hi-table (table-from-save merge-save))) 
	(format nil "~a/merge-antigens.txt" reference-antigen-root)
	:error
	nil
	:write-outer-list-elements-individually t)

    (fi (hi-table-sera (un-as-hi-table (table-from-save merge-save))) 
	(format nil "~a/merge-sera.txt" reference-antigen-root)
	:error
	nil
	:write-outer-list-elements-individually t)))


;; --------------- make reference-antigen merge -------------------------
(defun make-reference-antigen-map (root run 
				   &optional &key 
					     (num-runs 100)
					     run-at-sfi-p                 
					     (bundled-runs-if-run-at-sfi-p t) ;; default changed to be same as running tables
					     master-orientation-root
					     master-orientation-run
					     reference-antigen-map-to-compare-root
					     reference-antigen-map-to-compare-run
					     )

  (if (not (and master-orientation-root master-orientation-run))
      (error "Pass master-orientation-root and master-orientation-run to make-reference-antigen-map (pre NH 2006 defaulted to cdc 20050128-grouped-names), master will be taken from the reference antigen map of the specified root run"))

  (let* ((reference-antigen-root (format nil "~a/investigations/reference-antigen-map/~a/" (directory-from-root root) run))
	 (merge-save (fi-in (format nil "~a/merge.save" reference-antigen-root))))

    (run-shell-command (print (format nil "mkdir ~a/routine-diagnostics/" reference-antigen-root)) :wait t)
    (run-shell-command (print (format nil "cp ~a/../../../../index-templates/individual-table-index-template.html ~a/routine-diagnostics/index.html" reference-antigen-root reference-antigen-root)) :wait nil)
    
    (table-analsysis
     (color-code-reference-antigen-save 
      (set-save-keyword-entry merge-save :reference-antigens (hi-table-antigens (un-asl-hi-table (table-from-save merge-save))) ::not-found-action :add))
     (format nil "~a/routine-diagnostics/" reference-antigen-root)
     :num-runs num-runs
     :run-at-sfi-p run-at-sfi-p
     :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p
     ;;:master-orientation-save-filename "mds/investigations/strain-selection-meeting/database/cdc/investigations/reference-antigen-map/routine-diagnostics/save-after-runs-reoriented.save"
     ;;:master-orientation-save-filename (format nil "~a/src/mds/mds/investigations/strain-selection-meeting/database/orientation-saves/cdc-20050128-grouped-names-reference-antigen-map.save" (sys:getenv "MDS_ROOT"))
     :master-orientation-save-filename (format nil "~a/save-after-runs-reoriented.save" (reference-antigen-root-run-routine-diagnostics master-orientation-root master-orientation-run))))
  (if (and (not run-at-sfi-p)
	   reference-antigen-map-to-compare-root
	   reference-antigen-map-to-compare-run)
      (compare-ref-ag-maps 
       root run
       reference-antigen-map-to-compare-root reference-antigen-map-to-compare-run)))

(defun make-reference-antigen-map-continue-after-runs-finished-at-sfi (root run
								       &optional &key
										 master-orientation-root
										 master-orientation-run
										 reference-antigen-map-to-compare-root
										 reference-antigen-map-to-compare-run)

  (if (not (and master-orientation-root master-orientation-run))
      (error "Pass master-orientation-root and master-orientation-run to make-reference-antigen-map (pre NH 2006 defaulted to cdc 20050128-grouped-names), master will be taken from the reference antigen map of the specified root run"))

  (let* ((routine-diagnoistics-dir (format nil "~a/investigations/reference-antigen-map/~a/routine-diagnostics" (directory-from-root root) run)))
    (recover-mds-runs-from-sfi-in-routine-diagnostics-directory 
     routine-diagnoistics-dir
     :num-solutions-to-show 5)
    (make-mds-run-tk-graphics-in-routine-diagnostics-directory 
     routine-diagnoistics-dir
     :num-solutions-to-show 5
     :master-orientation-save-filename (format nil "~a/save-after-runs-reoriented.save" (reference-antigen-root-run-routine-diagnostics master-orientation-root master-orientation-run)))
    (convert-mds-run-tk-graphics-to-ps-in-routine-diagnostics-directory 
     routine-diagnoistics-dir
     :num-solutions-to-show 5))
  (if (and reference-antigen-map-to-compare-root
	   reference-antigen-map-to-compare-run)
      (compare-ref-ag-maps 
       root run
       reference-antigen-map-to-compare-root reference-antigen-map-to-compare-run)))



#|
;; need to move aside stress-* and save-after-runs-reoriented.save in the routine-diagnostics
(defun make-reference-antigen-map-continue-after-runs-finished-at-sfi-hack-to-reorient-after-running (root run)
  (let* ((routine-diagnoistics-dir (format nil "~a/investigations/reference-antigen-map/~a/routine-diagnostics" (directory-from-root root) run)))
    (make-mds-run-tk-graphics-in-routine-diagnostics-directory 
     routine-diagnoistics-dir
     :num-solutions-to-show 5
     :master-orientation-save-filename (format nil "~a/src/mds/mds/investigations/strain-selection-meeting/database/orientation-saves/cdc-20050128-grouped-names-reference-antigen-map.save" (sys:getenv "MDS_ROOT")))
    (convert-mds-run-tk-graphics-to-ps-in-routine-diagnostics-directory 
     routine-diagnoistics-dir
     :num-solutions-to-show 5)))
|#


;; ----------------- combining the above ----------------------

(defun set-up-to-look-at-reference-map-strains (root run
						&optional &key
							  exclude-tables
							  (multiple-values-f #'average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold)
							  )
  ;; (unpack-root-run-data root run)  twice now, i've wanted this separately, so make it so
  (make-reference-antigen-directory-structure root run)
  (make-reference-antigen-merge root run :exclude-tables exclude-tables :multiple-values-f multiple-values-f))



;;;----------------------------------------------------------------------
;;;                compare ref ag maps from two runs
;;;----------------------------------------------------------------------

(defun compare-ref-ag-maps (master-root master-run slave-root slave-run)
  (let ((master-ref-ag-save (get-reference-antigen-root-run-save-after-runs-reoriented master-root master-run))
	(slave-ref-ag-save (get-reference-antigen-root-run-save-after-runs-reoriented slave-root slave-run)))
    (orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
     master-ref-ag-save
     slave-ref-ag-save
     (format nil "~a/pc-~a-~a" (reference-antigen-root-run master-root master-run) slave-root slave-run)
     :map-window-hook (^ (mds-window) (map-save-on-to-mds-window mds-window slave-ref-ag-save)))))


;;;----------------------------------------------------------------------
;;;       merge each tables epi strains with the lab-ref-ag-map
;;;----------------------------------------------------------------------

(defun setup-data-output-before-runs (root run &optional &key tables-to-exclude run-from-table run-to-table)
  (process-hi-tables :name run
		     :run-from-table run-from-table
		     :run-to-table run-to-table
		     :tables-to-exclude tables-to-exclude
		     :root-directory (runs-directory-from-root root)
		     :save-filename "save-of-table-only.save"
		     :save-color-code-f #'color-code-epi-strong-reference-background-save
		     :unpack-database-output nil
		     :make-internal-directory-structure nil
		     :save-raw-data t
		     :dont-save-save-of-table-only t
		     :batch-runs nil
		     :save-batch-runs nil
		     :batch-runs-graphics-diagnostics nil
		     ))

(defun make-table-runs (root run &optional &key tables-to-exclude run-from-table run-to-table (num-runs 10) run-at-sfi-p (bundled-runs-if-run-at-sfi-p t))
  (process-hi-tables :name run
		     :run-from-table run-from-table
		     :run-to-table run-to-table
		     :tables-to-exclude tables-to-exclude
		     :root-directory (runs-directory-from-root root)
		     :save-filename "save-of-table-only.save"
		     :save-color-code-f #'color-code-epi-strong-reference-background-save
		     :unpack-database-output nil
		     :make-internal-directory-structure nil
		     :save-raw-data nil
		     :batch-runs t
		     :save-batch-runs t
		     :num-runs num-runs
		     :num-dimensions 2
		     :dim-anneal-starting-coords 5
		     :run-at-sfi-p run-at-sfi-p
		     :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p 
		     :batch-runs-graphics-diagnostics nil
		     ))

(defun recover-batch-runs-from-sfi (root run &optional &key tables-to-exclude run-from-table run-to-table)
  (process-hi-tables :name run
		     :run-from-table run-from-table
		     :run-to-table run-to-table
		     :tables-to-exclude tables-to-exclude
		     :root-directory (runs-directory-from-root root)
		     :save-filename "/pending/save-after-runs.save"
		     :save-color-code-f #'color-code-epi-strong-reference-background-save
		     :unpack-database-output nil
		     :make-internal-directory-structure nil
		     :save-raw-data nil
		     :batch-runs nil
		     :recover-batch-runs-from-sfi t
		     :save-batch-runs t
		     :run-at-sfi-p nil
		     :batch-runs-graphics-diagnostics nil
		     ))

(defun make-table-run-graphics (root run &optional &key tables-to-exclude run-from-table run-to-table (master-orientation-root root) (master-orientation-run run))
  (process-hi-tables :name run
		     :run-from-table run-from-table
		     :run-to-table run-to-table
		     :tables-to-exclude tables-to-exclude
		     :root-directory (runs-directory-from-root root)
		     :save-filename "save-after-runs.save"
		     :save-color-code-f #'color-code-epi-strong-reference-background-save
		     :unpack-database-output nil
		     :make-internal-directory-structure nil
		     :save-raw-data nil
		     :batch-runs nil
		     :save-batch-runs nil
		     :batch-runs-graphics-diagnostics t
		     :master-orientation-save-filename (format nil "~a/investigations/reference-antigen-map/~a/routine-diagnostics/save-after-runs-reoriented.save" (directory-from-root master-orientation-root) master-orientation-run)
		     :num-solutions-to-show 3
		     ))



(defun make-table-run-graphics-in-parts-avoiding-wish-from-lisp (root run &optional &key 
										    tables-to-exclude
										    run-from-table
										    run-to-table
										    (master-orientation-root root)
										    (master-orientation-run  run))

    (setq *ugly-hack-to-write-to-file-instead-of-wish* t)
    (make-table-run-graphics root run
			     :master-orientation-root master-orientation-root
			     :master-orientation-run  master-orientation-run
			     :tables-to-exclude       tables-to-exclude
			     :run-from-table          run-from-table
			     :run-to-table            run-to-table)
    (setq *ugly-hack-to-write-to-file-instead-of-wish* nil)


    (setq *ugly-hack-to-write-to-file-instead-of-wish* nil)
    (setq *ugly-hack-to-convert-tcl-to-ps* t)
    (make-table-run-graphics root run
			     :master-orientation-root master-orientation-root
			     :master-orientation-run  master-orientation-run
			     :tables-to-exclude       tables-to-exclude
			     :run-from-table          run-from-table
			     :run-to-table            run-to-table)
    (setq *ugly-hack-to-convert-tcl-to-ps* nil)

    (label-gifs-in-root-run         root run :exclude-tables tables-to-exclude :run-from-table run-from-table :run-to-table run-to-table)
    (make-300x300-gifs-in-root-run  root run :exclude-tables tables-to-exclude :run-from-table run-from-table :run-to-table run-to-table)
    (label-300x300-gifs-in-root-run root run :exclude-tables tables-to-exclude :run-from-table run-from-table :run-to-table run-to-table)
    (border-gifs-in-root-run        root run :exclude-tables tables-to-exclude :run-from-table run-from-table :run-to-table run-to-table)

    (write-ref-ag-with-epi-index-pages root run :if-exists-action :supersede))




;;;----------------------------------------------------------------------
;;;             special handing if greater-thans, for csl
;;;----------------------------------------------------------------------

(defun change-gt-to-dc-in-table (table)
  (f-hi-table
   (^ (titer)
      (if (gt-threshold-p titer)
	  'dont-care
	titer))
   table))
   
(defun change-gt-to-dc-in-save (save)
  (set-table-in-save
   save
   (change-gt-to-dc-in-table (table-from-save save))))

(defun change-gt-to-dc-in-tables (root run &optional &key 
						     tables-to-exclude
						     run-from-table
						     run-to-table)
  (loop for table in (get-tables-in-run-directory root run 
						  :exclude-tables tables-to-exclude
						  :run-from-table run-from-table
						  :run-to-table run-to-table) do
	(let ((save-of-table-only (get-save-of-table-only root run table)))
	  (run-shell-command (format nil "mv ~a/save-of-table-only.save ~a/save-of-table-only-before-change-gt-to-dc-in-save.save"
				     (directory-from-root-run-table root run table)
				     (directory-from-root-run-table root run table)))
	  (write-save-form 
	   (change-gt-to-dc-in-save save-of-table-only)
	   (string-append (directory-from-root-run-table root run table) "save-of-table-only.save")))))



;;;----------------------------------------------------------------------
;;;                      grouping of names
;;;----------------------------------------------------------------------

(defun abbrev-long-to-abbrev-short-alist (names)
  (loop for name in names collect
	(let* ((name (string name))
	       (name-without-internal-suffix 
		(if (not (= 0 (length (substring-after-char #\- name))))
		    ;; name contains a "-"
		    (read-from-string
		     (string-append 
		      (substring-before-char #\- name)
		      "/"
		      (substring-after-char #\/ (substring-after-char #\- name)))))))
	  (list (read-from-string name) name-without-internal-suffix))))

(defun get-potential-name-equivalents (names)
  (let* (name-equivalents
	 (abbrev-long-to-abbrev-short-alist (abbrev-long-to-abbrev-short-alist names)))
    (loop for name in names do
	  (let* ((short-name (assoc-value-1 name abbrev-long-to-abbrev-short-alist)))
	    (if short-name
		(if (assoc short-name name-equivalents)
		    ;;(push-end name (assoc short-name name-equivalents))
		    (setq name-equivalents
		      (replace-nth (position short-name (nths 0 name-equivalents))
				   (reverse (cons name (reverse (assoc short-name name-equivalents))))
				   name-equivalents))
		  (if t ; (member short-name names)  ;; only make equivalence class if we also have the shorter name
		      (setq name-equivalents (cons (list short-name name) name-equivalents)))))))
    (reverse 
     (loop for (first . rest) in name-equivalents append
	   (if (member first names)
	       (list (cons first rest))
	     (if (> (length rest) 1)
		 (list rest)))))))


(defun write-pp-table-group-name-subsets (root run save-subsets ag-or-sr-p)
  (if (not (file-or-directory-exists-p (reference-antigen-group-names-root root run)))
      (run-shell-command (format nil "mkdir ~a" (reference-antigen-group-names-root root run)) :wait t))
  (loop for save in save-subsets do
	(pp-hi-table (un-asl-hi-table (table-from-save save)) 'full 4 nil 
		     :filename (format nil "~a/~a.txt" 
				       (reference-antigen-group-names-root root run)
				       (international-strain-format-to-lapedes-strain-format 
					(car (funcall (case ag-or-sr-p
							(ag #'hi-table-antigens-short)
							(sr #'hi-table-sera-short)
							(t  #'hi-table-antigens))
						      (table-from-save save)))))))
  (loop for save in save-subsets do
	(pp-hi-table (un-asl-hi-table (table-from-save save)) 'short 4 nil 
		     :filename (format nil "~a/~a-short-sr.txt" 
				       (reference-antigen-group-names-root root run)
				       (international-strain-format-to-lapedes-strain-format 
					(car (funcall (case ag-or-sr-p
							(ag #'hi-table-antigens-short)
							(sr #'hi-table-sera-short)
							(t  #'hi-table-antigens))
						      (table-from-save save))))))))


(defun write-grouped-name-images (root run name-equivalent-classes ag-or-sr-p)
  (let* ((master-orientation-save (get-reference-antigen-root-run-save-after-runs-reoriented root run))
	 (merge-save-runs master-orientation-save))
    (loop for name-equivalent-class in name-equivalent-classes do
	  (orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif
	   (turn-on-names-in-save merge-save-runs (mapcar (case ag-or-sr-p
							    (ag #'suffix-as-ag)
							    (sr #'suffix-as-sr)
							    (t #'id))
							  name-equivalent-class))
	   master-orientation-save
	   (format nil "~a/~a" 
		   (reference-antigen-group-names-root root run)
		   (international-strain-format-to-lapedes-strain-format 
		    (funcall 
		     (case ag-or-sr-p
		       (ag #'suffix-as-ag)
		       (sr #'suffix-as-sr)
		       (t  #'id))
		     (car name-equivalent-class))))
	   :reoriented-save-filename
	   (format nil "~a/~a.save" 
		   (reference-antigen-group-names-root root run)
		   (international-strain-format-to-lapedes-strain-format 
		    (funcall 
		     (case ag-or-sr-p
		       (ag #'suffix-as-ag)
		       (sr #'suffix-as-sr)
		       (t  #'id))
		     (car name-equivalent-class))))))))

(defun write-ref-ag-grouped-name-diagnostics-page (grouped-name-root names &optional &key (if-exists-action :error))
  (with-open-file (out (format nil "~a/index.html" grouped-name-root) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>Muliple instances of antigens and sera, to possibly group</CENTER></H1>")
    (newline out)
    (format out "<PRE>")
    (newline out)
    (loop for name in names do
	  (format out "<hr><h2>~a</h2>" name)
	  (newline out)
	  (format out "<IMG src=~a.gif alt=~a.gif>" name name)
	  (newline out)
	  (newline out)
	  (format out "pdf <A href=~a.pdf>here</A>" name)
	  (newline out)
	  (format out "save <A href=~a.save>here</A>" name)
	  (newline out)
	  (newline out)
	  (format out "HI table, extracted from the reference strain merge, of the variants of this strain (with full serum names) <A href=~a.txt>here</A>" name)
	  (newline out)
	  (format out "Same table, with abbreviated serum names, follows:")
	  (newline out)
	  (loop for line in (fi-in-readline (format nil "~a/~a-short-sr.txt" grouped-name-root name)) do
		(format out line)
		(newline out)))
    (newline out)
    (format out "</PRE>")
    (newline out)))




(defun make-name-group-diagnostic-page (root run
					&optional &key
						  (antigen-name-equivalent-classes 
						   (get-potential-name-equivalents (get-reference-antigen-map-antigen-names root run)))
						  (sera-name-equivalent-classes    
						   (get-potential-name-equivalents (get-reference-antigen-map-sera-names    root run))))

  (format t "~2%   Might want to hand edit the following ag and sr lists to change the diagnostics page groupings~%       (if so rerun make-name-group-diagnostic-page and pass them as arguments (will have to delete some files that were created on this run)~%")
  (ppl antigen-name-equivalent-classes)
  (newline)
  (ppl sera-name-equivalent-classes)
  (newline)
  (newline)
  (newline)

  (let* ((merge-save (get-reference-antigen-root-run-save-after-runs-reoriented root run))

	 (sera-name-equivalent-classes-save-subsets
	  (let ((antigen-names (hi-table-antigens-short (table-from-save merge-save))))
	    (loop for sera-name-equivalent-class in sera-name-equivalent-classes collect
		  (subset-save-form merge-save (append antigen-names (print (mapcar #'suffix-as-sr sera-name-equivalent-class)))))))

	 (antigen-name-equivalent-classes-save-subsets
	  (let ((sera-names (hi-table-sera-short (table-from-save merge-save))))
	    (loop for antigen-name-equivalent-class in antigen-name-equivalent-classes collect
		  (subset-save-form merge-save (append (print (mapcar #'suffix-as-ag antigen-name-equivalent-class)) sera-names))))))

    (write-pp-table-group-name-subsets root run sera-name-equivalent-classes-save-subsets 'sr)
    (write-pp-table-group-name-subsets root run antigen-name-equivalent-classes-save-subsets 'ag)

    (write-grouped-name-images root run sera-name-equivalent-classes 'sr)
    (write-grouped-name-images root run antigen-name-equivalent-classes 'ag)

    (write-ref-ag-grouped-name-diagnostics-page
     (reference-antigen-group-names-root root run)
     (mapcar 
      #'international-strain-format-to-lapedes-strain-format
      (append 
       (mapcar #'suffix-as-ag (mapcar #'car antigen-name-equivalent-classes))
       (mapcar #'suffix-as-sr (mapcar #'car sera-name-equivalent-classes))))
     :if-exists-action :supersede)))


 ;;;------------------- name grouping -------------------------------


(defun make-old-new-ag-sr-alist (antigens-to-group sera-to-group)
  ;; check for an error i've made before, leaving the -ag or -sr on the strain
  (loop for name in (flatten (list antigens-to-group sera-to-group)) do
	(if (or (ag-name-p name)
		(sr-name-p name))
	    (error "~%Strain ~a has a -ag or -sr suffix, should not." name)))
  (let ((antigen-old-new-alist
	 (loop for (new olds) in antigens-to-group append
	       (loop for old in olds collect
		     (list old new))))
	(sera-old-new-alist
	 (loop for (new olds) in sera-to-group append
	       (loop for old in olds collect
		     (list old new)))))
    (append
     (loop for pair in antigen-old-new-alist collect (mapcar #'suffix-as-ag pair))
     (loop for pair in sera-old-new-alist    collect (mapcar #'suffix-as-sr pair)))))


(defun save-change-names-remove-dont-care-rows-and-cols (save-of-table-only old-new-alist
							 &optional &key table-output-filename)
  (let* ((reduced-save (merge-saves-no-coordss 
			(list 
			 (set-names-in-save-from-name-alist
			  save-of-table-only
			  old-new-alist
			  :permit-alist-having-names-not-in-save t))
			:table-output-filename table-output-filename))
	 (old-new-antigen-alist
	  (loop for (old new) in old-new-alist
	      when (ag-name-p old)
	      collect (mapcar #'remove-ag-sr-from-name (list old new))))
	 (reduced-reduced-save
	  (make-save-form
	   :hi-table (table-from-save reduced-save)
	   :reference-antigens (remove-duplicates 
				(loop for ref-ag in (reference-antigens-from-save reduced-save) collect
				      (if (assoc ref-ag old-new-antigen-alist)
					  (assoc-value-1 ref-ag old-new-antigen-alist)
					ref-ag))))))
    reduced-reduced-save))


(defun change-names-in-save-by-old-new-alist-and-remerge-to-remove-dont-cares (root run old-new-alist)
  (error "name changed to change-names-in-saves-by-old-new-alist-and-remerge-to-remove-dont-cares (saves not save), or better use save-change-names-remove-dont-care-rows-and-cols"))

(defun change-names-in-saves-by-old-new-alist-and-remerge-to-remove-dont-cares (root run old-new-alist)
  (loop for table in (get-tables-in-run-directory root run) do
	(let ((save-of-table-only (get-save-of-table-only root run table)))
	  (run-shell-command (format nil "mv ~a/save-of-table-only.save ~a/save-of-table-only-before-name-grouping.save"
				     (directory-from-root-run-table root run table)
				     (directory-from-root-run-table root run table)))
	  (write-save-form 
	   (save-change-names-remove-dont-care-rows-and-cols save-of-table-only old-new-alist)
	   (string-append (directory-from-root-run-table root run table) "save-of-table-only.save")))))



;;;----------------------------------------------------------------------
;;;                        name manipulation
;;;----------------------------------------------------------------------

(defun smart-location-suffix (name suffix)
  (setq name (string name))
  (read-from-string 
   (if (and (substring-after-char #\/ name)
	    (substring-after-char #\/ (substring-after-char #\/ name)))
       (let* ((first-part  (substring-before-char #\/ name))
	      (second-part (substring-before-char #\/ (substring-after-char #\/ name)))
	      (rest-part   (substring-after-char #\/ (substring-after-char #\/ name))))
	 (string-append
	  first-part
	  "/"
	  second-part
	  suffix
	  "/"
	  rest-part))
     (if (and (substring-after-char #\/ name))
	 (let* ((first-part  (substring-before-char #\/ name))
		(rest-part   (substring-after-char #\/ name)))
	   (string-append
	    first-part
	    suffix
	    "/"
	    rest-part))
       (string-append
	suffix   ;; name will start with - if suffix does, but that is ok
	"-"
	name)))))
	 


;;;----------------------------------------------------------------------
;;;                    routine diagnostics on save
;;;----------------------------------------------------------------------

(defun routine-diagnostics-on-save (save directory-for-results &optional &key 
									 (num-runs 10)
									 (num-dimensions 2)
									 (dim-anneal-starting-coords 5)
									 (master-orientation-save-filename save)
									 (num-solutions-to-show 5)
                                                                         run-at-sfi-p
                                                                         (bundled-runs-if-run-at-sfi-p t)
									 )
  (if (not (file-or-directory-exists-p directory-for-results))
      (run-shell-command (format nil "mkdir ~a" directory-for-results) :wait t))
  ;; the below commended out as table-analysis writes it as default
  ;;(write-save-form save (format nil "~a/save-of-table-only.save" directory-for-results) :if-exists if-exists-action)

  (if (not (file-or-directory-exists-p (format nil "~a/index.html" directory-for-results)))
      (run-shell-command (format nil "cp ~a/mds/individual-table-index-template.html ~a/index.html" 
                                 *unix-source-filename-root*
                                 directory-for-results) :wait t))

  ;; todo: make a link
  (run-shell-command-and-wait 
   (print (format nil "perl -pi.raw -e \"s#master-oriention-save-filename-place-holder#~a#\" ~a"
           (if (stringp master-orientation-save-filename)
               (format nil "The above line of maps were procrustesed against, and all maps on this page oriented to, the master-orientiaion-save specified for these routine diagnostics: ~a"
                       master-orientation-save-filename)
             "No master-orientation-save-filename was specified.")
           (format nil "~a/index.html" directory-for-results))))

  (table-analsysis 
   save
   directory-for-results
   :num-runs num-runs
   :num-dimensions num-dimensions 
   :dim-anneal-starting-coords dim-anneal-starting-coords 
   :run-at-sfi-p run-at-sfi-p
   :bundled-runs-if-run-at-sfi-p bundled-runs-if-run-at-sfi-p    ;; have not yet set up protocol to get runs back and continue
   :num-solutions-to-show num-solutions-to-show
   :panel-map-correlation-plot t
   :master-orientation-save-filename master-orientation-save-filename
   :save-raw-data t 
   :batch-runs t 
   :save-batch-runs t 
   :batch-runs-graphics-diagnostics t
   ))

;; error above, writing the postscript (i think), when the whole, or part of, the filename is too long, this last part of the one below for the
;; directory is too long
;; "mds/investigations/strain-selection-meeting/database/nimr/investigations/overlay-merge/20040910-ref-ag-merge/overlay-merge-save-raw-selected-reoptimized-from-scratch-diagnostics/"



(defun routine-diagnostics-on-table-or-save-from-gui (filename &optional &key 
                                                                         (num-runs 25)
                                                                         (num-dimensions 2)
                                                                         (dim-anneal-starting-coords 5)
                                                                         master-orientation-save-filename
                                                                         (num-solutions-to-show 3)
                                                                         run-at-sfi-p
                                                                         (bundled-runs-if-run-at-sfi-p t)
                                                                         )
  (let ((save (if (save-file-p filename)
                  (fi-in filename)
                (make-save-form :hi-table (read-hi-table-and-convert filename 1 0)))))
    (routine-diagnostics-on-save 
     (set-starting-coordss-in-save save nil :not-found-action :add)
     (format nil "~a-rd" 
             (if (substring-after-char #\. (reverse filename))
                 (reverse (substring-after-char #\. (reverse filename)))
               filename))
     :num-runs                         num-runs
     :num-dimensions                   (if num-dimensions 
                                           num-dimensions
                                         (if (num-dimensions-from-save save)
                                             (num-dimensions-from-save save)
                                           2))
     :dim-anneal-starting-coords       dim-anneal-starting-coords
     :master-orientation-save-filename (if master-orientation-save-filename
                                           master-orientation-save-filename
                                         save)
     :num-solutions-to-show            num-solutions-to-show
     :run-at-sfi-p                     run-at-sfi-p
     :bundled-runs-if-run-at-sfi-p     bundled-runs-if-run-at-sfi-p
     )))
   


(defun routine-diagnostics-on-existing-save (save directory-for-results &optional &key 
										  (master-orientation-save-filename save)
										  (num-solutions-to-show 5)
										  )
  (if (not (file-or-directory-exists-p directory-for-results))
      (run-shell-command (format nil "mkdir ~a" directory-for-results) :wait t))
  ;; the below commended out as table-analysis writes it as default
  ;;(write-save-form save (format nil "~a/save-of-table-only.save" directory-for-results) :if-exists if-exists-action)
  (if (not (file-or-directory-exists-p (format nil "~a/index.html" directory-for-results)))
      (run-shell-command (format nil "cp ~a/mds/individual-table-index-template.html ~a/index.html" 
                                 *unix-source-filename-root*
                                 directory-for-results)
                         :wait t))
  (table-analsysis 
   save
   directory-for-results
   :batch-runs nil
   :num-solutions-to-show num-solutions-to-show
   :panel-map-correlation-plot t
   :master-orientation-save-filename master-orientation-save-filename
   :save-raw-data t 
   :save-batch-runs t 
   :batch-runs-graphics-diagnostics t
   ))



(defun routine-diagnostics-on-existing-save-from-gui (save-filename &optional &key 
                                                                              master-orientation-save-filename
                                                                              (num-solutions-to-show 3)
                                                                              )
  (let ((save (fi-in save-filename)))
    (routine-diagnostics-on-existing-save
     save
     (format nil "~a-rd" (reverse (substring-after-char #\. (reverse save-filename))))
     :master-orientation-save-filename (if master-orientation-save-filename
                                           master-orientation-save-filename
                                         save)
     :num-solutions-to-show            num-solutions-to-show)))
     


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;;                            FOR MAKE
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------


;;;----------------------------------------------------------------------
;;;                      msf to save 
;;;----------------------------------------------------------------------

(defun msf-to-save-in-routine-diagnostics-directory (routine-diagnostics-directory)
  (write-save-form
   (eval (fi-in (format nil "~a/input.msf" routine-diagnostics-directory)))
   (format nil "~a/input.save" routine-diagnostics-directory)))


;;;----------------------------------------------------------------------
;;;                    color code save
;;;----------------------------------------------------------------------

(defun color-code-save-in-routine-diagnostics-directory (routine-diagnostics-directory &optional &key color-code-save-f)
  (write-save-form
   (funcall color-code-save-f (fi-in (format nil "~a/input.save" routine-diagnostics-directory)))
   (format nil "~a/input-color-coded.save" routine-diagnostics-directory)))


;;;----------------------------------------------------------------------
;;;                      do the mds runs
;;;----------------------------------------------------------------------

(defun mds-runs-in-routine-diagnostics-directory (routine-diagnostics-directory 
						  &optional &key
							    (num-runs                     10) 
							    (run-at-sfi-p                 nil)
							    (bundled-runs-if-run-at-sfi-p t)
							    (num-dimensions               2)
							    (dim-anneal-starting-coords   5)
							    (num-solutions-to-show        3)
							    )
  (table-analsysis
   (fi-in (format nil "~a/input-color-coded.save" routine-diagnostics-directory))
   routine-diagnostics-directory
   :num-runs                         num-runs
   :num-dimensions                   num-dimensions
   :dim-anneal-starting-coords       dim-anneal-starting-coords
   :run-at-sfi-p                     run-at-sfi-p
   :bundled-runs-if-run-at-sfi-p     bundled-runs-if-run-at-sfi-p 
   :recover-batch-runs-from-sfi      nil
   :num-solutions-to-show            num-solutions-to-show
   :master-orientation-save-filename 'not-set
   :save-raw-data                    nil
   :dont-save-save-of-table-only     nil
   :batch-runs                       t
   :save-batch-runs                  t
   :batch-runs-graphics-diagnostics  nil
   ))


;;;----------------------------------------------------------------------
;;;                   recover batch runs from sfi
;;;----------------------------------------------------------------------

;;(recover-mds-runs-from-sfi-in-routine-diagnostics-directory routine-diagnostics-directory)

(defun recover-mds-runs-from-sfi-in-routine-diagnostics-directory (routine-diagnostics-directory
								   &optional &key
									     (num-solutions-to-show      3)
									     )
  (table-analsysis
   (fi-in (format nil "~a/pending/save-after-runs.save" routine-diagnostics-directory))
   routine-diagnostics-directory
   :num-runs                         'not-set
   :num-dimensions                   'not-set
   :dim-anneal-starting-coords       'not-set
   :run-at-sfi-p                     nil
   :recover-batch-runs-from-sfi      t
   :num-solutions-to-show            num-solutions-to-show
   :master-orientation-save-filename 'not-set
   :save-raw-data                    nil
   :dont-save-save-of-table-only     nil
   :batch-runs                       nil
   :save-batch-runs                  t
   :batch-runs-graphics-diagnostics  nil
   ))


;;;----------------------------------------------------------------------
;;;          generate the (tk) graphics commands from the runs
;;;----------------------------------------------------------------------

(defun make-mds-run-tk-graphics-in-routine-diagnostics-directory (routine-diagnostics-directory
								  &optional &key
									    (master-orientation-save-filename 'not-set)
									    (num-solutions-to-show            3))
									    
  (setq *ugly-hack-to-write-to-file-instead-of-wish* t)

  (table-analsysis
   (fi-in (format nil "~a/save-after-runs.save" routine-diagnostics-directory))
   routine-diagnostics-directory
   :num-runs                         'not-set
   :num-dimensions                   'not-set
   :dim-anneal-starting-coords       'not-set
   :run-at-sfi-p                     nil
   :recover-batch-runs-from-sfi      nil
   :num-solutions-to-show            num-solutions-to-show
   :master-orientation-save-filename master-orientation-save-filename
   :save-raw-data                    nil
   :dont-save-save-of-table-only     nil
   :batch-runs                       nil
   :save-batch-runs                  nil
   :batch-runs-graphics-diagnostics  t
   )

  (setq *ugly-hack-to-write-to-file-instead-of-wish* nil))



;;;----------------------------------------------------------------------
;;;                      convert tcl to ps
;;;----------------------------------------------------------------------

(defun convert-mds-run-tk-graphics-to-ps-in-routine-diagnostics-directory (routine-diagnostics-directory
									   &optional &key
										     (num-solutions-to-show 3))
									    
  (setq *ugly-hack-to-write-to-file-instead-of-wish* nil)
  (setq *ugly-hack-to-convert-tcl-to-ps* t)

  (table-analsysis
   (fi-in (format nil "~a/save-after-runs.save" routine-diagnostics-directory))
   routine-diagnostics-directory
   :num-runs                         'not-set
   :num-dimensions                   'not-set
   :dim-anneal-starting-coords       'not-set
   :run-at-sfi-p                     nil
   :recover-batch-runs-from-sfi      nil
   :num-solutions-to-show            num-solutions-to-show
   :master-orientation-save-filename 'not-set
   :save-raw-data                    nil
   :dont-save-save-of-table-only     nil
   :batch-runs                       nil
   :save-batch-runs                  nil
   :batch-runs-graphics-diagnostics  t
   )

  (setq *ugly-hack-to-convert-tcl-to-ps* nil))


;;;----------------------------------------------------------------------
;;;                 giffs and make smaller and bordered images
;;;----------------------------------------------------------------------

(defun make-smaller-and-bordered-gifs-in-routine-diagnostics-directory (routine-diagnostics-directory &optional &key (label ""))
  (label-gifs-in-routine-diagnostics-directory  routine-diagnostics-directory label)
  (border-gifs-in-routine-diagnostics-directory routine-diagnostics-directory)
  (make-300x300-gifs-in-routine-diagnostics-directory routine-diagnostics-directory)
  (label-300x300-gifs-in-routine-diagnostics-directory routine-diagnostics-directory label))



;;;----------------------------------------------------------------------
;;;                            TO ADD
;;;----------------------------------------------------------------------

;; making preliminary data such as text version of hi table and sfi format
;; gt-to-dont-care for melb data




#|
cp ~/mds/src/mds/mds/investigations/strain-selection-meeting/database/csl/runs/20040912/batch-processing/20031002/save-of-table-only.save /tmp/test    ;; out of date, should go to input.save
(msf-to-save-in-routine-diagnostics-directory 
 "/tmp/test")   
(color-code-save-in-routine-diagnostics-directory 
 "/tmp/test"
 :color-code-save-f #'color-code-reference-antigen-save)
(mds-runs-in-routine-diagnostics-directory 
 "/tmp/test"
 :num-runs 2
 :num-dimensions 2
 :dim-anneal-starting-coords 5
 :run-at-sfi-p nil
 :num-solutions-to-show 3)
(make-mds-run-tk-graphics-in-routine-diagnostics-directory 
 "/tmp/test"
 :num-solutions-to-show 3
 :master-orientation-save-filename (format nil "~a/src/mds/mds/investigations/strain-selection-meeting/master-save-detail.save" (sys:getenv "MDS_ROOT")))
(convert-mds-run-tk-graphics-to-ps-in-routine-diagnostics-directory 
 "/tmp/test"
 :num-solutions-to-show 3)
(make-smaller-and-bordered-gifs-in-routine-diagnostics-directory 
 "/tmp/test"
 :label "Test")

(run-shell-command "mkdir /tmp/test-with-batch-at-sfi")
(run-shell-command "cp /home/dsmith/mds/src/mds/mds/investigations/strain-selection-meeting/database/csl/runs/20040912/batch-processing/20031002/save-of-table-only.save /tmp/test-with-batch-at-sfi/input.save")
(msf-to-save-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi")
(color-code-save-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :color-code-save-f #'color-code-reference-antigen-save)
(mds-runs-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :num-runs 3
 :num-dimensions 2
 :dim-anneal-starting-coords 5
 :run-at-sfi-p t
 :num-solutions-to-show 3)
(recover-mds-runs-from-sfi-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :num-solutions-to-show 3)
(make-mds-run-tk-graphics-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :num-solutions-to-show 3
 :master-orientation-save-filename (format nil "~a/src/mds/mds/investigations/strain-selection-meeting/master-save-detail.save" (sys:getenv "MDS_ROOT")))
(convert-mds-run-tk-graphics-to-ps-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :num-solutions-to-show 3)
(make-smaller-and-bordered-gifs-in-routine-diagnostics-directory 
 "/tmp/test-with-batch-at-sfi"
 :label "Test")

|#