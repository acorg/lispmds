(in-package user)

;; optionally pp the merged table, with diagnostics
(defun merge-tables (tables &optional &key 
				      (table-output-stream t)
				      filename
                                      (sort-hi-table-names t)
				      (if-exists-action :error)
				      (num-dims 2)
				      ;;(multiple-values-f #'geometric-average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold)
                                      (multiple-values-f #'geometric-average-multiples-thresholded-and-non-thresholded-take-inc-max-titer-dont-care-sd-gt-1))
  ;; should work for a single table, and if that table has duplicate rows (or cols?) should merge them (does the new-new-new- etc bullshit merge do the row unification?)
  (if filename
      (with-open-file (out filename :direction :output :if-exists if-exists-action)
	(merge-tables tables 
                      :table-output-stream out
                      :sort-hi-table-names sort-hi-table-names
                      :num-dims num-dims
                      :multiple-values-f multiple-values-f))
    (let* ((merge (let ((merge (merge-hi-tables 
                                tables
                                multiple-values-f
                                'zmerged)))
                    (if sort-hi-table-names 
                        (hi-table-sort-strains merge)
                      merge)))
	   (diagnostic-merge (let ((merge (merge-hi-tables 
                                           (append tables (list merge))
                                           #'list-multiples
                                           'diagnostic-merge
                                           t)))
                               (if sort-hi-table-names
                                   (hi-table-sort-strains merge)
                                 merge)))
           (diagnostic-merge-merged-rows-and-cols-only (extract-hi-table-by-removing-non-merged-rows-and-columns diagnostic-merge)))
    
      (if table-output-stream
	  ;; print the merged table, the long form of the merge, and the individial tables, and return the save form
	  (let ((out table-output-stream))
	    (format out ";; MDS merge table and diagnositics (version 0.0).~%;; Created at ~a~%~%" (time-and-date))
	    (pp-hi-table merge 'full-and-short 4 nil :stream out)
	    (newline out :times 4)      
	    (format out "~%;;;----------------------------------------------------------------------------")
	    (format out "~%;;;                             DIAGNOSTICS                             ")
	    (format out "~%;;;       (common titers, and how they merged, and the individual tables)")
	    (format out "~%;;;----------------------------------------------------------------------------")
	    (newline out :times 2)
	    (pp-hi-table diagnostic-merge 'full-and-short 4 nil :stream out :line-prefix ";; ")
	    (loop for table in tables do
		  (newline out :times 3)
		  (format out ";;;--------------------------------------------------------------------------------")
		  (format out "~%;; Table ~a" (hi-table-name table))
		  (pp-hi-table table 'full-and-short 4 nil :stream out :line-prefix ";; "))
            (newline out :times 3)
            (format out "~%;;;--------------------------------------------------------------------------------")
	    (format out "~%;;;    Table merge subset showing only rows and columns that have merged values")
	    (format out "~%;;;        (same as first diagnostic output, but subsetted for changes only)")
            (format out "~%;;;--------------------------------------------------------------------------------")
	    (pp-hi-table diagnostic-merge-merged-rows-and-cols-only 'full-and-short 4 nil :stream out :line-prefix ";; ")))

      (make-save-form 
       :hi-table (hi-table-to-asl merge)   ;; no, not if the tables are not HI tables...
       :mds-dimensions num-dims 
       :pre-merge-tables tables
       ))))
  


;; this function designed to be called by the gui, and the text merged hi table, gets saved to a file
;; and that is the normal call, tables to table
;; but the non-filename-based main function above returns a save
(defun merge-tables-by-filename (input-filenames output-filename
				 &optional &key
					   (autoConvert 1)
					   (panel 0)
					   (numDims 2)
					   (if-exists-action :error)  ;; but call from gui with :supersede, as tk will check there is not a conflict
					   )
  (print "Careful, Derek, what about filenames with spaces in them?")
  (print "Careful, Derek, what about windows files? the / \ stuff")
  (let* ((tables (loop for input-filename in input-filenames collect
		       (let ((table (read-hi-table-and-convert
				     input-filename
				     autoconvert
				     panel)))
			 (if (ag-sr-table-p table)
			     (un-asl-hi-table table)
			   table)))))
    (with-open-file (out output-filename :direction :output 
		     :if-exists if-exists-action
		     :if-does-not-exist :create)
      (merge-tables tables :table-output-stream out :num-dims numDims))))
