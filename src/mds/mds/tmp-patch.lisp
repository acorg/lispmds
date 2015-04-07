(in-package user)

;;;----------------------------------------------------------------------
;;;               code to hack the lapedes col adjust
;;;----------------------------------------------------------------------


;;--------------------- adjusts to hi-table for hacking in a J line -----------------

;;---------------- for writing out -----------------------------

(defun hi-table-to-lapedes (hi-table &key starting-coordss)
  (if (ag-sr-table-p hi-table)
      (setq hi-table (hi-table-lt10s-to-5s
		      (unlog-hi-table
		       hi-table))))	      
  (if (lapedes-data-is-distance-matrix-with-threshold hi-table)
      (print "this is a sequence similarity table, should we preprocess it to send to lapedes?"))
  (let ((lapedes-hi-table (make-hi-table (mapcar #'international-strain-format-to-lapedes-strain-format
						 (hi-table-antigens hi-table))
					 (mapcar #'international-strain-format-to-lapedes-strain-format
						 (hi-table-sera hi-table))
					 (hi-table-values hi-table))))
    (append
     (lapedes-format-header lapedes-hi-table :num-dimensions (if starting-coordss (length (car starting-coordss)) 5)
			    :tmp-pass-coordss-plus-more starting-coordss)
     (lapedes-format-coords-required lapedes-hi-table starting-coordss)
     (lapedes-format-dists lapedes-hi-table))))


(defun lapedes-hack-col-offsets (hi-table starting-coordss)
  (let ((adjusts 
	 (append (loop for i below (length (hi-table-antigens-short hi-table)) collect 0)
		 (mapcar #'log-to-std-titer
			 (nthcdr (length (hi-table-antigens-short hi-table)) 
				 (col-bases starting-coordss))))))
    (format nil "O {~{~f,~}~f};"
	    (mapcar (^ (x) (coerce x 'short-float)) (butlast adjusts))
	    (coerce (car (last adjusts)) 'short-float))))

(defun lapedes-hack-row-adjusts (hi-table starting-coordss)
  ;; pass (expt 2 my-adjust)   ;; MIGHT BE -my-adjust
  (let ((adjusts 
	 (append (mapcar (^ (x) (expt 2 (- x)))
			 (firstn (length (hi-table-antigens-short hi-table)) 
				 (row-adjusts starting-coordss)))
		 (loop for i below (length (hi-table-sera-short hi-table)) collect (expt 2 0)))))
    (format nil "~%J {~{~f,~}~f};"
	    (mapcar (^ (x) (coerce x 'short-float)) (butlast adjusts))
	    (coerce (car (last adjusts)) 'short-float))))

(defun lapedes-format-header (hi-table &key (num-dimensions 5) tmp-pass-coordss-plus-more)
  hi-table
  num-dimensions
  (list ;; (format nil "n_dim=~d;" num-dimensions)
	;; tmp change to set alpha (format nil "~%rank=0;")
   (lapedes-hack-col-offsets hi-table tmp-pass-coordss-plus-more)
   (lapedes-hack-row-adjusts hi-table tmp-pass-coordss-plus-more)
   (format nil "~%energy=0;")))


;;---------------- and for reading back in -----------------------------

(defun lapedes-hack-col-offsets-input (lines)
  (mapcar #'std-log-titer
	  (string-to-atoms
	   (string-subst
	    #\, #\space
	    (substring-before-char #\} (substring-after-char #\{ (nth 0 lines)))))))

(defun lapedes-hack-row-adjusts-input (lines)
  (mapcar (^ (x) (- (log x 2)))
	  (string-to-atoms
	   (string-subst
	    #\, #\space
	    (substring-before-char #\} (substring-after-char #\{ (nth 1 lines)))))))

(defun lapedes-to-hi-table (lines)
  (let* ((stress (lapedes-stress lines))
         (hack-col-offsets (lapedes-hack-col-offsets-input lines))
	 (hack-row-adjusts (lapedes-hack-row-adjusts-input lines))
	 (C-lines (collect #'lapedes-C-line-p lines))
	 (A-lines (collect #'lapedes-A-line-p lines))
	 (names (mapcar #'lapedes-C-line-name c-lines))
	 (starting-coordss (mapcar #'lapedes-C-line-coords c-lines))
	 (parsed-A-lines (mapcar (^ (line)
                                    (list (lapedes-A-line-point-1 line)
                                          (lapedes-A-line-point-2 line)
                                          (lapedes-A-line-numeric line)))
                                 A-lines))
	 (table-values (loop for i below (length names) collect
			     (loop for j below (length names) collect
				   (progn
				     i j
				     'dont-care)))))
    (loop for (point-1 point-2 distance) in parsed-A-lines do
	  ;; make upper rectangle
	  (let ((row (min (position point-1 names) (position point-2 names)))
		(col (max (position point-1 names) (position point-2 names))))
	    (let ((existing-value (nth col (nth row table-values))))
	      (if (not (eql 'dont-care existing-value))
		  (if (eql existing-value distance)
		      (format t "Warning: two lapdeds-format values (the same value, ~d) for ~a ~a~%" distance point-1 point-2)
		    (format t "Warning: two lapdeds-format values (~d and ~d) for ~a ~a~%" existing-value distance point-1 point-2))
		(setf (nth col (nth row table-values)) distance)))))
    (let ((table (make-hi-table names names table-values)))
      (if ;;(or (ag-sr-table-p table)
	  ;;    (tricky-read-back-from-lapedes-detection-of-similarity-table-masquerading-as-ag-sr-table-p table))
	  (ag-sr-table-p table)
	  ;; lapedes will have 5's for <10, and HI titers.  convert to log and <10
	  (setq table
	    (hi-table-std-log-titers
	     (hi-table-5s-to-lt10s
	      table))))
      (let ((lapedes-data-is-distance-matrix-with-threshold (lapedes-data-is-distance-matrix-with-threshold table)))
	(if lapedes-data-is-distance-matrix-with-threshold
	    (setq table
	      (distance-table-to-similarity-table-with-threshold 
	       table
	       lapedes-data-is-distance-matrix-with-threshold))))
      (setq starting-coordss (filter #'null starting-coordss))  ;; when no starting-coordss a list of nils
      (if (not (or (null starting-coordss)
		   (= (length starting-coordss) (length names))))
	  (format t "Warning: the number of starting coords (~d) does not match the number of points (~d)~%" (length starting-coordss) (length names)))
      (values
       table
       starting-coordss
       stress
       hack-col-offsets
       hack-row-adjusts))))


(defun lapedes-file-input-common (filename)
  (multiple-value-bind (table starting-coordss stress hack-col-offsets hack-row-adjusts)
      (lapedes-to-hi-table (fi-in-readline filename))
    (if (or (ag-sr-table-p table)
	    (lapedes-data-is-distance-matrix-with-threshold table))
	(if starting-coordss
	    (setq starting-coordss
	      (append
	       starting-coordss
	       (if (and hack-col-offsets hack-row-adjusts)  ;; if just one then don't do anything -- tmp code (ha! will it bit me)
		   (make-extras-hack (append hack-col-offsets
					     hack-row-adjusts))
		 (make-coordss-more table))))))
    (values
     table
     starting-coordss
     stress)))
