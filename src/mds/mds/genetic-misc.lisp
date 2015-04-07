(in-package user)

;;;----------------------------------------------------------------------
;;;                      fasta file to save file
;;;----------------------------------------------------------------------

(defun make-multiple-thresholded-table (table threshold)
  (let ((max-distance 
         (apply-max (flatten (hi-table-values table)))
         ))
    (set-lower-triangle-to-dont-care
     (f-hi-table
      (^ (x)
	 (if (and threshold
		  (not (equal 'none threshold))  ;; coming from the gui
		  (> x threshold))
	     (threshold-symbol (inc (- max-distance x)))
	   (- max-distance x)))
      table))))


(defun nameSequencePairs-to-similarity-save (nameSequencePairs
					     &optional &key 
						       (threshold 30)
						       (num-dimensions 2))

  ;; ugly sideffect hack
  (loop for threshold-number from 0 to 70 do
	(add-threshold
	 (list (read-from-string (format nil "<~d" threshold-number))
	       threshold-number)))

  (if (not (apply #'nary-equal (mapcar (^ (seq) (length (string seq))) (nths 1 nameSequencePairs))))
      (error (format nil "~2%All sequences are not the same length.  Lengths are ~{~a ~}.~2%"
                     (mapcar (^ (seq) (length (string seq))) (nths 1 nameSequencePairs)))))

  (let* ((hamming-table (make-hi-table
			 (nths 0 nameSequencePairs)
			 (nths 0 nameSequencePairs)
			 (all-comparisons-square (mapcar #'explode-symbol (nths 1 nameSequencePairs)) #'hd-dash-does-not-count)
			 'hamming-table)))
    (make-save-form
     :hi-table (make-multiple-thresholded-table
		hamming-table
		threshold)
     :mds-dimensions num-dimensions)))
	 
(defun fasta-file-to-similarity-save-file (fasta-filename save-filename 
					   &optional &key 
						     (threshold 30)
						     (num-dimensions 2))
  (write-save-form
   (nameSequencePairs-to-similarity-save 
    (read-fas-format fasta-filename)
    :threshold threshold
    :num-dimensions num-dimensions)
   save-filename))




;;;----------------------------------------------------------------------
;;;            hamming distance from a named sequence
;;;----------------------------------------------------------------------

(defun distances-from-a-named-sequence (name-seq-s-or-filename root-name distance-function)
  (let* ((name-seq-s (if (stringp name-seq-s-or-filename)
			 (if (fas-file-p name-seq-s-or-filename)
			     (read-fasta-file name-seq-s-or-filename)
			   (groups-of-n 2 (fi-in-s name-seq-s-or-filename)))
		       name-seq-s-or-filename))
	 (exploded-root-sequence (explode-symbol (assoc-value-1 root-name name-seq-s))))
    (loop for (name seq) in name-seq-s collect
	  (list name (funcall distance-function (explode-symbol seq) exploded-root-sequence)))))

(defun hamming-distances-from-a-named-sequence (name-seq-s-or-filename root-name)
  (distances-from-a-named-sequence name-seq-s-or-filename root-name #'hd))

(defun non-synonymous-hamming-distances-from-a-named-sequence (name-seq-s-or-filename root-name)
  (distances-from-a-named-sequence name-seq-s-or-filename root-name #'non-synonymous-hd))

(defun synonymous-hamming-distances-from-a-named-sequence (name-seq-s-or-filename root-name)
  (distances-from-a-named-sequence name-seq-s-or-filename root-name #'synonymous-hd))


;;;-------------------------------------------------------------------------------
;;;   specification files to color code antigenic maps with genetic infomation
;;;-------------------------------------------------------------------------------

(defun strain-color-based-on-subst (name-pros-s location aa-color-priority-s)
  (loop for (name pros) in name-pros-s collect
        (let ((actual-aa (nth (dec location) (explode-symbol pros))))
          (loop for (aa color priority) in aa-color-priority-s
              when (equal aa actual-aa)
              do (return (list name (apply #'rgb-xxxxxx-color (tk-color-to-rgb (string color))) priority))      ;; particular aa found
              finally (return (list name (apply #'rgb-xxxxxx-color (tk-color-to-rgb "yellow")) 1))))))   ;; aa is other than any specified

(defun write-strain-color-based-on-subst-hardcoded-location (name-pros-s 
                                                             location
                                                             aa-color-optionalPrintColor-s
                                                             &optional &key 
                                                                       (directory "~/r/all/full/markings/")
                                                                       (if-exists :error))

  (let ((filename (format nil "~3,'0d.txt" location))
        (aa-color-priority-s (loop for (aa color) in aa-color-optionalPrintColor-s
                                 for priority from 2 
                                 collect (list aa color priority)))
        (aa-printColor-s (mapcar (^ (l) (list (nth 0 l) (if (nth 2 l) (nth 2 l) (nth 1 l)))) aa-color-optionalPrintColor-s)))

    (fi (format nil "~3,'0d" location)
        (string-append directory filename)
        if-exists
        t)

    (fi (format nil "~:{~a=~@(~a~), ~}unsequenced=grey" aa-printColor-s)
        (string-append directory filename)
        :append
        t)

    (fll 
     (strain-color-based-on-subst
      name-pros-s
      location
      aa-color-priority-s) 
     :filename (string-append directory filename)
     :if-exists :append)  ;; will exist, was written to above
    ))




