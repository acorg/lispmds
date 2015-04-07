(in-package user)

(defun subset-coordss (starting-coordss table table-extract)
  ;; table goes with the starting-coordss, the extract is the subset we want
  (new-starting-coords-matching-old-coordss
   starting-coordss
   (hi-table-antigens table)
   (hi-table-antigens table-extract)
   :col-bases (collect-common 
	       (hi-table-antigens table)
	       (hi-table-antigens table-extract)
	       :action-f (^ (position)
			    (if (null position)
				(error "expected the extract antigens to be in the original table")
			      (nth position (col-bases starting-coordss)))))))

(defun new-starting-coords-matching-old-coordss (old-coordss-plus-more old-names new-names &key col-bases new-hi-table-for-extracting-col-bases)
  ;; note, only works right now for ag-sr tables (as we add in the row and col adjusts)
  (if (and (not (or col-bases new-hi-table-for-extracting-col-bases))
	   new-names)  ;; because it is ok to not be able to determine the new col bases if we are extracting no coords
      (error "Supply either the col-bases or the new table for extracting the new table"))
  (if (and col-bases new-hi-table-for-extracting-col-bases)
      (error "Supply either the col-bases or the new table for extracting the new table -- but not both as you have"))
  (let ((old-coordss (coordss old-coordss-plus-more))
	(old-col-bases (col-bases old-coordss-plus-more))
	(old-row-adjusts (row-adjusts old-coordss-plus-more))
	new-coordss
	new-row-adjusts
	(new-col-bases (if (null new-names)
			   nil
			 (if col-bases 
			     col-bases 
			   (calc-col-bases-as-max-in-col new-hi-table-for-extracting-col-bases)))))
    (loop for new-name in new-names do
	  (if (member new-name old-names)
	      (let ((old-position (position new-name old-names))
		    (new-position (position new-name new-names)))
		(push-end (nth old-position old-coordss) new-coordss)
		(push-end (nth old-position old-row-adjusts) new-row-adjusts)
		(if (not (eql (nth old-position old-col-bases)
			      (nth new-position new-col-bases)))
		    (format t "Col adjusts do not match for ~a (old ~d, new ~d)~d" 
			    new-name
			    (nth old-position old-col-bases)
			    (nth new-position new-col-bases))))
	    (progn
	      (push-end (uniform-perturbs '(0 0) 50) new-coordss)
	      (push-end 0.0d0 new-row-adjusts)    
	      ;; now we supply (as we do not want to default to 7)   (push-end (if (sr-name-p new-name) 7.0d0 -1.0d+7) new-col-bases)
	      )))
    (make-coordss-plus-more
     new-coordss
     new-col-bases
     new-row-adjusts)))

#||
(defun new-starting-coords-matching-old-coordss (old-coordss-plus-more old-names new-names 
						 &key col-bases 
						      new-hi-table-for-extracting-col-bases
						      use-old-col-bases)
  ;; note, only works right now for ag-sr tables (as we add in the row and col adjusts
  (if (not (or col-bases new-hi-table-for-extracting-col-bases use-old-col-bases))
      (error "Supply either the col-bases or the new table for extracting the new table or specify to use the old col bases"))
  (if (not (= 1 (+ (bool->bit col-bases) 
		   (bool->bit new-hi-table-for-extracting-col-bases)
		   (bool->bit use-old-col-bases))))
      (error "Supply only one of col-bases or the new table for extracting the new table or specify to use the old col bases"))
  (let ((old-coordss (coordss old-coordss-plus-more))
	(old-col-bases (col-bases old-coordss-plus-more))
	(old-row-adjusts (row-adjusts old-coordss-plus-more))
	new-coordss
	new-row-adjusts
	(new-col-bases (cond (use-old-col bases nil)
			     (col-bases col-bases)
			     (new-hi-table-for-extracting-col-bases
			      (calc-col-bases-as-max-in-col new-hi-table-for-extracting-col-bases)))))
    (loop for new-name in new-names do
	  (if (member new-name old-names)
	      (let ((old-position (position new-name old-names))
		    (new-position (position new-name new-names)))
		(push-end (nth old-position old-coordss) new-coordss)
		(push-end (nth old-position old-row-adjusts) new-row-adjusts)
		(if (not (eql (nth old-position old-col-bases)
			      (nth new-position new-col-bases)))
		    (format t "Col adjusts do not match for ~a (old ~d, new ~d)~d" 
			    new-name
			    (nth old-position old-col-bases)
			    (nth new-position new-col-bases))))
	    (progn
	      (push-end (uniform-perturbs '(0 0) 50) new-coordss)
	      (push-end 0.0d0 new-row-adjusts)    
	      ;; now we supply (as we do not want to default to 7)   (push-end (if (sr-name-p new-name) 7.0d0 -1.0d+7) new-col-bases)
	      )))
    (make-coordss-plus-more
     new-coordss
     new-col-bases
     new-row-adjusts)))
||#



;;;----------------------------------------------------------------------
;;;                          fas format
;;;----------------------------------------------------------------------

(defun write-fas-format (name-sequence-ll filename &optional &key (if-exists :error) (fragment-length 50) name-comment-alist)
  ;; the name comment alist we usually use for duplicate names
  (with-open-file (out filename :direction :output 
		   :if-exists if-exists
		   :if-does-not-exist :create)
    (loop for (name sequence) in name-sequence-ll do
	  (if (assoc name name-comment-alist)
	      (write-fas-format-pair name sequence out :fragment-length fragment-length :comment (assoc-value-1 name name-comment-alist))
	    (write-fas-format-pair name sequence out :fragment-length fragment-length)))))

(defun write-fasta-file (name-sequence-ll filename &optional &key (if-exists :error) (fragment-length 50) name-comment-alist)
  (write-fas-format name-sequence-ll filename :if-exists if-exists :fragment-length fragment-length :name-comment-alist name-comment-alist))

(defun write-fas-format-pair (name sequence stream &optional &key (fragment-length 50) (comment ""))
  (format stream ">~a ~a~%" name comment)
  (write-fas-sequence (string sequence) stream :fragment-length fragment-length))

(defun write-fas-sequence (sequence stream &optional &key (fragment-length 50))
  (if (equal "" sequence)
      nil
    (progn
      (format stream "~a~%" (substring sequence 0 (min (dec fragment-length) (dec (length sequence)))))
      (write-fas-sequence (substring sequence (min fragment-length (length sequence))) stream :fragment-length fragment-length))))
  



(defun fas-name (string)
  (equal ">" (substring string 0 0)))

(defun fas-file-p (filename)
  (and (stringp filename)
       (let ((read (fi-in filename)))
         (and (not (listp read))
              (fas-name (string read))))))

(defun fasta-file-p (filename) (fas-file-p filename))

(defun unfas-name (string)
  (substring string 1))

(defun read-from-string-spaces-to-_ (string)
  (read-from-string (string-subst #\space #\_ (string-right-trim '(#\space) string))))

(defun read-fasta-format (filename) (read-fas-format filename))

(defun read-fas-format (filename)
  (let* ((file-lines (fi-in-readline filename))
	 (name-line-numbers (loop for line in file-lines for i from 0 when (fas-name line) collect i)))
    (loop for (start-line-number end-line-number) on (append name-line-numbers
							     (list (length file-lines)))
	  until (null end-line-number)
	  collect (list (read-from-string-spaces-to-_ (unfas-name (nth start-line-number file-lines)))
		        (glue-up (firstn (dec (- end-line-number start-line-number)) (nthcdr (inc start-line-number) file-lines))
				 "")))))

(defun read-fas-file (filename) (read-fas-format filename))
(defun read-fasta-file (filename) (read-fas-format filename))

(defun read-fas-format-or-name-sequence (filename)
  (if (fas-file-p filename)
      (read-fas-format filename)
    (groups-of-n 2 (fi-in-s filename))))


;;(mapcar (^ (l) (list (car l) (glue-up (cdddr l) ""))) (mapcar #'apply-append (groups-of-n 11 (read-fas-format "/tmp/bar"))))
;; just hacked quickly with the above, removed the > from the start of each line, and chose 11 as the number of lines in a sequence




(defun remove-duplicates-from-name-data (name-pro)
  ;; could have just done the below, but then the name we get is not the first one which is in the list
  ;; (setq name-pro-unique (remove-duplicates name-pro :test (^ (x y) (equal (nth 1 x) (nth 1 y)))))
  (let* ((name-pro-unique-with-duplicate-list
	  (let (unique-names
		unique-name-with-duplicates-s
		unique-pros)
	    (loop for (name pro) in name-pro do
		  (let ((position (position pro unique-pros)))
		    (if position
			;;(replace-nth position (push-end name (assoc (nth position unique-names) unique-name-with-duplicates-s)) unique-name-with-duplicates-s)
			(setq unique-name-with-duplicates-s (replace-nth position (reverse (cons name (reverse (nth position unique-name-with-duplicates-s)))) unique-name-with-duplicates-s))
		      (progn
			(push-end name unique-names)
			(push-end (list name) unique-name-with-duplicates-s)
			(push-end pro  unique-pros)))
		    ))
	    (list
	     (transpose unique-names unique-pros)
	     (collect (^ (l) (> (length l) 1)) unique-name-with-duplicates-s))))
	 (name-pro-unique (nth 0 name-pro-unique-with-duplicate-list))
	 (duplicates      (nth 1 name-pro-unique-with-duplicate-list)))
    (if (not 
	 (and 
	  (or (= (length name-pro) (length name-pro-unique))
	      (equal '(t)
		     (remove-duplicates 
		      (loop for duplicate-list in duplicates collect
			    (let ((should-be-the-same
				   (loop for duplicate in duplicate-list collect
					 (assoc-value-1 duplicate name-pro))))
			      (apply #'nary-equal should-be-the-same)))))
	      (= (+ (- (length name-pro-unique) (length duplicates))
		    (length (apply-append duplicates)))
		 (length name-pro)))))
	(error "Result consistency check failed"))
    (values
     name-pro-unique
     duplicates)))
  
(defun remove-duplicate-sequences-from-fasta (source-filename destination-filename &optional &key include-diagnostics (if-exists-action :error) diagnostics-only)
  (multiple-value-bind (name-data-uniqued duplicates)
      (remove-duplicates-from-name-data (read-fas-format source-filename))
    (if diagnostics-only
	(fi duplicates destination-filename if-exists-action nil :write-outer-list-elements-individually t :write-inner-list-elements-individually t)
      (write-fas-format
       name-data-uniqued
       destination-filename
       :name-comment-alist (if include-diagnostics (mapcar (^ (l) (list (car l) (glue-up-to-string " " (cdr l)))) duplicates))
       :if-exists if-exists-action))))

(defun remove-duplicate-sequences-from-fasta-tk (source-filename destination-filename action)
  (case action
    (removeDuplicatesOnly               (remove-duplicate-sequences-from-fasta source-filename destination-filename :if-exists-action :supersede))
    (removeDuplicatesIncludeDiagnostics (remove-duplicate-sequences-from-fasta source-filename destination-filename :if-exists-action :supersede :include-diagnostics t))
    (removeDuplicatesDiagnosticsOnly    (remove-duplicate-sequences-from-fasta source-filename destination-filename :if-exists-action :supersede :diagnostics-only t))
    (t (error "Unknown action passed from GUI"))))

#|
(progn
  (remove-duplicate-sequences-from-fasta
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profull.fas"
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profullunique397.fas"
   :if-exists-action :supersede
   :include-diagnostics nil)

  (remove-duplicate-sequences-from-fasta
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profull.fas"
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profullunique397withdiagnostics.fas"
   :if-exists-action :supersede
   :include-diagnostics t)
  
  (remove-duplicate-sequences-from-fasta
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profull.fas"
   "mds/investigations/strain-selection-meeting/database/combined/4-lab-genetic-map/raw-data/755profullunique397diagnosticsonly.fas"
   :if-exists-action :supersede
   :diagnostics-only t)
  )
|#


(defun reduce-name-seqs-to-variable-locations-only (name-seqs)
  (let* ((seqs (mapcar #'explode-symbol (nths 1 name-seqs)))
         (non-fully-conserved-locations (loop for i below (length (car seqs))
                                            when (> (num-unique-elements (nths i seqs)) 1)
                                            collect i))
         (seqs-unique-locations-only (loop for seq in seqs collect
                                           (implode-list (multiple-nth non-fully-conserved-locations seq)))))
    (values
     (mapcar #'list (nths 0 name-seqs) seqs-unique-locations-only)
     (mapcar #'inc non-fully-conserved-locations))))


;;;----------------------------------------------------------------------
;;;                   shortening names for phylip
;;;----------------------------------------------------------------------

(defun generate-simple-names-for-phylip (n)
  (loop for i from 1 to n collect
	(read-from-string (format nil "N~9,'0d" i))))
  

(defun convert-fasta-names-to-simple-identifiers (input-filename output-filename &optional &key (if-exists :error) (fragment-length 50) name-comment-alist)
  (let ((name-sequence-s (read-fasta-file input-filename)))
    (write-fasta-file  
     (loop for simple-name in (generate-simple-names-for-phylip (length name-sequence-s))
	   for sequence in (nths 1 name-sequence-s) 
	   collect
  	     (list simple-name sequence))
     output-filename
     :if-exists          if-exists
     :fragment-length    fragment-length
     :name-comment-alist name-comment-alist)))
     
(defun revert-simple-identifiers-to-fasta-names-in-newick-tree (tree-filename fasta-filename output-filename &optional &key (if-exists-action :error))
  (let* ((original-names (nths 0 (read-fasta-file fasta-filename)))
	 (simple-names   (generate-simple-names-for-phylip (length original-names))))
    (write-newick-tree 
     (substitute-names-in-tree-from-filename 
      (read-newick-tree tree-filename) 
      (transpose simple-names original-names)) 
     output-filename
     :if-exists-action if-exists-action)))

(defun make-simple-identifiers-to-fasta-names-correspondence-file (fasta-filename output-filename &optional &key (if-exists-action :error))
  (let* ((original-names (nths 0 (read-fasta-file fasta-filename)))
	 (simple-names   (generate-simple-names-for-phylip (length original-names))))
    (fll
     (transpose simple-names original-names)
     :filename output-filename
     :if-exists if-exists-action)))


;;;----------------------------------------------------------------------
;;;                  randomly ordered fasta files
;;;----------------------------------------------------------------------

(defun randomly-order-fasta-files (n filename directory &optional &key random-number-generator)
  (if (not (file-or-directory-exists-p directory))
      (run-shell-command (format nil "mkdir ~a" directory) :wait t))
  (let ((name-seq (read-fasta-file filename)))
    (loop for i below n do
          (write-fasta-file 
           (shuffle name-seq random-number-generator)
           (format nil "~a/random-order-~d.fasta" directory i)))))


;;;----------------------------------------------------------------------
;;;                         msf format
;;;----------------------------------------------------------------------

(defun read-msf-format (filename)
  ;; would be better reading the perl, as this was written from just one example of msf format
  (let* ((all-lines (fi-in-readline filename :comment-char #\# :line-process-f #'string))
	 (all-lines-no-trailing-blank-lines (butlastn (position "" (reverse all-lines) :test (^ (x y) (not (equal x y)))) all-lines))
	 (lines-after-// (mapcar #'space-delimited-string-to-list (nthcdr (position "//" all-lines :test #'equal) all-lines-no-trailing-blank-lines)))
	 (block-size (position nil lines-after-//))
	 (groups (apply-transpose (mapcar #'cddr (groups-of-n block-size lines-after-//))))  ;; cddr to remove the space (or in first group //) and numbers
	 (names (mapcar #'caar groups))
	 (sequences (mapcar (^ (group) (glue-up (nths 1 group) "")) groups)))
    (transpose names sequences)))




;;;----------------------------------------------------------------------
;;;                       name manipulation
;;;----------------------------------------------------------------------

(defun remove-isoltion-number-suffix-from-name (name)
  (let ((agp (ag-name-p name))
	(srp (sr-name-p name)))
    (let ((name (string (remove-ag-sr-from-name name))))
      (let ((new-name (read-from-string
		       (if (not (= 0 (length (substring-after-char #\- name))))
			   ;; name contains a "-"
			   (string-append 
			    (substring-before-char #\- name)
			    "/"
			    (substring-after-char #\/ (substring-after-char #\- name)))
			 name))))
	(cond (agp (suffix-as-ag new-name))
	      (srp (suffix-as-sr new-name))
	      (t   new-name))))))


;; written initially for uniqing duplicate names in fasta files
(defun unique-keys-by-adding-alt-n (alist)
  (let (so-far-name-count-alist)
    (loop for (key . rest) in alist collect
          (prog1
            (cons
             (if (assoc key so-far-name-count-alist)
                 (glue-up (list key 'alt (assoc-value-1 key so-far-name-count-alist)) #\_)   ;; assumes that alt_n is not already in the list
               key)
             rest)
            (push 
             (list 
              key
              (if (assoc key so-far-name-count-alist)
                  (inc (assoc-value-1 key so-far-name-count-alist))
                1))
             so-far-name-count-alist)))))

(defun unique-fasta-names (input-filename output-filename &optional &key (if-exists :error))
  (write-fasta-file
   (unique-keys-by-adding-alt-n
    (read-fasta-file input-filename))
   output-filename
   :if-exists if-exists))


;;;----------------------------------------------------------------------
;;;                     ugly tk hack for windows
;;;----------------------------------------------------------------------

(defun send-null-string-to-open-streams ()
  (loop for (stream-number stream) in *tk-streams* do
	(if (not (eql 'stream-closed stream))
	    (progn 
	      (tk-put stream-number "")
	      (tk-put stream-number "")))))