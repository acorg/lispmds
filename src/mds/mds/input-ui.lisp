(in-package user)

;;;----------------------------------------------------------------------
;;;                      THE USER INTERFACE
;;;----------------------------------------------------------------------

(defun make-input-ui ()
  (let ((tk (tk-open)))
    (set-input-ui-tk tk)
    (tk-put tk "source ~s" (uw-sfnr "mds/input-ui.tk" :assertIsFile t))
    (let ((user (user-name)))
      (cond ((equal user "terry")     (tk-put tk "set initialDir mds/investigations/merge-hi-tables/"))
	    ((equal user "dsmith")    (tk-put tk "set initialDir mds/data/"))
	    ((equal user "stefan")    (tk-put tk "set initialDir ~/Desktop/"))
	    ((equal user "mathilde")  (tk-put tk "set initialDir ~/Desktop/"))
	    (t (tk-put tk "set initialDir mds/"))))
    ;; dead sparrow
    ;;(if (running-on-windows-p)
    ;;	(let ((tk (tk-open)))
    ;;	  (tk-put tk "set bitmapDir ~s" (uw-sfnr "bitmaps" :assertIsDir t))
    ;;	  (tk-put tk "source ~s" (uw-sfnr "mds/dead-sparrow.tk" :assertIsFile t))))
    tk))

(defun launch-gui (&optional (filename "")
		   &key (auto-convert-tk-flag 1)
			num-dims  ;; only operates if we have a table, not a save or lapedes format
			(panel? 0))
  (make-input-ui)
  (if (not (equal "" filename))
      (read-hi-table-into-tk-interface filename auto-convert-tk-flag num-dims panel?))
  filename)


(defvar last-hi-table 'none)        ;;just so i can play in lisp (not used in any code)
(defvar last-hi-table-name 'none)   ;;just so i can play in lisp (not used in any code)

(defun hi-table-p (table)
  "Guess if table is an hi-table"
  (let ((elements (flatten (hi-table-values table))))
    (let ((numeric-elements (collect #'numberp elements)))
      (and (not (member nil (mapcar #'integerp numeric-elements)))  ;; all entries are integers
	   (not (member t (mapcar #'zerop numeric-elements)))       ;; no entries are zero
	   (> (length numeric-elements) 0)))))                       ;; there is at least one numeric element
	
(defun asl-hi-table-p (table)
  (not (equal (hi-table-antigens table) (mapcar #'remove-ag-sr-from-name (hi-table-antigens table)))))

(defun ctl-assay-table-p (table)
  ;; poor way right now, there has to be a >5000 or >10000
  (let ((elements (flatten (hi-table-values table))))
    (collect (^ (titer) 
		(or (eql '>5000 titer)
		    (eql '>10000 titer)))
	     elements)))

(defun read-hi-table-and-convert (file auto-convert-tk-flag panel?)
  (multiple-value-bind (hi-table hi-table-name hi-table-identity)
      (read-hi-table file)
    hi-table-name  ;; to avoid compiler warning
    ;;LATER USE HI TABLE NAME TO GIVE US A VARIABLE SET TO THE NAME IN THE LISP ENVIRONMENT
    (if (= auto-convert-tk-flag 1)
	(cond ((eql 'similarity-matrix hi-table-identity)
	       hi-table)
	      ;;ctl-assay table
	      ((ctl-assay-table-p hi-table)
	       (setq hi-table (ctl-assay-table-to-log-ag-sr-table hi-table)))
	      ;;hi table
	      ((or (hi-table-p hi-table)  ;; will not work if <10 or dont-care
		   (eql 'binding-assay hi-table-identity))
	       (case panel?                                         ;; should look at all values
		 (0 (setq hi-table (hi-table-to-thresholded-log-sr-table hi-table)))
		 (1 (setq hi-table (hi-table-to-euclidean-distance-matrix hi-table)))
		 (t (error "unexpected value"))))
	      ;;distance matrix
	      ((and (numberp (hi-table-value-by-indices hi-table 0 1))
		    (numberp (hi-table-value-by-indices hi-table 0 0))
		    (zerop (hi-table-value-by-indices hi-table 0 0))))
	       ;;(setq hi-table (scale-hi-table hi-table 10))) we used to scale for the hillclimber step size, now to nothing
	      ;;similarity matrix
	      ((and (numberp (hi-table-value-by-indices hi-table 0 1))
		    (numberp (hi-table-value-by-indices hi-table 0 0)))
	       (setq hi-table 
		 ;;(similarity-matrix-to-distance-matrix hi-table)
		 hi-table
		 ))
		 ;; (scale-hi-table (similarity-matrix-to-distance-matrix hi-table) 10)))  we used to scale, for the hillclimber step size
	      (t (error "should be tk message: cannot autoconvert this aquarium (dont know what to auto convert into)"))))
    (values 
     (setq last-hi-table hi-table)
     (setq last-hi-table-name (hi-table-name hi-table)))))

(defun pp-hi-in (filename)
  (let ((hi-table (un-asl-hi-table (read-hi-table-and-convert filename 1 0))))
    (pp-hi-table hi-table)
    hi-table))

(defun mds-configuration-file-p (filename)
  (with-open-file (in filename)
    (let ((first-line (read-line in nil 'eof)))
      (and (stringp first-line)
	   (>= (length first-line) 34)
	   (equal ";; MDS configuration file (version " (substring first-line 0 34))))))

(defun save-file-p (filename)
  (mds-configuration-file-p filename))

(defun mds-configuration-file-version (filename)
  (with-open-file (in filename)
    (let ((first-line (read-line in nil 'eof)))
      (if (and (stringp first-line)
	       (>= (length first-line) 34)
	       (equal ";; MDS configuration file (version " (substring first-line 0 34)))
	  (read-from-string (substring first-line 35))
	'not-mds-configuration-file))))

(defun lapedes-format-file-p (filename)
  (with-open-file (in filename)
    (let ((first-line (read-line in nil 'eof)))
      (and (stringp first-line)
	   (>= (length first-line) 5)
	   (equal "n_dim" (substring first-line 0 4))))))

(defun tk-error (tk string)
  (tk-put tk "bgerror ~s" string))

;;remove for now while we get lispworks working
;;use the id below
(defmacro tk-error-guard (tk form)
  `(handler-case 
       ,form
     (error (condition) 
       (let ((error-string (apply #'format nil 
				  (simple-condition-format-control condition)
				  (simple-condition-format-arguments condition))))
	 (format t "~a~%" error-string)
	 (tk-error ,tk error-string)
	 condition))))
#|
(defmacro tk-error-guard (tk form)
  form)
|#

(defun fi-in-any-save-format (file)
  (if (mds-configuration-file-p file)
      (fi-in file)
      (if (lapedes-format-file-p file)
	  (lapedes-file-to-save-format-sexp file))))

(defun msf-file-p (file)
  (let ((file-data (fi-in file)))
    (and (listp file-data)
         (equal 'make-save-form (nth 0 file-data)))))

(defun read-hi-table-into-tk-interface (file auto-convert-tk-flag num-dims panel?)
  (let (tk)
    (if (msf-file-p file)
        (setq tk (eval (eval (fi-in file))))
      (if (mds-configuration-file-p file)
          (let ((save (fi-in file)))
            ;; when i introduced arbitrary thresholds, i introduced an incompatibility 
            ;; in the save format.  we now (since sometime between march and may 2002, the time
            ;; of introducing arbitrary thresholds) save <10s in HI as <0.  the incompatibility
            ;; is that old saves have <10s in them.  Here we find old saves (previous version 
            ;; of file version number) as change <10 to <0.  making this change here is less 
            ;; complete than changing make-hi-table to see both, but putting this in
            ;; make-hi-table puts it there for ever, and one day we might have a <10 that is
            ;; a new <10, not an old <10 that should be a <0.  here at least we are only
            ;; making the change for old configuration files where the only <10s are <10s
            ;; in HI that we should change to <0 -- at least in my memory, it is possible that
            ;; there are <10s in some thresholded sequence save that should be a <10, but i
            ;; don't think so.  putting this fix here is also incomplete in that if i read
            ;; the save in other than with this fucntion (such as with a direct fi-in), then
            ;; i will not get the auto fix.  but this incompleteness is better than always
            ;; having to deal with <10s in the future, and this is the way most other users
            ;; of the software will deal with the files also (ie thru the ui).
            (if (<= (mds-configuration-file-version file) 0.3)
                (setq save
                  (subst '<0 '<10 save)))
            (setq tk (eval save)))
        (if (lapedes-format-file-p file)
            (setq tk (eval (lapedes-file-to-save-format-sexp file)))
          (multiple-value-bind (hi-table hi-table-name)
              (read-hi-table-and-convert file auto-convert-tk-flag panel?)
            (setq tk (make-master-mds-window-f
                      hi-table
                      (if (null hi-table-name)
                          file
                        hi-table-name)
                      :mds-dimensions num-dims))))))
    ;;the next is needed to pop the window up under Linux, OK in windoze?
    (sleep 0.5)   
    (tk-put tk "set nothing nil")
    ;;(values 
    ;; hi-table
    ;; hi-table-name)
    ))

(defun similarity-matrix-to-distance-matrix (hi-table)
  (if (not (equal (hi-table-width hi-table) (hi-table-length hi-table)))
      (error "Expected a square matrix, but this matrix has ~d colunms and ~d rows~%"
	     (hi-table-width hi-table) (hi-table-length hi-table)))
  (if (apply #'nary-equal (hi-table-diagonal-values hi-table))   ;;all values the same
      (f-hi-table (^ (x) 
		     (if (dont-care-p x)
			 x
		       (+ (hi-table-value-by-indices hi-table 0 0)
			  (- x))))
		  hi-table)
    (error "Expected a similarity matrix, with all diagonal values the same, but the diagonal values are ~{~a ~}~%"
	   (hi-table-diagonal-values hi-table))))

(defun log-hi-table (hi-table &optional (base 2))
  (f-hi-table (^ (x) 
		 (if (dont-care-p x)
		     x
		   (log (/ x 10) base)))
	      hi-table))

(defun hi-table-to-euclidean-distance-matrix (hi-table)
  (set-lower-triangle-to-dont-care 
   ;;(hi-table-to-dists-by-antigen-euclidean-dist (log-hi-table hi-table))                      
   ;;(scale-hi-table (similarity-matrix-to-distance-matrix (f-hi-table (^ (x) (dps x 4)) (hi-table-to-dists-by-antigen-correlation (log-hi-table hi-table)))) 10)
   ;;(hi-table-to-dists-by-antigen-normalized-euclidean-dist (log-hi-table hi-table))           
   (hi-table-to-dists-by-antigen-normalized-dont-care-euclidean-dist 
    (log-hi-table hi-table) 
    0.0   ;; ignore titers <10  (log titer < 0.0)
    4)    ;; require at least 4 cols in common
   ))

(defun hi-table-to-manhatten-distance-matrix (hi-table)
  (set-lower-triangle-to-dont-care 
   (hi-table-to-dists-by-antigen-normalized-dont-care-manhatten-dist (log-hi-table hi-table))   
   ))

#|
(defun hi-table-to-log-lt10-arg-sr-table (hi-table)
  ;; superseded by hi-table-to-thresholded-log-sr-table below
  (f-hi-table 
   #'std-log-titer
   (f-hi-table
    (^ (x)
       (if (eql 5 x)
	   '<10
	 x))
    (make-ag-sr-hi-table (hi-table-ferret-to-strain hi-table)))))
|#

(defun threshold-hi-table-values (hi-table &optional &key hi-table-threshold-data (default-threshold-data-from-external-lists t) (convert-5-or-less-to-lt10 *convert-5-or-less-to-lt10*))
  (if (not hi-table-threshold-data)
      (setq hi-table-threshold-data
	(if (and default-threshold-data-from-external-lists
		 (or (assoc-value (hi-table-name hi-table) *RIVM-table-lt-thresholds*)
		     (assoc-value (hi-table-name hi-table) *AHT-table-lt-thresholds*)))
	    (or (assoc-value (hi-table-name hi-table) *RIVM-table-lt-thresholds*)
		(assoc-value (hi-table-name hi-table) *AHT-table-lt-thresholds*))
          (if convert-5-or-less-to-lt10
              '(<10 8)     ;; the number used to be 10, but changed to 8 to accomodate AHT tables (which will get a <10, not a <8)
            '(<1 0.5)))))  ;; the number used to be 10, but changed to 8 to accomodate AHT tables (which will get a <10, not a <8)
  (let ((hi-table-threshold-titer-number (nth 1 hi-table-threshold-data))
	(hi-table-threshold-titer-symbol (nth 0 hi-table-threshold-data)))
    (f-hi-table
     (^ (x)
	(if (not (numberp x))
	    x
	  (if (< x hi-table-threshold-titer-number)
	      hi-table-threshold-titer-symbol
	    x)))
     hi-table)))

(defun hi-table-to-thresholded-log-sr-table (hi-table)
  (f-hi-table 
   #'std-log-titer
   (threshold-hi-table-values
    (make-ag-sr-hi-table 
     (hi-table-ferret-to-strain 
      hi-table)))))

(defun ctl-assay-table-to-log-ag-sr-table (table)
  (make-ag-sr-hi-table 
   (f-hi-table
    (^ (titer)
       (if (true-dont-care-p titer)
	   titer
	 (if (numberp titer)
	     (2dp (- 4 (log titer 10)))
	   (if (equal titer '>10000)
	       '<0
	     (if (equal titer '>5000)
		 '<0.3
	       (error "unexpected titer ~a" titer))))))
    table)))

(defun set-lower-triangle-to-dont-care (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (loop for i below (hi-table-length hi-table) collect
	 (loop for j below (hi-table-width hi-table) collect
	       (if (< j i)
		   'dont-care
		 (hi-table-value-by-indices hi-table i j))))
   (glue-up (list (hi-table-name hi-table) 'ldc))))


;;;----------------------------------------------------------------------
;;;                    READING NON-LISPY TABLES
;;;----------------------------------------------------------------------

(defun no-more-data-p (string)
  (loop for i below (length string) do
	(if (not (or (eql #\tab (aref string i))
		     (eql #\space (aref string i))))
	    (return nil))
	finally (return t)))

(defun string-to-substrings (string)
  ;;does not work...
  (loop until (no-more-data-p string) collect
	(let ((length (nth-value 1 (read-from-string string))))
	  (setq string (substring string (dec length)))
	  (substring string 0 (- length 2)))))

(defun hi-table-row-p (l)
  ;;note, have to do ratiop, as some sera are 345/6 etc
  ;;note, some strains are only numbers! eg in rivm90-92_2_2
  ;;  either i ignore them, or guess (or require sera to be identified) (currently ignore)
  (and (not (null l))
       (or (symbolp (car l)) (stringp (car l)))
       (> (length l) 1)
       (not (filter (^ (x)
		       (or (and (numberp x) (not (ratiop x)))
			   (dont-care-p x)))
		    (cdr l)))))

(defun hi-table-sera-line-p (l)
  ;;a sera line has a list of names all of which start with an f and the next char is a number
  (and (not (zerop (length l)))
       (null (filter #'sera-name-p l))))

(defun hi-table-sequence-line-p (l)
  ;;starts with Seq->
  (equal 'seq-> (car l)))

(defun hi-table-type-id-p (l)
  (or (equal '(similarity matrix) l)
      (equal '(binding-assay)     l)
      (equal '(sm) l)))

(defun hi-table-similarity-matrix-indicated-p (hi-table-id-lines)
  (and (= 1 (length hi-table-id-lines))
       (or (equal '(similarity matrix) (car hi-table-id-lines))
	   (equal '(sm) (car hi-table-id-lines)))))

(defun binding-assay-matrix-indicated-p (hi-table-id-lines)
  (and (= 1 (length hi-table-id-lines))
       (equal '(binding-assay) (car hi-table-id-lines))))

(defun hi-table-column-label-line (l)
  ;; be generic here, assume any non comment line, that does not contain 
  ;; numbers only after the first entry, is a column label line
  (let ((unknown-table-entries
	 (filter (^ (e)
		    (or (numberp e)
			(thresholdp e)
			(true-dont-care-p e)))
		 l)))
    (>= (length unknown-table-entries) 1)))

(defun raw-distance-matrix-p (ll)
  ;; by raw we mean no row or column labels
  (and (= 1 (length (remove-duplicates (mapcar #'length ll))))
       (= (length ll)
	  (length (car ll)))
       (loop for l in ll do
	     (if (not (zerop (length (filter #'numberp l))))
		 (return nil))
	     finally (return t))))

;;(pp-hi-table (setq foo (read-hi-table "~/mds/data/viro9293/HI-tables/RIVM90_92_2.original")))
;;(pp-hi-table (setq foo (read-hi-table "~/mds/data/viro9293/sequence-data/AgSITESmatrix")))


(defun hi-table-name-guess-from-filename (filename)
  (let ((name (read-from-string
	       (let ((name (reverse (substring-before-char #\/ (reverse filename)))))
		 (if (substring-before-char #\. name)
		     (substring-before-char #\. name)
		   name)))))
    (if (numberp name)
	(read-from-string (format nil "T-~a" name))
      name)))

(defun read-hi-table (filename)
  (let ((hi-table-name (hi-table-name-guess-from-filename filename))
	(hi-table-lines
	 (loop for line in (fi-in-readline filename) collect
	       (progn 
		 ;;get rid of return in DOS line
		 (if (and (not (zerop (length line)))
			  (eql #\return (aref line (- (length line) 1))))
		     (setq line (substring line 0 (- (length line) 2))))
		 ;;remove comments
		 (if (string-member ";" line)
		     (setq line (substring-before-char #\; line)))
		 ;;the lisp reader gets confused with :'s
		 (setq line (string-subst #\: #\| line))
		 ;; the lisp reader gets confused with .'s
		 (setq line (string-subst-string " . " " * " line))
		 ;; the lisp reader gets confused with ()'s
		 (setq line (string-subst-string "(" "-" line :all-occurrences t))
		 (setq line (string-subst-string ")" ""  line :all-occurrences t))
		 (let ((items (string-to-atoms line)))
		   (if (symbolp (car items))
		       ;;special case first symbol and convert to string
		       ;;well not yet, because we have not made rest work yet
		       nil)
		   (setq items 
		     (mapcar (^ (e) (if (or  (eql '--- e) (eql '* e))  ;;(eql '. e)  ;;note comment below when i fix for .
					'dont-care
				      e))
			     items)))))))
    
    (let (hi-table-id-lines
	  hi-table-non-sera-lines
	  hi-table-sera-lines
	  unassigned-lines)
      (loop for hi-table-line in hi-table-lines do
	    (cond ((hi-table-type-id-p hi-table-line)
		   (push hi-table-line hi-table-id-lines))
		  ((hi-table-row-p hi-table-line)
		   (push-end hi-table-line hi-table-non-sera-lines))
		  ((or (hi-table-sera-line-p hi-table-line)
		       (hi-table-sequence-line-p hi-table-line)
		       (hi-table-column-label-line hi-table-line))
		   (push-end hi-table-line hi-table-sera-lines))
		  (t (push-end hi-table-line unassigned-lines))))
      
      ;; check for a raw distance matrix (ie no labels), and add labels
      (if (and (zerop (length hi-table-non-sera-lines))
	       (raw-distance-matrix-p unassigned-lines))
	  (setq hi-table-non-sera-lines
	    (loop for unassigned-line in unassigned-lines 
		for i from 0 collect
		  (cons (number->string i)
			unassigned-line))))
      
      (let* ((hi-table-non-sera hi-table-non-sera-lines)
	     (hi-table-sera (let ((sera-lines hi-table-sera-lines))
			      (cond (nil ;;(zerop (length sera-lines))     <<<<<<< remove this option, don't think we use it, and
				     ;;use the antigens (if enough)        <<<<<<< it is a dangerous default as it might be wrong.
				     (progn
				       (format t "Using row labels as column labels~%")
				       ;;the error check for enuf labels is caught with other error check below
				       (firstn (length (cdr (car hi-table-non-sera)))
					       (mapcar #'car hi-table-non-sera))))
				    (t
				     (if (not (= 1 (length sera-lines)))
					 (format t "expected one sera line, but got ~d, using the last.  the sera lines were: ~{~%  ~s~}~%"
						 (length sera-lines)
						 sera-lines))
				     (let ((sera-line (car (last sera-lines))))
				       (if (hi-table-sequence-line-p sera-line)
					   (cdr sera-line)
					 sera-line)))))))
	;;(loop for sera-line in hi-table-sera-lines do
	;;	    (if (not (null sera-line))  ;;null is a blank line
	;;		(format t "Ignoring the line containing: ~a~%" sera-line)))
	(let ((hi-table (set hi-table-name (make-hi-table 
					    (mapcar #'car hi-table-non-sera)
					    hi-table-sera
					    (mapcar #'cdr hi-table-non-sera)
					    hi-table-name))))
	  (if (not (= (length hi-table-sera) (hi-table-width hi-table)))
	      (error "number of sera (~d) does not equal the number of data points in the table (~d), ~%sera are: ~s~%"
		     (length hi-table-sera)
		     (hi-table-width hi-table)
		     hi-table-sera))
	  (if (not (= 1 (length (remove-duplicates (mapcar #'length (hi-table-values hi-table))))))
	      (progn 
		(error "not all rows the same length in ~a.~%Lengths are ~a~%" 
                       hi-table-name
                       (mapcar #'length (hi-table-values hi-table)))
		(tk-put (get-input-ui-tk) "tk_messageBox -icon warning -title Warning -message \"~a\""
			"Not all rows in the table are the same length.")))
					;		    "make this a TK print: some table values are missing, the missing values 
					;                    are assumed to be at the end of the rows, if that is not correct, use '---' to 
					;                    represent a missing value.  Be careful, we don't yet do necessarily good stuff
					;                    with dont cares when we take euclidean distance, best to remove the colunms with
					;                    don't cares.  (A ';' can be used to comment out a line or part of a line.)")))
	  (values
	   hi-table
	   hi-table-name
	   (if (hi-table-similarity-matrix-indicated-p hi-table-id-lines)
	       'similarity-matrix
	     (if (binding-assay-matrix-indicated-p hi-table-id-lines)
		 'binding-assay
	       nil))))))))


;;(make-input-ui)


;;;----------------------------------------------------------------------
;;;                      misc
;;;----------------------------------------------------------------------

(defun read-tables-1-to-68 ()
  (setq all-jan-tables
    (loop for i from 1 to 68
	when (not (member i '(44)))  ;; because table44 is the same as table 4
	collect
	  (let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	    (set hi-table-name
		 (read-hi-table (format nil "mds/data/HI-2001-08/~a.txt" hi-table-name)))))))

(defun read-tables-1-to-75 ()
  (setq all-jan-tables
    (loop for i from 1 to 75
	when (not (member i '(44)))  ;; because table44 is the same as table 4
	collect
	  (let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	    (set hi-table-name
		 (read-hi-table (format nil "mds/data/HI-2001-08/~a.txt" hi-table-name)))))))

(defun read-tables-1-to-78 ()
  (setq all-jan-tables
    (loop for i from 1 to 78
	when (not (member i '(44)))  ;; because table44 is the same as table 4
	collect
	  (let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	    (set hi-table-name
		 (read-hi-table (format nil "mds/data/HI-2001-08/~a.txt" hi-table-name)))))))
    
(defun read-all-tables ()
  (setq all-jan-tables
    (append
     (loop for i from 1 to 81
	 when (not (member i '(44)))  ;; because table44 is the same as table 4
	 collect
	   (let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	     (set hi-table-name
		  (read-hi-table (format nil "mds/data/HI-2001-08/~a.txt" hi-table-name)))))
     (list
      (setq vaccine-table
	(read-hi-table "mds/data/HI-2001-08/VACCINETABLE-2002-01-18-SERUM-MODS.txt"))))))



;; ------------------------------ common debug state ----------------------------

(defun setup-tab1-state ()
  "Set up some commonly used debug state"
  (eval (setq save (make-save-form :hi-table (read-hi-table-and-convert "mds/data/HI-2001-08/TAB1.txt" 1 0))))
  (run-shell-command "cp mds/data/HI-2001-08/tab1.save /tmp/tab1.save")
  save)  

