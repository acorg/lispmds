(in-package user)

;;;----------------------------------------------------------------------
;;;                MAKE INTO AG-SR FROM EXISTING TABLE
;;;----------------------------------------------------------------------

#|
(defun make-ag-sr-hi-table (hi-table)
  (make-hi-table 
   (append (mapcar #'suffix-as-ag (hi-table-antigens hi-table)) (mapcar #'suffix-as-sr (hi-table-antigens hi-table)))
   (append  (mapcar #'suffix-as-ag (hi-table-sera hi-table)) (mapcar #'suffix-as-sr (hi-table-sera hi-table)))
   (place-rectangle-in-short-upper-triangle (hi-table-values hi-table) 'dont-care)))

(defun place-rectangle-in-short-upper-triangle (rectangle default-value)
  (append 
   (loop for row in rectangle collect
	 (append (loop for i below (length row) collect default-value)
		 row))
   (loop for row in rectangle collect
	 (loop for i below (* 2 (length row)) collect default-value))))

The above does not work for non-square tables

|#

(defun make-ag-sr-names (hi-table)
  (append (mapcar #'suffix-as-ag (hi-table-antigens hi-table)) 
	  (mapcar #'suffix-as-sr (hi-table-sera hi-table))))

(defun make-ag-sr-hi-table (hi-table)
  (make-hi-table 
   (make-ag-sr-names hi-table)
   (make-ag-sr-names hi-table)
   (make-ag-sr-hi-table-values hi-table 'dont-care)
   ;; (glue-up (list (hi-table-name hi-table) 'ag-sr))  ;; was nice idea, but because useless because unwieldy
   (hi-table-name hi-table)))

(defun make-ag-sr-hi-table-values (hi-table default-value)
  (let* ((original-hi-table-length (hi-table-length hi-table))
	 (original-hi-table-width  (hi-table-width  hi-table))
	 (ag-sr-table-width (+ original-hi-table-length original-hi-table-width)))
    (append 
     (loop for row in (hi-table-values hi-table) collect
	   (append (loop for i below original-hi-table-length collect default-value)
		   row))
     (loop for i below original-hi-table-width collect
	   (loop for i below ag-sr-table-width collect default-value)))))

(defun ag-sr-table-p (hi-table)
  (and (equal (hi-table-antigens hi-table) (hi-table-sera hi-table))
       (not (filter #'ag-or-sr-name-p (hi-table-antigens hi-table)))))  ;; all names end in -ag or -sr

(defun hi-table-not-in-ar-sr-form-p (hi-table)
  (not (= (hi-table-length hi-table) (hi-table-width hi-table))))

;;(defun similarity-table-masquerading-as-ag-sr-table-p (hi-table)
;;  (collect #'less-than-ten-p (flatten (hi-table-values hi-table))))

(defun tricky-read-back-from-lapedes-detection-of-similarity-table-masquerading-as-ag-sr-table-p (hi-table)
  (let ((diagonal-values (hi-table-diagonal-values hi-table)))
    (and (equal '(t) (remove-duplicates (mapcar #'numberp diagonal-values)))
	 (apply #'= diagonal-values)
	 (not (zerop (car diagonal-values))))))

(defun ag-name-p (strain)
  (let* ((string-strain (string strain))
	 (string-length (length string-strain)))
    (and (>= (length string-strain) 3)
	 (string-equal "-AG" (substring string-strain (- string-length 3) (dec string-length))))))

(defun sr-name-p (strain)
  (let* ((string-strain (string strain))
	 (string-length (length string-strain)))
    (and (>= (length string-strain) 3)
	 (string-equal "-SR" (substring string-strain (- string-length 3) (dec string-length))))))

(defun ag-or-sr-name-p (strain)
  (let* ((string-strain (string strain))
	 (string-length (length string-strain)))
    (and (>= (length string-strain) 3)
	 (let ((tail (substring string-strain (- string-length 3) (dec string-length))))
	   (or (string-equal "-AG" tail)
	       (string-equal "-SR" tail))))))



;; alternate of above

(defun serum-name-p (name)
  (sr-name-p name))

(defun antigen-name-p (name)
  (ag-name-p name))

(defun remove-ag-sr-from-name (name)
  (if (ag-or-sr-name-p name)
      (read-from-string (substring (string name) 0 (- (length (string name)) 4)))
    name))

(defun corresponding-sera-name-p (ag-name sera-names)
  (member (remove-ag-sr-from-name ag-name) (mapcar #'remove-ag-sr-from-name sera-names)))



;;;----------------------------------------------------------------------
;;;                      un as and asl
;;;----------------------------------------------------------------------

(defun un-as-hi-table (hi-table)
  (let* ((antigens (mapcar #'remove-ag-sr-from-name (hi-table-antigens-short hi-table)))
	 (sera     (mapcar #'remove-ag-sr-from-name (hi-table-sera-short     hi-table)))
	 (num-antigens (length antigens)))
    (make-hi-table 
     antigens
     sera
     (mapcar (^ (row) (nthcdr num-antigens row)) (firstn num-antigens (hi-table-values hi-table)))
     ;; (glue-up (list (hi-table-name hi-table) 'unagsr))
     (hi-table-name hi-table))))

(defun unlog-hi-table (hi-table)
  (f-hi-table
   #'log-to-std-titer
   hi-table))

(defun un-asl-hi-table (hi-table)
  (unlog-hi-table
   (un-as-hi-table hi-table)))

(defun hi-table-lt10s-to-5s (hi-table)
  (f-hi-table
   (^ (x)
      (if (less-than-ten-p x)
	  5
	x))
   hi-table))

;;;----------------------------------------------------------------------
;;; from an hi table to a log ag-sr table with <10s and sequence derived <10s
;;;----------------------------------------------------------------------

(defun add-experimental-lt10s (hi-table &optional (unique-5s (fi-in "mds/investigations/merge-hi-tables/unique-5s")))
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (loop for antigen in (hi-table-antigens hi-table)
       for row in (hi-table-values hi-table) collect
	 (loop for serum in (hi-table-sera hi-table)
	     for titer-in-merge in row
	     collect (let ((is-5-in-long-range-table (member (list antigen serum 5) unique-5s :test #'equal)))
		       (cond ((and is-5-in-long-range-table
				   (eql '<10 titer-in-merge))
			      '<10)
			     ((and is-5-in-long-range-table
				   (eql 'dont-care titer-in-merge))
			      '<10)
			     ((and is-5-in-long-range-table
				   (numberp titer-in-merge))
			      'dont-care)
			     ((and (not is-5-in-long-range-table)
				   (eql '<10 titer-in-merge))
			      '<10)
			     (t 
			      titer-in-merge)
			     ))))))

(defun hi-table-5s-to-lt10s (hi-table)
  (f-hi-table
   (^ (x)
      (if (or (eql 5 x) (eql 5.0 x))
	  '<10
	x))
   hi-table))

(defun hi-table-lt10titer-to-lt10s (hi-table)
  (f-hi-table
   (^ (x)
      (if (dont-care-p x)
	  x
	(if (< x 10)
	    '<10
	  x)))
   hi-table))

(defun hi-table-std-log-titers (hi-table)
  (f-hi-table
   #'std-log-titer
   hi-table))

(defun hi-table-un-std-log-titers (hi-table)
  (f-hi-table
   #'un-std-log-titer
   hi-table))

(defun hi-table-to-asl (hi-table)
  (f-hi-table
   #'std-log-titer
   (make-ag-sr-hi-table 
    hi-table)))

(defun add-lt10s-seq-lt10s-and-asl (hi-table &key (seq-lt10-table nl-pro) (seq-lt10-threshold 0.75))

  (setq hi-table-lt10s
    (hi-table-5s-to-lt10s hi-table))

  (setq hi-table-lt10s-seq-lt10s
    (add-lt10s-based-on-sequence-distance
     hi-table-lt10s
     seq-lt10-table
     seq-lt10-threshold))

  (setq hi-table-lt10s-seq-lt10s-asl
    (hi-table-to-asl hi-table-lt10s-seq-lt10s)))

(defun add-lt10s-and-asl (hi-table)

  (setq hi-table-lt10s
    (hi-table-5s-to-lt10s hi-table))

  (setq hi-table-lt10s-asl
    (hi-table-to-asl hi-table-lt10s)))


(defun add-lt10s-based-on-sequence-distance (hi-table seq-table seq-threshold)
  (let ((seq-table-sera (hi-table-sera seq-table))
	(hi-table-sera (hi-table-sera hi-table)))  ;; do here, as optimization
    (make-hi-table
     (hi-table-antigens hi-table)
     hi-table-sera
     (loop for row in (hi-table-values hi-table) for antigen in (hi-table-antigens hi-table) collect
	   (loop for entry in row for serum in hi-table-sera collect
		 (if (not (member serum seq-table-sera))   ;; 1/6th of the merged-hi sera are not sequenced  
		     entry                               
		   (if (not (member antigen seq-table-sera))  ;; so it works when not all ags are sequenced
		       entry
		     (let ((seq-dist (if (numberp (hi-table-value seq-table antigen serum :hi-table-sera-efficiency-hack seq-table-sera))
					 (hi-table-value seq-table antigen serum  :hi-table-sera-efficiency-hack seq-table-sera)
				       (if (numberp (hi-table-value seq-table serum antigen  :hi-table-sera-efficiency-hack seq-table-sera))
					   (hi-table-value seq-table serum antigen :hi-table-sera-efficiency-hack seq-table-sera)
					 (error "")))))
		       (cond ((numberp entry)   ;; 5s will have already been converted to lt10
			      (if (>= seq-dist seq-threshold)
				  (format t "Warning: HI entry Ag ~13a Sr ~13a has HI value ~4d, but seq-dist (~4,2f) > threshold (~4d); leaving HI value~%"
					  antigen serum entry seq-dist seq-threshold))
			      entry)
			     ((and (less-than-ten-p entry) (>= seq-dist seq-threshold))
			      (format t "   Info: Good, an existing <10 for Ag ~13a Sr ~13a has a seq-dist (~4,2f) >= threshold (~4d)~%"
				      antigen serum seq-dist seq-threshold)
			      entry)
			     ((and (less-than-ten-p entry) (< seq-dist seq-threshold))
			      (format t "   Info: Bad, an existing <10 for Ag ~13a Sr ~13a has a seq-dist (~4,2f) < threshold (~4d)~%"
				      antigen serum seq-dist seq-threshold)
			      entry)
			     ((>= seq-dist seq-threshold)
			      '<10)
			     (t (if (not (dont-care-p entry))
				    (error "expected dont-care, got ~a" entry))
				entry))))))))))


#|

(setq test-table
  (make-hi-table 
   '(a b c)
   '(x y)
   '((1280 120)
     ( 640  80)
     ( 320 240))
   'test-table))

(pp-hi-table (make-ag-sr-hi-table test-table))

       A_AG B_AG C_AG X_SR Y_SR 
A_AG    ---   .    .  1280  120
B_AG     .   ---   .   640   80
C_AG     .    .   ---  320  240
X_SR     .    .    .   ---   . 
Y_SR     .    .    .    .   ---

|#


;;;----------------------------------------------------------------------
;;;                    MAKE INTO AG-SR FROM COORDS
;;;----------------------------------------------------------------------

(defun coordss-to-upper-square-hi-table (coordss 
					 &optional 
					 (antigens (loop for i below (length coordss) collect (number->string i)))
					 (antisera (loop for i below (length coordss) collect (number->string i))))
  (make-hi-table
   antigens
   antisera
   (coordss-to-upper-square-distance-matrix coordss)))

(defun coordss-to-upper-square-distance-matrix (coordss &optional (distance-metric-f #'e-dist))
  (let ((upper-square-length (/ (length coordss) 2)))
    (loop for row in coordss for r from 0 collect
	  (loop for column in coordss for c from 0 collect
		(if (and (<  r upper-square-length)
			 (>= c upper-square-length))
		    (funcall distance-metric-f row column)
		  'dont-care)))))
 

;;;----------------------------------------------------------------------
;;;                             UTILS
;;;----------------------------------------------------------------------

(defun suffix-as-ag (x)
  (read-from-string (format nil "~a-ag" x)))

(defun suffix-as-sr (x)
  (read-from-string (format nil "~a-sr" x)))


;;;----------------------------------------------------------------------
;;;                    "SHORT" functions
;;;----------------------------------------------------------------------

(defun hi-table-antigens-short (hi-table)
  (let ((hi-table-antigens (hi-table-antigens hi-table)))
    (if (ag-sr-table-p hi-table)
	(collect #'ag-name-p hi-table-antigens)
      hi-table-antigens)))

(defun hi-table-sera-short (hi-table)
  (let ((hi-table-sera (hi-table-sera hi-table)))
    (if (ag-sr-table-p hi-table)
	(collect #'sr-name-p hi-table-sera)
      hi-table-sera)))

(defun hi-table-length-short (hi-table)
  (length (hi-table-antigens-short hi-table)))

(defun hi-table-width-short (hi-table)
  (length (hi-table-sera-short hi-table)))

(defun hi-table-values-short (hi-table)
  (let ((hi-table-values (hi-table-values hi-table)))
    (if (ag-sr-table-p hi-table)
	(firstn (hi-table-length-short hi-table)
		(lastns (hi-table-width-short hi-table) hi-table-values))
      hi-table-values)))

;;;----------------------------------------------------------------------
;;;              CONVERTING FERRET NUMBERS TO STRAINS
;;;----------------------------------------------------------------------

(defvar *ferret-strain-correspondence*)
;;(setq *ferret-strain-correspondence* (eval (fi-in "~/mds/FERRETS.txt")))

;; the a, b, c etc in the isolation are made by derek, 2002-02-11, to separate sera
;; the notused in the isolation is also by derek, 2002-02-11, it marks a duplicate sera that was not part of the all-strains-merge-take-2


(setq *ferret-strain-correspondence* 
  '((AM/1609/77 F596)
    (AU/10/97 F98001)

    ;;(BA/1samep/79 F254/5)        ;; derek, 2002-05-05.  in the tables jan found, 69-75, close enuf to ba/1/79?
    (BA/1/79 F254/5)        ;; derek, 2002-06-26, when running with this serum as separate point, ends up 2-fold
    (BA/1/79 F254)          ;;    from this pooled serum, so make them the same
    (BA/1/79 F458)

    (BA/2/79 F222)

    (BE/32a/92 F476)        ;;derek, 2002-10-14, this is the nomenclature in the jo-wu prediction table
    (BE/32a/92 F476/7)      ;;derek, 2002-02-11, split these 2 for all-strains-merge-take-2
    (BE/32b/92 F93017)
    (BE/32c/92 F93018)      ;; derek, 2003-03-22, see seq-t9.html, maybe can be merged with others?

    (BE/353/89 F411)
    (BE/353/89 F95032)
    (BE/353b/89 F92001)     ;; derek, 2003-03-22, see seq-t9.html, maybe can be merged with others?
    (BE/353C/89 F96017)     ;; Bjorn 06OCT09

    (BI/16190/68 F394/5)
    (BI/1905/76 F598)

    (BI/21793/72 F392/3)
    (BI/21793A/72 F09003)   ;; Bjorn added 2009-05-04
    (BI/21793B/72 F09004)   ;; Bjorn added 2009-05-04


    (BI/2461/78 F288)
    (BI/2461/78 F288/9)

    (BI/2600/75 F389)
    (BI/5168/76 F386/7)
    (BI/5930/74 F390/1)

    (BR/8/96 F582)
    (BR/8A/96 F09015)       ;; Bjorn added 2009-05-04
    (BR/8B/96 F09016)       ;; Bjorn added 2009-05-04

    (CC/4/85 F340/1)
    (CC/4/85 F494/5)

    (CC/28/03 F304HAY)      ;; derek 2004-06-20, tab86

    (CE/1a/84 F306/7)       ;;derek, 2002-02-11, split these 2 for all-strains-merge-take-2
    (CE/1b/84 F316/7)

    (CO/2/86 F379)
    (EN/23/76 F89)
    (EN/261/91 F92006)
    (EN/42/72 F44/5)
    (EN/427/88 F49)

    (EN/496/80 F264/5)      ;; derek, ok for these 2 englands to merge for all-strains-merge-take-2, 2002-02-11
    (EN/496/80 F264)        ;; derek added when we added tab68, 2002-02   

    (FI/170/03 FHay03)      ;; derek added 2004-01-13 from tab82
    (FI/170/03 F03021)      ;; derek added 2004-02-10 from tab84 (similar titers where measured against same ags)

    (FI/338/95 F544/5)
    (FI/338/95 F544)        ;; derek, this is the nomenclature used in the jo-wu prediction experiment

    (FU/411/02 FHay01)      ;; derek added 2004-01-13 from tab82
    (FU/411/02 F03013)      ;; derek added 2004-02-10 from tab84 (and comes to the same location in the map as the Hay01 serum)

    (GD/25/93 F95013)
    (GD/25/93 F95014)

    (GE/A9509/95 F534/5)    ;; derek added the A on 4oct2001 into old ferrets data, now copied into this new file 2001-09-03
    (GU/54/89 F382/3)

    (HK/1/68 F356)
    (HK/1/68 F6811)
    (HK/1/68 F87022)
    (HK/1A/68 F09009)        ;; Bjorn added 2009-05-04
    (HK/1B/68 F09010)        ;; Bjorn added 2009-05-04

    (HK/1/89 F375A)

    (HK/107/71 F344/5)
    (HK/107A/71 F09001)      ;; Bjorn added 2009-05-04
    (HK/107B/71 F09002)      ;; Bjorn added 2009-05-04

    (HK/23/92 F500/1)

    (HK/34/90 F452/3)
    (HK/34/90 F452)          ;; derek 2002-10-14, this is the ferret nomenclature in the jo-wu prediction table
    (HK/34A/90 F92007)       ;; Bjorn added 2009-05-04

    (BE/32/92 F96015)        ;; Bjorn added 2009-05-04 (not previously used in cartography, i.e not in seq-t9a-mod27)

    (NL/823A/92 F09013)      ;; Bjorn added 2009-05-04 (not previously used in cartography, i.e not in seq-t9a-mod27)
    (NL/823B/92 F09014)      ;; Bjorn added 2009-05-04 (not previously used in cartography, i.e not in seq-t9a-mod27)

    (PA/548A/92 F09007)      ;; Bjorn added 2009-05-04 (not previously used in cartography, i.e not in seq-t9a-mod27)
    (PA/548B/92 F09008)      ;; Bjorn added 2009-05-04 (not previously used in cartography, i.e not in seq-t9a-mod27)

    (JO/33/94 F532/3)        ;; derek 2002-02-11, ok to merge with f95012 below
    (JO/33/94 F533)          ;; derek 2003-01-24, names this way in the dead end table
    (JO/33/94 F95011)        ;; derek 2002-02-11, not used in all-strains merge take 2.  derek 2003-02-28, vaccine table, ok to merge
    (JO/33/94 F95012)
    (JO/33A/94 F03019)       ;; Bjorn added 2009-05-04
    (JO/33B/94 F03020)       ;; Bjorn added 2009-05-04

    (LE/360/86 F338/9)

    (LY/1149/91 F426)
    (LY/1149A/91 F09005)     ;; Bjorn added 2009-05-04
    (LY/1149B/91 F09006)     ;; Bjorn added 2009-05-04

    (LY/2279/95 F552/3)
    (LY/2279/95 F552)        ;; derek 2002-10-14, this is the nomenclature used in the jo-wu prediction table
    (LY/2279A/95 F07027)     ;; bjorn/derek added 2008-03-31
    (LY/2279B/95 F07028)     ;; bjorn/derek added 2008-03-31

    (MA/G102/93 F478/9)
    (MA/G102r/93 RABBIT73)   ;; derek, added 2003-03-22, see seq-t9.html

    (MA/G252/93 F502/3)

    (MW/10/99 F00007)        ;; derek, merge these 3, 2002-02-11
    (MW/10/99 F00008)
    (MW/10/99 F990052)
    (MW/10A/99 F04003)       ;; derek 2004-06-20, from table 86, do not know if can be merged with above yet

    (NA/933/95 F96013)
    (NA/933/95 F97002)
    (NA/933/95 F546/7)       ;; derek 2003-03-22, changed from wu, see merge-hi-tables/seq-t9.html, and further down in the table for checking ok to merge

    (NIB-8 F89)               ;; derek added 2001-09-03 from old ferret file

    (NL/1/95 F524)            ;; derek added 2003-03-05 from rons email of 2003-03-04
    (NL/1/95 F524/5)

    (NL/1/02 F02003)          ;; derek added 2002-10-04 from tab76
    (NL/109/03 F03009)        ;; derek added 2004-06-20 from tab86
    (NL/118/01 F01005)        ;; derek added 2002-10-04 from tab76
    (NL/124/01 F01008)        ;; derek added 2004-01-13 from tab83
    (NL/126/01 F01010)        ;; derek added 2002-10-04 from tab76

    (NL/172/96 F564/5)        ;; derek, 2002-02-11, ok to merge the 3 that are used
    (NL/172/96 F564)          ;; derek added 2001-09-19, appears as non-pooled in tables 52 53 60 61 62 64
    (NL/172/96 F569)   ;; derek added 2001-09-03 from old ferret file.  derek added the notused 2002-02-11.  derek 2003-02-28, vaccine table, ok to merge
    (NL/172/96 F96022)

    (NL/18/94 F94004)

    (NL/22/03 F03001)         ;; derek added 2004-01-13 from tab82

    (NL/209/80 F237)
    (NL/209/80 F530/1)
    (NL/209A/80 F09017)       ;; Bjorn added 2009-05-04  
    (NL/209B/80 F09018)       ;; Bjorn added 2009-05-04


;;  derek 2003-02-28, ok to merge (on the seq tables), guess i'd said separate for all tables
;;    (NL/218a/95 F548/9)       ;; derek, 2002-02-11 ok(ish) split for now with F95037 below
;;    (NL/218notused/95 F95036/7)  ;; derek, 2003-02-28, ok to merge, seeing in vaccine table
;;    (NL/218b/95 F95037)       ;; derek added 2001-09-19, appears in tab 59, add for now and ask ron
    (NL/218/95 F548/9)   
    (NL/218/95 F95036/7)
    (NL/218/95 F95036)          ;; derek added 2003-03-05, after ron's email of 2003-03-04
    (NL/218/95 F95037)   

    (NL/233/82 F279)
    (NL/241/82 F280/1)       

    (NL/241/93 F504)          ;; derek, 2002-02-11, ok to merge with below
    (NL/241/93 F93022)
    (NL/286/97 F97007)
    (NL/301/99 F00003)
    (NL/312/03 F04001)        ;; derek added 2004-06-20 from table 86
    
    (NL/33/94 F94019)
    (NL/33/94 F9419)          ;; derek added 2001-09-03 from old ferret file  (is one of these a typo?, i guess so, but which one?)

    (NL/330/85 F308/9)

    ;;(NL/333/85 F310/11)
    (NL/333/85 F310/1)        ;; derek, 2002-05-05 changed /11 to /1 as /11 only appears in tab10, but /1 now appears in tabs 69, and /1 is common elsewhere

    (NL/414/98 F98012)
    (NL/427/98 F98015)

    (NL/450/88 F350/1)
    (NL/450/88 F380/1)        ;; derek added 2001-09-03 from old ferret file  (is one of these a typo?, i guess so, but which one?)

    (NL/462/98 F99003)
    (NL/462/98 F99003/4)      ;; derek added 2003-03-05 from ron's email 2003-03-04

    (NL/47/95 F95017)

    (NL/5/93 F92020)
    (NL/5/98 F98004)

    (NL/501/88 F362/3)
    (NL/620/89 F375)
    (NL/3/00 F00012)         ;; derek added 2002-02-11, from ron email on tab50
    (NL/182/00 F00016)       ;; derek added 2002-02-11, from ron email on tab50

    (NL/88/03 F03003)        ;; derek added 2004-01-13 from tab82
    (NL/88A/03 F03004)       ;; derek added 2004-06-20 from tab86 don't know if can be merged with above yet

    (NL/132/04 f05003)       ;; derek added 2005-03-16 from table 89

    (OS/2352/93 F94020)

    (OS/807/04 f04023)       ;; derek added 2005-03-16 from table 89

    (PC/1/73 F166/7)
    (PC/1/73 F454/5)
    (PC/1/73 F83037)
    (PC/1A/73 F07029)        ;; bjorn/derek added 2008-03-31
    (PC/1B/73 F07030)        ;; bjorn/derek added 2008-03-31

    (PH/2/82 F282/3)    ;; derek, 2002-02-11, ok merged with below
    (PH/2/82 F282)      ;; derek added when we added tab68, 2002-02
    (PH/2notused/82 F86052)  ;; derek 2002-02-11, not used in the all-strains-merge-take-2

    (PM/2007/99 F00025)       ;; derek added 2002-10-04 from tab76
    (PM/2007A/99 F03017)      ;; derek 2004-06-20, from table 86, do not know if can be merged with above yet.  in map similar, but cols different
    (PM/2007B/99 F03018)      ;; derek 2004-06-20, from table 86, do not know if can be merged with above yet.  in map similar, but cols different

    (PR/413/94 F724)
    (RD/577/80 F290)

    (SD/9/93 F508/9)    ;;derek, 2002-02-11, ok to merge with below
    (SD/9/93 F508)      ;;derek, 2002-10-14, this is the nomenclature used in the jo-wu prediction table
    (SD/9/93 F94008)
    (SD/9A/93 F94006)   ;;Bjorn, 06OCT09
    (SD/9B/93 F97020)   ;;Bjorn, 06OCT09
    (SD/9notused/93 F94011)

    (SH/31/80 F268)     ;; derek added when we added tab68, 2002-02

    (SH/11/87 F368)
    (SH/11/87 F498)
    (SH/11A/87 F07025)    ;; bjorn/derek added 2008-03-31
    (SH/11B/87 F07026)    ;; bjorn/derek added 2008-03-31

    (SI/2/87 F346/7)
    (SI/2/87 F88008)

    (SL/840/74 F456/7)
    (SL/840/74 F76/7)

    (SP/1/96 F581)
    (SP/22/90 F417)

    (ST/10/85 F384/5)
    (ST/10A/85 F07023)     ;; bjorn/derek added 2008-03-31
    (ST/10B/85 F07024)     ;; bjorn/derek added 2008-03-31

    (SY/5a/97 F00002)     ;; derek merge with f98010, 2002-02-11
    (SY/5hay/97 F398HAY)  ;; derek keep separate for now, might merge later, 2002-02-11
    (SY/5b/97 F732)       ;; derek keep separate for now, might merge later, 2002-02-11
    (SY/5a/97 F98010)

    (TE/1b/77 F139)
    (TE/1a/77 F230/1)
    (TE/1a/77 F230)    ;; only in tab68, added later by derek when we added tab68, 2002-01
    ;;(TE/1/77?? F33)  ;; derek removed 20050811, never used on anything we know about (don't remember the ??) and messes up for the concentric circle test case

    (VI/2/90 F400)
    (VI/2A/90 F09011)     ;; Bjorn added 2009-05-04
    (VI/2B/90 F09012)     ;; Bjorn added 2009-05-04

    (VI/3b/75 F134)          ;; note, only in tab9, and almost identical to ba/1/79 in that table
    (VI/3a/75 F83/5)
    (VI/3c/75 F432/3)  ;; note used in all-strains-merge-take-2 (2002-02-11), and now (2002-09-19) used in tab78, also in tab5, and thus the vaccine table
    (VI/3D/75 F85033)     ;; Bjorn added 2009-05-04
    (VI/3E/75 F85034)     ;; Bjorn added 2009-05-04

    (WE/001/04 F04026)    ;;Bjorn added 06OCT09

    (NL/42/06 F06015)     ;; Bjorn added 2009-06-26

    (WN/67/05 F07010)     ;; Bjorn added 2009-06-26

    (VI/7/87 F332/3)
    (WE/4/85 F326/7)

    (WE/1-IVR-139/04 f04025)  ;; derek added 2005-03-16 from table 89

    ;;(WU/359a/95 F546/7)       ;; derek, split from b below, and notused is in all-strains-merge-take-2, 2002-02-11
                                ;; 2003-03-22, changed to nanchang (see merge-hi-tables/seq-t9.html)
    (WU/359notused/95 F96013) ;; derek, added 2001-09-03 from old ferret file
    (WU/359b/95 F00017)       ;; derek, added 2002-02-11 from on email on tab50

    (WY/3/03 F04008)          ;; derek added 2004-06-20 from table 86
    (WY/3-X-147/03 f04015)    ;; derek added 2005-03-16 from table 89
    (WY/3-X-147/03 f04016)    ;; derek added 2005-03-16 from table 89
    
    
    (HI/052/05 F06010)									;; Bjorn added 2012-05-01
    (NL/363/06 F07017)									;; Bjorn added 2012-05-01
    (NL/348/07 F08001)									;; Bjorn added 2012-05-01
    (BR/010/07 F08003)									;; Bjorn added 2012-05-01
    (NL/377/08 F08053)									;; Bjorn added 2012-05-01
    (PE/016/09 F10005)									;; Perth, Bjorn added 2012-05-01
    (NL/009/10 F10025)									;; Bjorn added 2012-05-01
    (VI/210/09 F10027)									;; Bjorn added 2012-05-01
    (NL/034/10 F11007)									;; Bjorn added 2012-05-01
    (NL/063/11 F11021)									;; Bjorn added 2012-05-01
       
    
;; H5 antisera

    (A/Mallard/Netherlands/3A/1999 F10041)				;; Bjorn added 2011-10-15
    (A/Mallard/Netherlands/3B/1999 F10042)				;; Bjorn added 2011-10-15
    (A/Hongkong/156A/1997 F10045)						;; Bjorn added 2011-10-15
    (A/Hongkong/156B/1997 F10046)						;; Bjorn added 2011-10-15
    (A/Vietnam/1194A/2004 F10019)						;; Bjorn added 2011-10-15
    (A/Vietnam/1194B/2004 F10020)						;; Bjorn added 2011-10-15
    (A/Indonesia/5A/2005 F08032)						;; Bjorn added 2011-10-15
    (A/Indonesia/5B/2005 F08033)						;; Bjorn added 2011-10-15
    (A/Turkey/TurkeyA/1/2005 F08029)					;; Bjorn added 2011-10-15
    (A/Turkey/TurkeyB/1/2005 F10017)					;; Bjorn added 2011-10-15
    (A/Anhui/1A/2005 F08030)							;; Bjorn added 2011-10-15
    (A/Anhui/1B/2005 F08031)							;; Bjorn added 2011-10-15
    (A/Chicken/W-Java/EURRG30A/2007	F10015)				;; Bjorn added 2011-10-15
    (A/Chicken/W-Java/EURRG30B/2007	F10016)				;; Bjorn added 2011-10-15
    (A/Chicken/CentralJava_Kra/051A/2009 F11001)		;; Bjorn added 2011-10-15
    (A/Chicken/CentralJava_Kra/051B/2009 F11002)		;; Bjorn added 2011-10-15
    (A/Chicken/WestJava_Sbg/119A/2010 F11003)			;; Bjorn added 2011-10-15
    (A/Chicken/WestJava_Sbg/119B/2010 F11004)			;; Bjorn added 2011-10-15
    (A/Chicken/EastJava_Sgsr/121A/2010 F11005)			;; Bjorn added 2011-10-15
    (A/Chicken/EastJava_Sgsr/121B/2010 F11006)			;; Bjorn added 2011-10-15
    (A/Chicken/NorthSumatra_Mdn/072/2010 F11017)		;; Bjorn added 2011-10-15
    (A/Duck/HongKong/205B/1977 F79109)					;; Bjorn added 2011-10-15
    (A/Hongkong/156C/1997 F97016)						;; Bjorn added 2011-10-15
    (A/Vietnam/1194C/2004 F08034)						;; Bjorn added 2011-10-15
    (A/Indonesia/5C/2005 F10031)						;; Bjorn added 2012-01-31
    (A/Indonesia/5D/2005 F10032)						;; Bjorn added 2012-01-31
    (A/Indonesia/5E/2005 F10043)						;; Bjorn added 2012-01-31
    (A/CHICKEN/SOUTHSULAWESIMAKASSAR/157A/2011 F12013)         ;;Bjorn added 2012-07-16
    (A/CHICKEN/SOUTHSULAWESIMAKASSAR/157B/2011 F12014)         ;;Bjorn added 2012-07-16    
    (A/CHICKEN/WESTJAVASUKABUMI/006A/2008 F12015)              ;;Bjorn added 2012-07-16
    (A/CHICKEN/WESTJAVASUKABUMI/006B/2008 F12016)              ;;Bjorn added 2012-07-16

;; H1 antisera

    (A/NL/602A/09 F09035)								;; Bjorn added 2012-03-12
    (A/NL/602B/09 F09036)								;; Bjorn added 2012-03-12
    (A/Cal/004A/09 F09037)								;; Bjorn added 2012-03-12
    (A/Cal/004B/09 F09038)								;; Bjorn added 2012-03-12
    (A/Cal/007A/09 F10001)								;; Bjorn added 2012-03-12
    (A/Cal/007B/09 F10002)								;; Bjorn added 2012-03-12
    (A/NL/219A/11 F11011)								;; Bjorn added 2012-03-12
    (A/NL/219B/11 F11012)								;; Bjorn added 2012-03-12
    (A/NL/007A/10 F11013)								;; Bjorn added 2012-03-12
    (A/NL/007B/10 F11014)								;; Bjorn added 2012-03-12
    (A/NL/151A/11 F11019)								;; Bjorn added 2012-03-12
    (A/NL/151B/11 F11020)								;; Bjorn added 2012-03-12
    (A/NL/2001A/09 F11032)								;; Bjorn added 2012-03-12
    (A/NL/2001B/09 F11033)  							;; Bjorn added 2012-03-12
    
    (A/NL/128/04 F05005)								;; Bjorn added 2012-05-15
    (A/NCa/20/99 F05013)								;; Bjorn added 2012-05-15, New Caledonia
    (A/SIs/3/06 F07008)									;; Bjorn added 2012-05-15, Solomon Islands
    (A/BR/59/07 F08016)									;; Bjorn added 2012-05-15
    (X-181A F10003)										;; Bjorn added 2012-05-15
    (A/BY/7/95 F591)									;; Bjorn added 2012-05-15, Bayern
    (A/USSR/92/77 F78036)								;; Bjorn added 2012-05-15
    (A/BZ/11/78 F82012)									;; Bjorn added 2012-05-15
    (A/CL/1/83 F84012)									;; Bjorn added 2012-05-15
    (A/NJ/8/76 F86046)									;; Bjorn added 2012-05-15
    (A/TA/1/86 F90018)									;; Bjorn added 2012-05-15
    (A/TX/36/91 F96006)									;; Bjorn added 2012-05-15
    (A/BE/262/95 F98011)								;; Bjorn added 2012-05-15

    
  
;; H7N2 antisera

    (A/MulderA/57 F12049)								;; Kim added 2013-10-12
    (A/MulderB/57 F12050)								;; Kim added 2013-10-12
    (A/BakkerA/68 F12051)								;; Kim added 2013-10-12
    (A/BakkerB/68 F12052)								;; Kim added 2013-10-12
    (A/BI/16190A/68 F12021)								;; Kim added 2013-10-12
    (A/BI/16190B/68 F12022)								;; Kim added 2013-10-12
    (A/BI/93A/70 F12025)								;; Kim added 2013-10-12
    (A/BI/93B/70 F12026)								;; Kim added 2013-10-12
    (A/BI/21793A/72 F12027)								;; Kim added 2013-10-12
    (A/BI/21793B/72 F12028)								;; Kim added 2013-10-12
    (A/BI/1761A/76 F12029)								;; Kim added 2013-10-12
    (A/BI/1761B/76 F12030)								;; Kim added 2013-10-12
    (A/BI/2271A/76 F12031)								;; Kim added 2013-10-12
    (A/BI/2271B/76 F12032)								;; Kim added 2013-10-12
    (A/NL/233A/82 F12033)								;; Kim added 2013-10-12
    (A/NL/233B/82 F12034)								;; Kim added 2013-10-12
    (A/NL/620A/89 F12035)								;; Kim added 2013-10-12
    (A/NL/620B/89 F12036)								;; Kim added 2013-10-12
    (A/NL/823A/92 F12037)								;; Kim added 2013-10-12
    (A/NL/823B/92 F12038)								;; Kim added 2013-10-12
    (A/NL/179A/93 F12039)								;; Kim added 2013-10-12
    (A/NL/179B/93 F12040)								;; Kim added 2013-10-12
    (A/NL/178A/95 F12003)								;; Kim added 2013-10-12
    (A/NL/178B/95 F12004)								;; Kim added 2013-10-12
    (A/NL/301A/99 F12041)								;; Kim added 2013-10-12
    (A/NL/301B/99 F12042)								;; Kim added 2013-10-12
    (A/NL/213A/03 F12023)								;; Kim added 2013-10-12
    (A/NL/213B/03 F12024)								;; Kim added 2013-10-12
    (A/NL/132A/04 F12043)								;; Kim added 2013-10-12
    (A/NL/132B/04 F12044)								;; Kim added 2013-10-12
    (A/NL/69A/07 F12045)								;; Kim added 2013-10-12
    (A/NL/69B/07 F12046)								;; Kim added 2013-10-12
    (A/NL/69A/09 F12047)								;; Kim added 2013-10-12
    (A/NL/69B/09 F12048)								;; Kim added 2013-10-12  
    (A/NL/42A/06 F13038)								;; Kim added 2014-01-26
    (A/NL/42B/06 F13039)								;; Kim added 2014-01-26
    (A/NL/69A/07 F13040)								;; Kim added 2014-01-26
    (A/NL/69B/07 F13041)								;; Kim added 2014-01-26
    (A/NL/63A/11 F13044)								;; Kim added 2014-01-26
    (A/NL/63B/11 F13045)								;; Kim added 2014-01-26  
;; H3N2 antisera for updating map post 2002

    (A/Brisbane/010/07 F07035)							       ;; Stefan added 2015-01-12    
    (A/Ned/700/11 F12053)							       ;; Stefan added 2015-01-12   
    (A/Ned/002/12 F12065)							       ;; Stefan added 2015-01-12    
    (A/Ned/622/12 F13012)							       ;; Stefan added 2015-01-12    
    (A/Victoria/361A/11 F13014)							       ;; Stefan added 2015-01-12    
    (A/Victoria/361B/11 F13015)							       ;; Stefan added 2015-01-12    
    (A/Ned/2249/13 F14009)							       ;; Stefan added 2015-01-12 
   (IVR-165 F12057)					    		               ;; Stefan added 2015-01-12 
   (X-223A F13016)							               ;; Stefan added 2015-01-12 

;; H2 antisera

	(Mallard/NL/31A/06	F12005)							;;	Bjorn	added	2013-11-25
	(Mallard/NL/31B/06	F12006)							;;	Bjorn	added	2013-11-25
	(Mallard/NL/14A/07	F12007)							;;	Bjorn	added	2013-11-25
	(Mallard/NL/14B/07	F12008)							;;	Bjorn	added	2013-11-25
	(Netherlands/K1A/63	F12009)							;;	Bjorn	added	2013-11-25
	(Netherlands/K1B/63	F12010)							;;	Bjorn	added	2013-11-25
	(Netherlands/B1A/68	F12011)							;;	Bjorn	added	2013-11-25
	(Netherlands/B1B/68	F12012)							;;	Bjorn	added	2013-11-25
	(Japan/305A/57	F70166)								;;	Bjorn	added	2013-11-25
	(Japan/305B/57	F70225)								;;	Bjorn	added	2013-11-25
	(Japan/305C/57	F75118)								;;	Bjorn	added	2013-11-25
	(Japan/305D/57	F75119)								;;	Bjorn	added	2013-11-25
	(Japan/305E/57	F78135)								;;	Bjorn	added	2013-11-25
	(Japan/305F/57	F78136)								;;	Bjorn	added	2013-11-25
	(Singapore/1A/57	F83050)							;;	Bjorn	added	2013-11-25
	(Singapore/1B/57	F83051)							;;	Bjorn	added	2013-11-25
	(Singapore/1C/57	R2273)							;;	Bjorn	added	2013-11-25
	(England/1A/66		F15024)							;;	Stefan	added	2015-10-20
	(England/1B/66		F15025)							;;	Stefan	added	2015-10-20
	(Tokyo/3A/67		F15026)							;;	Stefan	added	2015-10-20
	(Tokyo/3B/67		F15027)							;;	Stefan	added	2015-10-20


;; New H5 antisera
(A/mallard/Sweden/49/2002	F05026)		;;	Stefan added 2016-11-30
(A/HongKong/483A/1997	F16033)			;;	Stefan added 2016-11-30
(A/HongKong/483B/1997	F16034)			;;	Stefan added 2016-11-30
(A/Vietnam/1194D/2004	F16039)			;;	Stefan added 2016-11-30
(A/Vietnam/1194E/2004	F16040)			;;	Stefan added 2016-11-30
(A/Indonesia/5F/2005	F16037)			;;	Stefan added 2016-11-30
(A/Indonesia/5G/2005	F16038)			;;	Stefan added 2016-11-30
(A/Anhui/1C/2005	F16044)			;;	Stefan added 2016-11-30
(A/Iraq/755A/2006	F16013)			;;	Stefan added 2016-11-30
(A/Iraq/755B/2006	F16014)			;;	Stefan added 2016-11-30
(A/Chicken/Netherlands/EMC-3A/2014	F16021)		;;	Stefan added 2016-11-30
(A/Chicken/Netherlands/EMC-3B/2014	F16022)		;;	Stefan added 2016-11-30
(A/Guangzhou/39715A/2014	F16025)			;;	Stefan added 2016-11-30
(A/Guangzhou/39715B/2014	F16026)			;;	Stefan added 2016-11-30
(A/Egypt/NO1753A/2014	F16027)			;;	Stefan added 2016-11-30
(A/Egypt/NO1753B/2014	F16028)			;;	Stefan added 2016-11-30
(A/Guizhou/1A/2013	F16029)			;;	Stefan added 2016-11-30
(A/Guizhou/1B/2013	F16030)			;;	Stefan added 2016-11-30
(A/goose/EasternChina/1112A/2011	F16036)		;;	Stefan added 2016-11-30
(A/goose/EasternChina/1112B/2011	F16035)		;;	Stefan added 2016-11-30
(A/Duck/Bangladesh/19097A/2013	F16045)		;;	Stefan added 2016-11-30
(A/Duck/Bangladesh/19097B/2013	F16046)		;;	Stefan added 2016-11-30
(A/gyrfalcon/Washington/41088-6A/2014	F16055)	;;	Stefan added 2016-11-30
(A/gyrfalcon/Washington/41088-6B/2014	F16056)	;;	Stefan added 2016-11-30
(A/Anhui/1/2005_156TA_222QL_224GS_A	F16031)	;;	Stefan added 2016-11-30
(A/Anhui/1/2005_156TA_222QL_224GS_B	F16032)	;;	Stefan added 2016-11-30
(A/Cambodia/x0810301/13A		F17013) ;;	Stefan added 2016-06-29
(A/Cambodia/x0810301/13B		F17014) ;;	Stefan added 2016-06-29
(A/duck/Vietnam/NCVD-1283A/2012         F17030) ;;	Stefan added 2017-09-15
(A/duck/Vietnam/NCVD-1283B/2012         F17031) ;;	Stefan added 2017-09-15
(A/CHICKEN/JIANGSU/K0101A/2010          F17032) ;;	Stefan added 2017-09-15
(A/CHICKEN/JIANGSU/K0101B/2010          F17033) ;;	Stefan added 2017-09-15
(A/Sichuan/26221A/2014                  F17034) ;;	Stefan added 2017-09-15
(A/Sichuan/26221B/2014                  F17035) ;;	Stefan added 2017-09-15
(A/duck/Jiangxi/0114_NCJD064-PA/2015    F17036) ;;	Stefan added 2017-09-15
(A/duck/Jiangxi/0114_NCJD064-PB/2015    F17037) ;;	Stefan added 2017-09-15
(A/CHICKEN/CHIPING/0321A/2014           F17038) ;;	Stefan added 2017-09-15
(A/CHICKEN/CHIPING/0321B/2014           F17039) ;;	Stefan added 2017-09-15


;; Vaccine sera
(A/Indonesia/5/2005_156TA_222QL_224GS_VACC                  F902) ;;	Stefan added 2017-09-15
(A/Iraq/755/2006_VACC                                       F904) ;;	Stefan added 2017-09-15
(A/turkey/Turkey/65596/2006_167TA_VACC                      F906) ;;	Stefan added 2017-09-15
(A/Anhui/1/2005_156TA_222QL_224GS_VACC                      F908) ;;	Stefan added 2017-09-15
(A/mallard/Sweden/49/2002_119KR_126DE_222QL_224GS_VACC      F909) ;;	Stefan added 2017-09-15
(A/Vietnam/1194/2004_156TA_222QL_224GS_VACC                 F912) ;;	Stefan added 2017-09-15
  
))
(defun ferret-to-strain (ferret)
  (let ((strain (caar (member ferret *ferret-strain-correspondence* 
			      :test (^ (ferret strain-ferret-pair) (eql ferret (cadr strain-ferret-pair)))))))
    (if strain
	strain
      ferret)))

(defun ferret-to-strain-and-ferret (ferret)
  (let ((strain (caar (member ferret *ferret-strain-correspondence* 
			      :test (^ (ferret strain-ferret-pair) (eql ferret (cadr strain-ferret-pair)))))))
    (if strain
	(let ((strain-location (strain-location strain))
              (strain-isolate  (strain-isolate  strain))
              (strain-year (strain-year strain))
              (unslashed-ferret (read-from-string (string-subst #\/ #\_ (string ferret)))))
          (if (equal (print strain) (print (read-from-string (format nil "~a/~a/~a" 
                                                              strain-location 
                                                              strain-isolate 
                                                              (if (numberp strain-year)
                                                                  (format nil "~2,'0d")
                                                                strain-year)))))
              (read-from-string (format nil "~a/~a_~a/~a" 
                                        strain-location
                                        strain-isolate
                                        unslashed-ferret
                                        (if (numberp strain-year)
                                            (format nil "~2,'0d")
                                          strain-year)))
            (read-from-string (format nil "~a_~a" unslashed-ferret strain))))
      ferret)))

(defun strain-to-ferrets (s)
  (loop for (strain ferret) in *ferret-strain-correspondence* 
      when (eql s strain)
      collect ferret))

(defun hi-table-ferret-to-strain (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (mapcar #'ferret-to-strain (hi-table-sera hi-table))
   (hi-table-values hi-table)
   (hi-table-name hi-table)))

(defun hi-table-ferret-to-strain-and-ferret (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (mapcar #'ferret-to-strain-and-ferret (hi-table-sera hi-table))
   (hi-table-values hi-table)
   (hi-table-name hi-table)))

(defun ferret-serum-name-p (name)
  (setq name (anything->string name))
  (and (> (length name) 0)
       (or (equal #\f (aref name 0))
	   (equal #\F (aref name 0)))))
