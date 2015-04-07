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
