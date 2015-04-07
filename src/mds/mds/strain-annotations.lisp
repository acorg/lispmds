(in-package user)


;;;----------------------------------------------------------------------
;;;                      isolation dates and season
;;;----------------------------------------------------------------------

(defvar *isolation-dates*)
(setq *isolation-dates*
  (fi-in-readline
   (uw-sfnr "mds/data/isolation-dates/RIVM-annotated.txt" :assertIsFile t)
   :comment-char #\;
   :line-process-f #'space-delimited-string-to-list))

;; the dates in the RIVM file need changing into (yyyy mm dd) format
;; acutally the file stored version is yyyymmdd, then when i read in i go to this list format...
;; can set to whatever protocol i want, only used for the strain selection meeting timeseries right now
(defun isolation-date-year  (strain) (let ((date (assoc-value-2 strain *isolation-dates*))) (if (listp date) (nth 0 date) date)))
(defun isolation-date-month (strain) (let ((date (assoc-value-2 strain *isolation-dates*))) (if (listp date) (nth 1 date) date)))
(defun isolation-date-day   (strain) (let ((date (assoc-value-2 strain *isolation-dates*))) (if (listp date) (nth 2 date) date)))


;;;----------------------------------------------------------------------
;;;                      string isolation dates
;;;----------------------------------------------------------------------

(defun string-isolation-date-year  (date) (string->number (substring date 0 3)))
(defun string-isolation-date-month (date) (string->number (substring date 4 5)))
(defun string-isolation-date-day   (date) (string->number (substring date 6 7)))

(defun make-string-isolation-date (year month day)
  (format nil "~4d~2,'0d~2,'0d" year month day))

(defun string-isolation-date-increment-month (date month-increment)
  (make-string-isolation-date
   (+ (string-isolation-date-year  date) (bool->bit (> (+ (string-isolation-date-month date) month-increment) 12)))
   (inc (mod (dec (+ (string-isolation-date-month date) month-increment)) 12))
   (string-isolation-date-day   date)))

(defun string-isolation-date-< (date1 date2)
  (or (< (string-isolation-date-year date1) (string-isolation-date-year date2))
      (and (= (string-isolation-date-year date1) (string-isolation-date-year date2))
	   (< (string-isolation-date-month date1) (string-isolation-date-month date2)))
      (and (= (string-isolation-date-year date1) (string-isolation-date-year date2))
	   (= (string-isolation-date-month date1) (string-isolation-date-month date2))
	   (< (string-isolation-date-day   date1) (string-isolation-date-day   date2)))))      
(defun string-isolation-date-=  (date1 date2) (equal date1 date2))
(defun string-isolation-date-<= (date1 date2) (or (string-isolation-date-< date1 date2) (string-isolation-date-= date1 date2)))
(defun string-isolation-date->  (date1 date2) (not (string-isolation-date-<= date1 date2)))
(defun string-isolation-date->= (date1 date2) (not (string-isolation-date-< date1 date2)))


;;;----------------------------------------------------------------------
;;;                      3-tuple isolation dates
;;;----------------------------------------------------------------------

(defun 3tuple-isolation-date-year  (date) (nth 0 date))
(defun 3tuple-isolation-date-month (date) (nth 1 date))
(defun 3tuple-isolation-date-day   (date) (nth 2 date))

(defun make-3tuple-isolation-date (year month day)
  (list year month day))

(defun 3tuple-isolation-date-increment-month (date month-increment)
  (make-3tuple-isolation-date
   (+ (3tuple-isolation-date-year  date) (bool->bit (> (+ (3tuple-isolation-date-month date) month-increment) 12)))
   (inc (mod (dec (+ (3tuple-isolation-date-month date) month-increment)) 12))
   (3tuple-isolation-date-day   date)))

(defun 3tuple-isolation-date-< (date1 date2)
  (or (< (3tuple-isolation-date-year date1) (3tuple-isolation-date-year date2))
      (and (= (3tuple-isolation-date-year date1) (3tuple-isolation-date-year date2))
	   (< (3tuple-isolation-date-month date1) (3tuple-isolation-date-month date2)))
      (and (= (3tuple-isolation-date-year date1) (3tuple-isolation-date-year date2))
	   (= (3tuple-isolation-date-month date1) (3tuple-isolation-date-month date2))
	   (< (3tuple-isolation-date-day   date1) (3tuple-isolation-date-day   date2)))))      
(defun 3tuple-isolation-date-=  (date1 date2) (equal date1 date2))
(defun 3tuple-isolation-date-<= (date1 date2) (or (3tuple-isolation-date-< date1 date2) (3tuple-isolation-date-= date1 date2)))
(defun 3tuple-isolation-date->  (date1 date2) (not (3tuple-isolation-date-<= date1 date2)))
(defun 3tuple-isolation-date->= (date1 date2) (not (3tuple-isolation-date-< date1 date2)))



(defun y2k-offset (n)
  ;; add 14 years
  (if (<= n 14)
      (+ n 100)
    n))

(defun isolation-season-< (a b)
  (let* ((a-string (string a))
	 (b-string (string b))
	 (a-lower (y2k-offset (string->number (substring a-string 0 1))))
	 (a-upper (y2k-offset (string->number (substring a-string 3 4))))
	 (b-lower (y2k-offset (string->number (substring b-string 0 1))))
	 (b-upper (y2k-offset (string->number (substring b-string 3 4)))))
    (or (< a-lower b-lower)
	(< a-upper b-upper))))

(defun isolation-season-> (a b)
  (isolation-season-< b a))

(defun isolation-season-<= (a b)
  (or (equal a b)
      (isolation-season-< a b)))

(defun isolation-season-= (a b)
  (equal a b))

(defun sort-isolation-seasons (l)
  (my-sort l #'isolation-season-<))

(defun isolation-season-difference (a b)
  (let* ((a-string (string a))
	 (b-string (string b))
	 (a-lower (y2k-offset (string->number (substring a-string 0 1))))
	 (a-upper (y2k-offset (string->number (substring a-string 3 4))))
	 (b-lower (y2k-offset (string->number (substring b-string 0 1))))
	 (b-upper (y2k-offset (string->number (substring b-string 3 4)))))
    (av (list (- a-lower b-lower) (- a-upper b-upper)))))



(defvar *seasons*)
(setq *seasons* (sort-isolation-seasons (remove-duplicates (nths 1 *isolation-dates*))))

(defvar *all-winter-seasons*)
(setq *all-winter-seasons*
  ;; note, this is different from seasons, above, this is all the winter seasons
  ;; whether or not there are strains, the above is the seasons where we have strains
  (append 
   (loop for y from 68 to 98 collect
	 (read-from-string (format nil "~a-~a" y (inc y))))
   '(99-00 00-01 01-02)))

(defvar *all-summer-seasons*)
(setq *all-summer-seasons*
  ;; note, this is different from seasons, above, this is all the winter seasons
  ;; whether or not there are strains, the above is the seasons where we have strains
  (append 
   (loop for y from 68 to 99 collect
	 (read-from-string (format nil "~a-~a" y y)))
   '(00-00 01-01 02-02)))

(defun strain-isolation-season-< (a b)
  (isolation-season-< (strain-isolation-season a) (strain-isolation-season b)))

(defun strain-isolation-season-> (a b)
  (strain-isolation-season-< b a))

(defun sort-strains-by-season (strains)
  (my-sort
   (sort-strains strains)
   #'strain-isolation-season-<))

(defun y2k-sensitive-dec (x)
  (if (zerop x)
      99
    (dec x)))

(defun strain-isolation-season (strain)
  (let ((isolation-season (assoc-value-1 strain *isolation-dates*)))
    (if isolation-season
	isolation-season
      (let ((strain-year (strain-year strain)))
	(if (not (numberp strain-year))
	    (error "Strain ~a has no season, and one cannot be assumed as the strain year (~a) is not a number" strain strain-year)
	  (let ((assumed-season (read-from-string (format nil "~2,'0d-~2,'0d" (y2k-sensitive-dec strain-year) strain-year))))
	    (format t "No isolation seasons for ~a, assuming the winter season isolation after jan1; thus, ~a.~%" strain assumed-season)
	    assumed-season))))))

(defun strain-isolation-date (strain)
  (assoc-value-2 strain *isolation-dates*))

(defun strain-jan-prototype (strain)
  (assoc-value-3 strain *isolation-dates*))



(defun summer-season-p (season)
  (setq season (anything->string season))
  (let ((lower (y2k-offset (string->number (substring season 0 1))))
	(upper (y2k-offset (string->number (substring season 3 4)))))
    (eql lower upper)))

(defun winter-season-p (season)
  (setq season (anything->string season))
  (let ((lower (y2k-offset (string->number (substring season 0 1))))
	(upper (y2k-offset (string->number (substring season 3 4)))))
    (eql lower (dec upper))))

(defun summer-season-preceeding-winter-season (winter-season)
  (read-from-string (format nil "~a-~a" (substring (string winter-season) 0 1) (substring (string winter-season) 0 1))))

(defun summer-season-following-winter-season (winter-season)
  (read-from-string (format nil "~a-~a" (substring (string winter-season) 3 4) (substring (string winter-season) 3 4))))

(defun first-year-of-season (season)
  (read-from-string (substring (string season) 0 1)))

(defun next-winter-season (season)
  (read-from-string 
   (format nil "~2,'0d-~2,'0d" 
	   (mod (+ 1 (first-year-of-season season)) 100)
	   (mod (+ 2 (first-year-of-season season)) 100))))

(defun strain-from-year-season-p (strain winter-season)
  (member (strain-isolation-season strain) (list winter-season (summer-season-following-winter-season winter-season))))


;;;----------------------------------------------------------------------
;;;                      strains from seasons
;;;----------------------------------------------------------------------

(defun years-strains-from-season (season strains)
  (if (summer-season-p season)
      (error "add summer season stuff to 'years-strains-from-season'")
    (let ((following-summer-season (summer-season-following-winter-season season)))
      (loop for strain in strains 
	  when (let ((strain-season (strain-isolation-season strain)))
		 (or (eql strain-season season)
		     (eql strain-season following-summer-season)))
	  collect strain))))


;;;----------------------------------------------------------------------
;;;                        Jan prototypes
;;;----------------------------------------------------------------------

(defvar *jan-prototypes*)
(setq *jan-prototypes* (collect (^ (l) (= 4 (length l))) *isolation-dates*)) 

(defvar *jan-prototype-strains*)
(setq *jan-prototype-strains* (nths 0 *jan-prototypes*))


;;;----------------------------------------------------------------------
;;;                      sequenced strains
;;;----------------------------------------------------------------------

;; REMEMBER WHEN I UPDATE THIS TO INCLUDE THE DUPLICATES

(defvar *sequenced-strains*)
;; updated 2003-01-13 with fujian strains
(setq *sequenced-strains*  ;; as of 2002-10  (the strains in the 373 dataset by ron, includes 83 non-eur sequenced strains, but no matter)
  '(AC/2/68 
    BI/15793/68 
    BI/16190/68 
    BI/16398/68 
    HK/1/68 
    NT/60/68 
    BI/808/69 
    BI/908/69 
    BI/17938/69 
    EN/878/69 
    EN/939/69 
    BI/93/70 
    BI/2668/70 
    QU/7/70 
    BI/6449/71 
    BI/21438/71 
    BI/21801/71 
    HK/107/71 
    ME/1/71 
    BI/6022/72 
    BI/21793/72 
    BI/23290/72 
    BI/23337/72 
    BI/23488/72 
    EN/42/72 
    ME/102/72 
    UD/307/72 
    BI/552/73 
    BI/748/73 
    BI/3517/73 
    DU/4/73 
    PC/1/73 
    BI/5146/74 
    BI/5930/74 
    BI/5931/74 
    BI/7398/74 
    BI/9459/74 
    BI/2600/75 
    BI/2813/75 
    EN/864/75 
    MY/1/75 
    SP/4/75 
    TY/1/75 
    VI/3/75 
    BI/628/76 
    BI/1761/76 
    BI/1905/76 
    BI/2271/76 
    BI/5029/76 
    BI/5657/76 
    BI/6545/76 
    AM/1609/77 
    BI/3895/77 
    EN/321/77 
    RD/5828/77 
    RD/8179/77 
    TE/1/77 
    BI/2461/78 
    BA/1/79 
    BA/2/79 
    DH/33/80 
    NL/209/80 
    RD/577/80 
    SH/31/80 
    AB/1/81 
    BG/2/81 
    BI/4791/81 
    BI/10684/82 
    CR/231/82 
    EN/951/82 
    NL/228/82 
    NL/233/82 
    NL/241/82 
    PH/2/82 
    OI/3/83 
    OS/13676/83 
    PR/2/83 
    TE/12764/83 
    TE/12835/83 
    WE/3248/83 
    AX/8/84 
    CE/1/84 
    TE/17988/84 
    TE/18088/84 
    TE/18733/84 
    BA/25/85 
    CC/4/85 
    CN/4/85 
    GF/V728/85 
    GM/346/85 
    MC/1/85 
    MI/1/85 
    NJ/4/85 
    NL/330/85 
    ST/10/85 
    WE/4/85 
    CO/2/86 
    CZ/4/86 
    EQ/4/86 
    LE/360/86 
    ME/6/86 
    CO/2/87 
    GD/9/87 
    GU/1/87 
    GU/3/87 
    QT/10/87 
    SH/11/87 
    SI/2/87 
    SY/1/87 
    TY/1275/87 
    VI/7/87 
    CB/38/88 
    CC/2/88 
    EN/427/88 
    EN/428/88 
    HD/1/88 
    KO/768/88 
    NL/450/88 
    NL/501/88 
    ST/12/88 
    TE/39989/88 
    UR/3/88 
    AT/211/89 
    BE/352/89 
    BE/353/89 
    GE/5007/89 
    GU/54/89 
    HK/1/89 
    NL/620/89 
    NL/650/89 
    NL/738/89 
    SP/34/89 
    SP/35/89 
    SP/36/89 
    SP/40/89 
    SP/53/89 
    VI/1/89 
    WE/5/89 
    CH/22/90 
    FI/133/90 
    HK/25/90 
    HK/29/90 
    HK/34/90 
    ME/2/90 
    ME/5/90 
    SH/24/90 
    SP/20/90 
    SP/22/90 
    SP/23/90 
    SU/1/90 
    VI/2/90 
    AS/1/91 
    CA/1/91 
    EN/260/91 
    EN/261/91 
    GE/6447/91 
    HO/57165/91 
    KM/14/91 
    LY/459/91 
    LY/1149/91 
    LY/1182/91 
    LY/1189/91 
    LY/1276/91 
    LY/1337/91 
    LY/1373/91 
    LY/1594/91 
    LY/5441/91 
    LY/23672/91 
    LY/24103/91 
    LY/24222/91 
    MA/G12/91 
    NL/816/91 
    SP/1/91 
    SP/2/91 
    SP/3/91 
    ST/20/91 
    ST/6092/91 
    WA/15/91 
    AM/4112/92 
    BE/32/92 
    ES/1285/92 
    FI/218/92 
    FI/220/92 
    FI/247/92 
    GE/5113/92 
    ;; HK/23/92     removed sept 04 2002 because eur sequence is inconsistent with database sequence (the removal carried forward from the last 299 dataset)
    HO/56798/92 
    HO/56829/92 
    HO/56941/92 
    MA/G58/92 
    OV/31/92 
    NI/3126/92 
    NI/3129/92 
    NL/819/92 
    NL/823/92 
    NL/935/92 
    NL/938/92 
    PA/320/92 
    PA/325/92 
    PA/407/92 
    PA/417/92 
    PA/424/92 
    PA/457/92 
    PA/467/92 
    PA/490/92 
    PA/512/92 
    PA/548/92 
    PA/564/92 
    PA/583/92 
    PA/597/92 
    PA/614/92 
    PE/1/92 
    RD/100540/92 
    SA/8/92 
    SA/9/92 
    SA/23/92 
    SA/27/92 
    SE/C273/92 
    ST/7/92 
    ST/8/92 
    ST/12/92 
    ST/13/92 
    TI/5957/92 
    UM/1982/92 
    UM/2000/92 
    VI/68/92 
    AK/4/93 
    EN/247/93 
    ES/5458/93 
    GD/25/93 
    LY/672/93 
    LY/1631/93 
    LY/1803/93 
    LY/1815/93 
    LY/22686/93 
    LY/23602/93 
    MA/G101/93 
    MA/G102/93 
    MA/G109/93 
    MA/G116/93 
    MA/G122/93 
    MA/G130/93 
    MA/G252/93 
    NL/3/93 
    NL/12/93 
    NL/17/93 
    NL/101/93 
    NL/115/93 
    NL/126/93 
    NL/165/93 
    NL/179/93 
    NL/241/93 
    NL/276/93 
    NL/316/93 
    NL/330/93 
    NL/357/93 
    NL/371/93 
    NL/372/93 
    NL/398/93 
    NL/399/93 
    NL/440/93 
    OS/2219/93 
    OS/2352/93 
    PA/287/93 
    SD/9/93 
    SG/6/93 
    SL/142/93 
    SL/160/93 
    SP/3/93 
    ST/20/93 
    VI/104/93 
    WE/59/93 
    YA/56/93 
    YA/61/93 
    YA/62/93 
    EN/7/94 
    GE/8378/94 
    HK/1/94 
    HK/2/94 
    HK/55/94 
    HK/56/94 
    JO/33/94 
    JO/47/94 
    NL/11/94 
    NL/18/94 
    NL/33/94 
    SA/15/94 
    SA/25/94 
    WE/6/94 
    FI/338/95 
    FI/339/95 
    FI/381/95 
    GE/9846/95 
    GE/298971/95 
    GE/A9509/95 
    HK/3/95 
    HK/32/95 
    HK/38/95 
    HK/49/95 
    HK/55/95 
    LY/2279/95 
    NA/933/95 
    NL/1/95 
    NL/178/95 
    NL/218/95 
    NL/226/95 
    NL/271/95 
    SY/26/95 
    VI/75/95 
    WU/359/95 
    BR/8/96 
    GE/3958/96 
    HK/20/96 
    HK/42/96 
    HK/357/96 
    HK/358/96 
    HK/434/96 
    LY/1781/96 
    NL/91/96 
    NL/172/96 
    SP/1/96 
    SP/4/96 
    SQ/1147/96   ;; derek, added 2003-03-22  
    AU/10/97 
    HK/1/97 
    HK/280/97 
    HK/387/97 
    HK/391/97 
    JO/10/97 
    NE/491/97 
    NL/300/97 
    OS/21/97 
    OS/244/97 
    SY/5/97 
    TA/307/97 
    AH/1/98 
    CC/45/98 
    CH/3/98 
    CX/27/98 
    FI/572/98 
    GW/10/98 
    HK/4437/98 
    NL/5/98 
    NL/414/98 
    NL/427/98 
    NL/462/98 
    AH/135/99 
    CV/10/99 
    GO/1/99 
    GW/109/99 
    MM/1/99 
    MW/10/99 
    NL/301/99 
    NW/1/99 
    OS/2137/99 
    PM/2007/99   ;; added 2003-04
    PR/1/99 
    PR/3/99 
    ST/1/99 
    MM/1/00 
    MM/2/00 
    NL/3/00 
    NL/182/00 
    OS/841/00 
    OS/6391/00 
    ST/2/00 
    ST/3/00 
    ST/8/00 
    UM/2/00 
    UM/3/00 
    NL/118/01 
    NL/124/01 
    NL/126/01 
    NL/1/02 
    NL/120/02
    NL/368/02   ;; added in 2003-04, first strain of the 2003 season
    FI/1/02     ;; added 2004-01-13, for fujian ms update
    FU/411/02   ;; added 2004-01-13, for fujian ms update
    NL/368/02   ;; added 2004-01-13, for fujian ms update
    NL/20/03    ;; added 2004-01-13, for fujian ms update
    NL/22/03    ;; added 2004-01-13, for fujian ms update
    NL/56/03    ;; added 2004-01-13, for fujian ms update
    NL/88/03    ;; added 2004-01-13, for fujian ms update
    NL/112/03   ;; added 2004-01-13, for fujian ms update
    NL/133/03   ;; added 2004-01-13, for fujian ms update
    NL/213/03   ;; added 2004-01-13, for fujian ms update
    NL/217/03   ;; added 2004-01-13, for fujian ms update
    NL/222/03   ;; added 2004-01-13, for fujian ms update
    NL/255/03   ;; added 2004-01-13, for fujian ms update
    FI/170/03   ;; added 2004-01-13, for fujian ms update
    FI/278/03   ;; added 2004-01-13, for fujian ms update
    WY/3/03     ;; added 2004-01-13, for fujian ms update
    ))
#|
(setq *sequenced-strains*  ;; as of 2002-02  (updated to remove hk/23/92 on sept 4, 2002)
  '(BI/15793/68       
    BI/16190/68	  
    BI/16398/68	  
    HK/1/68		  
    BI/17938/69	  
    BI/808/69	  
    BI/908/69	  
    BI/2668/70	  
    BI/93/70	  
    BI/21438/71	  
    BI/21801/71	  
    BI/6449/71	  
    HK/107/71	  
    BI/21793/72	  
    BI/23290/72	  
    BI/23337/72	  
    BI/23488/72	  
    BI/6022/72	  
    EN/42/72	  
    BI/3517/73	  
    BI/552/73	  
    BI/748/73	  
    PC/1/73		  
    BI/5146/74	  
    BI/5930/74	  
    BI/5931/74	  
    BI/7398/74	  
    BI/9459/74	  
    BI/2600/75	  
    BI/2813/75	  
    VI/3/75		  
    BI/1761/76	  
    BI/1905/76	  
    BI/2271/76	  
    BI/5029/76	  
    BI/5657/76	  
    BI/628/76	  
    BI/6545/76	  
    AM/1609/77	  
    BI/3895/77	  
    EN/321/77	  
    RD/5828/77	  
    RD/8179/77	  
    TE/1/77		  
    BI/2461/78	  
    BK/1/79		  
    NL/209/80	  
    RD/577/80	  
    BI/4791/81	  
    BI/10684/82	  
    EN/951/82	  
    NL/228/82	  
    NL/233/82	  
    NL/241/82	  
    PH/2/82		  
    OS/13676/83	  
    WE/3248/83	  
    CE/1/84		  
    CC/4/85		  
    GF/V728/85	  
    NL/330/85	  
    ST/10/85	  
    WE/4/85		  
    CO/2/86		  
    LE/360/86	  
    SH/11/87	  
    SI/2/87		  
    VI/7/87		  
    CC/2/88		  
    EN/427/88	  
    NL/450/88	  
    NL/501/88	  
    ST/12/88	  
    AT/211/89	  
    BE/352/89	  
    BE/353/89	  
    GE/5007/89	  
    GU/54/89	  
    HK/1/89		  
    NL/620/89	  
    NL/650/89	  
    NL/738/89	  
    SP/34/89	  
    SP/35/89	  
    SP/36/89	  
    SP/40/89	  
    SP/53/89	  
    VI/1/89		  
    WE/5/89		  
    CH/22/90	  
    HK/25/90	  
    HK/29/90	  
    HK/34/90	  
    ME/2/90		  
    ME/5/90		  
    SU/1/90		  
    SH/24/90	  
    SP/20/90	  
    SP/22/90	  
    SP/23/90	  
    VI/2/90		  
    AS/1/91		  
    CA/1/91		  
    EN/260/91	  
    EN/261/91	  
    GE/6447/91	  
    HO/57165/91	  
    KM/14/91	  
    LY/1149/91	  
    LY/1182/91	  
    LY/459/91	  
    LY/5441/91	  
    LY/23672/91	  
    LY/24103/91	  
    LY/24222/91	  
    LY/1189/91	  
    LY/1276/91	  
    LY/1337/91	  
    LY/1373/91	  
    LY/1594/91	  
    MA/G12/91	  
    NL/816/91	  
    SP/1/91		  
    SP/2/91		  
    SP/3/91		  
    ST/20/91	  
    ST/6092/91	  
    WA/15/91	  
    AM/4112/92	  
    BE/32/92	  
    ES/1285/92	  
    FI/218/92	  
    FI/220/92	  
    FI/247/92	  
    GE/5113/92	  
    ;; HK/23/92	     removed sept 04 2002 because eur sequence is inconsistent with database sequence
    HO/56798/92	  
    HO/56829/92	  
    HO/56941/92	  
    MA/G58/92	  
    MA/OV31/92	  
    NL/819/92	  
    NL/823/92	  
    NL/935/92	  
    NL/938/92	  
    NI/3126/92	  
    NI/3129/92	  
    PA/320/92	  
    PA/325/92	  
    PA/407/92	  
    PA/417/92	  
    PA/424/92	  
    PA/457/92	  
    PA/467/92	  
    PA/490/92	  
    PA/512/92	  
    PA/548/92	  
    PA/564/92	  
    PA/583/92	  
    PA/597/92	  
    PA/614/92	  
    PE/1/92		  
    RD/100540/92	  
    SE/C273/92	  
    SA/23/92	  
    SA/27/92	  
    SA/8/92		  
    SA/9/92		  
    ST/12/92	  
    ST/13/92	  
    ST/7/92		  
    ST/8/92		  
    TI/5957/92	  
    UM/1982/92	  
    UM/2000/92	  
    VI/68/92	  
    AK/4/93		  
    EN/247/93	  
    ES/5458/93	  
    GD/25/93	  
    LY/1631/93	  
    LY/1803/93	  
    LY/1815/93	  
    LY/22686/93	  
    LY/23602/93	  
    LY/672/93	  
    MA/G101/93	  
    MA/G102/93	  
    MA/G109/93	  
    MA/G116/93	  
    MA/G122/93	  
    MA/G130/93	  
    MA/G252/93	  
    NL/101/93	  
    NL/115/93	  
    NL/12/93	  
    NL/126/93	  
    NL/165/93	  
    NL/17/93	  
    NL/179/93	  
    NL/241/93	  
    NL/276/93	  
    NL/3/93		  
    NL/316/93	  
    NL/330/93	  
    NL/357/93	  
    NL/371/93	  
    NL/372/93	  
    NL/398/93	  
    NL/399/93	  
    NL/440/93	  
    OS/2219/93	  
    OS/2352/93	  
    PA/287/93	  
    SL/142/93	  
    SL/160/93	  
    SD/9/93		  
    SG/6/93		  
    SP/3/93		  
    ST/20/93	  
    VI/104/93	  
    WE/59/93	  
    YA/56/93	  
    YA/61/93	  
    YA/62/93	  
    EN/7/94		  
    GE/8378/94	  
    HK/1/94		  
    HK/2/94		  
    HK/55/94	  
    HK/56/94	  
    JO/33/94	  
    JO/47/94	  
    NL/11/94	  
    NL/18/94	  
    NL/33/94	  
    SA/15/94	  
    SA/25/94	  
    WE/6/94		  
    FI/338/95	  
    FI/339/95	  
    FI/381/95	  
    GE/298971/95	  
    GE/9846/95	  
    GE/A9509/95	  
    HK/3/95		  
    HK/32/95	  
    HK/38/95	  
    HK/49/95	  
    HK/55/95	  
    LY/2279/95	  
    NA/933/95	  
    NL/1/95		  
    NL/178/95	  
    NL/218/95	  
    NL/226/95	  
    NL/271/95	  
    SY/26/95	  
    VI/75/95	  
    WU/359/95	  
    BR/8/96		  
    GE/3958/96	  
    HK/20/96	  
    HK/357/96	  
    HK/358/96	  
    HK/42/96	  
    HK/434/96	  
    LY/1781/96	  
    NL/172/96	  
    NL/91/96	  
    SP/1/96		  
    SP/4/96		  
    AU/10/97	  
    HK/1/97		  
    HK/280/97	  
    HK/387/97	  
    HK/391/97	  
    JO/10/97	  
    NL/300/97	  
    NE/491/97	  
    OS/21/97	  
    OS/244/97	  
    SY/5/97		  
    TA/307/97	  
    HK/4437/98	  
    NL/414/98	  
    NL/427/98	  
    NL/462/98	  
    NL/5/98		  
    MW/10/99	  
    NL/301/99	  
    PR/1/99		  
    PR/3/99		  
    NL/182/00	  
    NL/3/00		  
    NL/118/01	  
    NL/124/01	  
    NL/126/01))
|#


;;;----------------------------------------------------------------------
;;;                     which prototypes are sequenced?
;;;----------------------------------------------------------------------

#|

(length (reverse (intersection *sequenced-strains* *jan-prototype-strains*)))
72
(length *jan-prototype-strains*)
97

(ppl (sort-strains (set-difference *jan-prototype-strains* (reverse (intersection *sequenced-strains* *jan-prototype-strains*)))))

the jan prototype strains that we have hi for, but no sequence:

BI/1843/75 
BI/4273/75 
BI/5168/76 
BI/2501/78 
LY/2380/81 
AL/4382/82 
BI/9768/82 
NL/333/85 
MO/20/87 
AT/3572DASH5/88 
OK/5/88 
VI/1/88 
VI/11/88 
EI/3447/89 
EN/138/89 
WK/1/89 
WE/3/90 
GE/5366/91 
NL/891/91 
VI/33/92 
SP/19/93 
GE/9509/95 
NL/47/95 
ST/506/95 
HK/286/97 

|#


;;;----------------------------------------------------------------------
;;;                    <10 thresholds for RIVM tables
;;;----------------------------------------------------------------------

(defvar *RIVM-table-lt-thresholds*)
(setq *RIVM-table-lt-thresholds*
    '((TAB1 <10  10)  
      (TAB2 <10  10)  
      (TAB3 <10  10)  
      (TAB4 <10  10)  
      (TAB5 <10  10)  
      (TAB6 <10  10)  
      (TAB7 <10  10)  
      (TAB8 <10  10)  
      (TAB9 <10  10)  
      (TAB10 <10  10) 
      (TAB11 <10  10) 
      (TAB12 <10  10) 
      (TAB13 <10  10) 
      (TAB14 <10  10) 
      (TAB15 <10  10) 
      (TAB16 <10  10) 
      (TAB17 <10  10) 
      (TAB18 <10  10) 
      (TAB19 <10-and-<40  40)
      (TAB19-lt40 <10  10)   ;; the 20s for f400 have already been replaced by <40
      (TAB20 <10  10) 
      (TAB21 <10  10) 
      (TAB22 <10  10) 
      (TAB23 <40  40) 
      (TAB24 <10  10) 
      (TAB25 <10  10) 
      (TAB26 <40  40) 
      (TAB27 <40  40) 
      (TAB28 <40  40) 
      (TAB29 <10  10)  ;; there is a <10 table that looks like it is in the middle of a bunch of <40 tables, but it is an earlier table
      (TAB30 <40  40) 
      (TAB31 <40  40) 
      (TAB32 <40  40) 
      (TAB33 <10  10)  ;; 2 5's that we are not sure whether they should be <10 or <40.  small table
      (TAB34 <10  10)  ;; no low titers, so could be a <40 or a <10 table
      (TAB35 <40  40) 
      (TAB36 <40  40) 
      (TAB37 <40  40) 
      (TAB38 <40  40) 
      (TAB39 <40  40) 
      (TAB40 <40  40) 
      (TAB41 <40  40) 
      (TAB42 <40  40) 
      (TAB43 <40  40) 
      (TAB44 <10  10) 
      (TAB45 <10  10) 
      (TAB46 <10  10) 
      (TAB47 <10  10) 
      (TAB48 <10  10) 
      (TAB49 <10  10) 
      (TAB50 <10  10) 
      (TAB51 <10  10) 
      (TAB52 <10  10) 
      (TAB53 <10  10) 
      (TAB54 <10  10) 
      (TAB55 <10  10) 
      (TAB56 <10  10) 
      (TAB57 <10  10) 
      (TAB58 <10  10) 
      (TAB59 <10  10) 
      (TAB60 <10  10) 
      (TAB61 <10  10) 
      (TAB62 <10  10) 
      (TAB63 <10  10) 
      (TAB64 <10  10) 
      (TAB65 <10  10) 
      (TAB66 <10  10) 
      (TAB67 <10  10)))


(defvar *AHT-table-lt-thresholds*)
(setq *AHT-table-lt-thresholds*
  '((AHT-FERRETS-2004-02-29-UNIX-HAND-MOD-FOR-LENGTH-HI-AG-SR <8  8)
    (AHT-FERRETS-2004-03-04-UNIX-HAND-MOD-RAW-AG-SR <8  8)
    (AHT-FERRETS-2004-03-04-combined-AG-SR <8  8)))


;;;----------------------------------------------------------------------
;;;                      vaccine years
;;;----------------------------------------------------------------------

(defvar *season-vaccine-alist*)
(setq *season-vaccine-alist*
  '((68-69 HK/1/68  AC/2/68)
    (69-70 HK/1/68  AC/2/68)
    (70-71 HK/1/68  AC/2/68)
    (71-72 HK/107/71)
    (72-73 EN/42/72)
    (73-74 EN/42/72)
    (74-75 PC/1/73)
    (75-76 PC/1/73  SL/840/74)
    (76-77 VI/3/75)
    (77-78 VI/3/75)
    (78-79 VI/3/75  EN/864/75)
    (79-80 TE/1/77)
    (80-81 BA/1/79)
    (81-82 BA/1/79)
    (82-83 BA/1/79)
    (83-84 PH/2/82)
    (84-85 PH/2/82)
    (85-86 PH/2/82)
    (86-87 MI/1/85  CC/4/85)
    (87-88 LE/360/86)
    (88-89 SI/2/87)
    (89-90 SH/11/87)
    (90-91 GU/54/89)
    (91-92 BE/353/89)
    (92-93 BE/353/89)
    (93-94 BE/32/92)
    (94-95 SD/9/93)
    (95-96 JO/33/94)
    (96-97 NA/933/95  WU/359/95)
    (97-98 NA/933/95  WU/359/95)
    (98-99 SY/5/97)
    (99-00 SY/5/97)
    (00-01 PM/2007/99 MW/10/99)
    (01-02 PM/2007/99 MW/10/99)
    (02-03 PM/2007/99 MW/10/99)
    (03-04 WY/3/3)))

(defun winter-season-vaccines (winter-season)
  (assoc-value winter-season *season-vaccine-alist*))
