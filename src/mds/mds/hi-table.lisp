(in-package user)

 
(defvar *threshold-alist*)
(defvar *threshold-symbols*)

(defun add-threshold (l)
  (push l *threshold-alist*)
  (push (car l) *threshold-symbols*))

(progn
  (setq *threshold-alist*   nil)
  (setq *threshold-symbols* nil)
  (loop for l in '((<5   5 <-1)
		   (<10 10 <0)
		   (<10 10 <0.0)
		   (<20 20 <1)
		   (<40 40 <2)
		   (>10000 10000)
		   (>5000 5000)
		   (<0  0)
		   (<0.0  0)
		   (<0.3 0.3)  
		   ;;(<1  1)
                   (<2  2 <-2.321928)
                   (<3  3 <-1.7369655)
                   (<4  4 <-1.3219281)
                   (<6  6 <-0.7369656)
                   (<8  8 <-0.32192808)
                   (<12  12 <0.2630344)
                   (<16 16 <0.6780719)
                   (<32 32 <1.6780719)
                   ) do
	(add-threshold l)))

#|
replaced below, to also handle >'s
(defun thresholdp (x)
  ;; hardcode for now as we think that converting to string might be compute intensive
  (let ((ans (or (member x *threshold-symbols*)
		 (and (listp x) (eql '< (car x))))))
    (if (and (not ans)
	     (not (true-dont-care-p x))
	     (symbolp x))
	;; maybe this is a thresholded value that we need to work with
	(let ((string (anything->string x)))
	  (if (and (> (length string) 0)
		   (eql #\< (aref string 0)))
	      (let ((number (read-from-string (substring string 1))))
		(if (numberp number)
		    (progn 
		      (add-threshold (list x number (read-from-string (format nil "<~d" (std-log-titer number)))))
		      ;; this next one is for reading back logged hi tables in saves and being able to unlog them
		      (add-threshold (list (read-from-string (format nil "<~d" (un-std-log-titer number))) (un-std-log-titer number) x))
		      t)
		  (error "got a thresholded value, ~a, that was not in the form <X where X is a number" x)))))
      ans)))
||#

(defun gt-threshold-p (x)
  (and (thresholdp x)
       (eql #\> (aref (string x) 0))))
  
  

(defun thresholdp (x)
  ;; hardcode for now as we think that converting to string might be compute intensive
  (let ((ans (or (member x *threshold-symbols*)
		 (and (listp x) (eql '< (car x))))))
    (if (and (not ans)
	     (not (true-dont-care-p x))
	     (symbolp x))
	;; maybe this is a thresholded value that we need to work with
	(let ((string (anything->string x)))
	  (if (and (> (length string) 0)
		   (eql #\< (aref string 0)))
	      (let ((number (read-from-string (substring string 1))))
		(if (numberp number)
		    (progn 
		      (add-threshold (list x number (read-from-string (format nil "<~d" (std-log-titer number)))))
		      ;; this next one is for reading back logged hi tables in saves and being able to unlog them
		      (add-threshold (list (read-from-string (format nil "<~d" (un-std-log-titer number))) (un-std-log-titer number) x))
		      t)
		  (error "got a thresholded value, ~a, that was not in the form <X where X is a number" x)))
	    (if (and (> (length string) 0)
		     (eql #\> (aref string 0)))
		(let ((number (read-from-string (substring string 1))))
		  (if (numberp number)
		      (progn 
			(add-threshold (list x number (read-from-string (format nil ">~d" (std-log-titer number)))))
			;; this next one is for reading back logged hi tables in saves and being able to unlog them
			(add-threshold (list (read-from-string (format nil ">~d" (un-std-log-titer number))) (un-std-log-titer number) x))
			t)
		    (error "got a thresholded value, ~a, that was not in the form >X where X is a number" x))))))
      ans)))
	     

(defun threshold-number (threshold-symbol)
  (assoc-value-1 threshold-symbol *threshold-alist*))

(defun threshold-symbol (threshold-number &optional &key add-if-not-seen-before)
  (if (and add-if-not-seen-before
           (not (position threshold-number (nths 1 *threshold-alist*))))
      (add-threshold (list (read-from-string (format nil "<~d" threshold-number))
                           threshold-number
                           (read-from-string (format nil "<~d" (std-log-titer threshold-number))))))
  (nth 0 (nth (position threshold-number (nths 1 *threshold-alist*)) *threshold-alist*)))

(defun threshold-std-log-titer (threshold)
  (nth 2 (assoc threshold *threshold-alist*)))
(defun threshold-un-std-log-titer (threshold)
  (nth 0 (nth (position threshold *threshold-alist* :test (^ (item list) (eql item (nth 2 list)))) *threshold-alist*)))

(defun min-threshold (thresholds)
  (threshold-symbol (apply-min (mapcar #'threshold-number thresholds))))

(defun max-threshold (thresholds)
  (threshold-symbol (apply-max (mapcar #'threshold-number thresholds))))
    


(defun true-dont-care-p (x) (eql x 'dont-care))

(defun dont-care-p (x) 
  (or (eql x 'dont-care)
      (thresholdp x)))

#|
this really needs to be a <10 not a number less than 10, because when we are
not an HI table, there are numbers less than 10, and things get messy
(defun less-than-ten-p (x)
  (if (and (numberp x) (< x *less-than-threshold*))
      (format t "seeing a number (~d) less than 10, this is OK, i just wanted to know when it happened~%" x))
  (or (eql x '<10)
      (and (numberp x) (< x *less-than-threshold*))))  ;; this allows us to read in an HI table with 5's in it
                                    ;; it is also best for the future, where we make the threshold not a fixed
                                    ;; threshold, but a variable one.  a less-than-threshold, rather than less-than-10
                                    ;; (remember to change dtarget5 to dtarget-threshold in the conj gradient)
|#

(defun less-than-ten-p (x)
  (eql x '<10))


(defun prediction-p (x)
  (and (listp x) (eql 'prediction (car x))))

(defun *-w-dc (&rest args)
  (if (collect #'dont-care-p args)
      'dont-care
    (apply #'* args)))

(defun minus-w-dc (&rest args)
  (if (collect #'dont-care-p args)
      'dont-care
    (apply #'- args)))

(defun <=-dc (x y) 
  (if (or (dont-care-p x) (dont-care-p y))
      'dont-care
    (<= x y)))

(defun <-dc (x y) 
  (if (or (dont-care-p x) (dont-care-p y))
      'dont-care
    (< x y)))

(defun dps-w-dc (x dps)
  (if (dont-care-p x)
      'dont-care
    (dps x dps)))

(defun 2dp-w-dc (x)
  (if (dont-care-p x)
      'dont-care
    (2dp x)))

(defun max-dc (&rest l)
  (let ((l-without-dc-s (collect #'numberp l)))
    (apply #'max l-without-dc-s)))  ;; could later be smart for an empty list


;;;----------------------------------------------------------------------;;;---------------------------------------------------------------------------------------------
;;;                               SELECTED HI TABLES
;;;---------------------------------------------------------------------------------------------

#|
NOTE, THESE TABLES NEED TO BE MADE WITH MAKE-HI-TABLES NOW (aug 00)
(setq hi-1/21/77 '(
(a/hongkong/8/68      640  640 2560  320  320  160  320  320   80   80   80   80   80   40  320  640 1280   80)
(a/hongkong/107/71     40  640   40 1280   80  160   80   80   20   40   40   40   40   10   40   40   40   40)
(a/england/42/72      160  160 2560  160 1280  640  320  320  320  160   80   80   80   40  320  640 1280  160)
(a/hongkong/5/72      160 2560  320 2560  640  320  320  320  160  160   80   80   80   40  160  160  160  160)
(a/hannover/61/73      40  160  320  160 1280  640  640  320  640  160   80   80   80   20  160  160   80  160)
(a/portchalmers/1/73   80  160  640  160 1280 2560  320  640  160  160   80   80   80   40  320  160  160  160)
(a/england/635/74      40   80   80  160  640  160  640  160  160  160   40   40   40   20   80   80   80   80)
(a/pr/1/74             80  160  160  160  320 1280  160 1280   80   80  160   80   80   40  160  160   80  160)
(a/scotland/840/74     40   80  320  160 2560  640  640  160 1280  160   80   80   40   20   80   80   80   80)
(a/mayoclinic/4/75     80  160  160  160  320   80  160  160   40 1280   80   80   80   20  160   80  160  160)
(a/hongkong/9/75       80   80   80  160  320  160   80  160   40   80 2560  640   80   40  640 1280  640  640)
(a/victoria/3/75       80   80  160   80  320  320  160  160   80  320 5120 2560  160   80 1280 2560 1280  640)
(a/england/864/75     160  320  320  320  640  320  320  320   80  320  320  320 2560   80  640  640  320 1280)
(a/tokyo/1/75          80  160   80   80  320  160  160  160   40  160 1280  320   80 1280  640 1280  160  640)
(a/brazil/25/76        20   40   40   40   80   40   40   40   10   40 1280  320   40   20  320  640  320  320)
(a/brazil/31/76        40   40   80   40   80   80   40   40   20   40 1280  640   80   40  640 1280  640  320)
(a/AllegCo/29/76       40  160  160   80  320  160  160   80   40   80  640  160   80   40  640 1280 2560  320)
(a/victoria/112/76     40   80   80   80  160   80   80   80   40   80  320   80  320   20  320  640  320 1280)
))
(setq hi77 hi-1/21/77)

(setq hi-3/13/85 '(
(a/oregon/4/80          2560  640  320 1280 1280  320   80  320  640  640  160)
(a/bangkok/1/79         2560 1280  160 2560  320  640  160  320  320  320   40)
(a/bangkok/2/79         2560  320 2560  320  320   80   80  320  320  320   80)
(a/shanghai/31/80        640  640   80 2560  160  640  160  160  160  160   40)
(a/philippines/2/82_eq   320   80   80  160  640   80   80  160  160  160   40)
(a/taiwan/16/83         1280  320   80 1280  160 1280  160  320  160  160   40)
(a/panama/1/83           640  160  160  160  320  160 1280 2560  320  640  320)
(a/caen/1/84             320  160  160  160  320  160  640 2560  320  640  160)
(a/mississippi/1/85      640  320  160  320  640  320  320 1280 1280 1280  320)
(a/washington/1/85       320  160   80  160  320  160  160  640  320  640  160)
(a/newjersey/1/85         80   40   40   40   40   40   80  320   80  160  320)
))
(setq hi85 hi-3/13/85)

(setq hi-2/27/90 '(
(a/leningrad/360/86     2560  160   40  160  160   80  160  160  640   80   20   40   20   80)
(a/victoria/07/87        480 1280   80  320  160   80  160  160  640   80  160  160  160   80)
(a/sichuan/02/87          80   40  640  320  120  640  160  320  320  320   80   20   80  120)
(a/shanghai/11/87        320   80  120  640  160  160  160  320  640  320  160   40  160   80)
(a/england/427/88        240  120  160  320  160  320  640  640 1280  480   80   40   80  320)
(a/czechoslovakia/19/89  160   40  960  320  180 1280  320  480 2560  640  640   40 1280  640)
(a/victoria/5/89        1280  480  480 1280  640  640 2560 1280 5120  960  320  160  320  640)
(a/sichuan/68/89         640  240  320  640  640  640 1280 1920 5120 1280  160   80  160 1280)
(a/guizhou/54/89          20   20   80   80   40   80  160  320 1280  320   40   20   20  320)
(a/shanghai/16/89         30   20   80  120   80  160  160  320  640  320   40   40   20  320)
(a/beijing/352/89        120  160   40  160  160   40   80   80  240   80  160   80  320   80)
(a/beijing/337/89        160  320   80  160  120   80   80   80  320   40   80  320   80   80)
(a/beijing/353/89        160   80   80  160  160  160   80   80  160   80 1280   80 1280   80)
(a/guangdong/39/89        40   20   80  160   80  160  160  320 1920  320   60   40   40  320)
))
(setq hi90 hi-2/27/90)

(setq hi-86-92-murphey-webster 
  '((a/HongKong/8/68   2560 1280   40    5    5    5   10   10    5    5)
    (A/England/42/72    320 1280   80   20    5    5    5    5    5    5)
    (A/Victoria/3/75      5   80  320   40   10   20   10    5    5    5)
    (A/Texas/1/77         5   80  160 1280  160  320  320   10   10    5)
    (A/Bangkok/1/79       5    5   80  640  640  640  640   20   10    5)
    (A/Philippines/2/82   5    5   10   40   40  320  160   10   10    5)
    (A/Mississippi/1/85   5    5   40  160   80  640  640   40   20   20)
    (A/Shanghai/11/87     5    5    5    5    5    5   80  320  160   20)
    (A/Beijing/353/89     5    5    5    5    5    5    5  160  320   40)
    (A/Beijing/32/92      5    5    5    5    5    5   10   20   80  640)))
(setq hi92 hi-86-92-murphey-webster)
|#		 


;;;---------------------------------------------------------------------------------------------
;;;                               HI TO LOG DIFFERENCES
;;;---------------------------------------------------------------------------------------------

(defun titer-diff-to-log (t1 t2)
  (- (log t1 2) (log t2 2)))

(defun log-titer (titer)
  (log titer 2))

(defun std-log-titer (titer)
  (if (listp titer)   ;; for multiple titers when we have a listed-merge of bootstrapped titers
      (mapcar #'std-log-titer titer)
    (if (true-dont-care-p titer)
	titer
      (if (thresholdp titer)
	  (threshold-std-log-titer titer)
	(log (/ titer 10) 2)))))

(defun log-to-std-titer (log)
  (if (listp log)     ;; for multiple titers when we have a listed-merge of bootstrapped titers
      (mapcar #'log-to-std-titer log)
    (if (true-dont-care-p log)
	log
      (if (thresholdp log)
	  (threshold-un-std-log-titer log)
	(round (* 10 (expt 2 log)))))))

(defun un-std-log-titer (log)
  (log-to-std-titer log))

(defun round-log-titer (titer)
  (round-to-nearest 0.5 (log titer 2)))

(defun strain-abbreviation (strain)
  (setq strain (format nil "~a" strain))
  (if (<= (length strain) 4)
      (format nil "~4@a" strain)
    (let ((string (make-string 4 :initial-element #\a)))
      (setf (aref string 0) (aref strain 2))
      (setf (aref string 1) (aref strain 3))
      (setf (aref string 2) (aref strain (- (length strain) 2)))
      (setf (aref string 3) (aref strain (- (length strain) 1)))
      string)))

(defun smart-strain-abbreviation (strain)
  (setq strain (format nil "~a" strain))
  (if (<= (length strain) 4)
      (format nil "~4@a" strain)
    (progn
      (if (equal "VAC" (reverse (substring-before-char #\- (reverse strain))))
	  (setq strain (reverse (substring-after-char #\- (reverse strain)))))
      (if (or (equal "A/" (substring strain 0 1))
	      (equal "B/" (substring strain 0 1)))
	  (setq strain (substring strain 2)))
      (let ((string (make-string 4 :initial-element #\a)))
	(setf (aref string 0) (aref strain 0))
	(setf (aref string 1) (aref strain 1))
	(setf (aref string 2) (aref strain (- (length strain) 2)))
	(setf (aref string 3) (aref strain (- (length strain) 1)))
	string))))

#|
;;replaced with the below, which uses ron's abbreviation
(defun smart-long-strain-abbreviation (strain)
  (setq strain (format nil "~a" strain))
  (if (<= (length strain) 4)
      (format nil "~4@a" strain)
    (if (not (substring-after-char #\/ strain))
	strain
      (if (not (substring-after-char #\/ (substring-after-char #\/ strain)))
	  strain
	(progn
	  (if (equal "VAC" (reverse (substring-before-char #\- (reverse strain))))
	      (setq strain (string-append (reverse (substring-after-char #\- (reverse strain))) "-V")))
	  (if (or (equal "A/" (substring strain 0 1))
		  (equal "B/" (substring strain 0 1)))
	      (setq strain (substring strain 2)))
	  (let* ((name (if (substring-before-char #\/ strain)
			   (substring-before-char #\/ strain)
			 strain))
		 (isolate (if (substring-before-char #\/ (substring-after-char #\/ strain))
			      (substring-before-char #\/ (substring-after-char #\/ strain))
			    ""))
		 (year (if (substring-after-char #\/ (substring-after-char #\/ strain))
			   (substring-after-char #\/ (substring-after-char #\/ strain))
			 ""))
		 (suffix (if (substring-after-char #\- strain)
			     (substring-after-char #\- strain)
			   "")))
	    suffix  ;;ignore suffix for now
	    (values 
	     (string-append (glue-up-to-string "/" (list (if (>= (length name) 2)
							     (substring name 0 1)
							   name)
							 isolate year)))
	     name
	     isolate
	     (if (string-equal "V-" (substring (reverse year) 0 1))
		 (substring year 0 (- (length year) 3))
	       year)
	     suffix)))))))
|#

;;these are from ~/mds/data/all-seq/abbrev.txt
(defvar name-abbrevs-short-to-long)
(defvar name-abbrevs-long-to-short)
(defvar name-string-abbrevs-short-to-long)
(defvar name-string-abbrevs-long-to-short)

(progn
  (setq name-abbrevs-short-to-long 
  '((AA	ANN_ARBOR)
    (AB	ALABAMA)
    (AC	Aichi)
    (AF	SOUTH_AFRICA)
    (AG	ARGENTINA)
    (AI	ASTURIAS)
    (AK	AKITA)
    (AL	AUSTRALIA)
    (AM AMSTERDAM)
    (AR	ARIZONA)
    (AS	ALICE_SPRINGS)
    (AT	ATLANTA)
    (AU	AUCKLAND)
    (AX	ALASKA)
    (BA	BANGKOK)
    (BB	BILBAO)
    (BE	BEIJING)
    (BG	BELGIUM)
    (BI	BILTHOVEN)
    (BR	BRISBANE)
    (BZ	BRAZIL)
    (CA	CANBERRA)
    (CB	CHIBA)
    (CC	CHRISTCHURCH)
    (CD	COLLINDALE)
    (CE	CAEN)
    (CF	CALIFORNIA)
    (CH	CHINA_CNIC)   ;;note china twice, so both go to CH on the reverse lookup
    (CH	CHINA)
    (CL	CHILE)
    (CM	CHENG_MEI)
    (CN	CONNECTICUT)
    (CO	COLORADO)
    (CW	CHANGWON)
    (CX	CANADA_SY_LIKE)
    (CZ	CZECHOSLOVAKIA)
    (DE	DELFT)
    (DU	DUNEDIN)
    (DW	DARWIN)
    (EI	EINDHOVEN)
    (EN	ENGLAND)
    (EQ	EQUADOR)
    (ES	ENSCHEDE)
    (FI	FINLAND)
    ;;(FJ	FUJIAN)   replaced below  derek june 3 2004
    (FL	FLORIDA)
    (FO	FLORENCE)
    (FR	FRANCE)
    (FS	FUKUSHIMA)
    ;; (FU FUKUOKA)    ;; changed now to FK (as we use FU for fujian), and no FUKUOKA found, so not renamed to anything
    (FU FUJIAN)
    (GD	GUANGDONG)
    (GE	GENEVA)
    (GF	Guidlford)
    (GG	GEORGIA)
    (GI	GIFU)
    (GL	GUALDALAGARA)
    (GM	GUMMA)
    (GN	GRANADA)
    (GO	GOTENBORG)
    (GR	GRONINGEN)
    (GU	GUIZHOU)
    (GX	GUANGXI)
    (GY	GERMANY)
    (GZ	GUANGZHOU)
    (HA	HAWAII)
    (HB	HARBIN)
    (HD	HOKKAIDO)
    (HE	HEBEI)
    (HK	HONGKONG)
    (HO	HOUSTON)
    (HU	HUNTINGTON)
    (IB	IBARAKI)
    (ID	INDONESIA)
    (IH	IDAHO)
    (II	INDIA)
    (IL	ILLINOIS)
    (IN	INDIANA)
    (IS	ISRAEL)
    (JO	JOHANNESBURG)
    (JP	JAPAN)
    (KA	KASAULI)
    (KG	KAGOSHIMA)
    (KO	KOBE)
    (KR	KOREA)
    (KW	KWANGJIN)
    (KY	KITAKYUSHU)
    (LA	LAUSANNE)
    (LE	LENINGRAD)
    (LI	LINNKOPING)
    (LO	LOSANGELES)
    (LU	LOUISIANA)
    (LY	LYON)
    (MA	MADRID)
    (MC	MICHIGAN)
    (ME	MEMPHIS)
    (MG	MIYAGI)
    (MI	MISSISSIPPI)
    (MM	MALMO)
    (MO	MISSOURI)
    (MS	MINNESOTA)
    (MX	MEXICO)
    (MY	MAYOCLINIC)
    (MW MOSCOW)     ;; derek added 2004-06-03
    (MZ	MASSACHUSETS)
    (NA	NANCHANG)
    (NB	NEBRASKA)
    (NC	NEWCASTLE)
    (NE	NICE)
    (NG	NIIGATA)
    (NI	NIJMEGEN)
    (NJ	NEW_JERSEY)
    (NL	NETHERLANDS)
    (NO	NORTH_CAROLINA)
    (NS	NAGASAKI)
    (NV	NEVADA)
    (NW	NEW_CALLEDONIA)
    (NX	NINGXIA)
    (NY	NEW_YORK)
    (OA	OSAKA)
    (OH	OHIO)
    (OI	OITA)
    (OK	OKLAHOMA)
    (OS	OSLO)
    (OV	OVIEDO)
    (PA	PARIS)
    (PC	PORT_CHALMERS)
    (PE	PERTH)
    (PH	PHILIPPINES)
    (PM	PANAMA)
    (PO	PUERTO_RICO)
    (PR	PRAGUE)
    (PS	PENNSYLVANIA)
    (QT	QINGDAO)
    (QU	QUEENSLAND)
    (RD	ROTTERDAM)
    (RJ	RIO_DE_JANEIRO)
    (RM	ROMANIA)
    (RU	RU)
    (SA	SOUTH_AUSTRALIA)
    (SB	SAGA)
    (SC	SOUTH_CAROLINA)
    (SD	SHANGDONG)
    (SE	SENDAI)
    (SG	SHIGA)
    (SH	SHANGHAI)
    (SI	SICHUAN)
    (SJ	SPAIN)
    (SL	SCOTLAND)
    (SN	ST_ETIENNE)
    (SO	SOUTH_DAKOTA)
    (SP	SINGAPORE)
    (SR	SAPPORO)
    (ST	STOCKHOLM)
    (SU	SEOUL)
    (SW	SHENZHEN)
    (SX	SOPHIA)
    (SY	SYDNEY)
    (SZ	SANTIAGO)
    (TA	TAIWAN)
    (TC	TOCHIGI)
    (TE	TEXAS)
    (TG	TONGA)
    (TH	THESSALONIKI)
    (TI	TILBURG)
    (TJ	TIANJIN)
    (TL	THAILAND)
    (TM	TASMANIA)
    (TO	TOULOUSE)
    (TR	TEHRAN)
    (TT	TOTTORI)
    (TV	TOWNSVILLE)
    (TY	TOKYO)
    (UD	UDORN)
    (UK	UK)
    (UM	UMEA)
    (UR	URUGUAY)
    (US	USSR/RUSSIA)
    (UT	UTRECHT)
    (UU	ULAN_UDE)
    (VA	VALENCIA)
    (VG	VIRGINIA)
    (VI	VICTORIA)
    (VN	VIENNA)
    (VT	VERMONT)
    (WA	WASHINGTON)
    (WE	WELLINGTON)
    (WH	WUZHOU)
    (WK	WAIKATO)
    (WN	WISCONSIN)
    (WU	WUHAN)
    (WV	WESTVIRGINIA)
    (WY	WYOMING)
    (YA	YAMAGA)
    (YI	YAMANESHI)
    (YO	YOKOHAMA)
    (YT	YAMAGATA)
    (ZA	ZAMBIA)
    (ZG	ZARAGOSSA)
    (ZL	ZLIN)
    ))
  (setq name-abbrevs-long-to-short (mapcar #'reverse name-abbrevs-short-to-long))
  (setq name-string-abbrevs-long-to-short (mapcar (^ (x) (mapcar #'string x)) name-abbrevs-long-to-short))
  (setq name-string-abbrevs-short-to-long (mapcar (^ (x) (mapcar #'string x)) name-abbrevs-short-to-long))
)

(defun smart-long-strain-abbreviation-one-slash (strain)
  (let* ((name (if (substring-before-char #\/ strain)
		   (substring-before-char #\/ strain)
		 strain))
	 (isolate "")  ;; we only call this function when there is only one slash in the name, lets assume it is name/year
	 (year (if (substring-after-char #\/ strain)
		   (substring-after-char #\/ strain)
		 ""))
	 (suffix (if (substring-after-char #\- strain)
		     (substring-after-char #\- strain)
		   "")))
    (if (substring-before-char #\- year)
	(setq year (substring-before-char #\- year)))
    (let ((name-abbrev (if (= (length name) 2)
			   name   ;;it is already an abbreviation
			 (if (assoc-value-1 name name-string-abbrevs-long-to-short :test #'equal)
			     (assoc-value-1 name name-string-abbrevs-long-to-short :test #'equal)
			   (progn
			     ;;(format t "No standard abbreviation for name  ~s~%" name)
			     (if (>= (length name) 2)
				 name ;; (substring name 0 1)  ;; removed by derek 2007-11-21, so non-flu names with /'s do not get abbreved
			       name))))))
      (values 
       (if (equal "" suffix)
	   (glue-up-to-string "/" (list name-abbrev isolate year))
	 (string-append 
	  (glue-up-to-string "/" (list name-abbrev isolate year))
	  "-" suffix))
       name
       isolate
       (if (and (>= (length year) 2) (string-equal "V-" (substring (reverse year) 0 1)))
	   (substring year 0 (- (length year) 3))
	 year)
       suffix
       name-abbrev))))
  

(defun smart-long-strain-abbreviation (strain)
  (setq strain (format nil "~a" strain))
  (if (<= (length strain) 4)
      (format nil "~4@a" strain)
    (if (not (substring-after-char #\/ strain))
	strain
      (if (not (substring-after-char #\/ (substring-after-char #\/ strain)))
	  (smart-long-strain-abbreviation-one-slash strain)
	(progn
	  (if (equal "VAC" (reverse (substring-before-char #\- (reverse strain))))
	      ;;we were moving -vac to -v, but the hi tables have no -v so to got match
	      ;; we do not put the -v on here (the sequences have -vac on them, and that
	      ;; is when they will be removed)
	      ;;(setq strain (string-append (reverse (substring-after-char #\- (reverse strain))) "-V"))
	      (setq strain (string-append (reverse (substring-after-char #\- (reverse strain))) ""))
	    )
	  (if (or (equal "A/" (substring strain 0 1))
		  (equal "B/" (substring strain 0 1)))
	      (setq strain (substring strain 2)))
	  (let* ((name (if (substring-before-char #\/ strain)
			   (substring-before-char #\/ strain)
			 strain))
		 (isolate (if (substring-before-char #\/ (substring-after-char #\/ strain))
			      (substring-before-char #\/ (substring-after-char #\/ strain))
			    ""))
		 (year (if (substring-after-char #\/ (substring-after-char #\/ strain))
			   (substring-after-char #\/ (substring-after-char #\/ strain))
			 ""))
		 (suffix (if (substring-after-char #\- strain)
			     (substring-after-char #\- strain)
			   "")))
	    (if (substring-before-char #\- year)
		(setq year (substring-before-char #\- year)))
	    (let ((name-abbrev (if (= (length name) 2)
				   name   ;;it is already an abbreviation
				 (if (assoc-value-1 name name-string-abbrevs-long-to-short :test #'equal)
				     (assoc-value-1 name name-string-abbrevs-long-to-short :test #'equal)
				   (progn
				     ;;(format t "No standard abbreviation for name  ~s~%" name)
				     (if (>= (length name) 2)
					 name ;; (substring name 0 1)  ;; removed by derek 2007-11-21, so non-flu names with /'s do not get abbreved
				       name))))))
	      (values 
	       ;;(if (equal "" suffix)
		   (glue-up-to-string "/" (list name-abbrev isolate year))
		 ;;(string-append 
		  ;;(glue-up-to-string "/" (list name-abbrev isolate year))  ;; leave this out, because messes up with dashes in names
		  ;;"-" suffix))                                             ;; when we do cdc tables (2004-08-19), and is for old stuff as i recall
	       name
	       isolate
	       (if (and (>= (length year) 2) (string-equal "V-" (substring (reverse year) 0 1)))
		   (substring year 0 (- (length year) 3))
		 year)
	       suffix
	       name-abbrev))))))))


(defun strain-location (strain)
  (let ((strain-location (nth-value 1 (smart-long-strain-abbreviation strain))))
    (cond ((null strain-location) nil)
	  ((equal "" strain-location) nil)
	  (t (read-from-string strain-location)))))

(defun strain-isolate (strain)
  (let ((strain-isolate (nth-value 2 (smart-long-strain-abbreviation strain))))
    (cond ((null strain-isolate) nil)
	  ((equal "" strain-isolate) nil)
	  (t (read-from-string strain-isolate)))))

(defun strain-year (strain)
  (let ((strain-year (nth-value 3 (smart-long-strain-abbreviation strain))))
    (cond ((null strain-year) nil)
	  ((equal "" strain-year) nil)
	  (t (read-from-string strain-year)))))


(defun cons-strain-name (location isolate year)
  (glue-up-to-string "/" (list location isolate year)))

(defun sort-strains-by-year (strains)
  (my-sort strains
           (^ (x y) 
	      (let ((x-year (strain-year x))
		    (y-year (strain-year y)))

		;; convert yyfoo into yy if yy is a number (helps with the y2k processing)
		(if (and (not (numberp x-year))
			 (numberp (string->number (substring (string x-year) 0 1))))
		    (setq x-year (string->number (substring (string x-year) 0 1))))
		(if (and (not (numberp y-year))
			 (numberp (string->number (substring (string y-year) 0 1))))
		    (setq y-year (string->number (substring (string y-year) 0 1))))

		(if (and (numberp x-year) (numberp y-year))
		    (let ((y2k-offset 10))
		      (< (if (<= x-year y2k-offset) (+ x-year 100) x-year)
			 (if (<= y-year y2k-offset) (+ y-year 100) y-year)))
		  (string< (anything->string x-year) (anything->string y-year)))))))

(defun sort-strains-by-isolate (strains)
  (my-sort strains
           (^ (x y) 
	      (let ((x-isolate (strain-isolate x))
		    (y-isolate (strain-isolate y)))
		(if (and (numberp x-isolate) (numberp y-isolate))
		    (< x-isolate y-isolate)
		  (string< (anything->string x-isolate) (anything->string y-isolate)))))))

(defun sort-strains-by-location (strains)
  (my-sort strains
           (^ (x y) 
              (string< (string (strain-location x)) (string (strain-location y))))))


(defun strain-name-< (x y)
  (equal x (nth 0 (sort-strains (list x y)))))

(defun sort-strains (strains)
  (let* ((international-nomenclature-strains
	  (collect (^ (strain)
		      (and (strain-isolate  strain) 
			   (strain-location strain)
			   (strain-year     strain)))
		   strains))
	 (sorted-international-nomenclature-strains
	  (sort-strains-by-year (sort-strains-by-location (sort-strains-by-isolate international-nomenclature-strains))))
	 (non-international-nomenclature-strains
	  (filter  (^ (strain)
		      (and (strain-isolate  strain) 
			   (strain-location strain)
			   (strain-year     strain)))
		   strains)))
    (append
     (if (or (ag-name-p (nth 0 sorted-international-nomenclature-strains))
	     (sr-name-p (nth 0 sorted-international-nomenclature-strains)))
	 (append
	  (collect #'ag-name-p sorted-international-nomenclature-strains)
	  (collect #'sr-name-p sorted-international-nomenclature-strains))
       sorted-international-nomenclature-strains)
     (sort-alpha non-international-nomenclature-strains))))


(defun hi-table-sort-strains (hi-table)
  (extract-hi-table
   hi-table
   (sort-strains (hi-table-antigens hi-table))
   (sort-strains (hi-table-sera hi-table))))
;;(hi-table-sort-strains (hi-table-ferret-to-strain (read-hi-table "mds/data/all-HI/TAB23.txt")))


(defun strain-sort-f (&rest strains)
  (equal strains (sort-strains strains)))

(defun long-strain-abbreviation (strain)
  (setq strain (string strain))
  ;;length not fixed
  ;;first 2 chars of strain, number, year, optional -v if ends in -vac (indicating vaccine)

  (if (>= (length (substring-after-char #\/ strain)) 2)
      (let* ((name (substring (substring-after-char #\/ strain) 0 1))
	     (isolate (substring-before-char #\/ (substring-after-char #\/ (substring-after-char #\/ strain))))
	     (year (substring-after-char #\/ (substring-after-char #\/ (substring-after-char #\/ strain))))
	     (suffix (substring-after-char #\- strain)))
	(if suffix ;;has a -vac or _ag or _sr or -vac_ag or -vac_ag
	    (progn
	      (if (equal "VAC" suffix) 
		  "v"
		(if (substring-after-char #\_ suffix)
		    (string-append "_" (substring-after-char #\_ suffix))
		  suffix))
	      ;;(setq suffix (substring suffix 0 0))
	      (setq year (substring-before-char #\- year))))
	(string-append (glue-up-to-string "/" (list name isolate year)) suffix))
    strain))

(defun abbreviate-strains (strains)
  (mapcar #'strain-abbreviation strains))

(defun smart-long-strain-abbreviation-strains (strains)
  (mapcar #'smart-long-strain-abbreviation strains))

(defun hi-table-abbreviate-strains (hi-table)
  (make-hi-table
   (smart-long-strain-abbreviation-strains (hi-table-antigens hi-table))
   (smart-long-strain-abbreviation-strains (hi-table-sera     hi-table))
   (hi-table-values hi-table)
   (hi-table-name   hi-table)))

(defun hi-table-antigens (hi-table)
  (nths 0 hi-table))

(defun asl-hi-table-antigens-from-unasl-hi-table (hi-table)
  (append (mapcar #'suffix-as-ag (hi-table-antigens hi-table))
          (mapcar #'suffix-as-sr (hi-table-antisera hi-table))))

(defun antigen-indices-in-hi-table (hi-table antigens)
  (let ((hi-table-antigens (hi-table-antigens hi-table)))
    (loop for antigen in antigens collect
	  (position antigen hi-table-antigens))))

;;(defun hi-table-sera (hi-table)
;;  (firstn (hi-table-width hi-table) (hi-table-antigens hi-table)))
(defun hi-table-sera (hi-table &key (error-if-not-set t))
  (let ((sera (gethash hi-table *hi-table-sera-hash*)))
    (if (null hi-table)
	nil
      (if (not sera)
	  (if error-if-not-set
	      (error "no sera set")
	    'no-sera-set)
	(if (eql sera 'no-sera-deliberately)
	    nil
	  sera)))))

(defun hi-table-antisera (hi-table) 
  (hi-table-sera hi-table))

(defun hi-table-row (hi-table row)
  (nth row hi-table))

(defun hi-table-antigen (hi-table antigen)
  (hi-table-row hi-table (position antigen (hi-table-antigens hi-table))))

(defun hi-table-row-values (hi-table)
  (mapcar #'cdr hi-table))

(defun hi-table-column (hi-table column)
  (cons (nth column (hi-table-sera hi-table))
	(nths (inc column) hi-table)))

(defun hi-table-column-values-by-serum (hi-table serum)
  (cdr (hi-table-column hi-table serum)))

(defun hi-table-diagonal-values (hi-table)
  (loop for i below (length (hi-table-antigens hi-table))
        for row in (hi-table-row-values hi-table) collect
        (nth i row)))

(defun hi-table-column-values (hi-table)
  ;;no!, only if the matrix is symetric
  (apply-transpose (hi-table-row-values hi-table)))

(defun hi-table-values (hi-table)
  (mapcar #'cdr hi-table))

(defun hi-table-num-values (hi-table)
  (let ((values (apply-append (hi-table-values hi-table))))
    (length (collect (^ (x) (or (numberp x) (thresholdp x))) values))))

(defun hi-table-antigen-values (hi-table ag)
  (nth (position ag (hi-table-antigens hi-table) :test #'equal) (hi-table-values hi-table)))

(defun hi-table-sera-values (hi-table sr)
  (nths (position sr (hi-table-sera hi-table) :test #'equal) (hi-table-values hi-table)))

;; correctly named version of the above table
(defun hi-table-serum-values (hi-table sr)
  (hi-table-sera-values hi-table sr))

(defun hi-table-antigen-values-by-index (hi-table ag-index)
  (nth ag-index (hi-table-values hi-table)))

(defun hi-table-sera-values-by-index (hi-table sr-index)
  (hi-table-serum-values-by-index hi-table sr-index))

;; correctly named version of the above table
(defun hi-table-serum-values-by-index (hi-table sr-index)
  (nths sr-index (hi-table-values hi-table)))

(defun hi-table-value (hi-table antigen serum &key hi-table-sera-efficiency-hack)
  ;; the efficiency hack because hi-table-sera can take 20mS on table size 200, which we can fix
  ;; when better implementation of hi-table-sera, for now allow this call, when hi-table-sera is
  ;; of more normal speed, just remove the call
  (nth (position serum (if hi-table-sera-efficiency-hack
			   hi-table-sera-efficiency-hack
			 (hi-table-sera hi-table)) :test #'equal)
       (nth (position antigen (hi-table-antigens hi-table) :test #'equal)
	    (hi-table-values hi-table))))

(defun hi-table-value-by-indices (hi-table ag-index sr-index)
  (nth sr-index (nth ag-index (hi-table-values hi-table))))

(defun hi-table-value-by-indices-from-upper-triangle (hi-table ag-index sr-index)
  (hi-table-value-by-indices hi-table (min ag-index sr-index) (max ag-index sr-index)))

(defun hi-table-value-from-upper-triangle (hi-table antigen serum &optional &key hi-table-sera-efficiency-hack)
  (hi-table-value-by-indices-from-upper-triangle 
   hi-table
   (position antigen (hi-table-antigens hi-table))
   (position serum (if hi-table-sera-efficiency-hack
		       hi-table-sera-efficiency-hack
		     (hi-table-sera hi-table)) :test #'equal)))

(defun hi-table-values-value-by-indices (hi-table-values ag-index sr-index)
  (nth sr-index (nth ag-index hi-table-values)))

(defun hi-table-antigen-by-index (hi-table antigen-index)
  (nth antigen-index (hi-table-antigens hi-table)))

(defun hi-table-antigens-by-indices (hi-table antigen-indices)
  (loop for antigen-index in antigen-indices collect
	(hi-table-antigen-by-index hi-table antigen-index)))

(defun hi-table-serum-by-index (hi-table serum-index)
  (nth serum-index (hi-table-sera hi-table)))

(defun hi-table-sera-by-indices (hi-table sera-indices)
  (loop for serum-index in sera-indices collect
	(hi-table-serum-by-index hi-table serum-index)))

(defun hi-table-length (hi-table)
  (length (hi-table-antigens hi-table)))

(defun hi-table-width (hi-table)
  (let ((hi-table-values (hi-table-values hi-table)))
    (if (null hi-table-values)
	;; empty table
	0
      (apply-max (mapcar #'length hi-table-values)))))

(defun hi-table-num-antigens (hi-table)
  (length (hi-table-antigens hi-table)))

(defun hi-table-num-sera (hi-table)
  (apply-max (mapcar #'length (hi-table-values hi-table))))

;;(defun make-hi-table (antigens antisera values)
;;  antisera
;;  (loop for antigen in antigens
;;        for row in values
;;      collect (cons antigen row)))
(defvar *hi-table-sera-hash*)
(defvar *hi-table-name-hash*)
(setq *hi-table-sera-hash* (make-hash-table :test #'equal))
(setq *hi-table-name-hash* (make-hash-table :test #'equal))   ;;note, not all make-hi-table calls have added name stuff
(defun make-hi-table (antigens sera values &optional (name 'name-not-set))
  ;; at some later date, it would be nice to have sera stored more efficiently
  ;; and we would also like to store the table name, and maybe some other stuff
  ;; maybe make an hi-table an object
  ;; a problem with this hash table way is that two tables with the same set of values
  ;; will have the same sera and the same name (the sera and name of the last one to be defined)
  (if (> (length (anything->string name)) 100)
      (progn
	(format t "~%shortening name (can create problem with procrustes constructed filenames) from ~a to NAME-SHORTENED-AUTOMATICALLY~%"
		name)
	(setq name 'NAME-SHORTENED-AUTOMATICALLY)))
  (let ((hi-table (loop for antigen in antigens
		      for row in (*-to-dont-care values)
		      collect (cons antigen row))))
    (setf (gethash hi-table *hi-table-sera-hash*) (if (null sera) 'no-sera-deliberately sera))
    (setf (gethash hi-table *hi-table-name-hash*) name)
    hi-table))

(defun make-hi-table-form (hi-table)
  `(make-hi-table
    ',(hi-table-antigens hi-table)
    ',(hi-table-sera hi-table)
    ',(hi-table-values hi-table)
    ',(hi-table-name hi-table)))

(defun hi-table-name (hi-table)
  (gethash hi-table *hi-table-name-hash*))

(defun hi-table-copy (hi-table &optional (initial-value nil initial-value-passed?))
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   ;; change to below, this was taking a long time for large tables 
   ;; as in 15 seconds for the 250 antigen and sera nl flu sequences
   ;; which was almost all the time when making the strain selection window
   ;;(loop for antigen in (hi-table-antigens hi-table) collect
   ;;	 (loop for serum in (hi-table-sera hi-table) collect
   ;;		(if initial-value-passed?
   ;;		    initial-value
   ;;		  (hi-table-value hi-table antigen serum))))
   (loop for row in (hi-table-values hi-table) collect
	 (loop for column in row collect
	       (if initial-value-passed?
		   initial-value
		 column)))
   (hi-table-name hi-table)))

(defun set-hi-table-values-by-indices (hi-table agindex-srindex-newvalue-triples)
  (let ((hi-table-values-copy (mapcar #'copy-list (hi-table-values hi-table))))
    (loop for (ag-index sr-index new-value) in agindex-srindex-newvalue-triples do
	  (setf (nth sr-index (nth ag-index hi-table-values-copy)) new-value))
    (make-hi-table
     (hi-table-antigens hi-table)
     (hi-table-sera     hi-table)
     hi-table-values-copy
     (hi-table-name     hi-table))))

(defun set-hi-table-values (hi-table ag-sr-newvalue-triples &optional &key (not-found-action :error))
  (let* ((hi-table-antigens (hi-table-antigens hi-table))
	 (hi-table-sera     (hi-table-sera hi-table))
	 clean-agIndex-agIndex-value-triples)
    (loop for (ag-name sr-name new-value) in ag-sr-newvalue-triples do
	  (let ((ag-index (position ag-name hi-table-antigens))
		(sr-index (position sr-name hi-table-sera)))
	    (if (and ag-index sr-index)
		(push-end (list ag-index sr-index new-value) clean-agIndex-agIndex-value-triples)
	      (case not-found-action
		(:warn  (format t "Warning: asked to set the ~a ~a titer but either one or both of the ag and sr do not exist in table~%" ag-name sr-name))
		(:error (error "Asked to set the ~a ~a titer but either one or both of the ag and sr do not exist in table" ag-name sr-name))
		(t (error "Unexpected :not-found-action ~a" not-found-action))))))
    (set-hi-table-values-by-indices
     hi-table
     clean-agIndex-agIndex-value-triples)))

(defun set-hi-table-value (hi-table antigen serum new-value)
  (set-hi-table-values hi-table (list (list antigen serum new-value))))

(defun set-hi-table-value-by-indices (hi-table ag-index sr-index new-value)
  (set-hi-table-values-by-indices hi-table (list (list ag-index sr-index new-value))))


(defun unsafe-set-hi-table-value-by-indices! (hi-table ag-index sr-index new-value)
  ;; this is unsafe because hi-table-sera will not longer be able to find the sera
  ;; (because sera is currently hacked by keeping a hash table whose key is hi-table-values)
  (setf (nth sr-index (nth ag-index (hi-table-values hi-table))) new-value))

(defun hi-table-to-log (hi-table &optional allow-negative-values)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (loop for row-values in (apply-transpose
				  (loop for column in (hi-table-column-values hi-table)
				      for diagonal in (hi-table-diagonal-values hi-table)
				      collect (mapcar (^ (x) 
							 (if (dont-care-p x)
							     x
							   (2dp (titer-diff-to-log
								 diagonal
								 (if allow-negative-values
								     x
								   (min diagonal x))))))
						      column)))
       collect row-values)))

(defun hi-table-to-normal-columns (hi-table)
  (error "need to convert this to use real-sera (ie go thru make-hi-table)")
  (loop for antigen in (hi-table-antigens hi-table)
        for row-values in (apply-transpose
                           (loop for column in (hi-table-column-values hi-table)
			       collect (let ((column-x-av (av (filter #'dont-care-p column)))
					     (column-x-sd (sd (filter #'dont-care-p column))))
					 (mapcar (^ (x) 
						  (if (dont-care-p x)
						      x
                                                    (/ (- x column-x-av) 
						       column-x-sd)))
                                                 column))))
      collect (cons antigen row-values)))

(defun hi-table-to-dists-by-antigen-euclidean-dist (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera     hi-table)
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (loop for j below (hi-table-num-antigens hi-table) collect
	       (e-dist (hi-table-antigen-values-by-index hi-table i)
		       (hi-table-antigen-values-by-index hi-table j))))))

(defun hi-table-to-dists-by-antigen-normalized-euclidean-dist (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera     hi-table)
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (loop for j below (hi-table-num-antigens hi-table) collect
	       (/ (e-dist (hi-table-antigen-values-by-index hi-table i)
			  (hi-table-antigen-values-by-index hi-table j))
		  (sqrt (hi-table-width hi-table)))))))

(defun hi-table-to-dists-by-antigen-normalized-euclidean-dist-within-table (hi-table)
  ;;this function i think is not used, and is not as tested, or as efficient, or as comprehensive as the one below
  ;; (hi-table-to-dists-by-antigen-normalized-dont-care-euclidean-dist)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera     hi-table)   ;;should be antigens, like the function below?, and because we are iterating on antigens twice
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (loop for j below (hi-table-num-antigens hi-table) collect
	       (let (xs ys (sum 0))
		 (loop for x in (hi-table-antigen-values-by-index hi-table i)
		     for y in (hi-table-antigen-values-by-index hi-table j) do
		       (if (and (not (dont-care-p x))
				(not (dont-care-p y)))
			   (progn
			     (cons x xs)
			     (cons y ys)
			     (setq sum (inc sum)))))
		 (/ (e-dist xs ys)
		    (sqrt sum)))))))

(defun hi-table-to-dists-by-antigen-normalized-dont-care-euclidean-dist (hi-table &optional ignore-if-value-less-than ignore-if-num-columns-less-than)
  ;;this could be twice as fast (important for >1000 strain tables) if we did just one half as the data are symetric
  (setq match-lengths nil)        ;;used for me assessing how we use this function
  (setq used-match-lengths nil)   ;;used for me assessing how we use this function
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-antigens hi-table)   ;;because we are making this square
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (progn
	   ;;(print i)
	   (loop for j below (hi-table-num-antigens hi-table) collect
		 (if (< j i)
		     'dont-care     ;;because the table is symetric
		   (let ((antigen-values-i (hi-table-antigen-values-by-index hi-table i))
			 (antigen-values-j (hi-table-antigen-values-by-index hi-table j))
			 matched-ag-i-values
			 matched-ag-j-values)
		     (loop for i-value in antigen-values-i
			 for j-value in antigen-values-j
			 when (not (or (dont-care-p i-value)
				       (dont-care-p j-value)
				       (if ignore-if-value-less-than
					   (or (< i-value ignore-if-value-less-than)  
					       (< j-value ignore-if-value-less-than)))))
			 do (push i-value matched-ag-i-values)
			    (push j-value matched-ag-j-values))
		     (let ((length (length matched-ag-i-values)))
		       (push length match-lengths)
		       (if (or (zerop length)   ;;no overlap
			       (and ignore-if-num-columns-less-than (< length ignore-if-num-columns-less-than)))
			   'dont-care
			 (progn
			   (push length used-match-lengths)
			   (/ (e-dist matched-ag-i-values
				      matched-ag-j-values)
			      (sqrt length))))))))))
   ;; (glue-up (list (hi-table-name hi-table) 'edm))  ; was good idea, but names became too long to be useful
   (hi-table-name hi-table)
   ))

(defun hi-table-to-dists-by-antigen-normalized-dont-care-manhatten-dist (hi-table &optional ignore-if-value-less-than ignore-if-num-columns-less-than)
  ;;this could be twice as fast (important for >1000 strain tables) if we did just one half as the data are symetric
  (setq match-lengths nil)        ;;used for me assessing how we use this function
  (setq used-match-lengths nil)   ;;used for me assessing how we use this function
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-antigens hi-table)   ;;because we are making this square
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (progn
	   ;;(print i)
	   (loop for j below (hi-table-num-antigens hi-table) collect
		 (if (< j i)
		     'dont-care     ;;because the table is symetric
		   (let ((antigen-values-i (hi-table-antigen-values-by-index hi-table i))
			 (antigen-values-j (hi-table-antigen-values-by-index hi-table j))
			 matched-ag-i-values
			 matched-ag-j-values)
		     (loop for i-value in antigen-values-i
			 for j-value in antigen-values-j
			 when (not (or (dont-care-p i-value)
				       (dont-care-p j-value)
				       (if ignore-if-value-less-than
					   (or (< i-value ignore-if-value-less-than)  
					       (< j-value ignore-if-value-less-than)))))
			 do (push i-value matched-ag-i-values)
			    (push j-value matched-ag-j-values))
		     (let ((length (length matched-ag-i-values)))
		       (push length match-lengths)
		       (if (or (zerop length)   ;;no overlap
			       (and ignore-if-num-columns-less-than (< length ignore-if-num-columns-less-than)))
			   'dont-care
			 (progn
			   (push length used-match-lengths)
			   (/ (m-dist matched-ag-i-values      ;;only difference from euclidean distance analogue is here, m-dist not e-dist
				      matched-ag-j-values)
			      length)))))))))                  ;;  and here, normalize by the length not the sqrt length
   ;; (glue-up (list (hi-table-name hi-table) 'edm))           ;;  was good idea but names because too long to be useful
   (hi-table-name hi-table)
   ))

(defun hi-table-fake-sera (hi-table)
  (loop for i below (hi-table-width hi-table) collect
	(read-from-string (format nil "~d-sr" i))))

(defun hi-table-to-dists-by-sera-euclidean-dist (hi-table)
  (make-hi-table
   (hi-table-fake-sera hi-table)
   (hi-table-fake-sera hi-table)
   (loop for i below (hi-table-width hi-table) collect
	 (loop for j below (hi-table-width hi-table) collect
	       (e-dist (hi-table-sera-values-by-index hi-table i)
		       (hi-table-sera-values-by-index hi-table j))))))

(defun hi-table-to-dists-by-antigen-correlation (hi-table)
  ;;watch out, we might get negative correlation here
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-antigens hi-table)
   (loop for i below (hi-table-num-antigens hi-table) collect
	 (loop for j below (hi-table-num-antigens hi-table) collect
	       (correlation (transpose (hi-table-antigen-values-by-index hi-table i)
				       (hi-table-antigen-values-by-index hi-table j)))))))

(defun fi-hi-table (hi-table filename &optional (if-exists-action :error))
  "File and HI table in such a way that reading it back will set the sera."
  ;;if-exists-actions are :error :new-version :overwirte :append :supersede nil
  ;;CAREFUL :overwrite IS JUST OVERWRITE WHAT IS THERE, AND LEAVE THE REST
  (fi `(make-hi-table
	',(hi-table-antigens hi-table)
	',(hi-table-sera     hi-table)
	',(hi-table-values   hi-table)
	',(hi-table-name     hi-table))
      filename
      if-exists-action))


(defun fi-in-hi-table (filename)
  "Read in an HI table from file, one that has been written by fi-hi-table (so as to set the sera)."
  (eval (fi-in filename)))

;; this is replaced by the below, as the below is orders of magnitude faster on large hi-tables
;;(defun extract-hi-table (hi-table antigens &optional (sera (hi-table-sera hi-table)))
;;  "Extract entries from a larger HI table to make a smaller table."
;;  ;; careful, if the matrix is upper triangular (with don't cares in the lower half) and we extract antigens
;;  ;; in a different order than they are in the original table, we will get don't cares in the upper triangle
;;  ;; and values in the bottom.  this is 'correct' behaviour.
;;  ;; (to see this, think about the last entry in hi-table, it has only one non-dont-care, if it is not
;;  ;;  last in the extracted table, then taking values from that row will result in dont-cares for everything
;;  ;;  but the final entry) (a good application of randy's 'take it to the extreme to see')
;;  (make-hi-table
;;   antigens
;;   sera
;;   (loop for antigen in antigens collect
;;	 (loop for serum in sera collect
;;	       (hi-table-value hi-table antigen serum)))
;;   (glue-up (list (hi-table-name hi-table) 'extract))))

(defun extract-hi-table (hi-table antigens &optional (sera (remove-duplicates (hi-table-sera hi-table))))
  "Extract entries from a larger HI table to make a smaller table."
  ;; careful, if the matrix is upper triangular (with don't cares in the lower half) and we extract antigens
  ;; in a different order than they are in the original table, we will get don't cares in the upper triangle
  ;; and values in the bottom.  this is 'correct' behaviour.
  ;; (to see this, think about the last entry in hi-table, it has only one non-dont-care, if it is not
  ;;  last in the extracted table, then taking values from that row will result in dont-cares for everything
  ;;  but the final entry) (a good application of randy's 'take it to the extreme to see')
  ;;CAREFUL, if duplicate sera names (as in some RIVM HI tables when we convert the ferret numbers to strain names)
  (let* ((all-antigens (hi-table-antigens hi-table))
	 (all-sera     (hi-table-sera     hi-table))
         (antigen-indices (loop for antigen in antigens append (positions antigen all-antigens)))
         (sera-indices    (loop for serum   in sera     append (positions serum   all-sera)))
         ;;(antigen-indices (loop for i below (length all-antigens) 
         ;;                     for table-antigen in all-antigens 
         ;;                     when (member table-antigen antigens)
         ;;                     collect i))
         ;;(sera-indices    (loop for i below (length all-sera) 
         ;;                     for table-serum in all-sera
         ;;                     when (member table-serum sera)
         ;;                     collect i))
         )
    (extract-hi-table-by-indices
     hi-table
     antigen-indices
     sera-indices)))

(defun extract-hi-table-by-indices (hi-table antigen-indices &optional (sera-indices antigen-indices))
  "Extract entries from a larger HI table by the indices (starting at 0) of the antigens and sera."
  (make-hi-table
   (loop for antigen-index in antigen-indices collect
	 (hi-table-antigen-by-index hi-table antigen-index))
   (loop for serum-index in sera-indices collect
	 (hi-table-serum-by-index hi-table serum-index))
   (let ((hi-table-values (hi-table-values hi-table))
         (all-sera-in-order-p (equal sera-indices (series 0 (dec (hi-table-width hi-table))))))
     (loop for antigen-index in antigen-indices collect
           (let ((full-row (nth antigen-index hi-table-values)))
             (if all-sera-in-order-p
                 full-row
               (multiple-nth sera-indices full-row)))))
   ;;(glue-up (list (hi-table-name hi-table) 'extract))
   (hi-table-name hi-table)))

(defun extract-hi-table-by-excluding-indices (hi-table antigen-indices-to-exclude
					      &optional (sera-indices-to-exclude antigen-indices-to-exclude))
  "Extract entries from a larger HI table by excluding these indices (starting at 0) of the antigens and sera."
  (let ((antigen-indices-to-include
	 (loop for i below (hi-table-length hi-table) 
	     when (not (member i antigen-indices-to-exclude))
	     collect i))
	(sera-indices-to-include
	 (loop for i below (hi-table-width hi-table) 
	     when (not (member i sera-indices-to-exclude))
	     collect i)))
    (extract-hi-table-by-indices hi-table antigen-indices-to-include sera-indices-to-include)))

(defun extract-hi-table-by-excluding (hi-table antigens-to-exclude
				      &optional sera-to-exclude)
  "Extract entries from a larger HI table by excluding these antigen and sera."
  (extract-hi-table-by-excluding-indices
   hi-table
   (map-append (^ (ag) (positions ag (hi-table-antigens hi-table))) antigens-to-exclude)
   (map-append (^ (sr) (positions sr (hi-table-sera     hi-table))) sera-to-exclude)
   ;;(loop for i below (length (hi-table-antigens hi-table))
   ;;    for table-antigen in (hi-table-antigens hi-table)
   ;;    when (member table-antigen antigens-to-exclude)
   ;;    collect i)
   ;;(loop for i below (length (hi-table-sera hi-table))
   ;;    for table-serum in (hi-table-sera hi-table)
   ;;    when (member table-serum sera-to-exclude)
   ;;    collect i)
  ))

(defun extract-hi-table-window (hi-table offset width &optional (height width))
  ;; is this right?
  (extract-hi-table-by-indices
   hi-table
   (mapcar (^ (x) (+ x offset))
	   (series 0 (dec width)))
   (mapcar (^ (x) (+ x offset))
	   (series 0 (dec height)))))

(defun hi-table-antigen-indices (hi-table)
  (series 0 (dec (hi-table-length hi-table))))

(defun hi-table-sera-indices (hi-table)
  (series 0 (dec (hi-table-width hi-table))))

(defun extract-hi-table-by-antigen-indices (hi-table ag-indices)
  (extract-hi-table-by-indices
   hi-table
   ag-indices
   (hi-table-sera-indices hi-table)))

(defun extract-hi-table-by-sera-indices (hi-table sr-indices)
  (extract-hi-table-by-indices
   hi-table
   (hi-table-antigen-indices hi-table)
   sr-indices))

(defun extract-hi-table-based-on-intersection-with-other-hi-table-ag-and-sr (hi-table other-hi-table)
  (extract-hi-table
   hi-table
   (reverse (intersection (hi-table-antigens hi-table) (hi-table-antigens other-hi-table)))
   (reverse (intersection (hi-table-sera hi-table) (hi-table-sera other-hi-table)))))

(defun hi-tables-intersection-list-merge (tables &optional &key names)
  (let ((antigen-intersection (apply #'nary-intersection (mapcar #'hi-table-antigens tables)))
	(sera-intersection    (apply #'nary-intersection (mapcar #'hi-table-sera     tables))))
    (apply #'f-hi-tables 
	   (^ (args)
	      (mapcar #'list (if names names (mapcar #'hi-table-name tables)) args))
	   (mapcar (^ (table) (extract-hi-table table antigen-intersection sera-intersection)) tables))))

(defun hi-table-extract-random-sample-from-list (hi-table num-strains strain-list &optional &key (with-replacement t))
  (error "depreciated function, use hi-table-extract-random-antigen-sample-from-list or hi-table-extract-random-sera-sample-from-list")
  (extract-hi-table
   hi-table
   (sort-strains
    (if with-replacement
	(random-samples-with-replacement num-strains strain-list)
      (random-samples-without-replacement (min (length strain-list) num-strains) strain-list)))
   (hi-table-sera hi-table)))

(defun hi-table-extract-random-antigen-sample-from-list (hi-table num-strains strain-list &optional &key (with-replacement t))
  (extract-hi-table
   hi-table
   (sort-strains
    (if with-replacement
	(random-samples-with-replacement num-strains strain-list)
      (random-samples-without-replacement (min (length strain-list) num-strains) strain-list)))
   (hi-table-sera hi-table)))

(defun hi-table-extract-random-sera-sample-from-list (hi-table num-strains strain-list &optional &key (with-replacement t))
  (extract-hi-table
   hi-table
   (hi-table-antigens hi-table)
   (sort-strains
    (if with-replacement
	(random-samples-with-replacement num-strains strain-list)
      (random-samples-without-replacement (min (length strain-list) num-strains) strain-list)))
   ))

(defun hi-table-extract-symmetric-random-sample-from-list (hi-table num-strains strain-list &optional &key (with-replacement t))
  (let ((sample (sort-strains
		 (if with-replacement
		     (random-samples-with-replacement num-strains strain-list)
		   (random-samples-without-replacement (min (length strain-list) num-strains) strain-list)))))
    (extract-hi-table
     hi-table
     sample
     sample)))

(defun hi-table-enths (hi-table enths &optional (start 0))
  (extract-hi-table-by-indices
   hi-table
   (enths enths (series 0 (dec (hi-table-length hi-table))) start)
   (enths enths (series 0 (dec (hi-table-width  hi-table))) start)))

(defun hi-table-remove-duplicates (hi-table)
  (let* ((antigens (hi-table-antigens hi-table))
	 (sera     (hi-table-sera     hi-table))
	 (antigens-no-duplicates (remove-duplicates antigens))
	 (sera-no-duplicates (remove-duplicates sera))
         (antigen-positions-no-duplicates (mapcar (^ (x) (position x antigens)) antigens-no-duplicates))
	 (sera-positions-no-duplicates    (mapcar (^ (x) (position x sera))     sera-no-duplicates)))
    (extract-hi-table-by-indices hi-table antigen-positions-no-duplicates sera-positions-no-duplicates)))

(defun extract-hi-table-by-removing-dont-care-columns (hi-table)
  (let ((dont-care-only-columns
	 (loop for column below (hi-table-width hi-table) 
	     when (not (filter #'dont-care-p (cdr (hi-table-column hi-table column))))
	     collect column)))
    (extract-hi-table-by-excluding-indices hi-table '() dont-care-only-columns)))

(defun hi-table-remove-all-dont-care-sera (hi-table &optional &key 
							      num-numeric-titers-threshold
							      num-threshold-titers-threshold
							      (num-total-titers-threshold 0))
  ;;careful, this does not get rid of a col if we have a distance matrix 
  ;; (in which the diagonal will be all zeroes, even if all the cols are dont-cares
  (let ((sera-to-keep
	 (loop for serum in (hi-table-sera hi-table) append
	       (let* ((serum-values (hi-table-serum-values hi-table serum))
		      (num-numeric-titers   (length (collect (^ (x) (or (numberp x)
									(and (listp x)  ;; for multiple values in a list during merge
									     (apply #'nary-equal (cons t (mapcar #'numberp x))))))
							     serum-values)))
		      (num-threshold-titers (length (collect #'thresholdp serum-values)))
		      (num-total-titers     (+ num-numeric-titers num-threshold-titers)))
		 (if (and (if num-numeric-titers-threshold   (> num-numeric-titers   num-numeric-titers-threshold)   t)
			  (if num-threshold-titers-threshold (> num-threshold-titers num-threshold-titers-threshold) t)
			  (if num-total-titers-threshold     (> num-total-titers     num-total-titers-threshold)     t))
		     (list serum))))))
    (extract-hi-table hi-table (hi-table-antigens hi-table) sera-to-keep)))

(defun hi-table-remove-all-dont-care-antigens (hi-table &optional &key 
								  num-numeric-titers-threshold
								  num-threshold-titers-threshold
								  (num-total-titers-threshold 0))
  ;;careful, this does not get rid of a col if we have a distance matrix 
  ;; (in which the diagonal will be all zeroes, even if all the cols are dont-cares
  (let ((antigens-to-keep
	 (loop for antigen in (hi-table-antigens hi-table) append
	       (let* ((antigen-values (hi-table-antigen-values hi-table antigen))
		      (num-numeric-titers   (length (collect (^ (x) (or (numberp x)
									(and (listp x)  ;; for multiple values in a list during merge
									     (apply #'nary-equal (cons t (mapcar #'numberp x))))))
							     antigen-values)))
		      (num-threshold-titers (length (collect #'thresholdp antigen-values)))
		      (num-total-titers     (+ num-numeric-titers num-threshold-titers)))
		 (if (and (if num-numeric-titers-threshold   (> num-numeric-titers   num-numeric-titers-threshold)   t)
			  (if num-threshold-titers-threshold (> num-threshold-titers num-threshold-titers-threshold) t)
			  (if num-total-titers-threshold     (> num-total-titers     num-total-titers-threshold)     t))
		     (list antigen))))))
    (extract-hi-table hi-table antigens-to-keep (hi-table-sera hi-table))))

(defun hi-table-remove-all-dont-care-sera-and-antigens (hi-table &optional &key 
									   num-numeric-titers-threshold
									   num-threshold-titers-threshold
									   (num-total-titers-threshold 0))
  (hi-table-remove-all-dont-care-antigens
   (hi-table-remove-all-dont-care-sera 
    hi-table 
    :num-numeric-titers-threshold   num-numeric-titers-threshold
    :num-threshold-titers-threshold num-threshold-titers-threshold
    :num-total-titers-threshold     num-total-titers-threshold)
   :num-numeric-titers-threshold   num-numeric-titers-threshold
   :num-threshold-titers-threshold num-threshold-titers-threshold
   :num-total-titers-threshold     num-total-titers-threshold))



(defun extract-hi-table-by-removing-non-merged-rows (hi-table)
  (let ((merged-as-list-only-rows
	 (loop for row below (hi-table-length hi-table) 
	     when (not (collect (^ (l) (and (listp l) (> (length l) 2))) (cdr (hi-table-row hi-table row))))
	     collect row)))
    (extract-hi-table-by-excluding-indices hi-table merged-as-list-only-rows '())))

(defun extract-hi-table-by-removing-non-merged-columns (hi-table)
  (let ((merged-as-list-only-columns
	 (loop for column below (hi-table-width hi-table) 
	     when (not (collect (^ (l) (and (listp l) (> (length l) 2))) (cdr (hi-table-column hi-table column))))
	     collect column)))
    (extract-hi-table-by-excluding-indices hi-table '() merged-as-list-only-columns)))

(defun extract-hi-table-by-removing-non-merged-rows-and-columns (hi-table)
  (extract-hi-table-by-removing-non-merged-columns
   (extract-hi-table-by-removing-non-merged-rows
    hi-table)))



(defun hi-table-values-upper-short-triangle (hi-table-values)
  ;;the short means without the diagonal
  (loop for i below (length hi-table-values) 
      for row in hi-table-values collect
	(nthcdr (inc i) row)))

(defun hi-table-values-upper-triangle (hi-table-values)
  ;;include the diagonal
  (loop for i below (length hi-table-values) 
      for row in hi-table-values collect
	(nthcdr i row)))

(defun hi-table-values-lower-short-triangle (hi-table-values)
  ;;the short means without the diagonal
  (loop for i below (length hi-table-values) 
      for row in hi-table-values collect
	(firstn i row)))

(defun hi-table-values-transpose (hi-table-values)
  (loop for ag-index below (length (car hi-table-values)) collect
	(loop for sr-index below (length hi-table-values) collect
	      (hi-table-values-value-by-indices hi-table-values sr-index ag-index))))

(defun hi-table-transpose (hi-table)
  (make-hi-table
   (hi-table-sera hi-table)
   (hi-table-antigens hi-table)
   (hi-table-values-transpose (hi-table-values hi-table))))

(defun hi-table-values-upper-rectangle-to-lower (hi-table-values)
  (let ((half-length (/ (length hi-table-values) 2)))
    (if (not (integerp half-length))
	(error "not coded yet for non-even-length tables"))
    (loop for ag-index below (length hi-table-values) collect
	  (loop for sr-index below (length hi-table-values) collect
		(if (or (< ag-index half-length)
			(>= sr-index half-length))
		    (hi-table-values-value-by-indices hi-table-values ag-index sr-index)
		  (hi-table-values-value-by-indices hi-table-values (- ag-index half-length) (+ sr-index half-length)))))))

(defun hi-table-upper-rectangle-to-lower (hi-table)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (hi-table-values-upper-rectangle-to-lower (hi-table-values hi-table))))

(defun hi-table-values-upper-triangle-to-lower-triangle (hi-table)
  (if (not (= (hi-table-width hi-table) (hi-table-length hi-table)))
      (error "not coded yet for non-square tables"))
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (loop for ag-index below (hi-table-length hi-table) collect
	 (loop for sr-index below (hi-table-width hi-table) collect
	       (if (< ag-index sr-index)
		   (hi-table-value-by-indices hi-table ag-index sr-index)
		 (hi-table-value-by-indices hi-table sr-index ag-index))))))

(defun plus-minus-p (x)
  (let ((string (format nil "~a" x)))
    (eql #\* (aref string (dec (length string))))))

(defun less-than-p (x)
  (let ((string (format nil "~a" x)))
    (and (> (length string) 0) (eql #\< (aref string 0)))))

(defun greater-than-p (x)
  (let ((string (format nil "~a" x)))
    (and (> (length string) 0) (eql #\> (aref string 0)))))

(defun hi-table-integer-p (x)
  (member x '(10 20 40 80 160 320 640 1280 2560 5120 10240
	       5 15 30 60  80 240 480  960 1920 3840 7680)))

(defun no-entry-p (x)
  (or (eql 'ig x)
      (eql '-  x)))

(defun hi-table-max-num-sera-values (hi-table)
  (hi-table-width hi-table))
				    
(defun sera-name-p (e)
  (let ((string (format nil "~s" e)))
    (and (>= (length string) 2)
       (equal "F" (substring string 0 0))
       (integerp (read-from-string (substring string 1 1))))))

(defun serum-abbreviation (serum-name)
  (if (sera-name-p serum-name)
      (let ((serum-name (string serum-name)))
	(setq serum-name (substring serum-name 1))
	(if (and (substring-before-char #\/ serum-name) (substring-after-char #\/ serum-name))
	    (setq serum-name (string-append (substring-before-char #\/ serum-name) (substring-after-char #\/ serum-name)))
	  serum-name))
    (smart-strain-abbreviation serum-name)))

(defun chop-off-f (serum-name)
  (if (sera-name-p serum-name)
      (let ((serum-name (string serum-name)))
	(setq serum-name (substring serum-name 1)))))

(defun pp-hi-table (hi-table &optional (pp-serum-names 'short) (serum-name-width 4) serum-names-bottom-justified 
		    &key (stream t)
			 filename
			 (if-exists-action :error)
			 antigen-part-length
			 (line-prefix "")  ;; for adding ";" for example to comment outo lines so pp can go in live files as diagnostics
			 (line-suffix "")  ;; can be list, then one per line, for marking some lines
			 num-significant-figures)      
  (if filename
      (with-open-file (out filename :direction :output :if-exists if-exists-action)
	(pp-hi-table hi-table pp-serum-names serum-name-width serum-names-bottom-justified :stream out :antigen-part-length antigen-part-length :line-prefix line-prefix :line-suffix line-suffix :num-significant-figures num-significant-figures))
    (let* ((table-type (cond ((not (zerop (length (collect #'listp (apply-append (hi-table-values hi-table))))))
			      'list-merged-hi-table)
			     ((let ((all-numbers-in-table (collect #'numberp (flatten (hi-table-values hi-table)))))
				(and (not (null all-numbers-in-table))
				     (>= (apply-max all-numbers-in-table) 0)
				     (<= (apply-max all-numbers-in-table) 1)))
			      'sequence)
			     (t 'hi-table)))
	   ;;((or (hi-table-integer-p (cadar hi-table)) 
	   ;;     (less-than-p (cadar hi-table))
	   ;;     (greater-than-p (cadar hi-table))
	   ;;     (plus-minus-p (cadar hi-table))) 'hi-table)
	   ;;((> (apply #'max (collect #'numberp (flatten (hi-table-values hi-table)))) 20) 'lapedes)
	   ;;(t 'log-diff))))
	   (antigen-part-length (if antigen-part-length
				    antigen-part-length
				  (if (eql table-type 'list-merged-hi-table)
				      60
				    (+ 4 (let ((antigen-name-width (apply-max (mapcar #'length (mapcar #'anything->string (hi-table-antigens hi-table))))))
                                           (if antigen-name-width
                                               antigen-name-width
                                             56)))))))
      (if (atom line-suffix)
	  (setq line-suffix (loop for i below (hi-table-length hi-table) collect line-suffix)))
      (if (listp pp-serum-names)
	  (progn
	    (format stream (format nil "~~%~a~~~da" line-prefix antigen-part-length) "")
	    (loop for i from 1 to (length pp-serum-names) do
		  (format stream " ~4d. " i))
	    (setq pp-serum-names nil) ;;ugly, cause i don't want the CDC tables to have poor abbrevs for now
	    (newline)))
      (if serum-names-bottom-justified
	  ;;this could be cobined with below, but not for now
	  (let* ((serum-name-strings 
		  (firstn (hi-table-max-num-sera-values hi-table)
			  (mapcar #'string 
				  (if (eql 'short pp-serum-names)
				      (mapcar #'serum-abbreviation (hi-table-sera hi-table))
				    (hi-table-sera hi-table)))))
		 (serum-name-lengths (mapcar #'length serum-name-strings))
		 (longest-name (apply-max serum-name-lengths)))
	    (loop for line below longest-name do
		  (format stream (format nil "~~%~a~~~da~~{      ~~a~~}" line-prefix (dec antigen-part-length)) ""
			  (loop for serum-name-string in serum-name-strings 
			      for serum-name-length in serum-name-lengths collect
				(let ((letter-index (+ line (- serum-name-length longest-name))))
				  (if (minusp letter-index)
				      " "
				    (aref serum-name-string letter-index))))))
	    (newline)))
      (if (and pp-serum-names (not serum-names-bottom-justified))
	  (let* ((serum-name-strings 
		  (firstn (hi-table-max-num-sera-values hi-table)
			  (mapcar #'string 
				  (if (eql 'short pp-serum-names)
				      (mapcar #'serum-abbreviation (hi-table-sera hi-table))
				    (if (eql 'full pp-serum-names)
					(mapcar #'anything->string (hi-table-sera hi-table))
				      (if (listp pp-serum-names)
					  (mapcar #'serum-abbreviation pp-serum-names)
					(mapcar #'anything->string (hi-table-sera hi-table))))))))
		 (abbreviated-serum-name-strings (mapcar #'serum-abbreviation (hi-table-sera hi-table)))
		 (serum-name-lengths (mapcar #'length abbreviated-serum-name-strings))  ;; ugly, just only used for short below
		 (longest-name (if serum-name-lengths (apply-max serum-name-lengths) 20)))
	    (if (member pp-serum-names '(full full-and-short))
		(format stream (format nil "~~%~a~~~da~~{ ~~a~~}" line-prefix antigen-part-length) "" serum-name-strings))
	    (if (member pp-serum-names '(short full-and-short))
		(loop for line below (ceiling (/ longest-name serum-name-width)) do
		      (format stream (case table-type
				       (sequence (format nil "~~%~a~~~da~~{ ~~4a~~}" line-prefix antigen-part-length))
				       (t (format nil "~~%~a~~~da~~{  ~~4a ~~}" line-prefix antigen-part-length)))
			      (if (eql pp-serum-names 'full-and-short) ";;  " "    ")
			      (loop for serum-name-string in (if (eql pp-serum-names 'short) serum-name-strings abbreviated-serum-name-strings)  ;; ugly ugly, relies on if member above
				  for serum-name-length in serum-name-lengths collect
				    (substring serum-name-string 
					       (min serum-name-length (* line serum-name-width))
					       (dec (min serum-name-length
							 (* (inc line) serum-name-width))))))))
	    '(newline)))
      (loop for antigen in (hi-table-antigens hi-table)
          for row in (hi-table-row-values hi-table) 
	  for line-suffix-element in line-suffix
	  for i from 0 do
	    (pp-hi-table-row antigen row i table-type :stream stream :antigen-part-length antigen-part-length :line-prefix line-prefix :line-suffix line-suffix-element :num-significant-figures num-significant-figures))
      )))


(defun pp-hi-table-row (antigen row i table-type &optional &key (stream t) (antigen-part-length 31) (line-prefix "") (line-suffix "") num-significant-figures)
  (if (eql table-type 'list-merged-hi-table)
      (pp-hi-table-list-merged-row antigen row i :stream stream :antigen-part-length antigen-part-length :line-prefix line-prefix :line-prefix line-prefix :num-significant-figures num-significant-figures)
    (progn
      (format stream (format nil "~~%~a~~~da" line-prefix antigen-part-length) antigen)
      (loop for x in row 
	  for j from 0 do
	    (case table-type
	      (hi-table (cond ;;((less-than-ten-p x) (format stream "   <10 "))
			      ((true-dont-care-p x) (format stream "     * "))
			      ((null x)       (format stream "       "))
			      ((no-entry-p x) (format stream "    -  "))
			      ((plus-minus-p x) (format stream " ~6@a" (let ((x (format nil "~a" x))) x)))
			      ((thresholdp x)   (format stream " ~5@a " (let ((x (format nil "~a" x))) x)))
			      ((greater-than-p x) (format stream " ~5@a " (let ((x (format nil "~a" x))) x)))
			      ((listp x) (format stream " ~a" x))
			      (t (format stream " ~5@a " x))))
	      (lapedes  (cond ((and (= i j) (or (dont-care-p x) (zerop x))) (format stream "  ---"))
			      ((less-than-ten-p x) (format stream "  <10"))
			      ((thresholdp x) (error "thresholds other than <10 not supported in lapedes format yet"))
			      ((true-dont-care-p x) (format stream "   * "))
			      (t (format stream " ~4d" (round x)))))
	      (log-diff (pp-hi-table-element-log-diff x i j :stream stream))
	      (sequence (cond ((and (= i j) (or (true-dont-care-p x) (zerop x)) (not num-significant-figures)) (format stream "   ---"))
			      ((true-dont-care-p x) (format stream "   * "))
			      (t (if num-significant-figures
				     (format stream (format nil " ~~~df " num-significant-figures) x)
				   (format stream " ~4,3f" x)))))))
      (format stream "~a" line-suffix))))

(defun unique-ids-with-replicates (ll)
  (let* ((merged-hists (map-append #'hist ll))
	 (unique-ids (sort-alpha (remove-duplicates (nths 0 merged-hists)))))
    (loop for unique-id in unique-ids collect
	  (list unique-id (apply #'max (loop for (id num) in merged-hists when (equal id unique-id) collect num))))))


(defun list-after-n-of (n item list &optional &key (test #'eql))
  (if (zerop n)
      list
    (list-after-n-of
     (dec n)
     item
     (let ((position (position item list :test test)))
       (if position
	   (nthcdr (inc position) list)
	 nil))
     :test test)))

(defun pp-hi-table-list-merged-row (antigen row i &optional &key (stream t) (antigen-part-length 31) (line-prefix "") num-significant-figures)
  ;; note, if multile entries in list for same table, only prints the first
  (let* ((includes-table-names 
	  (loop for entry in row 
	      when (and (listp entry) (listp (car entry)))
	      do (return t)
	      finally (return nil)))
	 (listified-row (mapcar (^ (entry) (if (not (listp entry)) (list entry) entry)) row))
	 (num-lines (apply-max (mapcar #'length listified-row))))

    (pp-hi-table-row ""      (loop for e in row collect (progn e nil)) i 'hi-table :stream stream :antigen-part-length antigen-part-length :line-prefix line-prefix :num-significant-figures num-significant-figures) ;; a blank line
    (pp-hi-table-row antigen (loop for e in row collect (progn e nil)) i 'hi-table :stream stream :antigen-part-length antigen-part-length :line-prefix line-prefix :num-significant-figures num-significant-figures) ;; line with antigen only

    (if includes-table-names
	(let ((table-names-with-replicates (unique-ids-with-replicates (loop for e in row when (listp e) collect (nths 0 e)))))
	  (loop for (table-name replicates) in table-names-with-replicates do
		(loop for replicate below replicates do
		      (pp-hi-table-row 
		       (string-append "       " (string table-name))
		       (loop for e in row collect 
			     (if (listp e)    
				 (assoc-value-1 table-name (list-after-n-of replicate table-name e :test (^ (x y) (equal x (car y)))) :test #'equal)
			       nil))   ;;convert a dont-care into a nil for this pp (not a ".")
		       i
		       'hi-table
		       :stream stream
		       :antigen-part-length antigen-part-length
		       :line-prefix line-prefix
		       :num-significant-figures num-significant-figures))))
      (loop for line-number below num-lines do
	    (pp-hi-table-row
	     ""
	     (loop for e in row collect
		   (if (listp e)
		       (nth line-number e)    ;; will be null if this e is not long enuf, and we want nil anyhow to get blank
		     (if (true-dont-care-p e)
			 nil ;; get a blank if we have a dont-care
		       (if (not (listp e))
			   (if (zerop line-number)
			       e             ;; for an entry which is not a list, only print on the first line
			     nil)        
			 nil))))
	     i
	     'hi-table
	     :stream stream
	     :antigen-part-length antigen-part-length
	     :line-prefix line-prefix
	     :num-significant-figures num-significant-figures)))))


#|
(setq foo (make-hi-table '(a b) '(x y) '((fax fay) (fbx fby)) 'foo))
(setq bar (make-hi-table '(b c) '(y z) '((bby bbz) (bcy bcz)) 'bar))
(setq foobar (merge-hi-tables (list foo bar) #'list-multiples 'foobar t))

(pp-hi-table foobar)
                                    X      Y      Z 
A                                                   
       FOO                        FAX    FAY        
B                                                   
       FOO                        FBX    FBY        
       BAR                               BBY    BBZ 
C                                                   
       BAR                               BCY    BCZ 
|#


(defun pp-hi-table-element-log-diff (x i j &optional &key (stream t))
  (cond ((and (= i j) (or (true-dont-care-p x) (and (numberp x) (zerop x)))) 
	 (format stream "  ---"))
	;;((less-than-ten-p x) (format stream "  <10"))
	((thresholdp x) (format stream "~5@a" x))
	((true-dont-care-p x)     (format stream "   * "))
	((listp x) (progn 
		     (format stream " (")
		     (mapcar (^ (x) (pp-hi-table-element-log-diff x i j)) x)
		     (format stream " )")))
	(t (format stream " ~4,1f" x))))


(defun hi-table-sera-from-sera-name (sera-name)
  (let* ((sera (eval sera-name))
	 (hi-table-name (read-from-string (substring (string sera-name) 0 (- (length (string sera-name)) 6))))
	 (hi-table (eval hi-table-name))
	 (hi-table-antigens (hi-table-antigens hi-table)))
    (loop for serum in sera collect
	  (cond ((numberp serum) (nth (dec serum) hi-table-antigens))
		((and (stringp serum) (numberp (read-from-string (substring serum 0 0))))
		 (if (and (not (string-equal " " (substring serum 1 1)))
			  (numberp (read-from-string (substring serum 1 1))))
		     (string-append (nth (dec (read-from-string (substring serum 0 1))) hi-table-antigens)
				    (substring serum 2))
		   (string-append (nth (dec (read-from-string (substring serum 0 0))) hi-table-antigens)
				  (substring serum 1))))
		(t serum)))))
    

(defun hi-table-sera-from-table-name (name)
  (let* ((sera-name (glue-up `(,name sera)))
	 (sera (if (boundp sera-name)
		   (hi-table-sera-from-sera-name sera-name)
		 (hi-table-sera (eval name)))))
    sera
    ))

(defun cdc?-to-?? (tree)
  ;;called in pp-hi-table-sera-from-table-name and pp-hi-table-macro
  (string-subst-string-in-tree "CDC?" "??" tree))

(defun pp-hi-table-sera-from-table-name (name)
  (format t "~%Sera")
  (newline)
  (loop for serum in (cdc?-to-?? (hi-table-sera-from-table-name name)) for i from 1 do
	(format t "~%  ~2d. ~a" i serum))
  )

(defmacro pp-hi-table-macro (name)
  `(progn
     (push ',name hi-tables)
     (format t "~%~a ~%~a" 
	     (substring-before-char #\- (string ',name))
	     (substring-after-char #\- (string ',name)))
     (pp-hi-table (cdc?-to-?? ,name) (cdc?-to-?? (hi-table-sera-from-table-name ',name)))
     (newline)
     (newline)
     (newline)
     (pp-hi-table-sera-from-table-name ',name)
     (newline)
     (newline)
     (newline)))



#|
(progn (setq hi77d (hi-table-to-log hi77))
       (setq hi85d (hi-table-to-log hi85))
       (setq hi90d (hi-table-to-log hi90))
       (setq hi92d (hi-table-to-log hi92)))
|#

#|
(pp-hi-table (hi-table-to-log hi77))
A/HONGKONG/8/68            ---  0.0  0.0  3.0  2.0  4.0  1.0  2.0  4.0  4.0  5.0  5.0  5.0  5.0  0.0  1.0  1.0  4.0
A/HONGKONG/107/71          4.0  ---  6.0  1.0  4.0  4.0  3.0  4.0  6.0  5.0  6.0  6.0  6.0  7.0  3.0  5.0  6.0  5.0
A/ENGLAND/42/72            2.0  2.0  ---  4.0  0.0  2.0  1.0  2.0  2.0  3.0  5.0  5.0  5.0  5.0  0.0  1.0  1.0  3.0
A/HONGKONG/5/72            2.0  0.0  3.0  ---  1.0  3.0  1.0  2.0  3.0  3.0  5.0  5.0  5.0  5.0  1.0  3.0  4.0  3.0
A/ANNOVER/61/73            4.0  2.0  3.0  4.0  ---  2.0  0.0  2.0  1.0  3.0  5.0  5.0  5.0  6.0  1.0  3.0  5.0  3.0
A/PORTCHALMERS/1/73        3.0  2.0  2.0  4.0  0.0  ---  1.0  1.0  3.0  3.0  5.0  5.0  5.0  5.0  0.0  3.0  4.0  3.0
A/ENGLAND/635/74           4.0  3.0  5.0  4.0  1.0  4.0  ---  3.0  3.0  3.0  6.0  6.0  6.0  6.0  2.0  4.0  5.0  4.0
A/PR/1/74                  3.0  2.0  4.0  4.0  2.0  1.0  2.0  ---  4.0  4.0  4.0  5.0  5.0  5.0  1.0  3.0  5.0  3.0
A/SCOTLAND/840/74          4.0  3.0  3.0  4.0  0.0  2.0  0.0  3.0  ---  3.0  5.0  5.0  6.0  6.0  2.0  4.0  5.0  4.0
A/MAYOCLINIC/4/75          3.0  2.0  4.0  4.0  2.0  5.0  2.0  3.0  5.0  ---  5.0  5.0  5.0  6.0  1.0  4.0  4.0  3.0
A/HONGKONG/9/75            3.0  3.0  5.0  4.0  2.0  4.0  3.0  3.0  5.0  4.0  ---  2.0  5.0  5.0  0.0  0.0  2.0  1.0
A/VICTORIA/3/75            3.0  3.0  4.0  5.0  2.0  3.0  2.0  3.0  4.0  2.0  0.0  ---  4.0  4.0  0.0  0.0  1.0  1.0
A/ENGLAND/864/75           2.0  1.0  3.0  3.0  1.0  3.0  1.0  2.0  4.0  2.0  3.0  3.0  ---  4.0  0.0  1.0  3.0  0.0
A/TOKYO/1/75               3.0  2.0  5.0  5.0  2.0  4.0  2.0  3.0  5.0  3.0  1.0  3.0  5.0  ---  0.0  0.0  4.0  1.0
A/BRAZIL/25/76             5.0  4.0  6.0  6.0  4.0  6.0  4.0  5.0  7.0  5.0  1.0  3.0  6.0  6.0  ---  1.0  3.0  2.0
A/BRAZIL/31/76             4.0  4.0  5.0  6.0  4.0  5.0  4.0  5.0  6.0  5.0  1.0  2.0  5.0  5.0  0.0  ---  2.0  2.0
A/ALLEGCO/29/76            4.0  2.0  4.0  5.0  2.0  4.0  2.0  4.0  5.0  4.0  2.0  4.0  5.0  5.0  0.0  0.0  ---  2.0
A/VICTORIA/112/76          4.0  3.0  5.0  5.0  3.0  5.0  3.0  4.0  5.0  4.0  3.0  5.0  3.0  6.0  0.0  1.0  3.0  ---


(pp-hi-table (hi-table-to-log hi85))
A/OREGON/4/80              ---  1.0  3.0  1.0  0.0  2.0  4.0  3.0  1.0  0.0  1.0
A/BANGKOK/1/79             0.0  ---  4.0  0.0  1.0  1.0  3.0  3.0  2.0  1.0  3.0
A/BANGKOK/2/79             0.0  2.0  ---  3.0  1.0  4.0  4.0  3.0  2.0  1.0  2.0
A/SHANGHAI/31/80           2.0  1.0  5.0  ---  2.0  1.0  3.0  4.0  3.0  2.0  3.0
A/PHILIPPINES/2/82_EQ      3.0  4.0  5.0  4.0  ---  4.0  4.0  4.0  3.0  2.0  3.0
A/TAIWAN/16/83             1.0  2.0  5.0  1.0  2.0  ---  3.0  3.0  3.0  2.0  3.0
A/PANAMA/1/83              2.0  3.0  4.0  4.0  1.0  3.0  ---  0.0  2.0  0.0  0.0
A/CAEN/1/84                3.0  3.0  4.0  4.0  1.0  3.0  1.0  ---  2.0  0.0  1.0
A/MISSISSIPPI/1/85         2.0  2.0  4.0  3.0  0.0  2.0  2.0  1.0  ---  0.0  0.0
A/WASHINGTON/1/85          3.0  3.0  5.0  4.0  1.0  3.0  3.0  2.0  2.0  ---  1.0
A/NEWJERSEY/1/85           5.0  5.0  6.0  6.0  4.0  5.0  4.0  3.0  4.0  2.0  ---


(pp-hi-table (hi-table-to-log hi90))

A/LENINGRAD/360/86         ---  3.0  4.0  2.0  0.0  4.0  4.0  3.6  1.0  2.0  3.0  3.0  6.0  2.0
A/VICTORIA/07/87           2.4  ---  3.0  1.0  0.0  4.0  4.0  3.6  1.0  2.0  0.0  1.0  3.0  2.0
A/SICHUAN/02/87            5.0  5.0  ---  1.0  0.4  1.0  4.0  2.6  2.0  0.0  1.0  4.0  4.0  1.4
A/SHANGHAI/11/87           3.0  4.0  2.4  ---  0.0  3.0  4.0  2.6  1.0  0.0  0.0  3.0  3.0  2.0
A/ENGLAND/427/88           3.4  3.4  2.0  1.0  ---  2.0  2.0  1.6  0.0  0.0  1.0  3.0  4.0  0.0
A/CZECHOSLOVAKIA/19/89     4.0  5.0  0.0  1.0  0.0  ---  3.0  2.0  0.0  0.0  0.0  3.0  0.0  0.0
A/VICTORIA/5/89            1.0  1.4  0.4  0.0  0.0  1.0  ---  0.6  0.0  0.0  0.0  1.0  2.0  0.0
A/SICHUAN/68/89            2.0  2.4  1.0  0.0  0.0  1.0  1.0  ---  0.0  0.0  0.0  2.0  3.0  0.0
A/GUIZHOU/54/89            7.0  6.0  3.0  3.0  2.0  4.0  4.0  2.6  ---  0.0  2.0  4.0  6.0  0.0
A/SHANGHAI/16/89           6.4  6.0  3.0  2.4  1.0  3.0  4.0  2.6  1.0  ---  2.0  3.0  6.0  0.0
A/BEIJING/352/89           4.4  3.0  4.0  2.0  0.0  5.0  5.0  4.6  2.4  2.0  ---  2.0  2.0  2.0
A/BEIJING/337/89           4.0  2.0  3.0  2.0  0.4  4.0  5.0  4.6  2.0  3.0  1.0  ---  4.0  2.0
A/BEIJING/353/89           4.0  4.0  3.0  2.0  0.0  3.0  5.0  4.6  3.0  2.0  0.0  2.0  ---  2.0
A/GUANGDONG/39/89          6.0  6.0  3.0  2.0  1.0  3.0  4.0  2.6  0.0  0.0  1.4  3.0  5.0  ---


(pp-hi-table (hi-table-to-log hi92))

A/HONGKONG/8/68            ---  0.0  3.0  8.0  7.0  6.0  6.0  5.0  6.0  7.0
A/ENGLAND/42/72            3.0  ---  2.0  6.0  7.0  6.0  7.0  6.0  6.0  7.0
A/VICTORIA/3/75            9.0  4.0  ---  5.0  6.0  4.0  6.0  6.0  6.0  7.0
A/TEXAS/1/77               9.0  4.0  1.0  ---  2.0  0.0  1.0  5.0  5.0  7.0
A/BANGKOK/1/79             9.0  8.0  2.0  1.0  ---  0.0  0.0  4.0  5.0  7.0
A/PHILIPPINES/2/82         9.0  8.0  5.0  5.0  4.0  ---  2.0  5.0  5.0  7.0
A/MISSISSIPPI/1/85         9.0  8.0  3.0  3.0  3.0  0.0  ---  3.0  4.0  5.0
A/SHANGHAI/11/87           9.0  8.0  6.0  8.0  7.0  6.0  3.0  ---  1.0  5.0
A/BEIJING/353/89           9.0  8.0  6.0  8.0  7.0  6.0  7.0  1.0  ---  4.0
A/BEIJING/32/92            9.0  8.0  6.0  8.0  7.0  6.0  6.0  4.0  2.0  ---

|#

;;;---------------------------------------------------------------------------------------------
;;;                             HI TO LAPEDES MDS FORMAT
;;;---------------------------------------------------------------------------------------------

#|
As of April 14th, 2001, this handles <10, putting them in as 5,
and can also handle a table that we've already done -sr -ag to.
But the usual way to call this is without -ag -sr, and let it do that.
|#

(defun hi-table-to-lapedes (hi-table &key starting-coordss)
  (if (ag-sr-table-p hi-table)
      (setq hi-table 
	;;(hi-table-lt10s-to-5s      ;; used to do as sfi-format used 5 instead of <10, now handles <10. djs 2006-01-06
	 (unlog-hi-table
	  hi-table)
	;; )
	))	      
  (if (lapedes-data-is-distance-matrix-with-threshold hi-table)
      (print "this is a sequence somilarity table, should we preprocess it to send to lapedes?"))
  (let ((lapedes-hi-table (make-hi-table (mapcar #'international-strain-format-to-lapedes-strain-format
						 (hi-table-antigens hi-table))
					 (mapcar #'international-strain-format-to-lapedes-strain-format
						 (hi-table-sera hi-table))
					 (hi-table-values hi-table))))
    (append
     (lapedes-format-header lapedes-hi-table :num-dimensions (if starting-coordss (length (car starting-coordss)) 5))
     (lapedes-format-coords-required lapedes-hi-table starting-coordss)
     (lapedes-format-dists lapedes-hi-table))))

(defun lapedes-format-header (hi-table &key (num-dimensions 5))
  hi-table
  (list (format nil "n_dim=~d;" num-dimensions)
        (format nil "~%rank=0;")
        (format nil "~%energy=0;")))

(defun lapedes-append-_ag (antigen)
  (read-from-string (format nil "~a_ag" antigen)))

(defun lapedes-append-_sr (serum)
  (read-from-string (format nil "~a_sr" serum)))

(defun row-and-col-names-in-lapedes-compatible-form-p (hi-table)
  (or (equal (hi-table-antigens hi-table) (hi-table-sera hi-table))   ;; this is new, feb 2002, is it ok?
      (not (intersection (hi-table-antigens hi-table) (hi-table-sera hi-table)))  ;; no intersection in names
      (and (not (filter (^ (strain)                                                         ;; all row names end in -ag or -sr
			   (let ((string-strain (string strain)))
			     (and (>= (length string-strain) 2)
				  (or (string-equal "GA" (substring (reverse string-strain) 0 1))
				      (string-equal "RS" (substring (reverse string-strain) 0 1))))))
			(hi-table-antigens hi-table)))
	   (not (filter (^ (strain)                                                         ;; all col names end in -ag or -sr
			   (let ((string-strain (string strain)))
			     (and (>= (length string-strain) 2)
				  (or (string-equal "GA" (substring (reverse string-strain) 0 1))
				      (string-equal "RS" (substring (reverse string-strain) 0 1))))))
			(hi-table-sera hi-table))))))

;;this is the old way, it assumes square, and ag and sr are same names
;;(defun lapedes-format-coords-required (hi-table)
;;  (apply #'append
;;   (apply #'transpose
;;    (list
;;     (loop for antigen in (hi-table-antigens hi-table) collect
;;           (format nil "~%C ~a  {};" (lapedes-append-_ag antigen)))
;;     (loop for serum in (hi-table-sera hi-table) collect
;;           (format nil "~%C ~a  {};" (lapedes-append-_sr serum)))))))

(defun lapedes-format-coords-required (hi-table starting-coordss)
  (if (and starting-coordss
	   (not (equal (hi-table-antigens hi-table) (hi-table-sera hi-table))))
      (error "starting-coordss only implmented when table is square, because as that is all i needed so far"))
  (if (row-and-col-names-in-lapedes-compatible-form-p hi-table)
      (if (equal (hi-table-antigens hi-table) (hi-table-sera hi-table))   ;; case when we've already done -ag -sr
	  (loop for antigen in (hi-table-antigens hi-table) 
	      for i from 0 collect    
		(if starting-coordss
		    (format nil "~%C ~a  {~{~f,~}~f};" antigen 
			    (mapcar (^ (x) (coerce x 'short-float)) (butlast (nth i starting-coordss)))
			    (coerce (car (last (nth i starting-coordss))) 'short-float))
		  (format nil "~%C ~a  {};" antigen)))
	(append                                                         ;; need row and col names, they are distinct
	 (loop for antigen in (hi-table-antigens hi-table) collect
	       (format nil "~%C ~a  {};" antigen))
	 (loop for serum in (hi-table-sera hi-table) collect
	       (format nil "~%C ~a  {};" serum))))
    ;;we need to append _ag and _sr to make unique                      ;; here we make the row and col unique and have both
    (append
     (loop for antigen in (hi-table-antigens hi-table) collect
	   (format nil "~%C ~a  {};" (lapedes-append-_ag antigen)))
     (loop for serum in (hi-table-sera hi-table) collect
	   (format nil "~%C ~a  {};" (lapedes-append-_sr serum))))))

#|
;; from when <10's were converted into 5's above, and other thresholds were not handled (pre 20050106)
(defun lapedes-format-dists (hi-table)
  (let ((row-and-col-names-in-lapedes-compatable-form-p (row-and-col-names-in-lapedes-compatible-form-p hi-table))
	(hi-table-sera (hi-table-sera hi-table)))
    (loop for antigen in (hi-table-antigens hi-table)
        for row in (hi-table-row-values hi-table) append
	  (loop for serum in hi-table-sera 
	      for distance in row 
	      when (if (thresholdp distance)    ;; and <10s were converted into 5s above
		       (progn
			 (format t "thresholds other than <10, such as ~a, are not yet supported in sfi format -- making it a dont-care~%" distance)
			 nil)  ;; nil here, makes the if false, so no lapedes line is output -- which is a lapedes dont-care
		     (not (true-dont-care-p distance)))
	      collect
		(format nil "~%A ~a,~a ~f;" 
			(if row-and-col-names-in-lapedes-compatable-form-p
			    antigen
			  (lapedes-append-_AG antigen))
			(if row-and-col-names-in-lapedes-compatable-form-p
			    serum
			  (lapedes-append-_SR serum))
			distance)))))
|#

(defun lapedes-format-dists (hi-table)
  (let ((row-and-col-names-in-lapedes-compatable-form-p (row-and-col-names-in-lapedes-compatible-form-p hi-table))
	(hi-table-sera (hi-table-sera hi-table)))
    (loop for antigen in (hi-table-antigens hi-table)
        for row in (hi-table-row-values hi-table) append
	  (loop for serum in hi-table-sera 
	      for distance in row 
	      when (not (true-dont-care-p distance))
	      collect
		(format nil "~%A ~a,~a ~a;" 
			(if row-and-col-names-in-lapedes-compatable-form-p
			    antigen
			  (lapedes-append-_AG antigen))
			(if row-and-col-names-in-lapedes-compatable-form-p
			    serum
			  (lapedes-append-_SR serum))
			(if (thresholdp distance)
			    (format nil "~a" distance)
			  (format nil "~f" distance)))))))

(defun fi-lapedes-format (lines lapedes-filename)
  (fi lines
      lapedes-filename
      :supersede t :write-outer-list-elements-individually t :include-newline nil))

(defun save-file-to-lapedes-file (save-filename lapedes-filename)
  (let* ((save (fi-in save-filename))
	 (hi-table (extract-table-from-save save))
	 (starting-coordss (extract-best-coordss-from-save-or-starting-coords-if-no-batch-runs save)))
    (fi-lapedes-format (hi-table-to-lapedes hi-table :starting-coordss starting-coordss)
		       lapedes-filename)))

(defun save-to-lapedes-file (save lapedes-filename)
  (let* ((hi-table (extract-table-from-save save))
	 (starting-coordss (extract-best-coordss-from-save-or-starting-coords-if-no-batch-runs save)))
    (fi-lapedes-format (hi-table-to-lapedes hi-table :starting-coordss starting-coordss)
		       lapedes-filename)))
  
(defun msf-file-to-lapedes-file (msf-filename lapedes-filename)
  (save-to-lapedes-file (eval (fi-in msf-filename)) lapedes-filename))

(defun hi-table-file-to-lapedes-file (hi-table-filename lapedes-filename)
  (fi-lapedes-format
   (hi-table-to-lapedes 
    (read-hi-table-and-convert hi-table-filename 1 0))
   lapedes-filename))

#|
;;redefine for non sera = ag and non equal number of sera and antigen
;;just name the sera 0 thru n-1, and dont worry about the order of the coords
(defun lapedes-format-coords-required (hi-table)
  (apply #'append
    (list
     (loop for antigen in (hi-table-antigens hi-table) collect
           (format nil "~%C ~a  {};" (lapedes-append-_ag antigen)))
     (loop for serum in (series 0 (dec (hi-table-width hi-table))) collect
           (format nil "~%C ~a  {};" (lapedes-append-_sr serum))))))

;;again a redefine for non symetric tables
(defun lapedes-format-dists (hi-table)
  (loop for antigen in (hi-table-antigens hi-table)
        for row in (hi-table-row-values hi-table) append
        (loop for serum in (series 0 (dec (hi-table-width hi-table)))
              for distance in row collect
              (format nil "~%A ~a,~a ~f;" 
                      (lapedes-append-_AG antigen)
                      (lapedes-append-_SR serum)
                      distance))))
|#


#|
(fi (hi-table-to-lapedes hi77) "hi77.mds" :overwrite t)
(fi (hi-table-to-lapedes hi85) "hi85.mds" :overwrite t)
(fi (hi-table-to-lapedes hi90) "hi90.mds" :overwrite t)

(hi-table-to-lapedes '((a 1280 40) (b 20 640)))
|#


#|
;;this is sample from Alan Lapedes
n_dim=3;
rank=0;
energy=0;
C HK68_ag       {};
C HK68_sr       {};
C ENG72_ag      {};
C ENG72_sr      {};
C PC73_ag       {};
C PC73_sr       {};
C VIC75_ag      {};
C VIC75_sr      {};
A HK68_ag,HK68_sr 0.0;
A HK68_ag,ENG72_sr 0.5;
A HK68_ag,PC73_sr 4.0;
A HK68_ag,VIC75_sr 5.5;
A ENG72_ag,HK68_sr 1.0;
A ENG72_ag,ENG72_sr 0.0;
A ENG72_ag,PC73_sr 1.0;
A ENG72_ag,VIC75_sr 4.0;
A PC73_ag,HK68_sr 5.0;
A PC73_ag,ENG72_sr 3.0;
A PC73_ag,PC73_sr 0.0;
A PC73_ag,VIC75_sr 3.0;
A VIC75_ag,HK68_sr 4.5;
A VIC75_ag,ENG72_sr 4.0;
A VIC75_ag,PC73_sr 5.0;
A VIC75_ag,VIC75_sr 0.0;
|#


#|
(lapedes-to-hi-table (fi-in-readline "mds/investigations/merge-hi-tables/tab23.lapedes"))
(lapedes-to-hi-table (fi-in-readline "mds/investigations/merge-hi-tables/nov-2001-merge-with-sequence-5s-and-starting-coords.lapedes"))
|#

(defun lapedes-to-hi-table (lines)
  (let* ((stress (lapedes-stress lines))
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
       stress))))

(defun lapedes-data-is-distance-matrix-with-threshold (table)
  ;; just make true for now as we do not know how we will detect
  table
  30
  nil)

(defun distance-table-to-similarity-table-with-threshold (table gt-threshold)
  (f-hi-table
   (^ (value ag sr)
      (if (dont-care-p value)
	  (if (equal ag sr)
	      ;; alan has not included the similarity
	      gt-threshold
	    value)
	(let ((similarity (- gt-threshold value)))
	  (if (minusp similarity)
	      '<10
	    similarity))))
   table
   :pass-ag-sr t))

(defun lapedes-A-line-p (line)
  (and (>= (length line) 1) (eql #\A (aref line 0)) (eql #\space (aref line 1))))

(defun lapedes-C-line-p (line)
  (and (>= (length line) 1) (eql #\C (aref line 0)) (eql #\space (aref line 1))))

(defun lapedes-C-line-name (line)
  (lapedes-strain-format-to-international-strain-format
    (read-from-string 
     (substring-before-char #\space (substring-after-char #\space line)))))

(defun lapedes-stress (lapedes-lines)
  (string->number (substring-after-char #\= (substring-before-char #\; (nth 2 lapedes-lines)))))


(defun lapedes-strain-format-to-international-strain-format (A_place_id_year)
  (setq A_place_id_year (string A_place_id_year))
  (let ((ans (make-string (length A_place_id_year))))
    (loop for i below (length A_place_id_year) do
          (setf (aref ans i) (aref A_place_id_year i)))
    (loop for i below 3 do
          (if (position #\_ ans)
	      (if (= (position #\_ ans) (- (length ans) 3))
		  (setf (aref ans (position #\_ ans)) #\-)
		(setf (aref ans (position #\_ ans)) #\/))))
    (setq ans (read-from-string ans))
    (if (eql 'AT/3572/5/88_AG ans)
	'AT/3572-5/88-AG
      ans)
    ))

(defun international-strain-format-to-lapedes-strain-format (A/place/id/year)
  (setq A/place/id/year (string A/place/id/year))
  (let ((ans (make-string (length A/place/id/year))))
    (loop for i below (length A/place/id/year) do
          (setf (aref ans i) (aref A/place/id/year i)))
    (loop for i below 3 do
          (if (position #\/ ans) (setf (aref ans (position #\/ ans)) #\_)))
    (read-from-string (string-subst #\- #\_ ans))))

(defun lapedes-C-line-coords (line)
  (string-to-atoms
   (string-subst
    #\, #\space
    (substring-before-char #\} (substring-after-char #\{ line)))))

(defun lapedes-A-line-point-1 (line)
  (lapedes-strain-format-to-international-strain-format
   (read-from-string (substring-before-char #\, (substring-after-char #\space line)))))

(defun lapedes-A-line-point-2 (line)
  (lapedes-strain-format-to-international-strain-format
   (read-from-string (substring-before-char #\space (substring-after-char #\, line)))))

(defun lapedes-A-line-numeric (line)
  (read-from-string 
   (substring-before-char #\; 
                          (substring-after-char #\space
                                                (substring-after-char #\space line)))))




(defun lapedes-file-input-common (filename)
  (multiple-value-bind (table starting-coordss stress)
      (lapedes-to-hi-table (fi-in-readline filename))
    (if (or (ag-sr-table-p table)
	    (lapedes-data-is-distance-matrix-with-threshold table))
	(if starting-coordss
	    (setq starting-coordss
	      (append
	       starting-coordss
	       (make-coordss-more table)))))
    (values
     table
     starting-coordss
     stress)))

(defun lapedes-file-to-coordss-and-stress (filename)
  (multiple-value-bind (table starting-coordss stress)
      (lapedes-file-input-common filename)
    table  ;; not used
    (values
     starting-coordss
     stress)))

(defun lapedes-file-to-save-format-sexp (filename)
  (multiple-value-bind (table starting-coordss)
      (lapedes-file-input-common filename)
    (make-save-form :hi-table table
		    :starting-coordss starting-coordss)))

(defun lapedes-file-to-save-file (lapedes-filename save-filename)
  (fi (lapedes-file-to-save-format-sexp lapedes-filename) save-filename))


#|
(progn 
  (setq hi77laa (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi77.lapedes") 
		 hi77 'antigens 'antigens))
  (setq hi85laa (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi85.lapedes") 
		 hi85 'antigens 'antigens))
  (setq hi90laa (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi90.lapedes")
		 hi90 'antigens 'antigens)))
|#

#|
our system is a little assymetric, as the lapedes-to-hi-table takes a filename, whereas
hi-table-to-lapedes takes a list-of-lists

(hi-table-to-lapedes (lapedes-to-hi-table-A-line))

(lapedes-to-hi-table-A-lines (fi-in-readline "Macintosh HD:Desktop Folder:lapedes-eg"))
(lapedes-to-hi-table-C-lines (fi-in-readline "Macintosh HD:Desktop Folder:lapedes-eg"))
(lapedes-to-hi-table-C-lines (fi-in-readline "Macintosh HD:Desktop Folder:lapedes-85"))

(pp-hi-table hi77laa)
A/HONGKONG/8/68            --- 1338  742  397 1075  978 1375 1153 1184 1144 1165 1248  991 1312 2179 1754  925 1616
A/HONGKONG/107/71         1338  --- 1261 1566 1578 1350 2137 1483 1734 2230 1364 1709 1925 1803 2434 1851 1166 2525
A/ENGLAND/42/72            742 1261  --- 1018  584  263 1349  721  767 1623  905 1118 1426 1215 1988 1493  857 2071
A/HONGKONG/5/72            397 1566 1018  --- 1231 1212 1233 1301 1283  823 1412 1340  782 1357 2393 2038 1270 1490
A/HANNOVER/61/73          1075 1578  584 1231  ---  467  961 1080  208 1596 1396 1532 1712 1594 2085 1774 1297 2110
A/PORTCHALMERS/1/73        978 1350  263 1212  467  --- 1314  691  646 1740  975 1172 1575 1261 1971 1506 1012 2192
A/ENGLAND/635/74          1375 2137 1349 1233  961 1314  --- 1635  797 1102 2035 1933 1653 1918 2590 2459 1966 1886
A/PR/1/74                 1153 1483  721 1301 1080  691 1635  --- 1193 1847  673  594 1317  661 2118 1564 1171 2312
A/SCOTLAND/840/74         1184 1734  767 1283  208  646  797 1193  --- 1546 1552 1633 1752 1678 2165 1913 1471 2102
A/MAYOCLINIC/4/75         1144 2230 1623  823 1596 1740 1102 1847 1546  --- 1996 1820 1015 1790 2526 2418 1855 1062
A/HONGKONG/9/75           1165 1364  905 1412 1396  975 2035  673 1552 1996  ---  600 1362  762 1721 1049  729 2167
A/VICTORIA/3/75           1248 1709 1118 1340 1532 1172 1933  594 1633 1820  600  --- 1035  168 2048 1509 1211 2134
A/ENGLAND/864/75           991 1925 1426  782 1712 1575 1653 1317 1752 1015 1362 1035  ---  982 2392 2055 1507 1473
A/TOKYO/1/75              1312 1803 1215 1357 1594 1261 1918  661 1678 1790  762  168  982  --- 2153 1649 1355 2144
A/BRAZIL/25/76            2179 2434 1988 2393 2085 1971 2590 2118 2165 2526 1721 2048 2392 2153  ---  797 1525 2025
A/BRAZIL/31/76            1754 1851 1493 2038 1774 1506 2459 1564 1913 2418 1049 1509 2055 1649  797  ---  933 2159
A/ALLEGCO/29/76            925 1166  857 1270 1297 1012 1966 1171 1471 1855  729 1211 1507 1355 1525  933  --- 1875
A/VICTORIA/112/76         1616 2525 2071 1490 2110 2192 1886 2312 2102 1062 2167 2134 1473 2144 2025 2159 1875  ---

(pp-hi-table hi85laa)
A/OREGON/4/80              ---   81   65  129  165  128  134  135  117  166  293
A/BANGKOK/1/79              81  ---  136   50  175   67  142  142  139  183  322
A/BANGKOK/2/79              65  136  ---  182  191  187  191  192  168  206  319
A/SHANGHAI/31/80           129   50  182  ---  179   74  167  167  173  217  356
A/PHILIPPINES/2/82_EQ      165  175  191  179  ---  223  207  211  222  295  409
A/TAIWAN/16/83             128   67  187   74  223  ---  124  123  125  152  290
A/PANAMA/1/83              134  142  191  167  207  124  ---    4   40  105  215
A/CAEN/1/84                135  142  192  167  211  123    4  ---   38  102  212
A/MISSISSIPPI/1/85         117  139  168  173  222  125   40   38  ---   76  195
A/WASHINGTON/1/85          166  183  206  217  295  152  105  102   76  ---  140
A/NEWJERSEY/1/85           293  322  319  356  409  290  215  212  195  140  ---

(pp-hi-table hi90laa)
A/LENINGRAD/360/86         ---  174  203  185  148  216  138  149  200  191  215  197  234  196
A/VICTORIA/07/87           174  ---  143   85  121  174  115  148  219  196   60  153  112  204
A/SICHUAN/02/87            203  143  ---   65   84  159  140  163  189  153  123  235  158  173
A/SHANGHAI/11/87           185   85   65  ---   74  171  128  159  202  166   63  208  134  186
A/ENGLAND/427/88           148  121   84   74  ---  177  116  139  147  107  119  221  186  136
A/CZECHOSLOVAKIA/19/89     216  174  159  171  177  ---   90   83  172  183  190  136  123  155
A/VICTORIA/5/89            138  115  140  128  116   90  ---   35  139  140  147  116  131  124
A/SICHUAN/68/89            149  148  163  159  139   83   35  ---  125  139  178  120  154  112
A/GUIZHOU/54/89            200  219  189  202  147  172  139  125  ---   54  230  236  253   24
A/SHANGHAI/16/89           191  196  153  166  107  183  140  139   54  ---  199  247  243   50
A/BEIJING/352/89           215   60  123   63  119  190  147  178  230  199  ---  200  124  214
A/BEIJING/337/89           197  153  235  208  221  136  116  120  236  247  200  ---  132  222
A/BEIJING/353/89           234  112  158  134  186  123  131  154  253  243  124  132  ---  234
A/GUANGDONG/39/89          196  204  173  186  136  155  124  112   24   50  214  222  234  ---

|#

#|
(progn 
  (setq hi77las (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi77.lapedes") 
		 hi77 'antigens 'sera))
  (setq hi85las (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi85.lapedes") 
		 hi85 'antigens 'sera))
  (setq hi90las (lapedes-to-hi-table-C-lines 
		 (fi-in-readline "~dsmith/mds/data/hi90.lapedes")
		 hi90 'antigens 'sera)))
|#

#|
(pp-hi-table hi77las)
A/HONGKONG/8/68           1401 1411 1351 1412 1454 1452 1452 1522 1532 1582 1539 1563 2545 1430 1411 1374 1520 1429
A/HONGKONG/107/71         2558 1401 2458 1397 1517 1608 1522 2560 2558 1929 2491 1774 2860 2386 2365 2372 1937 1723
A/ENGLAND/42/72           1517 1458 1352 1454 1401 1440 1434 1414 1499 1563 1555 1682 2554 1413 1406 1397 1500 1398
A/HONGKONG/5/72           1454 1363 1413 1366 1429 1413 1412 1517 1516 1598 1623 1521 2454 1498 1489 1464 1518 1409
A/PORTCHALMERS/1/73       1654 1460 1411 1455 1366 1413 1409 1454 1505 1551 1615 1705 2557 1451 1454 1462 1480 1368
A/ENGLAND/635/74          2006 1520 1605 1517 1455 1410 1472 1508 1510 1775 1977 1789 2856 1712 1753 1771 1593 1404
A/PR/1/74                 1589 1459 1517 1455 1372 1454 1397 1544 1521 1485 1575 1644 1960 1490 1486 1524 1476 1415
A/SCOTLAND/840/74         1832 1521 1428 1515 1409 1409 1454 1397 1454 1665 1767 1794 2847 1523 1550 1559 1523 1366
A/MAYOCLINIC/4/75         1771 1512 1454 1517 1541 1454 1517 1774 1398 1608 1678 1520 2560 1504 1521 1517 1477 1414
A/HONGKONG/9/75           1678 1519 1537 1519 1454 1520 1477 1949 1617 1351 1410 1532 1989 1405 1373 1401 1411 1444
A/VICTORIA/3/75           1522 1519 1479 1520 1452 1517 1459 1772 1452 1350 1365 1518 1520 1367 1350 1398 1399 1443
A/ENGLAND/864/75          1480 1412 1452 1419 1452 1444 1420 1770 1413 1414 1451 1366 1771 1410 1399 1416 1397 1403
A/TOKYO/1/75              1550 1519 1520 1519 1453 1517 1455 1773 1454 1367 1413 1519 1397 1411 1399 1454 1411 1451
A/BRAZIL/25/76            2562 2128 1774 2131 1962 1912 2008 2892 1775 1396 1451 1775 2846 1421 1401 1451 1450 1700
A/BRAZIL/31/76            2209 1895 1685 1896 1770 1776 1812 2562 1774 1368 1401 1685 2557 1410 1371 1403 1451 1616
A/ALLEGCO/29/76           1776 1518 1513 1519 1497 1516 1520 2069 1704 1410 1455 1521 2557 1410 1369 1353 1432 1440
A/VICTORIA/112/76         2180 1760 1573 1770 1758 1626 1740 2498 1522 1443 1521 1415 2722 1413 1411 1413 1370 1495
A/HANNOVER/61/73          1774 1501 1413 1495 1400 1411 1445 1410 1482 1640 1723 1772 2818 1500 1517 1521 1517 1368

(pp-hi-table hi85las)
A/OREGON/4/80               14  121  133  114  112  190  236  160  124  123  234
A/BANGKOK/1/79              68  102  210   62  168  125  229  197  192  188  298
A/BANGKOK/2/79              75  134   76  146  133  237  293  199  133  134  238
A/SHANGHAI/31/80           117  130  257   73  201  117  233  228  235  234  339
A/PHILIPPINES/2/82_EQ      169  252  236  203  131  293  236  218  202  231  301
A/TAIWAN/16/83             114  133  255  110  202   85  205  194  215  202  306
A/PANAMA/1/83              132  218  234  203  132  204  115   74  133  126  200
A/CAEN/1/84                132  217  235  203  136  202  117   76  135  126  201
A/MISSISSIPPI/1/85         116  199  207  196  131  199  153   77  114   97  184
A/WASHINGTON/1/85          163  213  236  229  200  201  201  130  162  129  201
A/NEWJERSEY/1/85           293  346  322  368  294  331  267  199  237  205  199

(pp-hi-table hi90las)
A/LENINGRAD/360/86         107  156  208  170  162  189  157  169  137  204  278  219  274  199
A/VICTORIA/07/87           139  119  206  141  156  194  165  171  130  180  167  157  164  189
A/SICHUAN/02/87            176  223  137  141  174  123  160  151  146  141  181  250  177  174
A/SHANGHAI/11/87           154  176  173  137  160  159  156  153  136  153  172  213  167  177
A/ENGLAND/427/88           155  174  172  141  157  153  136  128  110  139  201  214  199  153
A/CZECHOSLOVAKIA/19/89     172  208  120  150  155  111  142  139  106  124  121  216  119  137
A/VICTORIA/5/89            118  139  139  115  121  122  107  109   38  120  152  164  150  121
A/SICHUAN/68/89            137  155  145  136  129  128  109  108   41  118  158  176  157  112
A/GUIZHOU/54/89            232  230  196  192  208  179  168  148  118  152  221  225  227  142
A/SHANGHAI/16/89           224  225  188  174  207  171  168  148  122  152  221  222  225  152
A/BEIJING/352/89           174  157  219  169  171  208  177  177  155  177  156  199  154  193
A/BEIJING/337/89           156  152  206  170  175  200  186  199  141  208  179  153  177  202
A/BEIJING/353/89           162  183  176  157  166  172  181  188  157  179  118  204  110  197
A/GUANGDONG/39/89          223  225  177  171  203  161  165  145  108  147  207  210  213  142

|#

(defun scale-lapedes-readlines (readlines scale)
  ;;scales dists, but not coords
  ;;could scale coords, but then i'd have to write it, not short-cut like i am below
  (hi-table-to-lapedes (scale-hi-table (lapedes-to-hi-table-C-lines readlines) scale)))


;;;---------------------------------------------------------------------------------------------
;;; AVERAGING HI DISTANCES
;;;---------------------------------------------------------------------------------------------

(defun average-hi-table (hi-table)
  (if (not (= (length (hi-table-row-values hi-table))
              (length (hi-table-column-values hi-table))))
    (error "Need square HI table to average across the diagonal"))
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (loop for i below (length (hi-table-row-values hi-table)) collect
         (loop for j below (length (hi-table-column-values hi-table)) collect
               (av (list (nth i (nth j (hi-table-row-values hi-table)))
                         (nth i (nth j (hi-table-column-values hi-table)))))))))

#|
(progn
  (setq hi77a (average-hi-table (hi-table-to-log hi77)))
  (setq hi85a (average-hi-table (hi-table-to-log hi85)))
  (setq hi90a (average-hi-table (hi-table-to-log hi90)))
  (setq hi92a (average-hi-table (hi-table-to-log hi92))))
|#

#|
(pp-hi-table (setq hi77a (average-hi-table (hi-table-to-log hi77))))
A/HONGKONG/8/68            ---  2.0  1.0  2.5  3.0  3.5  2.5  2.5  4.0  3.5  4.0  4.0  3.5  4.0  2.5  2.5  2.5  4.0
A/HONGKONG/107/71          2.0  ---  4.0  0.5  3.0  3.0  3.0  3.0  4.5  3.5  4.5  4.5  3.5  4.5  3.5  4.5  4.0  4.0
A/ENGLAND/42/72            1.0  4.0  ---  3.5  1.5  2.0  3.0  3.0  2.5  3.5  5.0  4.5  4.0  5.0  3.0  3.0  2.5  4.0
A/HONGKONG/5/72            2.5  0.5  3.5  ---  2.5  3.5  2.5  3.0  3.5  3.5  4.5  5.0  4.0  5.0  3.5  4.5  4.5  4.0
A/HANNOVER/61/73           3.0  3.0  1.5  2.5  ---  1.0  0.5  2.0  0.5  2.5  3.5  3.5  3.0  4.0  2.5  3.5  3.5  3.0
A/PORTCHALMERS/1/73        3.5  3.0  2.0  3.5  1.0  ---  2.5  1.0  2.5  4.0  4.5  4.0  4.0  4.5  3.0  4.0  4.0  4.0
A/ENGLAND/635/74           2.5  3.0  3.0  2.5  0.5  2.5  ---  2.5  1.5  2.5  4.5  4.0  3.5  4.0  3.0  4.0  3.5  3.5
A/PR/1/74                  2.5  3.0  3.0  3.0  2.0  1.0  2.5  ---  3.5  3.5  3.5  4.0  3.5  4.0  3.0  4.0  4.5  3.5
A/SCOTLAND/840/74          4.0  4.5  2.5  3.5  0.5  2.5  1.5  3.5  ---  4.0  5.0  4.5  5.0  5.5  4.5  5.0  5.0  4.5
A/MAYOCLINIC/4/75          3.5  3.5  3.5  3.5  2.5  4.0  2.5  3.5  4.0  ---  4.5  3.5  3.5  4.5  3.0  4.5  4.0  3.5
A/HONGKONG/9/75            4.0  4.5  5.0  4.5  3.5  4.5  4.5  3.5  5.0  4.5  ---  1.0  4.0  3.0  0.5  0.5  2.0  2.0
A/VICTORIA/3/75            4.0  4.5  4.5  5.0  3.5  4.0  4.0  4.0  4.5  3.5  1.0  ---  3.5  3.5  1.5  1.0  2.5  3.0
A/ENGLAND/864/75           3.5  3.5  4.0  4.0  3.0  4.0  3.5  3.5  5.0  3.5  4.0  3.5  ---  4.5  3.0  3.0  4.0  1.5
A/TOKYO/1/75               4.0  4.5  5.0  5.0  4.0  4.5  4.0  4.0  5.5  4.5  3.0  3.5  4.5  ---  3.0  2.5  4.5  3.5
A/BRAZIL/25/76             2.5  3.5  3.0  3.5  2.5  3.0  3.0  3.0  4.5  3.0  0.5  1.5  3.0  3.0  ---  0.5  1.5  1.0
A/BRAZIL/31/76             2.5  4.5  3.0  4.5  3.5  4.0  4.0  4.0  5.0  4.5  0.5  1.0  3.0  2.5  0.5  ---  1.0  1.5
A/ALLEGCO/29/76            2.5  4.0  2.5  4.5  3.5  4.0  3.5  4.5  5.0  4.0  2.0  2.5  4.0  4.5  1.5  1.0  ---  2.5
A/VICTORIA/112/76          4.0  4.0  4.0  4.0  3.0  4.0  3.5  3.5  4.5  3.5  2.0  3.0  1.5  3.5  1.0  1.5  2.5  ---


(pp-hi-table (setq hi85a (average-hi-table (hi-table-to-log hi85))))
A/OREGON/4/80              ---  0.5  1.5  1.5  1.5  1.5  3.0  3.0  1.5  1.5  3.0
A/BANGKOK/1/79             0.5  ---  3.0  0.5  2.5  1.5  3.0  3.0  2.0  2.0  4.0
A/BANGKOK/2/79             1.5  3.0  ---  4.0  3.0  4.5  4.0  3.5  3.0  3.0  4.0
A/SHANGHAI/31/80           1.5  0.5  4.0  ---  3.0  1.0  3.5  4.0  3.0  3.0  4.5
A/PHILIPPINES/2/82_EQ      1.5  2.5  3.0  3.0  ---  3.0  2.5  2.5  1.5  1.5  3.5
A/TAIWAN/16/83             1.5  1.5  4.5  1.0  3.0  ---  3.0  3.0  2.5  2.5  4.0
A/PANAMA/1/83              3.0  3.0  4.0  3.5  2.5  3.0  ---  0.5  2.0  1.5  2.0
A/CAEN/1/84                3.0  3.0  3.5  4.0  2.5  3.0  0.5  ---  1.5  1.0  2.0
A/MISSISSIPPI/1/85         1.5  2.0  3.0  3.0  1.5  2.5  2.0  1.5  ---  1.0  2.0
A/WASHINGTON/1/85          1.5  2.0  3.0  3.0  1.5  2.5  1.5  1.0  1.0  ---  1.5
A/NEWJERSEY/1/85           3.0  4.0  4.0  4.5  3.5  4.0  2.0  2.0  2.0  1.5  ---

(pp-hi-table (setq hi90a (average-hi-table (hi-table-to-log hi90))))
A/LENINGRAD/360/86         ---  2.7  4.5  2.5  1.7  4.0  2.5  2.8  4.0  4.2  3.7  3.5  5.0  4.0
A/VICTORIA/07/87           2.7  ---  4.0  2.5  1.7  4.5  2.7  3.0  3.5  4.0  1.5  1.5  3.5  4.0
A/SICHUAN/02/87            4.5  4.0  ---  1.7  1.2  0.5  2.2  1.8  2.5  1.5  2.5  3.5  3.5  2.2
A/SHANGHAI/11/87           2.5  2.5  1.7  ---  0.5  2.0  2.0  1.3  2.0  1.2  1.0  2.5  2.5  2.0
A/ENGLAND/427/88           1.7  1.7  1.2  0.5  ---  1.0  1.0  0.8  1.0  0.5  0.5  1.7  2.0  0.5
A/CZECHOSLOVAKIA/19/89     4.0  4.5  0.5  2.0  1.0  ---  2.0  1.5  2.0  1.5  2.5  3.5  1.5  1.5
A/VICTORIA/5/89            2.5  2.7  2.2  2.0  1.0  2.0  ---  0.8  2.0  2.0  2.5  3.0  3.5  2.0
A/SICHUAN/68/89            2.8  3.0  1.8  1.3  0.8  1.5  0.8  ---  1.3  1.3  2.3  3.3  3.8  1.3
A/GUIZHOU/54/89            4.0  3.5  2.5  2.0  1.0  2.0  2.0  1.3  ---  0.5  2.2  3.0  4.5  0.0
A/SHANGHAI/16/89           4.2  4.0  1.5  1.2  0.5  1.5  2.0  1.3  0.5  ---  2.0  3.0  4.0  0.0
A/BEIJING/352/89           3.7  1.5  2.5  1.0  0.5  2.5  2.5  2.3  2.2  2.0  ---  1.5  1.0  1.7
A/BEIJING/337/89           3.5  1.5  3.5  2.5  1.7  3.5  3.0  3.3  3.0  3.0  1.5  ---  3.0  2.5
A/BEIJING/353/89           5.0  3.5  3.5  2.5  2.0  1.5  3.5  3.8  4.5  4.0  1.0  3.0  ---  3.5
A/GUANGDONG/39/89          4.0  4.0  2.2  2.0  0.5  1.5  2.0  1.3  0.0  0.0  1.7  2.5  3.5  ---

(pp-hi-table (setq hi92a (average-hi-table (hi-table-to-log hi92))))
A/HONGKONG/8/68            ---  1.5  6.0  8.5  8.0  7.5  7.5  7.0  7.5  8.0
A/ENGLAND/42/72            1.5  ---  3.0  5.0  7.5  7.0  7.5  7.0  7.0  7.5
A/VICTORIA/3/75            6.0  3.0  ---  3.0  4.0  4.5  4.5  6.0  6.0  6.5
A/TEXAS/1/77               8.5  5.0  3.0  ---  1.5  2.5  2.0  6.5  6.5  7.5
A/BANGKOK/1/79             8.0  7.5  4.0  1.5  ---  2.0  1.5  5.5  6.0  7.0
A/PHILIPPINES/2/82         7.5  7.0  4.5  2.5  2.0  ---  1.0  5.5  5.5  6.5
A/MISSISSIPPI/1/85         7.5  7.5  4.5  2.0  1.5  1.0  ---  3.0  5.5  5.5
A/SHANGHAI/11/87           7.0  7.0  6.0  6.5  5.5  5.5  3.0  ---  1.0  4.5
A/BEIJING/353/89           7.5  7.0  6.0  6.5  6.0  5.5  5.5  1.0  ---  3.0
A/BEIJING/32/92            8.0  7.5  6.5  7.5  7.0  6.5  5.5  4.5  3.0  ---

|#


#|
(progn
  (setq hi77lasa (average-hi-table hi77las))
  (setq hi85lasa (average-hi-table hi85las))
  (setq hi90lasa (average-hi-table hi90las))
  '(setq hi92lasa (average-hi-table hi92las)))
|#

#|
(pp-hi-table hi77lasa)
A/HONGKONG/8/68           1401 1984 1434 1433 1554 1729 1521 1677 1652 1630 1530 1521 2047 1996 1810 1575 1850 1602
A/HONGKONG/107/71         1984 1401 1958 1380 1488 1564 1490 2041 2035 1724 2005 1593 2189 2257 2130 1945 1848 1612
A/ENGLAND/42/72           1434 1958 1352 1434 1406 1522 1475 1421 1477 1550 1517 1567 2037 1594 1545 1455 1536 1405
A/HONGKONG/5/72           1433 1380 1434 1366 1442 1465 1434 1516 1517 1558 1571 1470 1987 1814 1693 1492 1644 1452
A/PORTCHALMERS/1/73       1554 1488 1406 1442 1366 1434 1390 1432 1523 1503 1533 1578 2005 1707 1612 1479 1619 1384
A/ENGLAND/635/74          1729 1564 1522 1465 1434 1410 1463 1458 1482 1647 1747 1616 2187 1812 1764 1644 1610 1407
A/PR/1/74                 1521 1490 1475 1434 1390 1463 1397 1499 1519 1481 1517 1532 1708 1749 1649 1522 1608 1430
A/SCOTLAND/840/74         1677 2041 1421 1516 1432 1458 1499 1397 1614 1807 1769 1782 2310 2208 2056 1814 2011 1388
A/MAYOCLINIC/4/75         1652 2035 1477 1517 1523 1482 1519 1614 1398 1612 1565 1467 2007 1639 1647 1611 1499 1448
A/HONGKONG/9/75           1630 1724 1550 1558 1503 1647 1481 1807 1612 1351 1380 1473 1678 1400 1371 1405 1427 1542
A/VICTORIA/3/75           1530 2005 1517 1571 1533 1747 1517 1769 1565 1380 1365 1485 1467 1409 1376 1426 1460 1583
A/ENGLAND/864/75          1521 1593 1567 1470 1578 1616 1532 1782 1467 1473 1485 1366 1645 1593 1542 1469 1406 1587
A/TOKYO/1/75              2047 2189 2037 1987 2005 2187 1708 2310 2007 1678 1467 1645 1397 2128 1978 2005 2067 2134
A/BRAZIL/25/76            1996 2257 1594 1814 1707 1812 1749 2208 1639 1400 1409 1593 2128 1421 1405 1430 1432 1600
A/BRAZIL/31/76            1810 2130 1545 1693 1612 1764 1649 2056 1647 1371 1376 1542 1978 1405 1371 1386 1431 1567
A/ALLEGCO/29/76           1575 1945 1455 1492 1479 1644 1522 1814 1611 1405 1426 1469 2005 1430 1386 1353 1422 1480
A/VICTORIA/112/76         1850 1848 1536 1644 1619 1610 1608 2011 1499 1427 1460 1406 2067 1432 1431 1422 1370 1506
A/HANNOVER/61/73          1602 1612 1405 1452 1384 1407 1430 1388 1448 1542 1583 1587 2134 1600 1567 1480 1506 1368

(pp-hi-table hi85lasa)
A/OREGON/4/80               14   94  104  116  141  152  184  146  120  143  264
A/BANGKOK/1/79              94  102  172   96  210  129  224  207  196  201  322
A/BANGKOK/2/79             104  172   76  202  185  246  264  217  170  185  280
A/SHANGHAI/31/80           116   96  202   73  202  114  218  216  215  231  354
A/PHILIPPINES/2/82_EQ      141  210  185  202  131  247  184  177  167  215  297
A/TAIWAN/16/83             152  129  246  114  247   85  204  198  207  202  319
A/PANAMA/1/83              184  224  264  218  184  204  115   96  143  163  233
A/CAEN/1/84                146  207  217  216  177  198   96   76  106  128  200
A/MISSISSIPPI/1/85         120  196  170  215  167  207  143  106  114  130  210
A/WASHINGTON/1/85          143  201  185  231  215  202  163  128  130  129  203
A/NEWJERSEY/1/85           264  322  280  354  297  319  233  200  210  203  199

(pp-hi-table hi90lasa)
A/LENINGRAD/360/86         107  148  192  162  158  181  137  153  184  214  226  187  218  211
A/VICTORIA/07/87           148  119  214  158  165  201  152  163  180  203  162  154  173  207
A/SICHUAN/02/87            192  214  137  157  173  122  150  148  171  165  200  228  176  175
A/SHANGHAI/11/87           162  158  157  137  150  155  136  145  164  164  170  191  162  174
A/ENGLAND/427/88           158  165  173  150  157  154  129  128  159  173  186  194  183  178
A/CZECHOSLOVAKIA/19/89     181  201  122  155  154  111  132  134  143  148  165  208  145  149
A/VICTORIA/5/89            137  152  150  136  129  132  107  109  103  144  164  175  166  143
A/SICHUAN/68/89            153  163  148  145  128  134  109  108   94  133  167  188  173  129
A/GUIZHOU/54/89            184  180  171  164  159  143  103   94  118  137  188  183  192  125
A/SHANGHAI/16/89           214  203  165  164  173  148  144  133  137  152  199  215  202  150
A/BEIJING/352/89           226  162  200  170  186  165  164  167  188  199  156  189  136  200
A/BEIJING/337/89           187  154  228  191  194  208  175  188  183  215  189  153  190  206
A/BEIJING/353/89           218  173  176  162  183  145  166  173  192  202  136  190  110  205
A/GUANGDONG/39/89          211  207  175  174  178  149  143  129  125  150  200  206  205  142

|#

;;;---------------------------------------------------------------------------------------------
;;;                                 v1 v2 e DISTANCES
;;;---------------------------------------------------------------------------------------------

(defun cos-included-angle (a b c)
  (if (zerop (* 2 a b))
      0
    (float
     (/ (- (+ (square a) (square b)) (square c))
        (* 2 a b)))))

(defun separation-triangle-coords (pAg-bAg oAg-pAg oAg-bAg)
  (let ((cos-oAg-pAg-bAg (cos-included-angle pAg-bAg oAg-pAg oAg-bAg)))
    (list (list 0 0)
          (list pAg-bAg 0)
          (list (* oAg-pAg cos-oAg-pAg-bAg) (* oAg-pAg (sin (acos cos-oAg-pAg-bAg)))))))

(defun ag-sr-dist (antigen serum hi-table)
  (nth (position serum (hi-table-sera hi-table))
       (nth (position antigen (hi-table-antigens hi-table)) 
            (hi-table-row-values hi-table))))

(defun v1-v2-e-dists (v1 v2 e hi-table)
  ;;in the form v2-e;v1-e,v1-v2
  ;;or for lisp ((v2-e) (v1-e v1-v2))
  (list
   (list (ag-sr-dist  e v2 hi-table))
   (list (ag-sr-dist  e v1 hi-table)
         (ag-sr-dist v2 v1 hi-table)
         (strain-abbreviation v1))))

(defun dists-to-v1s (v2 e hi-table)
  (loop for v1 in (hi-table-antigens hi-table) collect
        (v1-v2-e-dists v1 v2 e hi-table)))

(defun plot-triangle (coords &optional refresh)
  (plot-polygon coords refresh))

(defun plot-square (x y radius &key refresh window)
  (plot-polygon (list (list (- x radius) (- y radius))
                      (list (+ x radius) (- y radius))
                      (list (+ x radius) (+ y radius))
                      (list (- x radius) (+ y radius)))
                :refresh refresh
                :window window))

(defun plot-polygon (coords &key refresh window)
  (g-plot (cons (car (last coords))
                coords)
          :refresh refresh
          :window window))

(defun v1-v2-e-dists-to-coords (v2s v1s)
  (mapcar (^ (coord)
             ;;make x's -ve
             (list (- (car coord)) (cadr coord)))
          (apply #'separation-triangle-coords (append v2s v1s))))

(defun plot-triangles (coordss)
  (setq coordss (filter (^ (x) (member t (mapcar #'complexp (flatten x)))) coordss))
  (let ((all-coords (apply #'append coordss)))
    (g-plot '((0 0))
            :x-min (apply #'min (nths 0 all-coords))
            :y-min (apply #'min (nths 1 all-coords))
            :x-max (apply #'max (nths 0 all-coords))
            :y-max (apply #'max (nths 1 all-coords))))
  (loop for coords in coordss do
        (plot-triangle coords nil)))

(defun plot-v1-v2-e-triangles (v2 e hi-table)
  (plot-triangles
   (mapcar (^ (x) (apply #'v1-v2-e-dists-to-coords x))
           (dists-to-v1s v2 e hi-table))))

(defun plot-v1v2-v1e-s (v1v2-v1e-s 
			&key (x-max 7) (y-max 7) (x-min 0) (y-min 0)
			     (refresh t) (style 'scatter) (element-symbol 'cross)
			     title)
  (g-plot v1v2-v1e-s
          :x-min x-min :y-min y-min :x-max x-max :y-max y-max
	  :x-title "V1-V2 distance"
	  :y-title "V1-E distance"
	  :legend-mapped nil
          :tag (loop for (x y name) in v1v2-v1e-s when name collect
		     (format nil "{~d ~d} -text ~a -anchor center -font -Adobe-Times-Medium-R-Normal-*-80-*" 
			     x y (format nil "~a" name)))
	  :refresh refresh
	  :style style
	  :element-symbol element-symbol
	  :title title))

(defun plot-v1-v2-e-xys (v2 e hi-table &key (squares *desired-locations*) window (normalize 2) (max 7) hi-table-name)
  (if normalize (setq hi-table (normalize-hi-table hi-table v2 e normalize)))
  (plot-v1v2-v1e-s (mapcar (^ (y-x-name) (list (nth 1 y-x-name) (nth 0 y-x-name) (nth 2 y-x-name)))
			   (nths 1 (dists-to-v1s v2 e hi-table)))
                   :x-max (apply #'max max (flatten (hi-table-values hi-table)))
                   :y-max (apply #'max max (flatten (hi-table-values hi-table)))
                   ;;:x-min (apply #'min (flatten (hi-table-values hi-table)))
		   ;;:y-min (apply #'min (flatten (hi-table-values hi-table)))
		   :title (format nil "~aV2=~a,  E=~a~a" 
				  (if hi-table-name (format nil "Table ~a; " hi-table-name) "")
				  v2 e 
				  (if normalize (format nil " (Normalized ~a)." normalize) "."))
                   )
  (loop for (x y radius) in squares do
        (plot-square x y radius :window window)))

(defun plot-v1-v2-e-xys-multiple-tables (v2 e hi-tables 
					 &key (squares *desired-locations*) (normalize 2))
  (if normalize (setq hi-tables (mapcar (^ (hi-table) (normalize-hi-table hi-table v2 e normalize)) hi-tables)))
  (let ((dists-to-v1s-s (mapcar (^ (xyn-xyn)
				   (if (> (length xyn-xyn) 2)
				       (error "restriction to 2 hi-tables here"))
				   (list (nth 0 xyn-xyn) (firstn 2 (nth 1 xyn-xyn))))
				(apply #'transpose 
				       (mapcar (^ (hi-table) (nths 1 (dists-to-v1s v2 e hi-table))) hi-tables)))))
    (plot-v1v2-v1e-s (car dists-to-v1s-s) :style 'line)
    (loop for pair-of-v1-dists in (cdr dists-to-v1s-s) do
	  (plot-v1v2-v1e-s pair-of-v1-dists :style 'line :refresh nil))
    (loop for (x y radius) in squares do
	  (plot-square x y radius))))

(defvar *desired-locations*)
(setq *desired-locations*
      '((0 2 0.5)
        (2 2 0.5)
        (4 2 0.5)
        (2 4 0.5)))

#|
(plot-v1-v2-e-xys 'a/england/42/72 'a/victoria/3/75 hi77a)
(plot-v1-v2-e-xys 'a/england/42/72 'a/victoria/3/75 hi77laa)
(plot-v1-v2-e-xys 'a/england/42/72 'a/victoria/3/75 hi77lasa)
(plot-v1-v2-e-triangles 'a/england/42/72 'a/victoria/3/75 hi77a)
(dists-to-v1s 'a/england/42/72 'a/victoria/3/75 hi77a)
((4.5) (4.0 1.0)) 
((4.5) (4.5 4.0)) 
((4.5) (4.5 0.0)) 
((4.5) (5.0 3.5)) 
((4.5) (3.5 1.5)) 
((4.5) (4.0 2.0)) 
((4.5) (4.0 3.0)) 
((4.5) (4.0 3.0)) 
((4.5) (4.5 2.5)) 
((4.5) (3.5 3.5)) 
((4.5) (1.0 5.0)) 
((4.5) (0.0 4.5)) 
((4.5) (3.5 4.0)) 
((4.5) (3.5 5.0)) 
((4.5) (1.5 3.0)) 
((4.5) (1.0 3.0)) 
((4.5) (2.5 2.5)) 
((4.5) (3.0 4.0)) 

(progn (plot-v1-v2-e-xys 'a/philippines/2/82_eq 'a/mississippi/1/85 hi85a)
       (plot-v1-v2-e-xys 'a/philippines/2/82_eq 'a/mississippi/1/85 hi85laa))
(plot-v1-v2-e-triangles 'a/philippines/2/82_eq 'a/mississippi/1/85 hi85a)
(dists-to-v1s 'a/philippines/2/82-eq 'a/mississippi/1/85 hi85a)
((1.5) (1.5 1.5)) 
((1.5) (2.0 2.5)) 
((1.5) (3.0 3.0)) 
((1.5) (3.0 3.0)) 
((1.5) (1.5 0.0)) 
((1.5) (2.5 3.0)) 
((1.5) (2.0 2.5)) 
((1.5) (1.5 2.5)) 
((1.5) (0.0 1.5)) 
((1.5) (1.0 1.5)) 
((1.5) (2.0 3.5))

(plot-v1-v2-e-xys 'a/england/427/88 'a/beijing/353/89 hi90a)
(plot-v1-v2-e-xys 'a/england/427/88 'a/beijing/353/89 hi90laa)
(plot-v1-v2-e-triangles 'a/england/427/88 'a/beijing/353/89 hi90a)
(dists-to-v1s 'a/england/427/88 'a/beijing/353/89 hi90a)
((2.0) (5.0 1.71)) 
((2.0) (3.5 1.71)) 
((2.0) (3.5 1.21)) 
((2.0) (2.5 0.5)) 
((2.0) (2.0 0.0)) 
((2.0) (1.5 1.0)) 
((2.0) (3.5 1.0)) 
((2.0) (3.79 0.79)) 
((2.0) (4.5 1.0)) 
((2.0) (4.0 0.5)) 
((2.0) (1.0 0.5)) 
((2.0) (3.0 1.71)) 
((2.0) (0.0 2.0)) 
((2.0) (3.5 0.5)) 

(plot-v1-v2-e-xys-multiple-tables 'A/CZECHOSLOVAKIA/19/89 'A/BEIJING/353/89 (list hi90a hi90lasa))
(plot-v1-v2-e-xys-multiple-tables 'A/OREGON/4/80 'A/PHILIPPINES/2/82_EQ (list hi85a hi85lasa))
(plot-v1-v2-e-xys-multiple-tables 'A/OREGON/4/80 'A/mississippi/1/85 (list hi85a hi85lasa))

(plot-v1-v2-e-xys-multiple-tables 'A/OREGON/4/80 'A/mississippi/1/85 (list hi85d hi85las))
|#

 
;;;----------------------------------------------------------------------
;;;                      SCALING AN HI TABLE
;;;----------------------------------------------------------------------

(defun translate-hi-table (hi-table translation)
  (make-hi-table 
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (f-elements (^ (x) (+ x translation)) (hi-table-values hi-table))))

(defun scale-hi-table (hi-table scale)
  (make-hi-table 
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (f-elements (^ (x) (*-w-dc x scale)) (hi-table-values hi-table))))

(defun f-hi-table (f hi-table &optional &key (do-not-recurse t) pass-ag-sr pass-ag-sr-indices)
  (if (and pass-ag-sr pass-ag-sr-indices)
      (error "pass only pass-ag-sr or pass-ag-sr-indices, not both"))
  (make-hi-table 
   (hi-table-antigens hi-table)
   (hi-table-sera hi-table)
   (let ((hi-table-sera (hi-table-sera hi-table)))  ;; optimization while hi-table-sera can take a while
     (if (not do-not-recurse)
	 (if (or pass-ag-sr pass-ag-sr-indices)
	     (print "passing ag and sr not implemented yet when we regress")
	   (f-elements f (hi-table-values hi-table)))
       (loop for row in (hi-table-values hi-table) 
	   for antigen in (hi-table-antigens hi-table)
	   for antigen-index from 0 collect
	     (loop for e in row 
		 for serum in hi-table-sera 
		 for serum-index from 0 collect
		   (if pass-ag-sr
		       (funcall f e antigen serum)
		     (if pass-ag-sr-indices
			 (funcall f e antigen-index serum-index)
		       (funcall f e)))))))
   ;;(glue-up (list (hi-table-name hi-table) 'f))  ;; was good idea, but names became too long to be useful
   (hi-table-name hi-table)
   ))

(defun normalize-hi-table (hi-table v2 e &optional (normal 1))
  (let* ((v2-self-dist (hi-table-value hi-table v2 v2))
	 (e-self-dist  (hi-table-value hi-table e  e ))
	 (translated-hi-table (translate-hi-table hi-table (- (min v2-self-dist e-self-dist))))
	 ;;(v2-e (hi-table-value translated-hi-table v2 e))
	 (e-v2 (hi-table-value translated-hi-table e v2))
	 ;;(scaled-hi-table (scale-hi-table translated-hi-table (float (/ normal (min v2-e e-v2)))))
	 (scaled-hi-table (scale-hi-table translated-hi-table (float (/ normal e-v2)))))
    scaled-hi-table))

(defun hi-table-min-value (hi-table) (apply-min (filter #'dont-care-p (flatten (hi-table-values hi-table)))))
(defun hi-table-max-value (hi-table) (apply-max (filter #'dont-care-p (flatten (hi-table-values hi-table)))))

(defun hi-table-values-min-value (hi-table-values) (apply-min (filter #'dont-care-p (flatten hi-table-values))))
(defun hi-table-values-max-value (hi-table-values) (apply-max (filter #'dont-care-p (flatten hi-table-values))))

(defun put-hi-table-into-range (hi-table lower upper)
  (let ((min-table-value (hi-table-min-value hi-table))
	(max-table-value (hi-table-max-value hi-table)))
    (f-hi-table
     (^ (x) (if (dont-care-p x)
		x
	      (linear-interpolation x min-table-value lower max-table-value upper)))
     hi-table)))
   
(defun put-hi-table-into-range-0-1 (hi-table)
  (put-hi-table-into-range hi-table 0 1))

(defun dont-care-hi-table (hi-table &optional (dont-care-threshold) (relation #'<=-dc))
  "replace any value less than or equal to dont-care-threshold with dont-care"
  ;;the relation defaults to dont-careing small numbers, this is because we are
  ;;most likely to use this on hi-tables
  (f-hi-table 
   (^ (x) (if (funcall relation x dont-care-threshold) 'dont-care x))
   hi-table))

(defun sera-normalize-hi-table (hi-table &optional (normal-homologous-titer 1024))
  "make all homologous sera 1024 (normalize columns)"
  (make-hi-table
   (hi-table-antigens hi-table)
   nil  ;;sera not used
   (loop for column in (hi-table-column-values hi-table)
       for homologous-titer in (hi-table-diagonal-values hi-table) collect
	 (mapcar (^ (e) 
		    (round 
		     (* e (/ normal-homologous-titer
			     homologous-titer))))
		 column))))

(defun f-hi-tables (f &rest args)  ;; this is a bit ugly, we want to pass any number of tables, then also some keyword ars
  (let* ((pass-ag-sr (snoop-keyword-arg :pass-ag-sr args :not-found-action :return-nil))
	 (pass-ag-sr-indices (snoop-keyword-arg :pass-ag-sr-indices args :not-found-action :return-nil))
	 (hi-tables (remove-keywords-and-args '(:pass-ag-sr :pass-ag-sr-indices) args :not-found-action :ignore))
	 (hi-table-antigens (hi-table-antigens (car hi-tables)))
	 (hi-table-sera     (hi-table-sera     (car hi-tables))))
    (if (not (nary-equal (mapcar #'hi-table-antigens hi-tables)))
	(error "All tables must have the same antigens, but they do not (have not yet tested to see if they have the same sera)"))
    (if (not (nary-equal (mapcar #'hi-table-sera hi-tables)))
	(error "All tables must have the same sera, but they do not"))
    (make-hi-table
     hi-table-antigens
     hi-table-sera
     (let ((hi-table-values-s (mapcar #'hi-table-values hi-tables)))
       (loop for i below (hi-table-length (car hi-tables)) for antigen in hi-table-antigens collect
	     (let ((corresponding-rows (nths i hi-table-values-s)))
	       (loop for j below (hi-table-width (car hi-tables)) for serum in hi-table-sera collect
		     (funcall 
		      f 
		      (append (nths j corresponding-rows)
			      (cond (pass-ag-sr (list antigen serum))
				    (pass-ag-sr-indices (list i j))
				    (t nil))))))))
     (glue-up (mapcar #'hi-table-name hi-tables))
     )))


;;;----------------------------------------------------------------------
;;;                        RON 3D VIEWER FORMAT
;;;----------------------------------------------------------------------

(defun ron-3d-format (hi-table lapedes-readlines &optional (scale 1.0))
  (multiple-value-bind (ignore ag-sr-coords) ;ag-sr-coords are pairs (ag and serum points) 
    (lapedes-to-hi-table-C-lines	     ;  of triples (if the lapedes gives 3d)
     lapedes-readlines
     hi-table)
    ignore
    (append 
     (loop for (ag-coords sr-coords) in ag-sr-coords 
	 for i from 0.2 by (float (/ 0.5 (length ag-sr-coords))) collect
	   (format nil "~{ ~d~} ~{ ~d~} ~d  ~f ~f ~f  ~d" 
		   (mapcar (^ (x) (* x scale)) ag-coords)
		   (mapcar (^ (x) (* x scale)) sr-coords)
		   -1 
		   i i i 
		   4))
      (loop for (start end) on (nths 0 ag-sr-coords)
	  until (null end) collect
	    (format nil "~{ ~d~} ~{ ~d~} ~d  ~f ~f ~f  ~d" 
		    (mapcar (^ (x) (* x scale)) start)
		    (mapcar (^ (x) (* x scale)) end)
		    -1 
		    0.5 0.0 0.5
		    1))
      (loop for (start end) on (nths 1 ag-sr-coords)
	  until (null end) collect
	    (format nil "~{ ~d~} ~{ ~d~} ~d  ~f ~f ~f  ~d" 
		    (mapcar (^ (x) (* x scale)) start)
		    (mapcar (^ (x) (* x scale)) end)
		    -1 
		    0.0 0.5 0.5
		    1)))))


#|
(ron-3d-format hi85 (fi-in-readline "~dsmith/mds/data/hi85.lapedes"))
(fi (ron-3d-format hi85 (fi-in-readline "~dsmith/mds/data/hi85.lapedes")) "~/mds/3d-view/hi85.data" :supersede t)
;;i have to go in and delete the parens

~high/top/3dview/
~/mds/3d-view/3dview  (make dataview)

(ron-3d-format hi85 (fi-in-readline "~dsmith/mds/data/hi85.lapedes"))

(ron-3d-format hi85 (scale-lapedes-readlines (fi-in-readline "~dsmith/mds/data/hi85.lapedes") 0.1))

(ron-3d-format hi85 (fi-in-readline "~dsmith/mds/data/hi85.lapedes") 0.1)
(fi (ron-3d-format hi85 (fi-in-readline "~dsmith/mds/data/hi85.lapedes") 0.1) "~/mds/3d-view/hi85.data.full" :supersede t)

in diretory ~/mds/3d-view do dataview hi85.data.full
|#


;;;----------------------------------------------------------------------
;;;                      COMPARING HI TABLES
;;;----------------------------------------------------------------------

(defun compare-hi-tables (a b)
  (if (not (equal (hi-table-antigens a) (hi-table-antigens b)))
      (cerror "Continue anyway"
	      "The two tables are not the same (by comparing the antigens)"))
  (let ((a-values (flatten (hi-table-values a)))
	(b-values (flatten (hi-table-values b))))
    (transpose a-values b-values)))


;;(g-plot (compare-hi-tables hi90 hi90las) :style 'scatter)
;;(g-plot (compare-hi-tables hi90d hi90las) :style 'scatter)


;;;----------------------------------------------------------------------
;;;                         ROW SCALING
;;;----------------------------------------------------------------------

(defun hi-table-row-scale (hi-table row-targets)
  (make-hi-table
   (hi-table-antigens hi-table)
   (hi-table-antisera hi-table)
   (loop for hi-table-row in (hi-table-values hi-table)
       for row-target in row-targets
       for i from 0 collect
	 (let ((multiplier (hi-difference row-target (nth i hi-table-row))))
	   (loop for hi-value in hi-table-row collect
		 (hi-round (hi-multiply hi-value multiplier)))))))

(defun hi-difference (x y)
  (round-to-nearest 0.5 (titer-diff-to-log x y)))

(defun hi-multiply (hi-value multiplier)
  (expt 2 (+ (log hi-value 2) multiplier)))

(defun hi-divide (hi-value dividor)
  (hi-multiply hi-value (- dividor)))

(defun hi-round (x)
  (find-closest x '(0 5 7 10 15 20 30 40 60 80 120 240 320 640 960 1280 1960 2560 3840 5120)))

#|
(pp-hi-table (hi-table-row-scale same-1 (loop for i below (hi-table-length same-1) collect 1280)))
(hi-tables-collate-rows 
 (mapcar (^ (hi-table)
	    (hi-table-row-scale hi-table (loop for i below (hi-table-length hi-table) collect 1280)))
	 (list same-1 same-2 same-3 same-4 same-5))
 t)
|#

;;;----------------------------------------------------------------------
;;;                          F HI TABLES
;;;----------------------------------------------------------------------

;;12/99 leave the first one (don't redefine)
'(defun f-hi-tables (f hi-tables)
  (make-hi-table 
   (hi-table-antigens (car hi-tables))
   (hi-table-sera (car hi-tables))
   (loop for i below (length (hi-table-antigens (car hi-tables))) collect
	 (loop for j below (length (hi-table-sera (car hi-tables))) collect
	       (apply f (loop for hi-table in hi-tables collect
			      (hi-table-value-by-indices hi-table i j)))))))

#|
(pp-hi-table (f-hi-tables (^ (&rest l) (funcall #'second l)) sames))
(pp-hi-table (f-hi-tables #'average sames))
(pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) sames))
(pp-hi-table (f-hi-tables #'median sames))
|#

;;;----------------------------------------------------------------------
;;;                      FINDING ROW TARGETS
;;;----------------------------------------------------------------------

(defun hi-tables-homologous-value-collective-value (hi-tables collective-value-f)
  (loop for i below (length (hi-table-antigens (car hi-tables))) collect
	(apply collective-value-f
	       (loop for hi-table in hi-tables collect
		     (hi-table-value-by-indices hi-table i i)))))

#|
(setq sames (list same-1 same-2 same-3 same-4 same-5))
(mapcar #'hi-round (hi-tables-homologous-value-collective-value sames #'average))
(hi-tables-collate-rows 
 (mapcar (^ (hi-table)
	    (hi-table-row-scale hi-table (mapcar #'hi-round (hi-tables-homologous-value-collective-value sames #'average))))
	 sames)
 t)
(hi-tables-collate-rows 
 (mapcar (^ (hi-table)
	    (hi-table-row-scale hi-table (hi-tables-homologous-value-collective-value sames #'median)))
	 sames)
 t)
|#

(defun hi-tables-ith-antigen-nth-value-collective-value (hi-tables i n collective-value-f)
  (apply collective-value-f
	 (loop for hi-table in hi-tables collect
	       (hi-table-value-by-indices hi-table i n))))

(defun hi-tables-ith-antigen-row-collective-values (hi-tables ith-antigen collective-value-f)
  (loop for sera below (length (hi-table-antisera (car hi-tables))) collect
	(hi-tables-ith-antigen-nth-value-collective-value hi-tables ith-antigen sera collective-value-f)))

(defun hi-tables-hi-table-ith-antigen-collective-value (hi-tables hi-table ith-antigen collective-value-f)
  (let ((homologous-value-hi-table (nth ith-antigen (cdr (hi-table-row hi-table ith-antigen))))
	(hi-table-row-differences-to-other-tables
	 (loop for hi-value in (cdr (hi-table-row hi-table ith-antigen))
	     for hi-collective-value in (hi-tables-ith-antigen-row-collective-values hi-tables ith-antigen collective-value-f)
	     collect (hi-difference hi-value hi-collective-value))))
    (hi-divide homologous-value-hi-table (av hi-table-row-differences-to-other-tables))))
  
 

#|
(hi-tables-ith-antigen-nth-value-collective-value sames 1 10 #'average)
(hi-tables-ith-antigen-row-collective-values sames 1 #'average)
(hi-tables-hi-table-ith-antigen-collective-value sames same-2 0 #'median)

(hi-tables-collate-rows 
 (mapcar (^ (hi-table)
	    (hi-table-row-scale 
	     hi-table
	     (loop for antigen below (hi-table-length hi-table) collect
		   (hi-tables-hi-table-ith-antigen-collective-value sames hi-table antigen #'median))))
	 sames)
 t)
|#

#|
;;;------------comparing adjusted tables------------------

(pp-hi-table (nth 0 sames))
(pp-hi-table (nth 0 
		  (mapcar (^ (hi-table)
			     (hi-table-row-scale hi-table (mapcar #'hi-round (hi-tables-homologous-value-collective-value 
									      sames #'average))))
			  sames)))
(pp-hi-table (nth 0 (mapcar (^ (hi-table)
			       (hi-table-row-scale 
				hi-table
				(loop for antigen below (hi-table-length hi-table) collect
				      (hi-tables-hi-table-ith-antigen-collective-value sames hi-table antigen #'median))))
			    sames)) )

(hi-tables-collate-rows
 (list (nth 0 sames)
       (nth 0 
	    (mapcar (^ (hi-table)
		       (hi-table-row-scale hi-table (mapcar #'hi-round (hi-tables-homologous-value-collective-value 
									sames #'average))))
		    sames))
       (nth 0 (mapcar (^ (hi-table)
			 (hi-table-row-scale 
			  hi-table
			  (loop for antigen below (hi-table-length hi-table) collect
				(hi-tables-hi-table-ith-antigen-collective-value sames hi-table antigen #'median))))
		      sames)))
 t)


;;;------------comparing averages------------------

(progn
  (pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    sames))
  (pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale hi-table (mapcar #'hi-round 
									    (hi-tables-homologous-value-collective-value 
									     sames #'average))))
				    sames)))
  (pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale 
					hi-table
					(loop for antigen below (hi-table-length hi-table) collect
					      (hi-tables-hi-table-ith-antigen-collective-value 
					       sames hi-table antigen #'median))))
				    sames))))

(hi-tables-collate-rows
 (list
   (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    sames)
   (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale hi-table (mapcar #'hi-round 
									    (hi-tables-homologous-value-collective-value 
									     sames #'average))))
				    sames))
  (f-hi-tables (^ (&rest l) (funcall (cfs #'hi-round #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale 
					hi-table
					(loop for antigen below (hi-table-length hi-table) collect
					      (hi-tables-hi-table-ith-antigen-collective-value 
					       sames hi-table antigen #'median))))
				    sames)))
  t)


(hi-tables-collate-rows
 (list
   (f-hi-tables (^ (&rest l) (funcall (cfs #'round-log-titer #'av) l)) 
			    sames)
   (f-hi-tables (^ (&rest l) (funcall (cfs #'round-log-titer #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale hi-table (mapcar #'hi-round 
									    (hi-tables-homologous-value-collective-value 
									     sames #'average))))
				    sames))
  (f-hi-tables (^ (&rest l) (funcall (cfs #'round-log-titer #'av) l)) 
			    (mapcar (^ (hi-table)
				       (hi-table-row-scale 
					hi-table
					(loop for antigen below (hi-table-length hi-table) collect
					      (hi-tables-hi-table-ith-antigen-collective-value 
					       sames hi-table antigen #'median))))
				    sames)))
 t)


(pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'round-log-titer #'av) l)) 
	                  sames))
(pp-hi-table (f-hi-tables (^ (&rest l) (funcall (cfs #'log-titer #'av) l)) 
	                  sames))
|#


;;;----------------------------------------------------------------------
;;;                       MERGING TABLES
;;;----------------------------------------------------------------------

;;to print hi-table rows togther
(defun hi-tables-collate-rows (hi-tables &optional pp)
  (let ((ans (loop for strain below (hi-table-length (car hi-tables)) collect 
		   (loop for hi-table in hi-tables collect 
			 (hi-table-row hi-table strain)))))
    (if pp (mapcar (^ (hi-table) (pp-hi-table hi-table nil) (newline)) ans))
    ans))

#|
(hi-tables-collate-rows (list same-1 same-2 same-3 same-4 same-5) t)

(hi-tables-collate-rows 
 (mapcar (^ (hi-table)
	    (hi-table-row-scale hi-table (loop for i below (hi-table-length hi-table) collect 1280)))
	 (list same-1 same-2 same-3 same-4 same-5))
 t))
|#

;;to print hi-table colunms together
(defun hi-tables-collate-colunms (hi-tables &optional pp)
  (let ((ans (loop for strain below (hi-table-length (car hi-tables)) collect 
		   (make-hi-table 
		    (hi-table-antigens (car hi-tables))
		    (loop for i below (length hi-tables) collect (nth strain (hi-table-antigens (car hi-tables))))
		    (apply-transpose (loop for hi-table in hi-tables collect
					     (cdr (hi-table-column hi-table strain))))))))
    (if pp (mapcar (^ (hi-table) (pp-hi-table hi-table nil) (newline)) ans))
    ans))

;;(hi-tables-collate-colunms (list same-1 same-2 same-3 same-4 same-5) t)
      

;;;----------------------------------------------------------------------
;;;                MERGING TABLES FOR JOINT TABLE MDS
;;;----------------------------------------------------------------------

(defun list-multiples (entry) 
  (setq entry (remove 'dont-care entry))
  (cond ((null entry) 'dont-care)
	((and (= 1 (length entry))
	      (not (listp (car entry))))  ;;case of a merge that includes table names
	 (car entry))
	(t entry)))

(defun list-multiples-some-elements-might-already-be-multiples (entry) 
  (setq entry (remove 'dont-care entry))
  (cond ((null entry) 'dont-care)
	((and (= 1 (length entry))
	      (not (listp (car entry))))  ;;case of a merge that includes table names
	 (car entry))
	(t (flatten entry))))  ;; might mess up if includes table names, but that is for later

(defun always-list-multiples (entry)
  ;; like lisp-multiples but does not reduce a single entry to an atom
  (setq entry (remove 'dont-care entry))
  (cond ((null entry) 'dont-care)
	(t entry)))

(defun list-multiples-ignore-less-than-equal-tos (&optional (less-than-equal-to 5))
  (^ (entry)
     (setq entry (remove 'dont-care entry))
     (setq entry (filter (^ (x) (<= x less-than-equal-to)) entry))
     (cond ((null entry) 'dont-care)
	   ((= 1 (length entry)) (car entry))
	   (t entry))))

(defun list-multiples-ignore-sd-greater-than (&optional (greater-than 1.0))
  (^ (entry)
     (setq entry (remove 'dont-care entry))
     (if (and (listp entry) (> (length entry) 1) (> (sd (mapcar #'std-log-titer entry)) greater-than))
	 (setq entry nil))
     (cond ((null entry) 'dont-care)
	   ((= 1 (length entry)) (car entry))
	   (t entry))))

(defun average-multiples (entry)
  (setq entry (filter #'dont-care-p entry))
  (cond ((null entry) 'dont-care)
	((= 1 (length entry)) (car entry))
	(t (av entry))))

(defun average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold (entry)
  (setq entry (filter #'true-dont-care-p entry))
  (if (null entry)
      'dont-care
    (let ((thresholded-entries     (collect #'dont-care-p entry))   ;; bit ugly, here and next line, as we rely on first line to remove
	  (non-thresholded-entries (filter  #'dont-care-p entry)))  ;; *'s, leaving (right now at least (2004-08-13) only thresholded values)
      (if non-thresholded-entries
	  (if (> (sd (mapcar #'std-log-titer non-thresholded-entries)) 1.0)
	      'dont-care
	    (round (av non-thresholded-entries)))
	(min-threshold thresholded-entries)))))

(defun geometric-average-multiples-unless-sd-gt-1-ignore-thresholded-unless-only-entries-then-min-threshold (entry)
  (setq entry (filter #'true-dont-care-p entry))
  (if (null entry)
      'dont-care
    (let ((thresholded-entries     (collect #'dont-care-p entry))   ;; bit ugly, here and next line, as we rely on first line to remove
	  (non-thresholded-entries (filter  #'dont-care-p entry)))  ;; *'s, leaving (right now at least (2004-08-13) only thresholded values)
      (if (and thresholded-entries non-thresholded-entries)
	  'dont-care
        (if non-thresholded-entries
            (if (> (sd (mapcar #'std-log-titer non-thresholded-entries)) 1.0)
                'dont-care
              (round (geometric-av non-thresholded-entries)))
          (min-threshold thresholded-entries))))))

(defun geometric-average-multiples-thresholded-and-non-thresholded-take-inc-max-titer-dont-care-sd-gt-1 (entry)
  (setq entry (filter #'true-dont-care-p entry))
  (if (null entry)
      'dont-care
    (let* ((non-thresholded-entries     (filter  #'dont-care-p entry))
           (thresholded-entries         (collect #'dont-care-p entry))
           (thresholded-entries-numbers (mapcar #'threshold-number thresholded-entries))
           (sd                          (sd (append (mapcar #'std-log-titer                 non-thresholded-entries)
                                                    (mapcar (^ (x) (- (std-log-titer x) 1)) thresholded-entries-numbers)))))
      (if (and thresholded-entries (not non-thresholded-entries))
          (min-threshold thresholded-entries)
        (if (> sd 1.0)
            'dont-care
          (if (and non-thresholded-entries (not thresholded-entries))
              (round (geometric-av non-thresholded-entries))
            (threshold-symbol 
             (if (> (apply-max thresholded-entries-numbers) (apply-max non-thresholded-entries))
                 (loop for thresholded-entry-number in (my-sort thresholded-entries-numbers)
                     when (> thresholded-entry-number (apply-max non-thresholded-entries))
                     do (return thresholded-entry-number)
                     finally (error "Unexpected condition, please contact Derek"))
               (* 2 (apply #'max non-thresholded-entries)))
             :add-if-not-seen-before t)))))))

(defun average-multiples-unless-sd-gt-2-ignore-thresholded-unless-only-entries-then-min-threshold (entry)
  (setq entry (filter #'true-dont-care-p entry))
  (if (null entry)
      'dont-care
    (let ((thresholded-entries     (collect #'dont-care-p entry))   ;; bit ugly, here and next line, as we rely on first line to remove
	  (non-thresholded-entries (filter  #'dont-care-p entry)))  ;; *'s, leaving (right now at least (2004-08-13) only thresholded values)
      (if non-thresholded-entries
	  (if (> (sd (mapcar #'std-log-titer non-thresholded-entries)) 2.0)
	      'dont-care
	    (round (av non-thresholded-entries)))
	(min-threshold thresholded-entries)))))

(defun average-multiples-ignore-less-than-equal-tos (&optional (less-than-equal-to (std-log-titer 5)))
  (^ (entry)
     (setq entry (filter #'dont-care-p entry))
     (setq entry (filter (^ (x) (<= x less-than-equal-to)) entry))
     (cond ((null entry) 'dont-care)
	   ((= 1 (length entry)) (car entry))
	   (t (av entry)))))

(defun geometric-average-multiples (entry)
  ;;assume here we are doing HI tables, and round the answer
  (setq entry (filter #'dont-care-p entry))
  (cond ((null entry) 'dont-care)
	((= 1 (length entry)) (car entry))
	(t (let ((geometric-av (geometric-av entry)))
	     (if (< geometric-av 1)
		 (error "tought we were averaging HI tables, but average of values is less than 1")
	       (round geometric-av))))))

(defun first-of-multiples (entry)
  (cond ((null entry) 'dont-care)
	(t (car entry))))

#||
(defun merge-hi-tables (hi-tables &optional (multiple-values-f #'list-multiples) (name 'merged) include-table-name-or-these-names)
  (let (;;(antigens (apply #'nary-union (mapcar #'hi-table-antigens hi-tables)))
	;;(sera (apply #'nary-union (mapcar #'hi-table-sera hi-tables)))
	(antigens (reverse (remove-duplicates (reverse (map-append #'hi-table-antigens hi-tables)))))
	(sera     (reverse (remove-duplicates (reverse (map-append #'hi-table-sera     hi-tables)))))
	(hi-table-names (if (and (not (null include-table-name-or-these-names)) 
				 (listp include-table-name-or-these-names))
			    include-table-name-or-these-names
			  (mapcar #'hi-table-name hi-tables))))
    (make-hi-table
       antigens
       sera
       (loop for antigen in antigens collect
	     (loop for serum in sera collect
		   (let ((entry (loop for hi-table in hi-tables
				    for hi-table-name in hi-table-names
				    when (and (member antigen (hi-table-antigens hi-table))
					      (member serum   (hi-table-sera hi-table)))
				    collect (if include-table-name-or-these-names
						(list hi-table-name (hi-table-value hi-table antigen serum))
					      (hi-table-value hi-table antigen serum)))))
		     (funcall multiple-values-f entry))))
       name)))
||#

;; modify the above, so dupicates in the same table are merged
(defun merge-hi-tables (hi-tables &optional (multiple-values-f #'list-multiples) (name 'merged) include-table-name-or-these-names)
  (let (;;(antigens (apply #'nary-union (mapcar #'hi-table-antigens hi-tables)))
	;;(sera (apply #'nary-union (mapcar #'hi-table-sera hi-tables)))
	(antigens (reverse (remove-duplicates (reverse (map-append #'hi-table-antigens hi-tables)))))
	(sera     (reverse (remove-duplicates (reverse (map-append #'hi-table-sera     hi-tables)))))
	(hi-table-names (if (and (not (null include-table-name-or-these-names)) 
				 (listp include-table-name-or-these-names))
			    include-table-name-or-these-names
			  (mapcar #'hi-table-name hi-tables))))
    (make-hi-table
       antigens
       sera
       (loop for antigen in antigens collect
	     (loop for serum in sera collect
		   (let ((entry (loop for hi-table in hi-tables
				    for hi-table-name in hi-table-names
				    when (and (member antigen (hi-table-antigens hi-table))
					      (member serum   (hi-table-sera hi-table)))
				    append (loop for table-antigen in (hi-table-antigens hi-table)
					       for table-antigen-values in (hi-table-values hi-table) 
					       when (equal table-antigen antigen)
					       append (loop for table-serum in (hi-table-sera hi-table)
							  for table-value in table-antigen-values
							  when (equal table-serum serum)
							  collect (if include-table-name-or-these-names
								      (list hi-table-name table-value)
								    table-value))))))
		     (funcall multiple-values-f entry))))
       name)))

#|
(merge-hi-tables (list (make-hi-table
			'(a b b c)
			'(x y z z)
			'((1 2 3 4)
			  (10 20 30 40)
			  (100 200 300 400)
			  (1000 2000 3000 4000)))))
((A 1 2 (3 4)) (B (10 100) (20 200) (30 40 300 400)) (C 1000 2000 (3000 4000)))

(merge-hi-tables (list (make-hi-table
			'(a b b c)
			'(x y z z)
			'((1 2 3 4)
			  (10 20 30 40)
			  (100 200 300 400)
			  (1000 2000 3000 4000)))
		       (make-hi-table
			'(a b)
			'(x z)
			'((10000 30000)
			  (100000 300000)))))
((A (1 10000) 2 (3 4 30000)) (B (10 100 100000) (20 200) (30 40 300 400 300000)) (C 1000 2000 (3000 4000)))

(merge-hi-tables (list (make-hi-table
			'(a b b c)
			'(x y z z)
			'((1 2 3 4)
			  (10 20 30 40)
			  (100 200 300 400)
			  (1000 2000 3000 4000))))
		 #'av)
((A 1.0 2.0 3.5) (B 55.0 110.0 192.5) (C 1000.0 2000.0 3500.0))

(merge-hi-tables (list (make-hi-table
			'(a b b c)
			'(x y z z)
			'((1 2 3 4)
			  (10 20 30 40)
			  (100 200 300 400)
			  (1000 2000 3000 4000)))
		       (make-hi-table
			'(a b)
			'(x z)
			'((10000 30000)
			  (100000 300000))))
		 #'av)
((A 5000.5 2.0 10002.333) (B 33370.0 110.0 60154.0) (C 1000.0 2000.0 3500.0))





(merge-tables (list (make-hi-table
		     '(a b b c)
		     '(x y z z)
		     '((1 2 3 4)
		       (10 20 30 40)
		       (100 200 300 400)
		       (1000 2000 3000 4000)) 'one))
	      :multiple-values-f #'av)
;; MDS merge table and diagnositics (version 0.0).
;; Created at 18:20:15 on 08/30/2004


      X Y Z
;;        X      Y      Z 
A       1.0    2.0    3.5 
B      55.0  110.0  192.5 
C     1000.0  2000.0  3500.0 




;;;----------------------------------------------------------------------------
;;;                             DIAGNOSTICS                             
;;;       (common titers, and how they merged, and the individual tables)
;;;----------------------------------------------------------------------------


;;       X Y Z
;; ;;        X      Y      Z 
;;                           
;; A                         
;;        ONE     1      2      3 
;;        ONE                   4 
;;        ZMERGED   1.0    2.0    3.5 
;;                           
;; B                         
;;        ONE    10     20     30 
;;        ONE   100    200     40 
;;        ONE                 300 
;;        ONE                 400 
;;        ZMERGED  55.0  110.0  192.5 
;;                           
;; C                         
;;        ONE  1000   2000   3000 
;;        ONE                4000 
;;        ZMERGED 1000.0  2000.0  3500.0 


;;;--------------------------------------------------------------------------------
;; Table ONE
;;       X Y Z Z
;; ;;        X      Y      Z      Z 
;; A         1      2      3      4 
;; B        10     20     30     40 
;; B       100    200    300    400 
;; C      1000   2000   3000   4000 







(merge-tables (list (make-hi-table
		     '(a b b c)
		     '(x y z z)
		     '((1 2 3 4)
		       (10 20 30 40)
		       (100 200 300 400)
		       (1000 2000 3000 4000)) 
		     'one)
		    (make-hi-table
		     '(a b)
		     '(x z)
		     '((10000 30000)
		       (100000 300000))
		     'two))
	      :multiple-values-f #'av)

;; MDS merge table and diagnositics (version 0.0).
;; Created at 18:23:02 on 08/30/2004


      X Y Z
;;        X      Y      Z 
A     5000.5    2.0  10002.333 
B     33370.0  110.0  60154.0 
C     1000.0  2000.0  3500.0 




;;;----------------------------------------------------------------------------
;;;                             DIAGNOSTICS                             
;;;       (common titers, and how they merged, and the individual tables)
;;;----------------------------------------------------------------------------


;;       X Y Z
;; ;;        X      Y      Z 
;;                           
;; A                         
;;        ONE     1      2      3 
;;        ONE                   4 
;;        TWO 10000         30000 
;;        ZMERGED 5000.5    2.0  10002.333 
;;                           
;; B                         
;;        ONE    10     20     30 
;;        ONE   100    200     40 
;;        ONE                 300 
;;        ONE                 400 
;;        TWO 100000         300000 
;;        ZMERGED 33370.0  110.0  60154.0 
;;                           
;; C                         
;;        ONE  1000   2000   3000 
;;        ONE                4000 
;;        ZMERGED 1000.0  2000.0  3500.0 


;;;--------------------------------------------------------------------------------
;; Table ONE
;;       X Y Z Z
;; ;;        X      Y      Z      Z 
;; A         1      2      3      4 
;; B        10     20     30     40 
;; B       100    200    300    400 
;; C      1000   2000   3000   4000 


;;;--------------------------------------------------------------------------------
;; Table TWO
;;       X Z
;; ;;        X      Z 
;; A     10000  30000 
;; B     100000  300000 


|#



(defun merged-hi-tables-colors (hi-tables)
  (let ((antigens (apply #'nary-union (mapcar #'hi-table-antigens hi-tables)))
	(table-colors (loop for i below (length hi-tables) collect (random-tk-color))
	              ;;'("#ff0000" "#0000ff" "#00ff00")
		      ))
    (loop for antigen in antigens collect
	  (let ((colors (loop for hi-table in hi-tables
			    for table-color in table-colors
			    when (member antigen (hi-table-antigens hi-table))
			    collect table-color)))
	    (if (= 1 (length colors))
		(car colors)
	      "#000000")))))   ;;black
			  

#|
(setq x '((a 0 1.1 2) (b 1 0 3) (c 2 3 0)))
(setq y '((b 0 2 4) (c 2 0 5) (d 4 5 0)))
(setq z '((a 0 10 11) (b 10 0 11) (d 11 11 0)))
(merge-hi-tables x y z)
                                    A      B      C      D 
A                               (0 0) (1.1 10)  2.0 11.0
B                               (1 10) (0 0 0) (3 2) (4 11)
C                                2.0 (3 2) (0 0)  5.0
D                               11.0 (4 11)  5.0 (0 0)

more stuff in investigations/piecing-together-table
|#


#|
superceded to parameter on f-hi-table
(defun f-list-merged-hi-table (f hi-table)
  ;; CAREFUL (one might expect the opposite to the following)
  ;; this function is different than f-hi-table, f-hi-table calls f-elements on the values, and the f recurses
  ;; into the merged-hi-table lists.  Here we want to operate on the list as a whole.
  (make-hi-table 
   (hi-table-antigens hi-table)
   (hi-table-sera     hi-table)
   (loop for row in (hi-table-values hi-table) collect
	 (loop for value in row collect
	       (funcall f value)))
   (glue-up (list (hi-table-name hi-table) 'flm))))
|#

(defun list-merged-hi-table-to-hi-table (hi-table)
  (f-hi-table
   (^ (x)
      (if (null x)
	  'dont-care
	(if (listp x)
	    (round (geometric-av x))    ;;this is set up for classic hi titers
	  x)))
   hi-table
   :do-not-recurse t))

(defun list-merged-hi-table-to-hi-table-geometric-av-numbers-lowest-threshold (hi-table)
  (f-hi-table
   (^ (x)
      (if (null x)
	  'dont-care
	(if (listp x)
	    (if (thresholdp (car x))
		(min-threshold x)
	      (round (geometric-av x)))
	  x)))
   hi-table
   :do-not-recurse t))


;;;----------------------------------------------------------------------
;;;                   MAKING SIMULATED HI TABLES
;;;----------------------------------------------------------------------

(defun make-mock-distance-hi-table (antigen-coordss sera-coordss 
				    &optional &key
					      (antigen-names (loop for i below (length antigen-coordss) collect (number->string i)))
					      (sera-names    (loop for i below (length sera-coordss) collect (number->string i))))
  (make-hi-table
   antigen-names
   sera-names
   (loop for antigen-coords in antigen-coordss collect
	 (loop for sera-coords in sera-coordss collect
	       (e-dist antigen-coords sera-coords)))))

(defun make-mock-hi-table-from-distance-hi-table (distance-hi-table &optional (2-fold-distance 0.1) (homologous-titer 1280))
  (let* ((num-steps (round (log (/ homologous-titer 10) 2)))
	 (distance-to-<10 (* num-steps 2-fold-distance)))
    (f-hi-table
     (^ (d)
	(if (> d distance-to-<10)
	    'dont-care   ;; really should be lt-10 (which is very different from dont-care)
	  (round (* 10 (expt 2 (/ (- distance-to-<10 d) 2-fold-distance))))))
     distance-hi-table)))

(defun hi-table-round-to-standard-titers (hi-table)
  (f-hi-table
   (^ (d)
      (if (dont-care-p d)
	  d
	(* 10 (expt 2 (round (log (/ d 10) 2))))))
   hi-table))


;;;----------------------------------------------------------------------
;;;                         MISC
;;;----------------------------------------------------------------------

(defun hi-tables-common-antigens (hi-tables)
  (apply #'nary-intersection
	 (mapcar #'hi-table-antigens hi-tables)))

(defun hi-table-year-range (hi-table)
  (list (apply-min (mapcar #'strain-year (hi-table-antigens hi-table)))
	(apply-max (mapcar #'strain-year (hi-table-antigens hi-table)))))

(defun hi-table-homologous-strains (hi-table)
  (my-intersection (hi-table-antigens hi-table) (hi-table-sera hi-table)))

(defun hi-table-homologous-values (hi-table)
  (let ((hi-table-sera (hi-table-sera hi-table)))
    (loop for common-ag-sr in (reverse (intersection (hi-table-antigens hi-table) (hi-table-sera hi-table))) collect
	  (hi-table-value hi-table common-ag-sr common-ag-sr :hi-table-sera-efficiency-hack hi-table-sera))))

(defun hi-table-homologous-strains-with-numeric-titers (hi-table)
  (let ((homologous-strains (hi-table-homologous-strains hi-table))
	(homologous-values  (hi-table-homologous-values  hi-table)))
    (loop for strain in homologous-strains
	for value in homologous-values
	when (numberp value)
	collect strain)))

(defun hi-table-homologous-serum-values (hi-table)
  (if (ag-sr-table-p hi-table)
      (append
       (loop for i below (length (hi-table-antigens-short hi-table)) collect 'not-a-serum)
       (mapcar (^ (titer) (if (numberp titer) (std-log-titer titer) titer)) (hi-table-homologous-serum-values (un-asl-hi-table hi-table))))
    (let ((hi-table-sera     (hi-table-sera     hi-table))
	  (hi-table-antigens (hi-table-antigens hi-table)))
      (loop for serum in hi-table-sera collect
	    (if (member serum hi-table-antigens)
		(hi-table-value hi-table serum serum :hi-table-sera-efficiency-hack hi-table-sera)
	      'no-homologus-titer)))))

(defun hi-table-name-square-p (hi-table)
  (equal (hi-table-antigens hi-table) (hi-table-sera hi-table)))

(defun similarity-table-p (hi-table)
  (or (ag-sr-table-p hi-table)
      (and (hi-table-name-square-p hi-table)
	   (apply #'nary-equal (hi-table-homologous-values hi-table))
	   (not (and (numberp (car (hi-table-homologous-values hi-table)))
		     (zerop (car (hi-table-homologous-values hi-table))))))))


(defun hi-table-serum-geometric-averages (table)
  (make-hi-table
   '(average)
   (hi-table-sera table)
   (list (mapcar 
	  (^ (l) 
	     (let ((serum-values (collect #'numberp l)))
	       (if serum-values
		   (round (geometric-av serum-values))
		 'dont-care)))
	  (apply-transpose (hi-table-values table))))))

(defun hi-table-serum-statistical-summary (table)
  (make-hi-table
   '(average log-av log-sd log-se number)
   (hi-table-sera table)
   (let* ((geometric-average (mapcar 
			     (^ (l) 
				(let ((serum-values (collect #'numberp l)))
				  (if serum-values
				      (round (geometric-av serum-values))
				    'dont-care)))
			     (apply #'transpose (hi-table-values table))))
	  (hi-table-std-log-values (mapcar (^ (l) (mapcar #'std-log-titer l)) (hi-table-values table)))
	  (std-log-av (mapcar 
		       (^ (l) 
			  (let ((serum-values (collect #'numberp l)))
			    (if serum-values
				(2dp (av serum-values))
			      'dont-care)))
		       (apply #'transpose hi-table-std-log-values)))
	  (std-log-sd (mapcar 
		       (^ (l) 
			  (let ((serum-values (collect #'numberp l)))
			    (if serum-values
				(2dp (sd serum-values))
			      'dont-care)))
		       (apply #'transpose hi-table-std-log-values)))
	  (std-log-se (mapcar 
		       (^ (l) 
			  (let ((serum-values (collect #'numberp l)))
			    (if serum-values
				(2dp (se-mean serum-values))
			      'dont-care)))
		       (apply #'transpose hi-table-std-log-values)))
	  (num-in-col (mapcar 
		       (^ (l) 
			  (length (collect #'numberp l)))
		       (apply #'transpose hi-table-std-log-values))))
     (list geometric-average
	   std-log-av
	   std-log-sd
	   std-log-se
	   num-in-col
	   ))))


;;;----------------------------------------------------------------------
;;;                      "thin" table
;;;----------------------------------------------------------------------

(defun thin-table (proportion-to-keep table &optional &key (keep-diagonal t))
  (f-hi-table 
   (^ (x i j)
      (if (or (and keep-diagonal (= i j))
              (< (knuth-random) proportion-to-keep))
          x
        'dont-care))
   table
   :pass-ag-sr-indices t))