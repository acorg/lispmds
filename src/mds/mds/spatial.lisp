(in-package user)


;;;----------------------------------------------------------------------
;;;    hack for derek, 
;;;    sometimes i want the spatial data loaded, most of the time not
;;;----------------------------------------------------------------------

(defvar *load-spatial-data-for-dsmith*)
(setq *load-spatial-data-for-dsmith* nil)

(defun load-spatial-data-p ()
  (and (equal (user-name) "dsmith")
       *load-spatial-data-for-dsmith*))


;;;----------------------------------------------------------------------
;;;             long names, abbreviations, and cdcids
;;;----------------------------------------------------------------------

(defvar *location-longname-to-location-abbrev-alist*)
(defvar *location-abbrev-to-location-longname-alist*)
(defvar *hiabbrev-cdcid*)
(defvar *hiabbrev-strain-longname*)

(if (load-spatial-data-p)
    (progn
      (setq *location-longname-to-location-abbrev-alist*
	(mapcar (^ (l) (list (nth 0 l) (nth 2 l))) 
		(fi-in-readline-to-list "mds/investigations/strain-selection-meeting/database/incomming/from-terry/format2/20070122/locations.by-name")))

      (setq *location-abbrev-to-location-longname-alist* (mapcar #'reverse *location-longname-to-location-abbrev-alist*))

      ;;in the top level of a cdc dist:
      ;;  extract-hi-abbrev-and-var-by-tag.pl -var cdc 200*/*.msf | sort -u -n +1 > ~/m/investigations/spatial/location-data/20060914-hi-abbrev.cdc-id
      (setq *hiabbrev-cdcid*
	(fi-in-readline-to-list "mds/investigations/spatial/location-data/20060914-hi-abbrev.cdc-id"))
      
      ;; get this file from terry (as he has DB)
      ;; ~/mds/bin $ ./list-abbrevs.pl -type strain > /tmp/all
      (setq *hiabbrev-strain-longname*
	(fi-in-readline "mds/investigations/spatial/location-data/20060914-strains.byabbr-untabified"
			:line-process-f #'string-to-symbol-then-trailing-string))

      ))


;;;----------------------------------------------------------------------
;;;                        utils
;;;----------------------------------------------------------------------

(defun string-longname-from-hiabbrev (hi-abbrev)
  (assoc-value-1 hi-abbrev *hiabbrev-strain-longname*))



;;;----------------------------------------------------------------------
;;;        long-lat, for strains with international standard names
;;; (and thus via our two-letter abbrev, and for cdc non-international
;;; name data, special cdc location info
;;;----------------------------------------------------------------------

(defun lat-long-from-location-info (location-info) (nth-range 10 15 location-info))

(defun location-info-has-no-lat-long-p (location-info)
  (equal '("" "" "" "" "" "") (lat-long-from-location-info location-info)))

(defvar *location-information*)
(if (load-spatial-data-p)
    (setq *location-information* 
      (filter 
       #'location-info-has-no-lat-long-p
       (cdr 
        (read-csv-file-into-ll 
         "mds/investigations/spatial/location-data/locations.csv"
         ;;"mds/investigations/strain-selection-meeting/database/incomming/from-terry/format2/20070122/locations.csv"
         )))))

#|
(mac-to-unix 
 "mds/investigations/spatial/cdc-location-data-current/cdcstates.csv"
 "mds/investigations/spatial/cdc-location-data-current/cdcstates-unix.csv")

(mac-to-unix 
 "mds/investigations/spatial/cdc-location-data-current/mycountries.csv"
 "mds/investigations/spatial/cdc-location-data-current/mycountries-unix.csv")

(mac-to-unix 
 "mds/investigations/spatial/cdc-location-data-current/cdcid-location-amantadine-mod1.csv"
 "mds/investigations/spatial/cdc-location-data-current/cdcid-location-amantadine-mod1-unix.csv")
(mac-to-unix
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060421.csv"
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060421-unix.csv")
(mac-to-unix
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060526.csv"
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060526-unix.csv")
(mac-to-unix
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060915.csv"
 "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060915-unix.csv")

(mac-to-unix 
 "mds/investigations/spatial/cdc-location-data-current/two-letter-cdc-codes-20060217.csv"
 "mds/investigations/spatial/cdc-location-data-current/two-letter-cdc-codes-20060217-unix.csv")


(csvll (mapcar (^ (l) (reorder-elements l '(12 13 14 15 2 1 3 4 5 6 7 8 9 10 11 0))) (READ-CSV-FILE-INTO-LL "mds/investigations/spatial/location-data/cdc-state-locations.csv.pre-recolumn.doublequote")) :filename "mds/investigations/spatial/location-data/cdc-state-locations.csv" :if-exists :supersede)

|#


(defvar *cdc-state-locations*)
(defvar *cdc-country-locations*)
(defvar *cdc-two-letter-code-region-name*)

(if (load-spatial-data-p)
    (progn
      (setq *cdc-patient-locations*
	(cdr (read-csv-file-into-ll "mds/investigations/spatial/location-data/cdc-patient-locations.csv")))
      
      (setq *cdc-state-locations*
	(cdr (read-csv-file-into-ll "mds/investigations/spatial/location-data/cdc-state-locations.csv")))

      (setq *cdc-country-locations* 
	(cdr (read-csv-file-into-ll "mds/investigations/spatial/location-data/cdc-country-locations.csv")))

      (setq *cdc-two-letter-code-region-name*
	(cdr (read-csv-file-into-ll "mds/investigations/spatial/location-data/two-letter-cdc-codes-20060217-unix.csv")))))

;;;----------------------------------------------------------------------
;;;                         accessors
;;;----------------------------------------------------------------------

(defun location-abbrev-to-location-longname (location-abbrev &optional &key (location-abbrev-to-location-longname-alist *location-abbrev-to-location-longname-alist*))
  (assoc-value-1 location-abbrev location-abbrev-to-location-longname-alist))

(defun location-abbrev-to-location-info (location-abbrev &optional &key (location-information *location-information*))
  (assoc (location-abbrev-to-location-longname location-abbrev) location-information))


;;;----------------------------------------------------------------------
;;;                   continent numbers/colors
;;;----------------------------------------------------------------------

(defvar *continentNumber-color-alist*)
(if (load-spatial-data-p)
    (setq   *continentNumber-color-alist* (fi-in-readline-to-list "mds/investigations/spatial/continentNumber-color")))
#|
1 darkblue    North America
2 turquoise   South America
3 green       Europe
4 orange      Africa
5 purple      Middle East
6 maroon      Russia
7 red         Asia
8 hotpink     Oceania
|#

(defun continent-color-from-continent-number (continent-number &optional &key 
									 (continentNumber-color-alist *continentNumber-color-alist*))
  (assoc-value-1 continent-number continentNumber-color-alist))
      

;;;----------------------------------------------------------------------
;;;                   location info from strain name
;;;----------------------------------------------------------------------

;; general     (UK     "" UK      L LONDON ENGLAND "United Kingdom" GBR GB 3 51 30 N   0 10 W)
;; cdc state   (MT     US MONTANA                                   USA US 1 45 47 N 108 30 W "" "" "" BILLINGS)  << changed
;; cdc country (POLAND "" PD      L WARSAW ""      POLAND                     POL PL 3 52 15 N  21  0 E)

#|
(defun cdc-state-location-info-line-to-standard-location-info-line (cdc-state-info-line)
  (append
   (nth-range 12 12 cdc-state-info-line)
   '("")
   (nth-range 13 15 cdc-state-info-line)
   '("" "United States of America")
   (nth-range  3 11 cdc-state-info-line)))
|#

(defun cdc-state-location-info-line-to-standard-location-info-line (cdc-state-info-line)
  (butlast cdc-state-info-line))

(defun cdc-country-location-info-line-to-standard-location-info-line (cdc-country-info-line)
  cdc-country-info-line)

(defun cdc-patient-location-info-line-to-standard-location-info-line (cdc-patient-info-line)
  cdc-patient-info-line)

(defun cdc-location-info-from-cdc-intermediate-location-name (cdc-intermediate-location-name)
  cdc-intermediate-location-name  ;; to stop compiler warning
  (error "depreciated, now with either -by-patient-location or -by-country-or-state")
  )

(defun cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state (cdc-intermediate-location-name)
  (let ((location-info-if-state
	 (let* ((position (position cdc-intermediate-location-name (nths 5 *cdc-state-locations*) :test #'equal))
		(assoc-line (if position (nth position *cdc-state-locations*))))
	   (if assoc-line
	       (let ((canonical-two-letter-abbrev (nth 2 assoc-line)))
		 (if (equal "" canonical-two-letter-abbrev)
		     (cdc-state-location-info-line-to-standard-location-info-line assoc-line)  ;; get from cdc state data
		   (location-abbrev-to-location-info canonical-two-letter-abbrev)))))) ;; get from general location data
	(location-info-if-country
	 (let* ((position (position cdc-intermediate-location-name (nths 6 *cdc-country-locations*) :test #'equal))
		(assoc-line (if position (nth position *cdc-country-locations*))))
	   (if assoc-line
	       (let ((canonical-two-letter-abbrev (nth 2 assoc-line)))
		 (if (equal "" canonical-two-letter-abbrev)
		     (cdc-country-location-info-line-to-standard-location-info-line assoc-line)  ;; get from cdc continent data
		   (location-abbrev-to-location-info canonical-two-letter-abbrev)))))))
    (if location-info-if-state
	location-info-if-state
      location-info-if-country)))

(defun cdc-location-info-from-cdc-intermediate-location-name-by-patient-location (cdc-intermediate-location-name)  ;; ie by city
  (if (equal "" cdc-intermediate-location-name)
      nil
    (let* ((position (position cdc-intermediate-location-name (nths 4 *cdc-patient-locations*) :test #'equal))  ;; the city, above is the country
	   (assoc-line (if position (nth position *cdc-patient-locations*))))
      (if assoc-line
	  (let ((canonical-two-letter-abbrev (nth 2 assoc-line)))
	    (if (equal "" canonical-two-letter-abbrev)
		(cdc-patient-location-info-line-to-standard-location-info-line assoc-line)  ;; get from cdc continent data
	      (location-abbrev-to-location-info canonical-two-letter-abbrev)))))))


(defun cdc-location-info-from-cdc-two-letter-code (strain-name)
  (let* ((two-letter-cdc-location-code
	  (if (equal "-" (substring (string strain-name) 2 2))
	      (read-from-string (substring (string strain-name) 0 1))
	    nil))
	 (cdc-intermediate-location-name 
	  (nth 2 (assoc two-letter-cdc-location-code *cdc-two-letter-code-region-name* :test #'equal))))
    (if cdc-intermediate-location-name 
	(cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state cdc-intermediate-location-name)
      nil)))

(defun cdc-location-info-from-strain-name (strain-name)
  (let* ((cdc-id (assoc-value-1 (remove-ag-sr-from-name strain-name) *hiabbrev-cdcid*))
	 (cdc-intermediate-location-name-by-patient-location (assoc-value-3 cdc-id *cdc-cdcid-location-amantadine*))
	 (cdc-intermediate-location-name-by-country          (assoc-value-4 cdc-id *cdc-cdcid-location-amantadine*))
	 (cdc-location-info-from-cdc-intermediate-location-name-by-patient-location
	  (cdc-location-info-from-cdc-intermediate-location-name-by-patient-location cdc-intermediate-location-name-by-patient-location))
	 (cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state
	  (cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state cdc-intermediate-location-name-by-country)))
    (if cdc-location-info-from-cdc-intermediate-location-name-by-patient-location
	(progn
	  (format t "~%>>>>>>>> Determining location by patient location ~a   ~a" strain-name cdc-location-info-from-cdc-intermediate-location-name-by-patient-location)
	  cdc-location-info-from-cdc-intermediate-location-name-by-patient-location)
      (if cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state
	  cdc-location-info-from-cdc-intermediate-location-name-by-country-or-state
	(cdc-location-info-from-cdc-two-letter-code strain-name)))))

	 
#|
(defun location-info-from-strain-name (strain-name)
  (if (equal "/" (substring (string strain-name) 2 2))
      (let ((location-info (location-abbrev-to-location-info
			    (read-from-string (substring (string strain-name) 0 1)))))
	(if (and 
	     (numberp (continent-number-from-location-info location-info))
	     (numberp (nth 0 (lat-long-from-location-info location-info)))
	     (numberp (nth 1 (lat-long-from-location-info location-info)))
	     (member  (nth 2 (lat-long-from-location-info location-info)) '(n s))
	     (numberp (nth 3 (lat-long-from-location-info location-info)))
	     (numberp (nth 4 (lat-long-from-location-info location-info)))
	     (member  (nth 5 (lat-long-from-location-info location-info)) '(e w)))
	    location-info
	  (progn
	    (format t "~%Warning: incomplete location info for strain ~a, location info: ~a" strain-name location-info)
	    nil)))
    (let ((cdc-location-info-from-strain-name (cdc-location-info-from-strain-name strain-name)))
      (if cdc-location-info-from-strain-name
	  cdc-location-info-from-strain-name
	(progn
	  (format t "~%Warning: no location info for strain ~a" strain-name)
	  nil)))))
|#

;; replaces above now we have long names, not abbrevs (20070813)
(defun location-info-from-strain-name (strain-name)
  (if (string-member "/" (string strain-name))
      (let ((location-info (let ((result (assoc
                                          (read-from-string (substring-before-char #\/ (string strain-name)))
                                          *location-information*
                                          )))
                             (print (list '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> strain-name result))
                             result)))
	(if (and 
	     (numberp (continent-number-from-location-info location-info))
	     (numberp (nth 0 (lat-long-from-location-info location-info)))
	     (numberp (nth 1 (lat-long-from-location-info location-info)))
	     (member  (nth 2 (lat-long-from-location-info location-info)) '(n s))
	     (numberp (nth 3 (lat-long-from-location-info location-info)))
	     (numberp (nth 4 (lat-long-from-location-info location-info)))
	     (member  (nth 5 (lat-long-from-location-info location-info)) '(e w)))
	    location-info
	  (progn
	    (format t "~%Warning: incomplete location info for strain ~a, location info: ~a" strain-name location-info)
	    nil)))
    (let ((cdc-location-info-from-strain-name (cdc-location-info-from-strain-name strain-name)))
      (if cdc-location-info-from-strain-name
	  cdc-location-info-from-strain-name
	(progn
	  (format t "~%Warning: no location info for strain ~a" strain-name)
	  nil)))))

(defun pretty-location-info-from-location-info (location-info)
  (format nil "~3d,~2d~1a  ~3d,~2d~1a  ~a ~a ~a"
	  (nth 10 location-info)
	  (nth 11 location-info)
	  (nth 12 location-info)
	  (nth 13 location-info)
	  (nth 14 location-info)
	  (nth 15 location-info)
	  (string-capitalize (string (nth  4 location-info)))
	  (nth  5 location-info)
	  (nth  6 location-info)))

(defun pretty-location-info-from-strain-name (strain-name)
  (pretty-location-info-from-location-info 
   (location-info-from-strain-name strain-name)))


;;;----------------------------------------------------------------------
;;;                   location from strain name
;;;----------------------------------------------------------------------

(defun location-from-location-info (location-info) (nth 4 location-info))

(defun location-from-strain-name (strain-name)
  (location-from-location-info (location-info-from-strain-name strain-name)))


(defun long-location-from-location-info (location-info) (multiple-nth '(0 3 4 5 8 7 6 9) location-info))  ;;(ENGLAND "" EN L LONDON ENGLAND "United Kingdom" GBR GB 3 51 30 N 0 10 W)

(defun long-location-from-strain-name (strain-name)
  (long-location-from-location-info (location-info-from-strain-name strain-name)))


;;;----------------------------------------------------------------------
;;;                   annotation from strain name
;;;----------------------------------------------------------------------

(defun annotation-from-location-info (location-info) (nth 1 location-info))

(defun annotation-from-strain-name (strain-name)
  (annotation-from-location-info (location-info-from-strain-name strain-name)))


;;;----------------------------------------------------------------------
;;;              continent nubmer/color from strain name
;;;----------------------------------------------------------------------

(defun continent-number-from-location-info (location-info) (nth 9 location-info))

(defun continent-number-from-strain-name (strain-name)
  (continent-number-from-location-info (location-info-from-strain-name strain-name)))

(defun continent-color-from-strain-name (strain-name)
  (continent-color-from-continent-number
   (continent-number-from-strain-name
    strain-name)))
      

;;;----------------------------------------------------------------------
;;;                     long/lat from strain name
;;;----------------------------------------------------------------------

;; this moved above, so is defined before we load the data files, so we can use it there.
;;(defun lat-long-from-location-info (location-info) (nth-range 10 15 location-info))

(defun lat-long-from-strain-name (strain-name)
  (lat-long-from-location-info (location-info-from-strain-name strain-name)))

(defun xytop-from-location-info (location-info)
  (apply #'ll-degrees-minutes-nsew-to-xytop (lat-long-from-location-info location-info)))

(defun xytop-from-strain-name (strain-name)
  (let ((lat-long-from-strain-name (lat-long-from-strain-name strain-name)))
    (if lat-long-from-strain-name
	(apply #'ll-degrees-minutes-nsew-to-xytop (lat-long-from-strain-name strain-name))
      nil)))

(defun utm-from-location-info (location-info)
  (let ((lat-long-from-location-info (lat-long-from-location-info location-info)))
    (if lat-long-from-location-info
	(apply #'ll-degrees-minutes-nsew-to-utm (lat-long-from-location-info location-info))
      nil)))

(defun utm-from-strain-name (strain-name)
  (let ((lat-long-from-strain-name (lat-long-from-strain-name strain-name)))
    (if lat-long-from-strain-name
	(apply #'ll-degrees-minutes-nsew-to-utm (lat-long-from-strain-name strain-name))
      nil)))

(defun utm-xytop-from-location-info (location-info)
  (let ((utm (utm-from-location-info location-info)))
    (if utm
	(apply #'utm-to-xytop utm)
      nil)))

(defun utm-xytop-from-strain-name (strain-name)
  (let ((utm (utm-from-strain-name strain-name)))
    (if utm
	(apply #'utm-to-xytop utm)
      nil)))


;;;----------------------------------------------------------------------
;;;                      cdc amantadine resistance data
;;;----------------------------------------------------------------------

(defvar *cdc-cdcid-location-amantadine*)
(if (load-spatial-data-p)
    (setq *cdc-cdcid-location-amantadine*
      (append
       (cdr (read-csv-file-into-ll "mds/investigations/spatial/location-data/cdcid-location-amantadine-unix.csv"))
       (mapcar (^ (l) (butnth 2 l)) (cdr (read-csv-file-into-ll "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060421-unix.csv")))
       (mapcar (^ (l) (butnth 2 l)) (cdr (read-csv-file-into-ll "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060526-unix.csv")))
       (mapcar (^ (l) (butnth 2 l)) (cdr (read-csv-file-into-ll "mds/investigations/spatial/amantadine/cdc-raw-data/adamantane-20060915-unix.csv"))))))

(defun cdc-amantadine-info-from-strain-name (strain-name)
  (let* ((cdc-id (assoc-value-1 (remove-ag-sr-from-name strain-name) *hiabbrev-cdcid*))
	 (cdc-amantadine-info (assoc-value-5 cdc-id *cdc-cdcid-location-amantadine*)))
      (if cdc-amantadine-info
	  cdc-amantadine-info
	(progn
	  ;;(push-end (list strain-name cdc-id cdc-amantadine-info) foo)
	  nil)
	)))

;;(hist (nths 5 *cdc-cdcid-location-amantadine*))
;;((R 604) (S35 2) (S 2072))



;;;----------------------------------------------------------------------
;;;                         map coloring 
;;;----------------------------------------------------------------------

(defun color-code-epi-continent-reference-background-strongest-marked (save &optional &key 
										      (reference-strains 'not-passed) 
										      marked 
										      intermediate-marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
	       ((member (car psline) intermediate-marked)
		(cons (car psline) (list :ds 8 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 14 :nc "black" :co "RED" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(if (continent-color-from-strain-name (car psline))
		    (print (cons (car psline) (list :ds 5 :wn "" :ns 12 :nc "black" :co 
					     (string (continent-color-from-strain-name (car psline)))
					     :oc "black")))
		  (progn
		    (print (car psline))
		    (cons (car psline) '(:ds 5 :wn "" :ns 12 :nc "black" :co "gray50"  :oc "black")))))))
   :not-found-action :add))


(defun color-code-amantadine-epi-continent-reference-background-strongest-marked (save &optional &key 
												 (reference-strains 'not-passed) 
												 marked 
												 intermediate-marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
	       ((member (car psline) intermediate-marked)
		(cons (car psline) (list :ds 8 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 14 :nc "black" :co "RED" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(let ((continent-color-from-strain-name (continent-color-from-strain-name     (car psline)))
		      (amantadine-info                  (cdc-amantadine-info-from-strain-name (car psline))))
		  `(,(car psline) 
		    :ds 5 
		    :wn "" 
		    :ns 12 
		    :nc "black" 
		    :co ,(if continent-color-from-strain-name
			     (string (continent-color-from-strain-name (car psline)))
			   "gray60")
		    :oc "black"
		    :sh ,(case amantadine-info
			   (R "DOWN-TRIANGLE")
			   (S "TRIANGLE")
			   (t "CIRCLE"))  ;; the 2 S35s, I will not guess are sensitive
		    )))))
   :not-found-action :add))


(defun color-code-amantadine2-epi-continent-reference-background-strongest-marked (save &optional &key 
												 (reference-strains 'not-passed) 
												 marked 
												 intermediate-marked)
  reference-strains ;; to stop the compiler bitching
  (set-plot-spec-in-save
   save
   (loop for psline in (plot-spec-from-save save) collect
	 (cond ((member (car psline) marked)
		(cons (car psline) (list :ds 11 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 24 :nc "black" :co "BLUE" :oc "black")))
	       ((member (car psline) intermediate-marked)
		(cons (car psline) (list :ds 8 :wn (format nil "~a" (remove-ag-sr-from-name (car psline))) :ns 14 :nc "black" :co "RED" :oc "black")))
	       ;; this line was first, changed 20040201, djs
	       ((and (equal "gray70" (snoop-keyword-arg :oc psline))
		     (serum-name-p (car psline)))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       ((equal "gray70" (snoop-keyword-arg :oc psline))
		(cons (car psline) '(:ds 6 :wn "" :ns 12 :nc "black" :co "{}" :oc "gray70")))
	       (t ;; assume is epi strain
		(let ((continent-color-from-strain-name (continent-color-from-strain-name     (car psline)))
		      (amantadine-info                  (cdc-amantadine-info-from-strain-name (car psline))))
		  `(,(car psline) 
		    :ds ,(case amantadine-info
			   (R 5)
			   (S 5)
			   (t 3))
		    :wn "" 
		    :ns 12 
		    :nc "black" 
		    :co ,(if continent-color-from-strain-name
			     (string (continent-color-from-strain-name (car psline)))
			   "gray60")
		    :oc "black"
		    :sh ,(case amantadine-info
			   (R "RECTANGLE")
			   (S "CIRCLE")
			   (t "CIRCLE"))  ;; the 2 S35s, I will not guess are sensitive
		    )))))
   :not-found-action :add))







