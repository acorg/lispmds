(in-package user)


;;;----------------------------------------------------------------------
;;;                  CONSISTENT COLORS FOR STRAIN NAMES
;;;----------------------------------------------------------------------

(defun color-from-name (name)
  (setq name (if (numberp name) 
		 (number->string name)
	       (if (null name)
		   ""
		 (string name))))
  (multiple-value-bind (full-abbreviation location isolate year suffix name-abbreviation)
      (smart-long-strain-abbreviation name)
    (if (and name-abbreviation
	     isolate
	     year
	     (not (equal "" name-abbreviation))
	     (not (equal "" isolate))
	     (not (equal "" year))
             (not (char-member #\/ isolate))  ;; can happen if avian name, or A/ B/ start, and can be interpreted by lisp as division
             (not (char-member #\/ year)))    ;; which can cause error if year is 00, and we get division by zero
	(hsv-tk-color-from-international-flu-strain-nomenclature name-abbreviation isolate year)
      (hsv-tk-color (0-to-1-from-string name) 1.0 1.0))))

(defun 0-to-1-from-string (string)
  (if (zerop (length string))
      0.0
    (let ((first-char (aref (string-upcase string) 0)))
      (cond ((char-alpha-p first-char) (float (/ (- (char-int first-char) (char-int #\A)) 25)))
	    ((char-number-p first-char) (float (/ (- (char-int first-char) (char-int #\0)) 9)))
	    (t 1.0)))))

(defun char-alpha-p (char)
  (and (>= (char-int char) (char-int #\A))
       (<= (char-int char) (char-int #\Z))))

(defun char-number-p (char)
  (and (>= (char-int char) (char-int #\0))
       (<= (char-int char) (char-int #\9))))

(defun hsv-tk-color-from-international-flu-strain-nomenclature (name-abbreviation isolate year)
  (let ((h    ;; use the first char of name for the hue
	      ;; but special case dutch strains for fun to orange
	 (if (or (string-equal name-abbreviation "NL")
		 (string-equal name-abbreviation "nl")
		 (string-equal name-abbreviation "BI")
		 (string-equal name-abbreviation "bi")
		 (string-equal name-abbreviation "RD")
		 (string-equal name-abbreviation "rd")
		 (string-equal name-abbreviation "AM")
		 (string-equal name-abbreviation "am")
		 (string-equal name-abbreviation "UT")
		 (string-equal name-abbreviation "ut"))
	     0.10
	   (0-to-1-from-string name-abbreviation)))
	(s    ;; use isolate 0 as saturation 1, if not numeric then use 1, 
	      ;; and do log falling off so we can go to 6 figures without going past 0.25
	      ;; maybe use this for year
	 (let ((isolate (read-from-string isolate)))
	   (if (numberp isolate)
	       (progn
		 (max 0.0
		      (- 1 (/ (log isolate 20) 5))))
	     0.5)))
	(v    ;; use year for value (duller is earlier)
	      ;; use 2010 or later as 1.0, and go back so 1968 is 0.5, don't go lower than 0.25
	      ;; if the year is not a number, then make it black
	 (let ((year (read-from-string year)))
	   (if (numberp year)
	       (progn
		 (if (< year 30)      ;; y2k bug in international naming, move problem to 2030
		     (setq year (+ year 100)))
		 (setq year (- year 30))  ;; set year 1930 as year 0, and give (and less) this v=0.0 (black)
		 (min 1.0
		      (/ year 80.0))  ;; set 2010 as the time of peak intensisty (later years will have same intensity)
		 1.0)  ;; NO, ALWAYS HAVE SATURATION AT MAXIMUM
	     0.0))))
    (values 
     (hsv-tk-color h s v)
     h s v)))
