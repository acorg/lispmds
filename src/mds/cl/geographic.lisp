(in-package user)

;;;----------------------------------------------------------------------
;;;                      projection utilities
;;;----------------------------------------------------------------------

(defun minutes-to-float (minutes)
  (if (> (length (anything->string minutes)) 2)
      (progn 
	(format t "~%Warning minutes (~s) > 2 digits" minutes)
	(setq minutes (read-from-string (substring (anything->string minutes) 0 1)))))  ;; should really just loose the last 2 chars
  (/ minutes 60.0))

(defun degrees-minutes-nsew-to-plus-minus-float (degrees minutes nsew)
  (let ((minutes-as-float (minutes-to-float minutes))
	(sign (cond ((or (eql 'n nsew) (eql 'e nsew)) 1)
		    ((or (eql 's nsew) (eql 'w nsew)) -1)
		    (t (error "~%Expected nsew to be N S E or W but got ~a~%" nsew)))))
    (* sign (+ degrees minutes-as-float))))


;;;----------------------------------------------------------------------
;;;                   latutude/longitide to xy
;;;----------------------------------------------------------------------

(defun ll-to-xytop (lat long)
  ;; where do maps start?
  (let ((x (+ 180.0 long))
	(y (if (plusp lat)
	       (- 90.0 lat)
	     (+ 90 (- lat)))))
    (if (or (< x 0)
	    (< y 0)
	    (> x 360)
	    (> y 180))
	(error "~%Lat (~d) or long (~d) out of range" lat long)
      (list x y))))
	    
	
(defun ll-degrees-minutes-nsew-to-xytop (lat-degrees  lat-minutes  lat-ns
					 long-degrees long-minutes long-ew)
  (ll-to-xytop
   (degrees-minutes-nsew-to-plus-minus-float lat-degrees  lat-minutes  lat-ns)
   (degrees-minutes-nsew-to-plus-minus-float long-degrees long-minutes long-ew)))


;;;----------------------------------------------------------------------
;;;            Converting Latitude and Longitude to UTM
;;;----------------------------------------------------------------------

;; (run-shell-command "open http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.HTM")

;; /Users/dsmith/m/investigations/spatial/lat-long-conversion-c
;; c++ UTMConversions.cpp SwissGrid.cpp LatLong-UTMconversion.cpp
;; c++ -o ll-to-utm ll-to-utm.cpp SwissGrid.cpp LatLong-UTMconversion.cpp
;; ll-to-utm 20 60.0123
;; 2214270.98407   187361.67979   41Q

(defun ll-to-utm (lat long)
  (run-shell-command (format nil "cl/UTM-ll-to-utm ~a ~a > /tmp/ll-to-utm" lat long))
  (car (fi-in-readline-to-list "/tmp/ll-to-utm")))

;; the below does not work on Mac OS X with c++, as i have to "make-shared", but c++ says -shared not supported
;; maybe this is on my install disk Xtools, but leave for now.  make do with the ugly call above.
;; (load "cl/UTM-ll-to-utm-split-for-ff-call.o" :foreign t)
;; ; Foreign loading cl/UTM-ll-to-utm-split-for-ff-call.o.
;; Error: This version of Allegro CL can only load shared object files. Recompile your foreign code in a position independent way
;;        ("cc -c  xxx.c" on PowerPC), then process the resulting object file to produce a shared object file ("make_shared -o
;;        xxx.ext ..." on PowerPC), and load the resulting shared object (N.B.: "ext" currently includes the following list:
;;       ("dylib")) into Allegro CL.

;; c++ -c -o UTM-ll-to-utm-split-for-ff-call.o UTM-ll-to-utm-split-for-ff-call.cpp
;;(if (equal (user-name) "dsmith")
;;    (progn
;;      (require :foreign)
;;      (load "cl/UTM-ll-to-utm-split-for-ff-call.o" :foreign t)
;;      (ff:defforeign 'llt_utm_split_northing :return-type :double-float)
;;      (ff:defforeign 'llt_utm_split_easting  :return-type :double-float)
;;      (ff:defforeign 'llt_utm_split_region   :return-type :double-float)))


(defun ll-degrees-minutes-nsew-to-utm (lat-degrees  lat-minutes  lat-ns
				       long-degrees long-minutes long-ew)
  (ll-to-utm
   (degrees-minutes-nsew-to-plus-minus-float lat-degrees  lat-minutes  lat-ns)
   (degrees-minutes-nsew-to-plus-minus-float long-degrees long-minutes long-ew)))

(defun utm-to-coords (northing easting region)
  (let* ((region-letters '(;;a b
			   c d e f g h 
			   ;;i 
			   j k l m 
			   n 
			   ;;o 
			   p q r s t u v w x
			   ;;y z
			   ))
	 (southern-hemisphere-region-letters (first-half  region-letters))
	 (northern-hemisphere-region-letters (second-half region-letters))
	 (region-number (read-from-string (reverse (substring (reverse (string region)) 1))))
	 (region-letter (read-from-string (substring (reverse (string region)) 0 0)))
	 (hemisphere (cond ((member region-letter northern-hemisphere-region-letters) 'north)
			   ((member region-letter southern-hemisphere-region-letters) 'south)
			   (t (error "Unexpected region-letter ~d" region-letter)))))
    (list 
     ;; each easting region is 6 degrees, which is 40,000 / 60 = 666,667 meters.
     ;; the center of the region is designated 500,000 meters to avoid negative eastings,
     ;; this means that the eastings range in a region is from 177,777 to 833,333 meters.
     (- (+ (* (dec region-number) 666667) easting) 177777)
     (if (eql hemisphere 'north)
	 (+ northing 10000000)
       northing))))

(defun utm-to-xytop (northing easting region)
  (let-list ((x y) (utm-to-coords northing easting region))
	    (list
	     x
	     (- 20000000 y))))


;;(fll (remove-duplicates (sort-nth 3 (map-apply #'utm-to-coords (filter #'null (nths 1 utm-infos)))) :test #'equal))

