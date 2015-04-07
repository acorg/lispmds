(in-package user)

(defun show-lapedes-coordss (lines &optional order-example colors)
  (multiple-value-bind (hi-table coordss)
      (lapedes-to-hi-table lines order-example)
    (let ((coordss (loop for coords in coordss collect
			 (loop for x in coords collect (/ x 100))))
	  (names (append (mapcar #'string-capitalize 
				 (mapcar #'strain-abbreviation (mapcar #'suffix-as-ag (hi-table-antigens hi-table)))) 
			 (mapcar #'string-capitalize 
				 (mapcar #'strain-abbreviation (mapcar #'suffix-as-sr (hi-table-antigens hi-table)))))))
      (show-coordss coordss "Lapedes Run" names colors)
      (values coordss names))))

(defun setup-lapedes (lines &optional order-example antigen-indices)
  (multiple-value-bind (hi-table coordss)
      (lapedes-to-hi-table lines order-example)
    (if antigen-indices 
	(setq hi-table (extract-hi-table-by-indices hi-table antigen-indices)))
    (if antigen-indices 
	(setq coordss (append 
		       (loop for index in antigen-indices collect (nth index coordss))
		       (loop for index in antigen-indices collect (nth index (nthcdr (/ (length coordss) 2) coordss))))))
    (make-and-visualize-random-coordss 
     (length coordss)
     (length (car coordss))
     :coordss coordss
     :upper-square t
     :coordss-names (mapcar #'strain-abbreviation (hi-table-antigens (make-ag-sr-hi-table hi-table))))))

(defun setup-lapedes-hi-table (lines &optional order-example antigen-indices num-dimensions)
  (multiple-value-bind (hi-table coordss)
      (lapedes-to-hi-table lines order-example)
    coordss
    (if antigen-indices 
	(setq hi-table (extract-hi-table-by-indices hi-table antigen-indices)))
    (make-strain-selection-window (make-ag-sr-hi-table hi-table) t nil num-dimensions)))

;;;----------------------------------------------------------------------
;;;                      RUNNING
;;;----------------------------------------------------------------------
#|
(setq 4b '((a/hongkong/8/68      0.0 0.5 4.0 5.5)
	   (a/england/42/72      1.0 0.0 1.0 4.0)
	   (a/portchalmers/1/73  5.0 3.0 0.0 3.0)
	   (a/victoria/3/75      4.5 4.0 5.0 0.0)))
(make-strain-selection-window (make-ag-sr-hi-table 4b) t)
(show-lapedes-coordss (fi-in-readline "~/mds/data/4b.lapedes"))

(setq christs '((a/hongkong/8/68      1280  960   80   30)
		(a/england/42/72       640 1280  640   80)
		(a/portchalmers/1/73    10   40  320   40)
		(a/victoria/3/75        30   40   20  640)))
(make-strain-selection-window (make-ag-sr-hi-table christs) t)
(make-strain-selection-window (make-ag-sr-hi-table christs) t nil 3)  ;;look in 3d

(make-strain-selection-window (make-ag-sr-hi-table hi85) t nil 3)
(show-lapedes-coordss (fi-in-readline "~dsmith/mds/data/hi85.lapedes") hi85)
(setup-lapedes (fi-in-readline "~dsmith/mds/data/hi85.lapedes") hi85)
(setup-lapedes-hi-table (fi-in-readline "~dsmith/mds/data/hi85.lapedes") hi85 nil 3)

(show-coordss (run-mds-from-dists #'ordinal-partial-mds #'log-squash-minus 
		     (hi-table-values (make-ag-sr-hi-table christs)) 3
		     :hillclimbs-args (list :max-climbs 10000 :max-trials 1000))
	      "Final"
	      (abbreviate-strains (hi-table-antigens (make-ag-sr-hi-table christs))))

(show-coordss (run-mds-from-dists #'ordinal-partial-mds #'log-squash-minus 
		     (hi-table-values (make-ag-sr-hi-table hi85)) 3
		     :hillclimbs-args (list :max-climbs 10000 :max-trials 1000))
	      "Final"
	      (abbreviate-strains (hi-table-antigens (make-ag-sr-hi-table hi85))))

(setq colors (determine-coords-colors 'random-paired (hi-table-length hi85)))
(show-lapedes-coordss (fi-in-readline "~dsmith/mds/data/hi85.lapedes") hi85 colors)
(make-strain-selection-window (make-ag-sr-hi-table hi85) t nil 3 colors)

(setq colors '("#a696e4" 
"#97c7fd" 
"#b517ea" 
"#ebb283" 
"#fb0e63" 
"#b3282e" 
"#f124ee" 
"#7e2810" 
"#0a7579" 
"#189417" 
"#1cf195" 
"#a696e4" 
"#97c7fd" 
"#b517ea" 
"#ebb283" 
"#fb0e63" 
"#b3282e" 
"#f124ee" 
"#7e2810" 
"#0a7579" 
"#189417" 
"#1cf195" ))

;;here are my coordss on hi85, stress 62
(-15.8339615 5.7491155 6.695419) 
(-19.487225 8.931915 -4.3033075) 
(-21.186424 -15.2170105 12.945486) 
(-22.328325 16.78205 -9.470088) 
(-10.390739 31.786587 7.3210344) 
(-13.995343 18.022863 -11.294607) 
(16.263794 0.6044546 -5.9192324) 
(16.08133 7.9236803 -5.790329) 
(7.2381372 10.442173 0.18705538) 
(10.531695 20.148993 -0.03421218) 
(44.911583 -14.54054 2.3198113) 
(-11.188126 -3.9235044 0.10796019) 
(-19.328365 -9.699995 -15.997232) 
(-8.547327 -20.165123 20.919458) 
(-24.06641 0.5879571 -13.404852) 
(-2.5206885 10.485255 24.452208) 
(-14.226655 4.1094875 -29.804626) 
(11.701634 -4.882919 -29.160555) 
(10.107625 -7.0543013 -2.2286437) 
(3.9058661 -0.9095979 21.428913) 
(7.177698 -1.1388956 17.67886) 
(22.912601 -8.960544 27.768377) 

;;and lapedes coordss on hi85
(0.20554075 0.45110667 -0.374702) 
(0.9248883 0.0955828 -0.27926627) 
(-0.0728781 0.8697445 -0.78988206) 
(1.3873857 0.02965487 -0.09346332) 
(0.77004373 1.4361522 0.82884425) 
(0.941528 -0.5640282 -0.14115411) 
(-0.05580657 -0.44930318 0.58800566) 
(-0.057789132 -0.48441228 0.5668258) 
(-0.24465705 -0.44277492 0.23996648) 
(-0.50371814 -1.0387692 -0.16066352) 
(-1.6981552 -1.7370219 0.061084937) 
(0.30585593 0.35478386 -0.3905201) 
(0.89841247 0.09649786 -1.2974774) 
(-0.74981356 1.1664083 -0.9577057) 
(1.2673243 0.28422436 -0.7663154) 
(-0.35420012 0.8374294 0.5119428) 
(1.429288 -0.96225065 -0.7116416) 
(0.080771744 -0.64007586 1.7187155) 
(-0.7118809 -0.16711602 0.7840815) 
(-0.8989286 0.49194068 0.17986783) 
(-0.9445943 0.16758117 -0.0557305) 
(-1.9498559 0.18703188 0.50957173) 
|#

