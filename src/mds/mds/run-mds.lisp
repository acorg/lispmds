(in-package user)

;;;----------------------------------------------------------------------
;;;                        RUNING METRIC MDS
;;;----------------------------------------------------------------------

#|
(defun make-random-coordss (number dimensions &optional (range 1))
  (loop for i below number collect 
	(loop for j below dimensions collect
	      (* range (knuth-random)))))
|#
#|
(defun make-random-coordss (number dimensions &optional (range 1) (include-row-and-col-adjusts nil) hi-table)
  (let ((random-coordss (loop for i below number collect 
			      (loop for j below dimensions collect
				    (* range (knuth-random)))))
	(max-log-titer (if include-row-and-col-adjusts (hi-table-values-max-value (hi-table-values hi-table)))))
    (if include-row-and-col-adjusts
	(append
	 random-coordss
	 (make-extras-hack 
	  (append (loop for i below number collect max-log-titer)  ;; this is a hack, but what is a better value?
		  (loop for i below number collect 0))))
      random-coordss)))
|#

#|
(setq all-strains-merge-take-2
  (extract-table-from-save
   (fi-in "mds/investigations/merge-hi-tables/all-strains-merge-take-2.save")))

(setq bar (calc-col-bases-as-max-in-col all-strains-merge-take-2))
(nthcdr (length (hi-table-antigens-short all-strains-merge-take-2)) 
	(transpose (hi-table-antigens all-strains-merge-take-2) bar))
|#


(defvar *all-strains-merge-take-2-max-in-cols*)
(setq *all-strains-merge-take-2-max-in-cols*
  '((HK/1/68-SR 7.0) 
    (EN/42/72-SR 8.0) 
    (PC/1/73-SR 9.0) 
    (SL/840/74-SR 8.0) 
    ;; (VI/3B/75-SR 7.0) 
    (VI/3B/75-SR 9.0)     ;; make this 9, as this is where the auto-adjust took this on tab9
    (VI/3A/75-SR 8.0) 
    (TE/1A/77-SR 9.0) 
    (TE/1B/77-SR 10.0) 
    (BA/1/79-SR 8.0) 
    (BA/2/79-SR 9.0) 
    (EN/496/80-SR 9.0) 
    (SH/31/80-SR 9.0) 
    (NL/241/82-SR 8.0) 
    (PH/2/82-SR 9.0) 
    (CE/1A/84-SR 7.442944) 
    (CE/1B/84-SR 7.0) 
    (NL/330/85-SR 9.0) 
    (NL/333/85-SR 8.0) 
    (WE/4/85-SR 10.0) 
    (CO/2/86-SR 10.0) 
    (LE/360/86-SR 7.0) 
    (SH/11/87-SR 10.0) 
    (SI/2/87-SR 10.0) 
    (VI/7/87-SR 10.0) 
    (NL/450/88-SR 10.0) 
    (BE/353/89-SR 9.0) 
    (GU/54/89-SR 10.0) 
    (NL/620/89-SR 11.0) 
    (HK/34/90-SR 10.0) 
    (VI/2/90-SR 10.0) 
    (LY/1149/91-SR 8.0) 
    (BE/32A/92-SR 8.0) 
    (BE/32B/92-SR 7.0) 
    (HK/23/92-SR 10.0) 
    (GD/25/93-SR 9.0) 
    (MA/G102/93-SR 11.0) 
    (MA/G252/93-SR 7.0) 
    (NL/241/93-SR 11.0) 
    (OS/2352/93-SR 8.0) 
    (SD/9/93-SR 8.16088) 
    (JO/33/94-SR 9.0) 
    (PR/413/94-SR 8.0) 
    (FI/338/95-SR 9.0) 
    (GE/A9509/95-SR 9.0) 
    (LY/2279/95-SR 8.0) 
    (NA/933/95-SR 7.3336024) 
    (NL/1/95-SR 9.0) 
    (NL/47/95-SR 8.0) 
    (NL/218A/95-SR 8.888743) 
    (NL/218B/95-SR 10.0) 
    (WU/359B/95-SR 8.0) 
    (WU/359A/95-SR 7.3336024) 
    (BR/8/96-SR 8.200163) 
    (NL/172/96-SR 8.0) 
    (SP/1/96-SR 8.200163) 
    (AU/10/97-SR 9.0) 
    (SY/5HAY/97-SR 8.584963) 
    (SY/5A/97-SR 11.0) 
    (SY/5B/97-SR 9.584963) 
    (NL/5/98-SR 9.0) 
    (NL/414/98-SR 11.0) 
    (NL/427/98-SR 10.584963) 
    (NL/462/98-SR 11.0) 
    (MW/10/99-SR 11.0) 
    (NL/301/99-SR 11.0) 
    (NL/3/00-SR 9.0) 
    (NL/182/00-SR 9.0)))

;; col basis assumptions now set in clinit.cl so that it can be set per site, and not changed on each release.  djs 20050406

(defun calc-col-bases-as-max-in-col (hi-table)
  ;; note, this takes a while, maybe 20 seconds for the all-strains-merge when we do calc of max in col
  ;; because picking out a col, when are 1500 strains in takes a while when done lots of times
  ;; we have this stuff to take max from all-strains-merge-take-2 because that makes sense
  ;; but it also doubles up as an efficiency hack
  (let* ((ag-sr-table-p (ag-sr-table-p hi-table))
	 (hi-table-sera (if ag-sr-table-p
			    (hi-table-antigens hi-table)  ;; efficiency hack, when sera are quick and just grab sera here
			  'not-set))
	 (homologous-values (if *set-col-basis-to-homologous-if-exists-otherwise-to-max-of-max-titer-and-1280*
				(hi-table-homologous-serum-values hi-table)))
         (max-in-table (if *set-col-bases-from-max-in-table* (apply-max (collect #'numberp (flatten (hi-table-values hi-table)))))))
    (loop for i below (hi-table-width hi-table) collect
          (if *set-col-bases-from-max-in-table*
              max-in-table
            (if (and *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2*
                     (listp hi-table-sera)   ;; not set sometimes as efficiency hack
                     (assoc-value (nth i hi-table-sera) *all-strains-merge-take-2-max-in-cols*))
                (assoc-value-1 (nth i hi-table-sera) *all-strains-merge-take-2-max-in-cols*)
              (if (and *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2*
                       (listp hi-table-sera)   ;; not set sometimes as efficiency hack
                       (ag-name-p (nth i hi-table-sera)))
                  -10000000
                (let* ((sera-values (flatten (hi-table-sera-values-by-index hi-table i)))   ;; flatten because for a list merge and bootstrap
                       ;; TIMING NOTE: THE ABOVE hi-table-sera-values-by-index TAKES ONE OR TWO MINS, and 96% OF TIME ON ALL STRAINS
                       ;; ALSO, DOING TRANSPOSE ON THE VALUES IS NOT MUCH FASTER
                       (numeric-sera-values (collect #'numberp sera-values))                ;;    we have a list of titers
                       (thresholded-sera-values (collect #'thresholdp sera-values)))
                  (if (null numeric-sera-values)
                      (if (null thresholded-sera-values)
                          -10000000
                        (if (or *set-col-basis-from-max-of-max-titer-and-this-titer*
                                *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2*)
                            (if *set-col-basis-from-max-of-max-titer-and-this-titer*
                                (std-log-titer *set-col-basis-from-max-of-max-titer-and-this-titer*)
                              7.0)
                          -10000000))
                    (if (or *set-col-basis-from-max-of-max-titer-and-this-titer*
                            *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2*)
                        (if ag-sr-table-p   ;; even if the 1280 is set (and it is the default) we only do this if we have an hi table
                            (max (if *set-col-basis-from-max-of-max-titer-and-this-titer*
                                     (std-log-titer *set-col-basis-from-max-of-max-titer-and-this-titer*)
                                   7.0)
                                 (apply-max numeric-sera-values))
                          (apply-max numeric-sera-values))
                      (if *set-col-basis-from-max-titer-in-col*
                          (apply-max numeric-sera-values)
                        (if *set-col-basis-to-this-value*
                            *set-col-basis-to-this-value*
                          (if *set-col-basis-to-homologous-if-exists-otherwise-to-max-of-max-titer-and-1280*
                              (max 7.0
                                   (let ((homologous-value (nth i homologous-values)))
                                     (if (numberp homologous-value)
                                         homologous-value
                                       (apply-max numeric-sera-values))))
                            (if *set-col-basis-from-max-of-max-titer-and-1280-plus-this-variable*
                                (+ (max 7.0 (apply-max numeric-sera-values))
                                   *set-col-basis-from-max-of-max-titer-and-1280-plus-this-variable*)
                              (apply-max numeric-sera-values))))))))))))))

(defun make-coordss-more (hi-table)
  (make-extras-hack 
   (append (calc-col-bases-as-max-in-col hi-table)
	   (loop for i below (hi-table-length hi-table) collect 0))))

(defun make-random-coordss (number dimensions &optional (range 1) (include-row-and-col-adjusts nil) hi-table &key zeroes)
  (let ((random-coordss (loop for i below number collect 
			      (loop for j below dimensions collect
				    (if (member j (assoc-value i zeroes))
					0.0
				      (* range (knuth-random))))))
	(col-bases
	 (if include-row-and-col-adjusts
	     (if (listp include-row-and-col-adjusts)   ;; coordss passed in, use the row and col adjusts from them
		 (col-bases include-row-and-col-adjusts)
	       (calc-col-bases-as-max-in-col hi-table))
	   'not-set))
	(row-adjusts
	 (if (listp include-row-and-col-adjusts)
	     (row-adjusts include-row-and-col-adjusts)
	   (loop for i below number collect 0))))
    (if include-row-and-col-adjusts
	(append
	 random-coordss
	 (make-extras-hack 
	  (append col-bases
		  row-adjusts)))
      random-coordss)))

(defun run-mds-from-coordss (trial-coordss &optional (starting-coords nil starting-coords-given?) &rest args)
  (apply #'metric-mds
	 (all-comparisons trial-coordss #'e-dist)
	 (if starting-coords-given?
	     starting-coords
	   (loop for coord in trial-coordss collect
		 (loop for x in coord collect (knuth-random))))
	 args))

(defun run-mds-from-dists (mds-f stress-component-f comparison-f target-dist-matrix starting-coordss &rest args)
  (if comparison-f
      (apply mds-f
	     target-dist-matrix
	     stress-component-f
	     comparison-f
	     (cond ((numberp starting-coordss)
		    (make-random-coordss (length target-dist-matrix)
					 starting-coordss
					 (hi-table-values-max-value target-dist-matrix)))
		   ((or (functionp starting-coordss)
			(and (listp starting-coordss) (eq 'lambda (car starting-coordss))))  ;; when we pass a list, remotely
		    (funcall starting-coordss))
		   (t starting-coordss))
	     args)
    (apply mds-f
	   target-dist-matrix
	   stress-component-f
	   (cond ((numberp starting-coordss)
		  (make-random-coordss (length target-dist-matrix)
				       starting-coordss
				       (hi-table-values-max-value target-dist-matrix)))
		 ((functionp starting-coordss)
		  (funcall starting-coordss))
		 (t starting-coordss))
	   args)))

(defun calc-stress-for-starting-coords-in-save (save)
  (let ((batch (multiple-value-list 
                (run-mds-from-dists
                 #'metric-mds-global-norm-conjugant-gradient-hi-metric
                 nil 
                 nil
                 (hi-table-values-to-base-1-symmetric-array-with-threshold-info (hi-table-values (table-from-save save)))
                 (starting-coordss-from-save save)
                 :hillclimbs-args '(:max-trials 0 :max-climbs 0)))))
    (values
     (nth 2 batch)       ;; the stress
     (butnth 1 batch)))) ;; in format for inclusion in batch runs
     

;;(make-random-coordss (hi-table-length hi-table) 
;;				       mds-dimensions
;;				       (hi-table-max-value hi-table))

#|
(run-mds-from-coordss '((0) (0.1) (1)))
((0.4541512913139898d0) (0.5540191378331234d0) (1.4537720386496757d0))
((0.8679678945289941d0) (0.16827777993108328d0) (0.4687069174124143d0))
((1.0832470116981123d0) (0.9823690303814003d0) (0.08115989077304189d0))
((-0.20823382109866134d0) (-0.10923324289535051d0) (0.7900672745440203d0))

(run-mds-from-coordss '((0) (0.1) (1)) :dribble nil)


(run-mds-from-coordss '((0 0) (0.1 0.1) (1 1)))
((0.827436707483077d0 0.9722484822821327d0) 
 (0.764384706790798d0 0.8460920585335048d0)
 (0.04776800782353709d0 -0.20427979622887837d0))

((-0.17678415777513035d0 0.9283021916844086d0) 
 (-0.04601424100043269d0 0.8760500179813269d0)
 (1.217222120381035d0 0.6993767651697986d0))





(run-mds-from-dists 
 (hi-table-values (extract-hi-table-by-indices hi77d '(0 2 5 11)))
 3
 :hillclimbs-args '(:max-climbs 10))

gave
((1.8873172263343934d0 2.2963315025165048d0 -0.1080256505342465d0)
 (1.2289314485867668d0 2.2024315307353026d0 -0.130377516744292d0)
 (-1.3670167070650119d0 1.686543164259317d0 -0.14990886860311586d0)
 (0.9439188621013308d0 -2.352304791160273d0 1.6986496376312825d0))
-1.34827969484771d0
EXHAUSTED-TRIALS-ON-INDIVIDUAL-CLIMB




;;here is the goal (the upper triangle here)
(pp-hi-table (extract-hi-table-by-indices hi77d '(0 2 5 11)))

                          HO68 EN72 PO73 VI75
A/HONGKONG/8/68            ---  0.0  4.0  5.0
A/ENGLAND/42/72            2.0  ---  2.0  5.0
A/PORTCHALMERS/1/73        3.0  2.0  ---  5.0
A/VICTORIA/3/75            3.0  4.0  3.0  ---

;;here is what we got
(mapcar #'2dp (all-comparisons foo #'e-dist))
(0.67 3.31 5.08 2.65 4.92 5.01)


(run-mds-from-dists 
 (hi-table-values hi85d)
 3
 :hillclimbs-args '(:max-climbs 1000))
(1.2861671590471688d0 0.6675073039686537d0 0.16517483606331954d0) 
(1.1584319179595641d0 -0.07915502902972091d0 -0.5141216402311413d0) 
(1.3710105863749176d0 2.4614100801578758d0 1.2187507956830343d0) 
(1.315418250696385d0 -0.14579939152378124d0 -0.8044437680720778d0) 
(2.1706079701936094d0 1.397902174408131d0 0.06084638986823156d0) 
(0.32504575211015196d0 -0.9809001754443545d0 -0.8098052578709967d0) 
(-1.052360611037799d0 0.29756314538485895d0 1.4787727103802715d0) 
(-1.0154455782982676d0 0.38958084647466956d0 1.5880341410046648d0) 
(0.21584735218812717d0 0.9627580294428746d0 1.1944932242145903d0) 
(0.21277793407819817d0 0.693128410188494d0 0.850637997707758d0) 
(-0.22637982900473894d0 0.8176716829299617d0 1.390240510245568d0) 
stress is -21.264655213311677d0  (started at -200)
EXHAUSTED-TRIALS-ON-INDIVIDUAL-CLIMB


(g-plot-correlate 
 (apply #'append (hi-table-values-upper-short-triangle (hi-table-values hi85d)))
 (mapcar #'2dp (all-comparisons foo #'e-dist)))




(run-mds-from-dists 
 (hi-table-values hi85d)
 3
 :hillclimbs-args '(:max-climbs 1000 :max-trials-in-one-climb 1000))

stress is -21.23336432684635d0
so about the same, whether 100 trials in one climb, or 1000

and same again -21.231066082046347d0

so maybe there is an easy optimum to reach, 
wonder if it is global?


;;now, look at a table the same size as hi85d, an 11x11, but without error
(setq table (loop for i below (length hi85d) collect (loop for i below 3 collect (knuth-random))))
(run-mds-from-coordss table :hillclimbs-args '(:max-climbs 1000 :max-trials-in-one-climb 1000))
stress is -0.04811046329043595d0

(g-plot-correlate 
 (mapcar #'2dp (all-comparisons table #'e-dist))
 (mapcar #'2dp (all-comparisons foo #'e-dist)))

ok, so i can easily work, and get good results on an 11x11 perfect table


to do:
- set up for ordinal
- set up for noisy data

|#



(defun show-coordss (coordss 
		     &optional (stress-text "") 
			       coords-names
			       coords-colors)

  (if (not coords-names)
      (setq coords-names (loop for i below (length coordss) collect (number->string i))))
  
  (if (not coords-colors)
      (setq coords-colors (determine-coords-colors 'name-based (length coordss) nil coords-names)))

  (let ((tk (make-mds-visualization-window)))
    (reset-canvas-coord-transformations tk)
    (set-basis-vectors tk (firstn 2 (make-unit-vectors (length (car coordss)))))
    (visualize-mds-coordss
     tk 'new
     coordss 
     coords-names 
     coords-colors
     (format nil "~s" stress-text))
    (values
     tk
     coordss)))

#|
(defun gnuplot-show-mds (coordss 
			 &optional (stress-text "") 
				   (coords-names (loop for i below (length coordss) collect (number->string i)))
				   coords-colors)
  (let* ((min-x (apply #'min (nths 0 coordss)))
	 (max-x (apply #'max (nths 0 coordss)))
	 (min-y (apply #'min (nths 1 coordss)))	
	 (max-y (apply #'max (nths 1 coordss)))
	 (x-range (- max-x min-x))
	 (y-range (- max-y min-y)))
    (gnuplot 
|#    
				   

(defun run-mds-mostly-offline-from-actual-coordss (mds-f actual-coordss 
						   &optional start-coordss 
							     (component-stress-f (cond ((eql mds-f #'metric-mds) #'square)
										       ((eql mds-f #'ordinal-full-mds) #'minus-square-zero)
										       ((eql mds-f #'ordinal-partial-mds) #'log-squash)
										       (t (error ""))))
						   &rest hillclimbs-args)
  (let ((mds-coordss
	 (funcall mds-f
		  (all-comparisons actual-coordss #'e-dist)
		  component-stress-f
		  (if start-coordss
		      start-coordss
		    (make-random-coordss (length actual-coordss) (length (car actual-coordss))))
		  :hillclimbs-args hillclimbs-args)))
    (show-coordss mds-coordss "Final")))

;;(show-coordss foo10 "Actual")
;;comment in show-coordss
;;(run-mds-mostly-offline-from-actual-coordss #'metric-mds foo10 nil #'square :max-climbs 250 :max-trials 250)





;;;----------------------------------------------------------------------
;;;                    new stuff, dec 2002
;;;        for running with coords fixed, and cols adjusted
;;;     in time these should be options to make-master-mds-window
;;;                  and batch-mds-keyword-args
;;;----------------------------------------------------------------------

(defun run-from-save (save &optional &key adjust-coordss adjust-columns adjust-rows)
  (let ((original-*adjust-coordss* *adjust-coordss*)
	(original-*adjust-columns* *adjust-columns*)
	(original-*adjust-rows*    *adjust-rows*))
    (setq *adjust-coordss* adjust-coordss)
    (setq *adjust-columns* adjust-columns)
    (setq *adjust-rows*    adjust-rows)
    (let* ((batch-run-result (batch-mds-keyword-args
			      (table-from-save save)
			      :starting-coordss (starting-coordss-from-save save))))

      (setq *adjust-coordss* original-*adjust-coordss*)
      (setq *adjust-columns* original-*adjust-columns*)
      (setq *adjust-rows*    original-*adjust-rows*)

      (values
       (make-save-form
	:hi-table (table-from-save save)
	:starting-coordss (nth 0 batch-run-result))
       (nth 1 batch-run-result)))))