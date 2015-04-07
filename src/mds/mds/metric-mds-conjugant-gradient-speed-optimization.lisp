
;;;----------------------------------------------------------------------
;;;                      SPEED OPTIMIZATION
;;;----------------------------------------------------------------------

(setq foo
  '((HK/1/68 0.0 2.6564548 3.2527776 3.81845 4.0248737 4.583572 4.99615 4.6043034 4.3391 5.291813 4.506673 5.5428805 4.097108
	     4.4731693 4.0081115 4.150055 4.5313287 3.9652767 4.3160377) 
    (HK/107/71 DONT-CARE 0.0 2.115701 2.7945526 3.0237157 3.9339788 4.859943 4.8648396 4.6352262 5.6760015 5.0095143 6.0599847
     4.7056193 5.103789 4.587 4.6547465 4.932883 4.3315015 4.6547465) 
    (EN/42/72 DONT-CARE DONT-CARE 0.0 1.4474938 2.6903708 3.0315797 4.4400773 4.990467 4.8067 5.5487313 5.1685863 6.1302986
     4.990467 5.296098 4.8000593 4.8648396 5.313953 4.995236 4.9038367) 
    (PC/1/73 DONT-CARE DONT-CARE DONT-CARE 0.0 2.6186147 2.4003968 3.8913822 4.5721726 4.421334 5.1573195 5.02849 5.849347 4.815649
     4.9777703 4.638616 4.7056193 5.122313 4.9473414 4.735881) 
    (SL/840/74 DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.6636887 4.186145 4.6547465 4.215571 5.0922766 4.4347115 5.2968626
     4.2370253 4.466925 4.3744493 4.053217 5.0190115 4.8697314 4.7056193) 
    (VI/3/75 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.7516227 4.3315015 3.9107437 4.329065 4.4400773 5.00082
     4.5145264 4.4454484 4.684584 4.5355735 5.4772253 5.3407683 5.173191) 
    (TE/1/77 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.6636887 3.0757303 2.7992928 3.7480152 4.2575564
     4.292851 4.0029306 4.618039 4.6649656 5.5075703 5.4248104 5.177791) 
    (BA/1/79 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 1.7995018 2.4143963 2.7945526 3.8047736
     3.1471832 3.1154943 3.4733143 3.927922 4.2650294 4.292851 3.927922) 
    (PH/2/82 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.79811 1.9783567 3.3946736
     2.4121892 2.5354626 2.8413105 3.2964034 3.7682366 3.8371108 3.451651) 
    (CC/4/85 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.658459 2.4984562
     3.3755007 2.5364628 3.8398619 3.9854307 4.9452085 4.9113903 4.7487183) 
    (LE/360/86 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.9776294
     2.0236695 2.0704484 2.5198467 2.8784914 3.7032802 3.7352886 3.5321653) 
    (SI/2/87 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0
     2.8800778 2.104417 3.6210003 4.060212 5.51272 5.4824038 5.2471843) 
    (SH/11/87 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE 0.0 1.7185526 1.9002807 2.845213 3.6903994 3.7225182 3.4503276) 
    (GU/54/89 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE 0.0 2.1140604 2.4497023 3.7034209 3.7354279 3.545768) 
    (BE/353/89 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE DONT-CARE 0.0 1.9377449 2.9183488 2.9908745 2.4186966) 
    (BE/32/92 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 2.4102952 2.4591906 2.2886884) 
    (SD/9/93 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 1.1338934 1.4474938) 
    (JO/33/94 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0 1.8126538) 
    (WU/359/95 DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE
     DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE DONT-CARE 0.0)))

(setq bar 
  '((0.47122732700000003d0 8.17472d-4) 
    (0.69492394d0 0.790359326d0) 
    (0.742570225d0 0.234752646d0) 
    (0.947228896d0 0.382326961d0) 
    (0.7829319210000001d0 0.5234796340000001d0) 
    (0.174627004d0 0.565250024d0) 
    (0.590253934d0 0.6845335880000001d0) 
    (0.087780919d0 0.6192015160000001d0) 
    (0.162400922d0 0.555211121d0) 
    (0.956611894d0 0.17453806000000002d0) 
    (0.798572727d0 0.12548483700000002d0) 
    (0.42027073400000003d0 0.5458595380000001d0) 
    (0.705232171d0 0.425049938d0) 
    (0.9844101500000001d0 0.870596777d0) 
    (0.8030386610000001d0 0.55832914d0) 
    (0.440412625d0 0.278731081d0) 
    (0.558947776d0 0.766198951d0) 
    (0.309764258d0 0.640248518d0) 
    (0.836040042d0 0.689282782d0)))

(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 21,850 msec user, 50 msec system
; cpu time (gc)     1,990 msec user, 0 msec system
; cpu time (total)  23,840 msec user, 50 msec system
; real time  29,993 msec
; space allocation:
;  2,115 cons cells, 0 symbols, 354,451,608 other bytes
((3.752794411436357d0 0.6930995289411661d0) (3.8797452362216016d0 -0.6741150215247242d0)
 (3.3852924499453487d0 -1.8428338298585965d0) (2.5815833673493875d0 -2.279017957755786d0)
 (1.9251616862976353d0 -1.7689390307562063d0) (0.7555905899526187d0 -2.74151428941689d0)
 (-1.1433539120436664d0 -2.24697554563235d0) (-1.2897684629357866d0 -0.5568762813181103d0)
 (-0.5553599105626531d0 0.2528872716920293d0) (-2.5014209945787673d0 -0.24993179202353932d0) ...)
NIL
0.16758109080109646d0
MULTIPLE-END-CONDITIONS
NIL


timing a function in lisp
(time (excl::time-a-funcall (^ () (loop for i below 10000 sum (+ 3 4 5))) (^ (&rest args) (print args))))

(50 0 340 20 379 270158 1 880184) 
; cpu time (non-gc) 290 msec user, 20 msec system
; cpu time (gc)     50 msec user, 0 msec system
; cpu time (total)  340 msec user, 20 msec system
; real time  390 msec
; space allocation:
;  270,183 cons cells, 1 symbol, 880,720 other bytes
120000

excl::time-a-funcall returns (i can figure from the above):
  gc-user gc-system cpu-total-user cpu-total-system real-time cons-cells symbols other-bytes



(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (sqrt
		   (/ (loop for r from 1 below NUM_POINTS sum
			    (loop for s from (+ r 1) to NUM_POINTS sum
				  (let ((dtarget (dhat-ll target-dist-matrix r s))
					(dmds    (d-from-array p r s NUM_DIMS)))
				    (if (dont-care-p dtarget)
					0
				      (expt (- dmds dtarget) 2)))))
		      (loop for r from 1 below NUM_POINTS sum
			    (loop for s from (+ r 1) to NUM_POINTS sum
				  (let ((dtarget (dhat-ll target-dist-matrix r s))
					(dmds    (d-from-array p r s NUM_DIMS)))
				    (if (dont-care-p dtarget)
					0                          ;; divide by 0 error here if all all target distances are 0
				      (expt dmds 2))))))))
	       (^ (&rest args) (print (list 'f args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (* 0.5
				       (sqrt
					(/ (loop for r from 1 below NUM_POINTS sum
						 (loop for s from (+ r 1) to NUM_POINTS sum
						       (let ((dtarget (dhat-ll target-dist-matrix r s))
							     (dmds    (d-from-array p r s NUM_DIMS)))
							 (if (dont-care-p dtarget)
							     0
							   (expt (- dmds dtarget) 2)))))
					   (loop for r from 1 below NUM_POINTS sum
						 (loop for s from (+ r 1) to NUM_POINTS sum
						       (let ((dtarget (dhat-ll target-dist-matrix r s))
							     (dmds    (d-from-array p r s NUM_DIMS)))
							 (if (dont-care-p dtarget)
							     0                          ;; divide by 0 error here if all all target distances are 0
							   (expt dmds 2)))))))
				       (/ (- (* (loop for r from 1 below NUM_POINTS sum
						      (loop for s from (+ r 1) to NUM_POINTS sum
							    (let ((dtarget (dhat-ll target-dist-matrix r s))
								  (dmds    (d-from-array p r s NUM_DIMS)))
							      (if (dont-care-p dtarget)
								  0
								(expt dmds 2)))))
						(loop for s from 1 to NUM_POINTS sum
						      (if (= a s)
							  0
							(let ((dmds    (d-from-array p a s NUM_DIMS))
							      (dtarget (dhat-ll target-dist-matrix a s)))
							  (if (dont-care-p dtarget)
							      0
							    (* 2 
							       (- dmds dtarget)
							       (/ 1 dmds)
							       (- (coord-from-array p a k NUM_DIMS) 
								  (coord-from-array p s k NUM_DIMS))))))))
					     ;; possible divide by 0 if all below is zero
					     (* (loop for r from 1 below NUM_POINTS sum
						      (loop for s from (+ r 1) to NUM_POINTS sum
							    (let ((dtarget (dhat-ll target-dist-matrix r s))
								  (dmds    (d-from-array p r s NUM_DIMS)))
							      (if (dont-care-p dtarget)
								  0  
								(expt (- dmds dtarget) 2)))))
						(loop for s from 1 to NUM_POINTS sum
						      (if (= a s)
							  0
							(let ((dtarget (dhat-ll target-dist-matrix a s)))
							  (if (dont-care-p dtarget)
							      0
							    (* 2 
							       (- (coord-from-array p a k NUM_DIMS) 
								  (coord-from-array p s k NUM_DIMS)))))))))
					  (expt 
					   (loop for r from 1 below NUM_POINTS sum
						 (loop for s from (+ r 1) to NUM_POINTS sum
						       (let ((dtarget (dhat-ll target-dist-matrix r s))
							     (dmds    (d-from-array p r s NUM_DIMS)))
							 (if (dont-care-p dtarget)
							     0  ;; divide by 0 error here if all all target distances are 0
							   (expt dmds 2)))))
					   2)))
				  0))))
		  (values p df))
	       (^ (&rest args) (print (list 'df args))))
	      ))
	 args))


USER(39): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))

(F (0 0 10 0 8 0 0 124792)) 
(DF (110 0 870 10 1126 0 0 12150656)) 
(F (0 0 10 0 8 0 0 157624)) 
(F (0 0 10 0 8 0 0 157560)) 
(F (0 0 10 0 8 0 0 157656)) 
(F (0 0 0 0 8 0 0 157560)) 
(F (0 0 10 0 8 0 0 157752)) 
(F (0 0 10 0 8 0 0 157816)) 
(F (0 0 10 0 8 0 0 157640)) 
(F (0 0 0 0 8 0 0 157496)) 
(F (0 0 10 0 7 0 0 157720)) 
(F (10 0 20 0 45 0 0 157720)) 
(F (0 0 10 0 7 0 0 157656)) 
(F (0 0 10 0 8 0 0 157656)) 
(F (0 0 0 0 8 0 0 157432)) 
(F (0 0 10 0 8 0 0 157656)) 
(F (0 0 10 0 8 0 0 157592)) 
(F (0 0 0 0 8 0 0 157656)) 
(F (0 0 10 0 8 0 0 157624)) 
(F (0 0 10 0 7 0 0 157544)) 
(F (10 0 30 0 44 0 0 157720)) 
(F (0 0 10 0 8 0 0 157592)) 
(F (0 0 10 0 8 0 0 157784)) 
(F (0 0 0 0 7 0 0 124888)) 
(DF (80 0 840 0 1046 0 0 12159968)) 
(F (0 0 10 0 7 0 0 157720)) 
(F (0 0 10 0 8 0 0 157544)) 
(F (0 0 10 0 8 0 0 157400)) 
(F (0 0 10 0 8 0 0 157720)) 
...
(DF (80 0 860 0 1053 0 0 12141344)) 
(F (0 0 10 0 8 0 0 157528)) 
(F (0 0 0 0 8 0 0 157224)) 
(F (0 0 0 0 8 0 0 157592)) 
(F (0 0 10 0 8 0 0 157592)) 
(F (0 0 10 0 8 0 0 157272)) 
(F (0 0 10 0 8 0 0 157752)) 
(F (0 0 10 0 8 0 0 157272)) 
(F (10 0 30 0 44 0 0 157592)) 
(F (0 0 10 0 8 0 0 157752)) 
(F (0 0 10 0 8 0 0 157720)) 
(F (0 0 10 0 7 0 0 157720)) 
(F (0 0 0 0 8 0 0 157688)) 
(F (0 0 0 0 8 0 0 157656)) 
; cpu time (non-gc) 21,950 msec user, 110 msec system
; cpu time (gc)     2,110 msec user, 10 msec system
; cpu time (total)  24,060 msec user, 120 msec system
; real time  31,726 msec
; space allocation:
;  8,301 cons cells, 0 symbols, 354,814,192 other bytes
((3.7527944392093078d0 0.693099609024839d0) (3.8797452180435297d0 -0.6741150016781146d0)
 (3.385292450594064d0 -1.8428338224181027d0) (2.58158334185514d0 -2.27901802058449d0)
 (1.925161898562927d0 -1.7689387562495171d0) (0.7555907172637286d0 -2.7415142865963067d0)
 (-1.1433539006932263d0 -2.246975566419524d0) (-1.2897684301229317d0 -0.5568763555037476d0)
 (-0.5553598933169067d0 0.2528872093815044d0) (-2.5014210153436016d0 -0.24993193590485446d0) ...)
NIL
0.16758109304807914d0
MULTIPLE-END-CONDITIONS
NIL




;;--------------  make the print of function call times be cumulative as well as last call


(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (/ (loop for r from 1 below NUM_POINTS sum
			      (loop for s from (+ r 1) to NUM_POINTS sum
				    (let ((dtarget (dhat-ll target-dist-matrix r s))
					  (dmds    (d-from-array p r s NUM_DIMS)))
				      (if (dont-care-p dtarget)
					  0
					(expt (- dmds dtarget) 2)))))
			(loop for r from 1 below NUM_POINTS sum
			      (loop for s from (+ r 1) to NUM_POINTS sum
				    (let ((dtarget (dhat-ll target-dist-matrix r s))
					  (dmds    (d-from-array p r s NUM_DIMS)))
				      (if (dont-care-p dtarget)
					  0                          ;; divide by 0 error here if all all target distances are 0
					(expt dmds 2))))))))
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))




USER(41): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))

(F (0 0 10 0 8 0 0 124792) (0 0 10 0 8 0 0 124792)) 
(DF (110 0 870 0 1112 0 0 12150656) (110 0 870 0 1112 0 0 12150656)) 
(F (0 0 20 0 16 0 0 282416) (0 0 10 0 8 0 0 157624)) 
(F (0 0 30 0 24 0 0 439976) (0 0 10 0 8 0 0 157560)) 
(F (0 0 40 0 32 0 0 597632) (0 0 10 0 8 0 0 157656)) 
(F (0 0 50 0 40 0 0 755192) (0 0 10 0 8 0 0 157560)) 
(F (0 0 50 0 48 0 0 912944) (0 0 0 0 8 0 0 157752)) 
(F (0 0 60 0 56 0 0 1070760) (0 0 10 0 8 0 0 157816)) 
(F (0 0 60 0 64 0 0 1228400) (0 0 0 0 8 0 0 157640)) 
(F (0 0 70 0 72 0 0 1385896) (0 0 10 0 8 0 0 157496)) 
(F (0 0 70 0 80 0 0 1543616) (0 0 0 0 8 0 0 157720)) 
(F (20 0 100 0 125 0 0 1701336) (20 0 30 0 45 0 0 157720)) 
(F (20 0 110 0 133 0 0 1858992) (0 0 10 0 8 0 0 157656)) 
(F (20 0 120 0 141 0 0 2016648) (0 0 10 0 8 0 0 157656)) 
(F (20 0 130 0 149 0 0 2174080) (0 0 10 0 8 0 0 157432)) 
(F (20 0 140 0 157 0 0 2331736) (0 0 10 0 8 0 0 157656)) 
(F (20 0 150 0 165 0 0 2489328) (0 0 10 0 8 0 0 157592)) 
(F (20 0 160 0 173 0 0 2646984) (0 0 10 0 8 0 0 157656)) 
(F (20 0 170 0 181 0 0 2804608) (0 0 10 0 8 0 0 157624)) 
(F (20 0 180 0 189 0 0 2962152) (0 0 10 0 8 0 0 157544)) 
(F (30 0 190 0 235 0 0 3119872) (10 0 10 0 46 0 0 157720)) 
(F (30 0 200 0 243 0 0 3277464) (0 0 10 0 8 0 0 157592)) 
(F (30 0 210 0 251 0 0 3435248) (0 0 10 0 8 0 0 157784)) 
(F (30 0 220 0 259 0 0 3560136) (0 0 10 0 8 0 0 124888)) 
(DF (180 0 1690 0 2159 0 0 24310624) (70 0 820 0 1047 0 0 12159968)) 
(F (30 0 230 0 267 0 0 3717856) (0 0 10 0 8 0 0 157720)) 
(F (30 0 240 0 275 0 0 3875400) (0 0 10 0 8 0 0 157544)) 
....
(F (450 0 3510 10 4623 0 0 58978584) (0 0 10 0 8 0 0 157528)) 
(F (450 0 3520 10 4631 0 0 59136304) (0 0 10 0 8 0 0 157720)) 
(F (450 0 3530 10 4639 0 0 59261000) (0 0 10 0 8 0 0 124696)) 
(DF (1770 0 20110 50 25885 0 0 291493472) (70 0 830 0 1072 0 0 12141344)) 
(F (450 0 3540 10 4647 0 0 59418528) (0 0 10 0 8 0 0 157528)) 
(F (450 0 3550 10 4655 0 0 59575752) (0 0 10 0 8 0 0 157224)) 
(F (450 0 3550 10 4663 0 0 59733344) (0 0 0 0 8 0 0 157592)) 
(F (450 0 3560 10 4671 0 0 59890936) (0 0 10 0 8 0 0 157592)) 
(F (450 0 3570 10 4679 0 0 60048208) (0 0 10 0 8 0 0 157272)) 
(F (450 0 3580 10 4687 0 0 60205960) (0 0 10 0 8 0 0 157752)) 
(F (470 0 3610 10 4737 0 0 60363232) (20 0 30 0 50 0 0 157272)) 
(F (470 0 3610 10 4745 0 0 60520824) (0 0 0 0 8 0 0 157592)) 
(F (470 0 3620 10 4753 0 0 60678576) (0 0 10 0 8 0 0 157752)) 
(F (470 0 3630 10 4761 0 0 60836296) (0 0 10 0 8 0 0 157720)) 
(F (470 0 3630 10 4769 0 0 60994016) (0 0 0 0 8 0 0 157720)) 
(F (470 0 3640 10 4777 0 0 61151704) (0 0 10 0 8 0 0 157688)) 
(F (470 0 3650 10 4785 0 0 61309360) (0 0 10 0 8 0 0 157656)) 
; cpu time (non-gc) 21,920 msec user, 60 msec system
; cpu time (gc)     2,240 msec user, 0 msec system
; cpu time (total)  24,160 msec user, 60 msec system
; real time  33,688 msec
; space allocation:
;  12,481 cons cells, 0 symbols, 354,844,312 other bytes
((3.7527944392093078d0 0.693099609024839d0) (3.8797452180435297d0 -0.6741150016781146d0)
 (3.385292450594064d0 -1.8428338224181027d0) (2.58158334185514d0 -2.27901802058449d0)
 (1.925161898562927d0 -1.7689387562495171d0) (0.7555907172637286d0 -2.7415142865963067d0)
 (-1.1433539006932263d0 -2.246975566419524d0) (-1.2897684301229317d0 -0.5568763555037476d0)
 (-0.5553598933169067d0 0.2528872093815044d0) (-2.5014210153436016d0 -0.24993193590485446d0) ...)
NIL
0.16758109304807914d0
MULTIPLE-END-CONDITIONS
NIL

                      real          cons
(F   (470 0  3650 10  4785 0 0  61309360)) 
(DF (1770 0 20110 50 25885 0 0 291493472)) 

33 seconds total, 30 of which are in f or df, 5 in f, 26 in df
ok, it is df which we must optimize

but i'll do f first because it is smaller, and i think i'll use the same techniques

interesting that the conjugant gradient calcs are <10%.


;;;----------------------------------------------------------------------
;;;                      optimization ideas
;;;----------------------------------------------------------------------

anything to be gained by type declarations?
only calculating dmds and dtarget once
and testing dont-care-p once
doing the dont-care-p without a funtion call
try doing square instead of expt  --  yes  about 13% on d
doing * x x instead of the square fucntion call  -- no
indexing the target-distance-matrix not as an ll but as an array


;;;----------------------------------------------------------------------
;;;                      OPTIMIZE D
;;;----------------------------------------------------------------------


;;;----------------------------------------------------------------------
;;;                 first try merging to loops
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (expt (- dmds dtarget) 2)))
					   (setq denominator (+ denominator (expt dmds 2))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


(F (290 0 1940 10 2747 0 0 35260320) (0 0 10 0 5 0 0 97368)) 
(F (290 0 1950 10 2752 0 0 35341368) (0 0 10 0 5 0 0 81048)) 
(DF (1630 0 19320 30 24879 0 0 279355232) (80 0 860 0 1078 0 0 12175488)) 
(F (290 0 1960 10 2757 0 0 35438832) (0 0 10 0 5 0 0 97464)) 
(F (290 0 1960 10 2762 0 0 35536136) (0 0 0 0 5 0 0 97304)) 
(F (290 0 1970 10 2766 0 0 35633488) (0 0 10 0 4 0 0 97352)) 
(F (290 0 1980 10 2771 0 0 35730776) (0 0 10 0 5 0 0 97288)) 
(F (290 0 1980 10 2776 0 0 35828160) (0 0 0 0 5 0 0 97384)) 
(F (290 0 1990 10 2780 0 0 35925560) (0 0 10 0 4 0 0 97400)) 
(F (290 0 2000 10 2785 0 0 36022944) (0 0 10 0 5 0 0 97384)) 
(F (290 0 2000 10 2790 0 0 36120280) (0 0 0 0 5 0 0 97336)) 
(F (300 0 2010 10 2838 0 0 36217632) (10 0 10 0 48 0 0 97352)) 
(F (300 0 2010 10 2843 0 0 36315048) (0 0 0 0 5 0 0 97416)) 
(F (300 0 2020 10 2848 0 0 36412336) (0 0 10 0 5 0 0 97288)) 
(F (300 0 2020 10 2853 0 0 36509624) (0 0 0 0 5 0 0 97288)) 
(F (300 0 2030 10 2858 0 0 36607008) (0 0 10 0 5 0 0 97384)) 
(F (300 0 2030 10 2862 0 0 36687880) (0 0 0 0 4 0 0 80872)) 
(DF (1710 0 20160 40 25960 0 0 291496576) (80 0 840 10 1081 0 0 12141344)) 
(F (300 0 2040 10 2867 0 0 36785168) (0 0 10 0 5 0 0 97288)) 
(F (300 0 2050 10 2872 0 0 36882296) (0 0 10 0 5 0 0 97128)) 
(F (300 0 2050 10 2877 0 0 36979616) (0 0 0 0 5 0 0 97320)) 
(F (300 0 2050 10 2882 0 0 37076936) (0 0 0 0 5 0 0 97320)) 
(F (300 0 2060 10 2887 0 0 37174112) (0 0 10 0 5 0 0 97176)) 
(F (300 0 2070 10 2892 0 0 37271480) (0 0 10 0 5 0 0 97368)) 
(F (300 0 2070 10 2897 0 0 37368656) (0 0 0 0 5 0 0 97176)) 
(F (300 0 2070 10 2901 0 0 37465976) (0 0 0 0 4 0 0 97320)) 
(F (300 0 2080 10 2906 0 0 37563376) (0 0 10 0 5 0 0 97400)) 
(F (310 0 2090 10 2951 0 0 37660760) (10 0 10 0 45 0 0 97384)) 
(F (310 0 2090 10 2955 0 0 37758144) (0 0 0 0 4 0 0 97384)) 
(F (310 0 2100 10 2959 0 0 37855512) (0 0 10 0 4 0 0 97368)) 
(F (310 0 2110 10 2964 0 0 37952864) (0 0 10 0 5 0 0 97352)) 
; cpu time (non-gc) 20,690 msec user, 60 msec system
; cpu time (gc)     2,020 msec user, 0 msec system
; cpu time (total)  22,710 msec user, 60 msec system
; real time  32,728 msec
; space allocation:
;  11,959 cons cells, 0 symbols, 331,349,584 other bytes
((3.7527944392098087d0 0.6930996090313815d0) (3.879745218042963d0 -0.6741150016735653d0)
 (3.385292450597992d0 -1.8428338224115024d0) (2.5815833418531735d0 -2.2790180205847874d0)
 (1.925161898574776d0 -1.7689387562308236d0) (0.7555907172712218d0 -2.7415142865910886d0)
 (-1.1433539006796305d0 -2.246975566422051d0) (-1.28976843011638d0 -0.556876355513068d0)
 (-0.5553598933151289d0 0.2528872093818697d0) (-2.501421015339549d0 -0.24993193591807492d0) ...)
NIL
0.16758109304807522d0
MULTIPLE-END-CONDITIONS
NIL

ok the answer is the same

old timing
                      real          cons
(F   (470 0  3650 10  4785 0 0  61309360)) 
(DF (1770 0 20110 50 25885 0 0 291493472)) 
; real time  33,688 msec

new timing
                      real          cons
(F   (310 0  2110 10  2964 0 0  37952864) (0 0 10 0 5 0 0 97352)) 
(DF (1710 0 20160 40 25960 0 0 291496576) (80 0 840 10 1081 0 0 12141344)) 
;; real time  32,728 msec

ok, so f went from 4.8 to 3.0.  about a 30% improvement.


;;;----------------------------------------------------------------------
;;;              now try square and * x x instead of expt
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (square (- dmds dtarget))))
					   (setq denominator (+ denominator (square dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


old timing
                      real          cons
(F   (310 0  2110 10  2964 0 0  37952864) (0 0 10 0 5 0 0 97352)) 
(DF (1710 0 20160 40 25960 0 0 291496576) (80 0 840 10 1081 0 0 12141344)) 
;; real time  32,728 msec

(F   (260 0  1920 10  2556 0 0    34718912) (0 0 10 0 5 0 0 89144)) 
(DF (1610 0 19750 30 25950 0 0 -4003470720) (70 0 820 0 1074 0 0 12141344)) 
;; real time  32,393 msec

wow, surprisingly it looks like it makes a difference: 3.0 to 2.6, about 13% difference

;;;----------------------------------------------------------------------
;;;                      now try * x x not square
;;;----------------------------------------------------------------------


(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (* (- dmds dtarget) (- dmds dtarget))))
					   (setq denominator (+ denominator (* dmds dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


old timing:
                      real            cons
(F   (260 0  1920 10  2556 0 0    34718912) (0 0 10 0 5 0 0 89144)) 
(DF (1610 0 19750 30 25950 0 0 -4003470720) (70 0 820 0 1074 0 0 12141344)) 
;; real time  32,393 msec

new timing:
                      real          cons
(F   (280 0  1980  0  2689 0 0  36335888) (0 0 0 0 5 0 0 93248)) 
(DF (1710 0 20220 10 26477 0 0 291496576) (70 0 840 0 1091 0 0 12141344))
;; real time  32,353 msec

no difference, if anything slightly worse, ah maybe because we are doing the subtraction twice


;;;----------------------------------------------------------------------
;;;                  do a let so only subtract once
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (let ((error (- dmds dtarget)))
					   (setq numerator   (+ numerator   (* error error)))
					   (setq denominator (+ denominator (* dmds dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


old timing:
                      real          cons
(F   (280 0  1980  0  2689 0 0  36335888) (0 0 0 0 5 0 0 93248)) 
(DF (1710 0 20220 10 26477 0 0 291496576) (70 0 840 0 1091 0 0 12141344))
;; real time  32,353 msec

new timing:
                       real          cons
(F  (280   0  1850  0  2623 0 0  34718912) (0 0 10 0 4 0 0 89144)) 
(DF (1640 10 19900 50 26164 0 0 291496576) (70 0 820 0 1088 0 0 12141344)) 
; real time  32,732 msec

it does not help, that is a surprise.  function call must be good!  (good!)




;;;----------------------------------------------------------------------
;;;                      revert to using square
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (square (- dmds dtarget))))
					   (setq denominator (+ denominator (square dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (sqrt
					  (/ (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt (- dmds dtarget) 2)))))
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0                          ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


old timing:
                      real          cons
(F   (280 0  1980  0  2689 0 0  36335888) (0 0 0 0 5 0 0 93248)) 
(DF (1710 0 20220 10 26477 0 0 291496576) (70 0 840 0 1091 0 0 12141344))
;; real time  32,353 msec

new timing:
                      real          cons
(F   (250 0  2070  0  2569 0 0  34718912) (0 0 10 0 5 0 0 89144)) 
(DF (1630 0 20880 60 23214 0 0 291496576) (70 0 850 0 1105 0 0 12141344)) 
; real time  29,030 msec

how did we get 3s faster?  less gc?  the mds result is the same.

maybe because we were not watching the printout?
yes:  

watching:     ; real time  33,740 msec
not watching  ; real time  27,593 msec

20% difference.

ok, make a new flash light, that prints out the final results and compares the answers





;;;----------------------------------------------------------------------
;;;                   do the 1/S correction in the df
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (square (- dmds dtarget))))
					   (setq denominator (+ denominator (square dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (* 0.5
					 (/ 1
					    (sqrt
					     (/ (loop for r from 1 below NUM_POINTS sum
						      (loop for s from (+ r 1) to NUM_POINTS sum
							    (let ((dtarget (dhat-ll target-dist-matrix r s))
								  (dmds    (d-from-array p r s NUM_DIMS)))
							      (if (dont-care-p dtarget)
								  0
								(expt (- dmds dtarget) 2)))))
						(loop for r from 1 below NUM_POINTS sum
						      (loop for s from (+ r 1) to NUM_POINTS sum
							    (let ((dtarget (dhat-ll target-dist-matrix r s))
								  (dmds    (d-from-array p r s NUM_DIMS)))
							      (if (dont-care-p dtarget)
								  0                          ;; divide by 0 error here if all all target distances are 0
								(expt dmds 2))))))))
					 (/ (- (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0
								  (expt dmds 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dmds    (d-from-array p a s NUM_DIMS))
								(dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- dmds dtarget)
								 (/ 1 dmds)
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))))))
					       ;; possible divide by 0 if all below is zero
					       (* (loop for r from 1 below NUM_POINTS sum
							(loop for s from (+ r 1) to NUM_POINTS sum
							      (let ((dtarget (dhat-ll target-dist-matrix r s))
								    (dmds    (d-from-array p r s NUM_DIMS)))
								(if (dont-care-p dtarget)
								    0  
								  (expt (- dmds dtarget) 2)))))
						  (loop for s from 1 to NUM_POINTS sum
							(if (= a s)
							    0
							  (let ((dtarget (dhat-ll target-dist-matrix a s)))
							    (if (dont-care-p dtarget)
								0
							      (* 2 
								 (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS)))))))))
					    (expt 
					     (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  ;; divide by 0 error here if all all target distances are 0
							     (expt dmds 2)))))
					     2)))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


old timing:
                      real          cons
(F   (250 0  2070  0  2569 0 0  34718912) (0 0 10 0 5 0 0 89144)) 
(DF (1630 0 20880 60 23214 0 0 291496576) (70 0 850 0 1105 0 0 12141344)) 
; real time  29,030 msec


(F   (270 0  2190  0  2679 0 0  34046032) (0 0 0 0 4 0 0 89096))
(DF (2000 0 22900 10 29895 0 0 327948464) (70 0 870 0 914 0 0 12133856)) 
;; real time  35,857 msec


24 df calls the old way   and stress 0.1675
28 the new way            and stress 0.1658




;;;----------------------------------------------------------------------
;;;             does the gradient function have to be spot on?
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (sqrt
		     (let ((numerator 0)
			   (denominator 0))
		       (loop for r from 1 below NUM_POINTS sum
			     (loop for s from (+ r 1) to NUM_POINTS sum
				   (let ((dtarget (dhat-ll target-dist-matrix r s))
					 (dmds    (d-from-array p r s NUM_DIMS)))
				     (if (not (dont-care-p dtarget))
					 (progn
					   (setq numerator   (+ numerator   (square (- dmds dtarget))))
					   (setq denominator (+ denominator (square dmds))))))))
		       (/ numerator denominator))))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (/ (- (* (loop for r from 1 below NUM_POINTS sum
						     (loop for s from (+ r 1) to NUM_POINTS sum
							   (let ((dtarget (dhat-ll target-dist-matrix r s))
								 (dmds    (d-from-array p r s NUM_DIMS)))
							     (if (dont-care-p dtarget)
								 0
							       (expt dmds 2)))))
					       (loop for s from 1 to NUM_POINTS sum
						     (if (= a s)
							 0
						       (let ((dmds    (d-from-array p a s NUM_DIMS))
							     (dtarget (dhat-ll target-dist-matrix a s)))
							 (if (dont-care-p dtarget)
							     0
							   (* 2 
							      (- dmds dtarget)
							      (/ 1 dmds)
							      (- (coord-from-array p a k NUM_DIMS) 
								 (coord-from-array p s k NUM_DIMS))))))))
					    ;; possible divide by 0 if all below is zero
					    (* (loop for r from 1 below NUM_POINTS sum
						     (loop for s from (+ r 1) to NUM_POINTS sum
							   (let ((dtarget (dhat-ll target-dist-matrix r s))
								 (dmds    (d-from-array p r s NUM_DIMS)))
							     (if (dont-care-p dtarget)
								 0  
							       (expt (- dmds dtarget) 2)))))
					       (loop for s from 1 to NUM_POINTS sum
						     (if (= a s)
							 0
						       (let ((dtarget (dhat-ll target-dist-matrix a s)))
							 (if (dont-care-p dtarget)
							     0
							   (* 2 
							      (- (coord-from-array p a k NUM_DIMS) 
								 (coord-from-array p s k NUM_DIMS)))))))))
					 (expt 
					  (loop for r from 1 below NUM_POINTS sum
						(loop for s from (+ r 1) to NUM_POINTS sum
						      (let ((dtarget (dhat-ll target-dist-matrix r s))
							    (dmds    (d-from-array p r s NUM_DIMS)))
							(if (dont-care-p dtarget)
							    0  ;; divide by 0 error here if all all target distances are 0
							  (expt dmds 2)))))
					  2))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


24 df calls the old way    and stress 0.1675   (DF (1640 10 20950 40 22548 0 0 291496576) (80 0 890 0 922 0 0 12141344)) 
28 the new way             and stress 0.1658   (DF (2000  0 22900 10 29895 0 0 327948464) (70 0 870 0 914 0 0 12133856)) 
29 without the * 0.5 (1/S) and stress 0.1658   (DF (1240 10 15210 50 17223 0 0 214795344) (30 0 530 0 556 0 0  7414288)) 



;;;----------------------------------------------------------------------
;;; take the sqrt out of the original stress (can add it later for display)
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (let ((f-calls-cumulative '(0 0 0 0 0 0 0 0))
	(df-calls-cumulative '(0 0 0 0 0 0 0 0)))
    (apply #'mds
	   (setq *metric-conjugant-gradient-stress-f* 
	     (^ (p)
		(excl::time-a-funcall 
		 (^ ()
		    (let ((numerator 0)
			  (denominator 0))
		      (loop for r from 1 below NUM_POINTS sum
			    (loop for s from (+ r 1) to NUM_POINTS sum
				  (let ((dtarget (dhat-ll target-dist-matrix r s))
					(dmds    (d-from-array p r s NUM_DIMS)))
				    (if (not (dont-care-p dtarget))
					(progn
					  (setq numerator   (+ numerator   (square (- dmds dtarget))))
					  (setq denominator (+ denominator (square dmds))))))))
		      (/ numerator denominator)))  ;; would be good to get for divide by zero
		 (^ (&rest args) 
		    (setq f-calls-cumulative
		      (lvector-sum f-calls-cumulative args))
		    (print (list 'f f-calls-cumulative args))))))
	   (let ((moveable-coords 
		  (if (listp (snoop-moveable-coords args))
		      (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		    (snoop-moveable-coords args))))  
	     (^ (p df)
		(excl::time-a-funcall 
		 (^ ()
		    (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			  (loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
				(setf (fref df (+ (* (dec a) NUM_DIMS) k))
				  (if (or (eql 'all moveable-coords)
					  (member a moveable-coords))
				      (/ (- (* (loop for r from 1 below NUM_POINTS sum
						     (loop for s from (+ r 1) to NUM_POINTS sum
							   (let ((dtarget (dhat-ll target-dist-matrix r s))
								 (dmds    (d-from-array p r s NUM_DIMS)))
							     (if (dont-care-p dtarget)
								 0
							       (expt dmds 2)))))
					       (loop for s from 1 to NUM_POINTS sum
						     (if (= a s)
							 0
						       (let ((dmds    (d-from-array p a s NUM_DIMS))
							     (dtarget (dhat-ll target-dist-matrix a s)))
							 (if (dont-care-p dtarget)
							     0
							   (* 2 
							      (- dmds dtarget)
							      (/ 1 dmds)
							      (- (coord-from-array p a k NUM_DIMS) 
								 (coord-from-array p s k NUM_DIMS))))))))
					    ;; possible divide by 0 if all below is zero
					    (* (loop for r from 1 below NUM_POINTS sum
						     (loop for s from (+ r 1) to NUM_POINTS sum
							   (let ((dtarget (dhat-ll target-dist-matrix r s))
								 (dmds    (d-from-array p r s NUM_DIMS)))
							     (if (dont-care-p dtarget)
								 0  
							       (expt (- dmds dtarget) 2)))))
					       (loop for s from 1 to NUM_POINTS sum
						     (if (= a s)
							 0
						       (let ((dtarget (dhat-ll target-dist-matrix a s)))
							 (if (dont-care-p dtarget)
							     0
							   (* 2 
							      (- (coord-from-array p a k NUM_DIMS) 
								 (coord-from-array p s k NUM_DIMS)))))))))
					 (expt 
					  (loop for r from 1 below NUM_POINTS sum
						(loop for s from (+ r 1) to NUM_POINTS sum
						      (let ((dtarget (dhat-ll target-dist-matrix r s))
							    (dmds    (d-from-array p r s NUM_DIMS)))
							(if (dont-care-p dtarget)
							    0  ;; divide by 0 error here if all all target distances are 0
							  (expt dmds 2)))))
					  2))
				    0))))
		    (values p df))
		 (^ (&rest args) 
		    (setq df-calls-cumulative
		      (lvector-sum df-calls-cumulative args))
		    (print (list 'df df-calls-cumulative args))))
		))
	   args)))


24 df calls the old way    and stress 0.1675   (DF (1640 10 20950 40 22548 0 0 291496576) (80 0 890 0 922 0 0 12141344)) 
28 the new way             and stress 0.1658   (DF (2000  0 22900 10 29895 0 0 327948464) (70 0 870 0 914 0 0 12133856)) 
29 without the * 0.5 (1/S) and stress 0.1658   (DF (1240 10 15210 50 17223 0 0 214795344) (30 0 530 0 556 0 0  7414288)) 
32 without sqrt in d       and stress 0.1658   (DF (1320 20 16040 50 17532 0 0 222226624) (50 0 560 0 575 0 0  7421840)) 

with sqrt in f     (F (260 0 2370 0 2429 0 0 37223896) (0 0 0 0 4 0 0 89176)) 
without sqrt in f  (F (260 0 2160 0 2320 0 0 37790608) (0 0 0 0 4 0 0 89024)) 
  (saves 0.5%)

not doing the 1/S in df almost halfs the other bytes consed!

OK: don't do the sqrt and the 1/s, so we have the same in both, and do a post-process on the stress?



;;;----------------------------------------------------------------------
;;;     now do the new function to show the run time data at the end
;;;                  and not do a bunch of printout
;;;----------------------------------------------------------------------

(setq final-coordss
  '((3.7585887301899428d0 0.47767691492578923d0) 
    (3.7915620598373696d0 -0.8978693600340909d0) 
    (3.2149071224504318d0 -2.0362486630555083d0) 
    (2.3559079261384657d0 -2.433953813190584d0) 
    (1.8049077983819455d0 -1.8024778323845188d0) 
    (0.5380378086904865d0 -2.737954522626052d0) 
    (-1.3246222567527557d0 -2.126386427606915d0) 
    (-1.3873363342412877d0 -0.4498517353976217d0) 
    (-0.5601748436718866d0 0.30582010955488914d0) 
    (-2.5677030723538334d0 0.004037716292217314d0) 
    (-1.0649856426718818d0 1.271888700031533d0) 
    (-3.042788479314647d0 1.6229163673741989d0) 
    (-0.5870661123850497d0 2.0702534764431006d0) 
    (-1.2573998202565042d0 2.095615771009208d0) 
    (0.26188138757545776d0 2.485878965051776d0) 
    (1.1025154553762415d0 2.161272650187338d0) 
    (2.0279729254517633d0 3.538205877764814d0) 
    (2.600521696992938d0 2.8735695639336596d0) 
    (1.8025197765628083d0 3.0078581517267646d0) ))


(defvar f-calls-cumulative)
(defvar df-calls-cumulative)
    
(defun run-test ()
  (setq f-calls-cumulative '(0 0 0 0 0 0 0 0))
  (setq df-calls-cumulative '(0 0 0 0 0 0 0 0))
  (let ((result
	 (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))))
    (if (equal (dps-tree result 10) (dps-tree final-coordss 10))
	(format t "~%Same answer as before")
      (format t "~%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  different answer  <<<<<<<<<<<<<<<<<<<<<<<"))
    (format t "~%~a~%~a" f-calls-cumulative df-calls-cumulative)))

(F   (270 0  2190  0  2679 0 0  34046032) (0 0 0 0 4 0 0 89096))
(DF (2000 0 22900 10 29895 0 0 327948464) (70 0 870 0 914 0 0 12133856)) 
;; real time  35,857 msec

    
(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (let ((numerator 0)
			(denominator 0))
		    (loop for r from 1 below NUM_POINTS sum
			  (loop for s from (+ r 1) to NUM_POINTS sum
				(let ((dtarget (dhat-ll target-dist-matrix r s))
				      (dmds    (d-from-array p r s NUM_DIMS)))
				  (if (not (dont-care-p dtarget))
				      (progn
					(setq numerator   (+ numerator   (square (- dmds dtarget))))
					(setq denominator (+ denominator (square dmds))))))))
		    (/ numerator denominator)))  ;; would be good to get for divide by zero
	       (^ (&rest args) 
		  (setq f-calls-cumulative
		    (lvector-sum f-calls-cumulative args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (/ (- (* (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0
							     (expt dmds 2)))))
					     (loop for s from 1 to NUM_POINTS sum
						   (if (= a s)
						       0
						     (let ((dmds    (d-from-array p a s NUM_DIMS))
							   (dtarget (dhat-ll target-dist-matrix a s)))
						       (if (dont-care-p dtarget)
							   0
							 (* 2 
							    (- dmds dtarget)
							    (/ 1 dmds)
							    (- (coord-from-array p a k NUM_DIMS) 
							       (coord-from-array p s k NUM_DIMS))))))))
					  ;; possible divide by 0 if all below is zero
					  (* (loop for r from 1 below NUM_POINTS sum
						   (loop for s from (+ r 1) to NUM_POINTS sum
							 (let ((dtarget (dhat-ll target-dist-matrix r s))
							       (dmds    (d-from-array p r s NUM_DIMS)))
							   (if (dont-care-p dtarget)
							       0  
							     (expt (- dmds dtarget) 2)))))
					     (loop for s from 1 to NUM_POINTS sum
						   (if (= a s)
						       0
						     (let ((dtarget (dhat-ll target-dist-matrix a s)))
						       (if (dont-care-p dtarget)
							   0
							 (* 2 
							    (- (coord-from-array p a k NUM_DIMS) 
							       (coord-from-array p s k NUM_DIMS)))))))))
				       (expt 
					(loop for r from 1 below NUM_POINTS sum
					      (loop for s from (+ r 1) to NUM_POINTS sum
						    (let ((dtarget (dhat-ll target-dist-matrix r s))
							  (dmds    (d-from-array p r s NUM_DIMS)))
						      (if (dont-care-p dtarget)
							  0  ;; divide by 0 error here if all all target distances are 0
							(expt dmds 2)))))
					2))
				  0))))
		  (values p df))
	       (^ (&rest args) 
		  (setq df-calls-cumulative
		    (lvector-sum df-calls-cumulative args))))
	      ))
	 args))


old timing:
                      real          cons
(F   (270 0  2190  0  2679 0 0  34046032) (0 0 0 0 4 0 0 89096))
(DF (2000 0 22900 10 29895 0 0 327948464) (70 0 870 0 914 0 0 12133856)) 


new timing:
                      real          cons
(F   (300  0  2100 0   3101 0 0    37790608))
(DF (1430 10 15950 80 21621 0 0 -4072740672))
; real time  24,905 msec


run again:
F:   (260  0  2190  0  2447 0 0  37790608)
df: (1250 10 16380 20 17571 0 0 222226624)
; real time  20,137 msec

     (230 0   2100 10  2320 0 0  37790608)
    (1350 0  16400 20 17556 0 0 222226624)
; real time  19,985 msec


;;;----------------------------------------------------------------------
;;;                      optimize df
;;;----------------------------------------------------------------------


;;;----------------------------------------------------------------------
;;;                    contract the loops
;;;----------------------------------------------------------------------


(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (let ((numerator 0)
			(denominator 0))
		    (loop for r from 1 below NUM_POINTS do
			  (loop for s from (+ r 1) to NUM_POINTS do
				(let ((dtarget (dhat-ll target-dist-matrix r s))
				      (dmds    (d-from-array p r s NUM_DIMS)))
				  (if (not (dont-care-p dtarget))
				      (progn
					(setq numerator   (+ numerator   (square (- dmds dtarget))))
					(setq denominator (+ denominator (square dmds))))))))
		    (/ numerator denominator)))  ;; would be good to get for divide by zero
	       (^ (&rest args) 
		  (setq f-calls-cumulative
		    (lvector-sum f-calls-cumulative args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (let ((numerator-term-1-rs-loop 0)
					  (numerator-term-1-s-loop  0)
					  (numerator-term-2-rs-loop 0)
					  (numerator-term-2-s-loop  0)
					  (denominator-rs-loop      0))
				      (loop for r from 1 below NUM_POINTS do
					    (loop for s from (+ r 1) to NUM_POINTS do
						  (let ((dtarget (dhat-ll target-dist-matrix r s))
							(dmds    (d-from-array p r s NUM_DIMS)))
						    (if (not (dont-care-p dtarget))
							(progn
							  (setq numerator-term-1-rs-loop 
							    (+ numerator-term-1-rs-loop (square dmds)))
							  (setq numerator-term-2-rs-loop 
							    (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
							  (setq denominator-rs-loop
							    numerator-term-1-rs-loop))))))
				      (loop for s from 1 to NUM_POINTS do
					    (if (= a s)
						0
					      (let ((dmds    (d-from-array p a s NUM_DIMS))
						    (dtarget (dhat-ll target-dist-matrix a s)))
						(if (dont-care-p dtarget)
						    0
						  (progn
						    (setq numerator-term-1-s-loop
						      (+ numerator-term-1-s-loop
							 (* 2 
							    (- dmds dtarget)
							    (/ 1 dmds)
							    (- (coord-from-array p a k NUM_DIMS) 
							       (coord-from-array p s k NUM_DIMS)))))
						    (setq numerator-term-2-s-loop
						      (+ numerator-term-2-s-loop
							 (* 2 
							    (- (coord-from-array p a k NUM_DIMS) 
							       (coord-from-array p s k NUM_DIMS))))))))))
				      (/ (- (* numerator-term-1-rs-loop
					       numerator-term-1-s-loop)
					    (* numerator-term-2-rs-loop
					       numerator-term-2-s-loop))
					 (square denominator-rs-loop))
				      )
				  0))))
		  (values p df))
	       (^ (&rest args) 
		  (setq df-calls-cumulative
		    (lvector-sum df-calls-cumulative args))))
	      ))
	 args))


old timing:
                       real          cons
F    (230 0   2100 10  2320 0 0  37790608)
DF  (1350 0  16400 20 17556 0 0 222226624)
; real time  19,985 msec


new timing:
                      real         cons
F      (250 0 2060 10 2182 0 0 35840112)
DF     (540 0 5760  0 6299 0 0 88861056)
; real time  8,605 msec

Same MDS results answer as before   GOOD!

and from 17.5s to 6s.  60% faster!



;;;----------------------------------------------------------------------
;;;                  only calc once the coords-coords
;;;----------------------------------------------------------------------


(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (let ((numerator 0)
			(denominator 0))
		    (loop for r from 1 below NUM_POINTS do
			  (loop for s from (+ r 1) to NUM_POINTS do
				(let ((dtarget (dhat-ll target-dist-matrix r s))
				      (dmds    (d-from-array p r s NUM_DIMS)))
				  (if (not (dont-care-p dtarget))
				      (progn
					(setq numerator   (+ numerator   (square (- dmds dtarget))))
					(setq denominator (+ denominator (square dmds))))))))
		    (/ numerator denominator)))  ;; would be good to get for divide by zero
	       (^ (&rest args) 
		  (setq f-calls-cumulative
		    (lvector-sum f-calls-cumulative args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (let ((numerator-term-1-rs-loop 0)
					  (numerator-term-1-s-loop  0)
					  (numerator-term-2-rs-loop 0)
					  (numerator-term-2-s-loop  0)
					  (denominator-rs-loop      0))
				      (loop for r from 1 below NUM_POINTS do
					    (loop for s from (+ r 1) to NUM_POINTS do
						  (let ((dtarget (dhat-ll target-dist-matrix r s))
							(dmds    (d-from-array p r s NUM_DIMS)))
						    (if (not (dont-care-p dtarget))
							(progn
							  (setq numerator-term-1-rs-loop 
							    (+ numerator-term-1-rs-loop (square dmds)))
							  (setq numerator-term-2-rs-loop 
							    (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
							  (setq denominator-rs-loop
							    numerator-term-1-rs-loop))))))
				      (loop for s from 1 to NUM_POINTS do
					    (if (= a s)
						0
					      (let ((dmds    (d-from-array p a s NUM_DIMS))
						    (dtarget (dhat-ll target-dist-matrix a s)))
						(if (dont-care-p dtarget)
						    0
						  (let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))
						    (setq numerator-term-1-s-loop
						      (+ numerator-term-1-s-loop
							 (* (- 1 (/ dtarget dmds))
							    Xak-Xsk)))
						    (setq numerator-term-2-s-loop
						      (+ numerator-term-2-s-loop
							 Xak-Xsk)))))))
				      (/ (- (* numerator-term-1-rs-loop
					       2 numerator-term-1-s-loop)
					    (* numerator-term-2-rs-loop
					       2 numerator-term-2-s-loop))
					 (square denominator-rs-loop))
				      )
				  0))))
		  (values p df))
	       (^ (&rest args) 
		  (setq df-calls-cumulative
		    (lvector-sum df-calls-cumulative args))))
	      ))
	 args))


old timing:
                      real         cons
F      (250 0 2060 10 2182 0 0 35840112)
DF     (540 0 5760  0 6299 0 0 88861056)
; real time  8,605 msec

new timing with coords-coords
F      (300  0 2050  0 2305 0 0 35840112)
DF     (500 10 5640 30 6159 0 0 88368576)
; real time  8,563 msec

NOTE, IT STILL MAKES A DIFFERENCE OF 3s IF WE ARE IN THE LISP WINDOW OR NOT

the difference in told to new timing is not significant, but the code is nicer


new timing with *2 out of the inner loop
F      (240 0 2050 0 2349 0 0 35840112)
DF     (480 0 5610 0 6217 0 0 87438336)
; real time  8,708 msec

longer, but not significant (again, leave in because it is cleaner)



new timing, without 1/dmds
F      (260  0 2020 20 2239 0 0 35840160)
DF     (460 10 5770 40 6248 0 0 86945856)
; real time  8,647 msec

again, not significant, but leave in


;;;----------------------------------------------------------------------
;;;                 use square not expt in d-from-array
;;;----------------------------------------------------------------------

;; use square not expt in d-from-array:

(defun d-from-array (p r s num-dims)
  (let ((r-start (inc (* (dec r) num-dims)))
	(s-start (inc (* (dec s) num-dims))))
    (sqrt
     (loop for dims below num-dims
	 for ri from r-start
	 for si from s-start sum
	   (square (- (fref p ri) (fref p si)))))))


old timing:
                       real         cons
F      (260  0 2020 20 2239 0 0 35840160)
DF     (460 10 5770 40 6248 0 0 86945856)
; real time  8,647 msec

new timing:
                       real         cons
F       (280 0 1470 0 1657 0 0 32310720)
DF      (460 0 4330 0 4570 0 0 76603776)
; real time  6,315 msec

wow!, 25% faster!



;;;----------------------------------------------------------------------
;;;                 make dhat-ll be an array ref
;;;----------------------------------------------------------------------

(defun dhat-ll (ll r s)
  (if (> r s)   ;; to pick out of the upper triangle (we assume symetric matrix)
      (aref ll (dec s) (dec r))
    (aref ll (dec r) (dec s))))

(defun run-test ()
  (setq f-calls-cumulative '(0 0 0 0 0 0 0 0))
  (setq df-calls-cumulative '(0 0 0 0 0 0 0 0))
  (let ((result
	 (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (array-list (hi-table-values foo)) bar))))
    (if (equal (dps-tree result 10) (dps-tree final-coordss 10))
	(format t "~%Same answer as before")
      (format t "~%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  different answer  <<<<<<<<<<<<<<<<<<<<<<<"))
    (format t "~%~a~%~a" f-calls-cumulative df-calls-cumulative)))



old timing:
                       real         cons
F       (280 0 1470 0 1657 0 0 32310720)
DF      (460 0 4330 0 4570 0 0 76603776)
; real time  6,315 msec

new timing:
                       real         cons
F       (210 0 1560  0 1654 0 0 32310720)
DF      (420 0 4120 30 4590 0 0 76603776)
; real time  6,359 msec

not significant difference (though the array is small)

try again on a large array


;;;----------------------------------------------------------------------
;;;                     put the sqrt back in
;;;----------------------------------------------------------------------

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (let ((numerator 0)
			(denominator 0))
		    (loop for r from 1 below NUM_POINTS do
			  (loop for s from (+ r 1) to NUM_POINTS do
				(let ((dtarget (dhat-ll target-dist-matrix r s))
				      (dmds    (d-from-array p r s NUM_DIMS)))
				  (if (not (dont-care-p dtarget))
				      (progn
					(setq numerator   (+ numerator   (square (- dmds dtarget))))
					(setq denominator (+ denominator (square dmds))))))))
		    (/ numerator denominator)))  ;; would be good to get for divide by zero
	       (^ (&rest args) 
		  (setq f-calls-cumulative
		    (lvector-sum f-calls-cumulative args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (let ((numerator-term-1-rs-loop 0)
					  (numerator-term-1-s-loop  0)
					  (numerator-term-2-rs-loop 0)
					  (numerator-term-2-s-loop  0)
					  (denominator-rs-loop      0))
				      (loop for r from 1 below NUM_POINTS do
					    (loop for s from (+ r 1) to NUM_POINTS do
						  (let ((dtarget (dhat-ll target-dist-matrix r s))
							(dmds    (d-from-array p r s NUM_DIMS)))
						    (if (not (dont-care-p dtarget))
							(progn
							  (setq numerator-term-1-rs-loop 
							    (+ numerator-term-1-rs-loop (square dmds)))
							  (setq numerator-term-2-rs-loop 
							    (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
							  (setq denominator-rs-loop
							    numerator-term-1-rs-loop))))))
				      (loop for s from 1 to NUM_POINTS do
					    (if (= a s)
						0
					      (let ((dmds    (d-from-array p a s NUM_DIMS))
						    (dtarget (dhat-ll target-dist-matrix a s)))
						(if (dont-care-p dtarget)
						    0
						  (let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))
						    (setq numerator-term-1-s-loop
						      (+ numerator-term-1-s-loop
							 (* (- 1 (/ dtarget dmds))
							    Xak-Xsk)))
						    (setq numerator-term-2-s-loop
						      (+ numerator-term-2-s-loop
							 Xak-Xsk)))))))
				      (/ (- (* numerator-term-1-rs-loop
					       2 numerator-term-1-s-loop)
					    (* numerator-term-2-rs-loop
					       2 numerator-term-2-s-loop))
					 (square denominator-rs-loop))
				      )
				  0))))
		  (values p df))
	       (^ (&rest args) 
		  (setq df-calls-cumulative
		    (lvector-sum df-calls-cumulative args))))
	      ))
	 args))


old timing:
                       real         cons
F       (280 0 1470 0 1657 0 0 32310720)
DF      (460 0 4330 0 4570 0 0 76603776)
; real time  6,315 msec

new timing:
                       real         cons
F       (230 0 1470 10 1520 0 0 32310720)
DF      (500 0 4140 30 4629 0 0 76603776)
; real time  6,265 msec

OK, we are back where we think we are (recompiled the old dhat-ll and the old run-test)
Now, try adding back the sqrt:

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (excl::time-a-funcall 
	       (^ ()
		  (sqrt
		   (let ((numerator 0)
			 (denominator 0))
		     (loop for r from 1 below NUM_POINTS do
			   (loop for s from (+ r 1) to NUM_POINTS do
				 (let ((dtarget (dhat-ll target-dist-matrix r s))
				       (dmds    (d-from-array p r s NUM_DIMS)))
				   (if (not (dont-care-p dtarget))
				       (progn
					 (setq numerator   (+ numerator   (square (- dmds dtarget))))
					 (setq denominator (+ denominator (square dmds))))))))
		     (/ numerator denominator))))  ;; would be good to get for divide by zero
	       (^ (&rest args) 
		  (setq f-calls-cumulative
		    (lvector-sum f-calls-cumulative args))))))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args))))  
	   (^ (p df)
	      (excl::time-a-funcall 
	       (^ ()
		  (loop for a from 1 to NUM_POINTS do       ;; a is the point       (NOTE DIFFERENT FROM ABOVE)
			(loop for k from 1 to NUM_DIMS do   ;; k is the dimension   (NOTE DIFFERENT FROM ABOVE)
			      (setf (fref df (+ (* (dec a) NUM_DIMS) k))
				(if (or (eql 'all moveable-coords)
					(member a moveable-coords))
				    (let ((numerator-term-1-rs-loop 0)
					  (numerator-term-1-s-loop  0)
					  (numerator-term-2-rs-loop 0)
					  (numerator-term-2-s-loop  0)
					  (denominator-rs-loop      0))
				      (loop for r from 1 below NUM_POINTS do
					    (loop for s from (+ r 1) to NUM_POINTS do
						  (let ((dtarget (dhat-ll target-dist-matrix r s))
							(dmds    (d-from-array p r s NUM_DIMS)))
						    (if (not (dont-care-p dtarget))
							(progn
							  (setq numerator-term-1-rs-loop 
							    (+ numerator-term-1-rs-loop (square dmds)))
							  (setq numerator-term-2-rs-loop 
							    (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
							  (setq denominator-rs-loop
							    numerator-term-1-rs-loop))))))
				      (loop for s from 1 to NUM_POINTS do
					    (if (= a s)
						0
					      (let ((dmds    (d-from-array p a s NUM_DIMS))
						    (dtarget (dhat-ll target-dist-matrix a s)))
						(if (dont-care-p dtarget)
						    0
						  (let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								    (coord-from-array p s k NUM_DIMS))))
						    (setq numerator-term-1-s-loop
						      (+ numerator-term-1-s-loop
							 (* (- 1 (/ dtarget dmds))
							    Xak-Xsk)))
						    (setq numerator-term-2-s-loop
						      (+ numerator-term-2-s-loop
							 Xak-Xsk)))))))
				      (* 0.5
					 (sqrt (/ numerator-term-1-rs-loop
						  numerator-term-2-rs-loop))
					 (/ (- (* numerator-term-1-rs-loop
						  2 numerator-term-1-s-loop)
					       (* numerator-term-2-rs-loop
						  2 numerator-term-2-s-loop))
					    (square denominator-rs-loop))))
				  0))))
		  (values p df))
	       (^ (&rest args) 
		  (setq df-calls-cumulative
		    (lvector-sum df-calls-cumulative args))))
	      ))
	   args))
  
  
old timing:
                       real         cons
F       (230 0 1470 10 1520 0 0 32310720)
DF      (500 0 4140 30 4629 0 0 76603776)
; real time  6,265 msec

new timing:
                       real         cons
F       (240 10 1320 20 1588 0 0 29114136)
DF      (470  0 3660 10 4054 0 0 69061312)
; real time  5,725 msec

less is consed, and it runs faster, but there is more work in each iteration,
it must be that there are fewer iterations


>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  different answer  <<<<<<<<<<<<<<<<<<<<<<<
the results do not match (ahh, see below, they do, just not precisely)

USER(119): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 4,070 msec user, 30 msec system
; cpu time (gc)     710 msec user, 0 msec system
; cpu time (total)  4,780 msec user, 30 msec system
; real time  7,528 msec
; space allocation:
;  9,390 cons cells, 0 symbols, 99,847,576 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0)
 (3.3529696984856354d0 -1.8827658113585406d0) (2.5062545266099945d0 -2.3375824915952723d0)
 (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0)
 (-0.5566778523664344d0 0.23693709352222894d0) (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL
USER(120): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 4,620 msec user, 20 msec system
; cpu time (gc)     750 msec user, 10 msec system
; cpu time (total)  5,370 msec user, 30 msec system
; real time  8,564 msec
; space allocation:
;  10,409 cons cells, 0 symbols, 110,771,128 other bytes
((3.7585887301935452d0 0.4776769149246175d0) (3.791562059841137d0 -0.897869360033893d0)
 (3.2149071224514087d0 -2.0362486630579464d0) (2.35590792614181d0 -2.4339538131908256d0)
 (1.8049077983866881d0 -1.802477832379631d0) (0.5380378086948533d0 -2.737954522627667d0)
 (-1.3246222567509391d0 -2.126386427610298d0) (-1.3873363342393445d0 -0.4498517353997635d0)
 (-0.5601748436809763d0 0.305820109558159d0) (-2.5677030723551018d0 0.004037716290155341d0) ...)
NIL
0.02749425128541343d0
MULTIPLE-END-CONDITIONS
NIL
USER(121): (setq a **)
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0)
 (3.3529696984856354d0 -1.8827658113585406d0) (2.5062545266099945d0 -2.3375824915952723d0)
 (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0)
 (-0.5566778523664344d0 0.23693709352222894d0) (-2.524508169266496d0 -0.1981715837074662d0) ...)
USER(122): (setq b **)
((3.7585887301935452d0 0.4776769149246175d0) (3.791562059841137d0 -0.897869360033893d0)
 (3.2149071224514087d0 -2.0362486630579464d0) (2.35590792614181d0 -2.4339538131908256d0)
 (1.8049077983866881d0 -1.802477832379631d0) (0.5380378086948533d0 -2.737954522627667d0)
 (-1.3246222567509391d0 -2.126386427610298d0) (-1.3873363342393445d0 -0.4498517353997635d0)
 (-0.5601748436809763d0 0.305820109558159d0) (-2.5677030723551018d0 0.004037716290155341d0) ...)
USER(123): (all-comparisons #'e-dist a)
NIL
USER(124): (all-comparisons a #'e-dist)
(1.371669884660106d0 2.5778174214349603d0 3.2503837446708785d0 2.9674973223918295d0 4.546845415453922d0 5.710720488239606d0
 5.22250438686794d0 4.332146483456259d0 6.337777695220369d0 4.875476986268608d0 ...)
USER(125): (all-comparisons b #'e-dist)
(1.3759414213622188d0 2.5720442263367174d0 3.231889066004786d0 3.002661295411091d0 4.551069520468858d0 5.71140786756884d0
 5.228848263808312d0 4.322181575151164d0 6.343997325147395d0 4.888521462583394d0 ...)
USER(126): (transpose * **)
((1.3759414213622188d0 1.371669884660106d0) (2.5720442263367174d0 2.5778174214349603d0)
 (3.231889066004786d0 3.2503837446708785d0) (3.002661295411091d0 2.9674973223918295d0)
 (4.551069520468858d0 4.546845415453922d0) (5.71140786756884d0 5.710720488239606d0) (5.228848263808312d0 5.22250438686794d0)
 (4.322181575151164d0 4.332146483456259d0) (6.343997325147395d0 6.337777695220369d0) (4.888521462583394d0 4.875476986268608d0)
 ...)
USER(127): (mapcar (^ (l) (apply #'- l)) *)
(0.004271536702112666d0 -0.0057731950982429225d0 -0.018494678666092668d0 0.03516397301926144d0 0.004224105014936086d0
 6.873793292339059d-4 0.006343876940372084d0 -0.009964908305095221d0 0.006219629927025849d0 0.013044476314785669d0 ...)
USER(128): (ppl *)

0.004271536702112666d0 
-0.0057731950982429225d0 
-0.018494678666092668d0 
0.03516397301926144d0 
0.004224105014936086d0 
6.873793292339059d-4 
0.006343876940372084d0 
-0.009964908305095221d0 
0.006219629927025849d0 
0.013044476314785669d0 
0.001980623359041367d0 
-0.022922306551020455d0 
-0.0010013267459978437d0 
0.020029053159453802d0 
0.00795901083800521d0 
0.023485909946888484d0 
0.00115656575226053d0 
-0.058657373946872315d0 
-0.012409898521613894d0 
-0.026969896561274354d0 
0.02783985447461701d0 
-0.001138105920524879d0 
-0.0037623444544871987d0 
0.004381278142819411d0 
-0.011466003078109743d0 
0.007249250645108063d0 
0.008604704771664196d0 
0.0033026286758763845d0 
-0.02924997690989528d0 
0.005827021160726531d0 
0.023525537602314905d0 
0.008313017129053968d0 
0.023732171640252098d0 
0.005394676049792935d0 
-0.053984550480445925d0 
-0.01453858960819887d0 
0.025349973325182074d0 
0.010975217722940744d0 
0.0071161396365990726d0 
0.012705581164240698d0 
-0.004719341491001394d0 
0.01786241527447352d0 
0.00891042780940765d0 
0.011143428136657363d0 
-0.031765841103390535d0 
0.015263725442117071d0 
0.024379722566595596d0 
0.005140015261647868d0 
0.014175936487782259d0 
-6.394550140065292d-6 
-0.05417208529779227d0 
-4.38159633003643d-4 
0.02431834778859754d0 
0.018908098583180433d0 
0.020067670027060558d0 
2.4244561186215208d-5 
0.027378657351611047d0 
0.008788580237487409d0 
0.01833005776834895d0 
-0.033499462132377644d0 
0.021600256557690933d0 
0.02225318249402619d0 
-5.811402483866246d-4 
0.00325830724066023d0 
-0.007223926231285205d0 
-0.054983989256205845d0 
-0.030622235484158322d0 
-0.0265375833634498d0 
-0.0048164321571047d0 
-0.008504765600528152d0 
7.02772034203214d-4 
0.005678783418241906d0 
0.0033815779825410175d0 
-0.026419547874827032d0 
0.02267870902699709d0 
0.04023020873240668d0 
0.02507168936107984d0 
0.03719976418301929d0 
0.03451146980690467d0 
-0.01706269161367935d0 
-0.0013996591677378767d0 
0.008129624952204484d0 
-4.242160509497772d-4 
0.017642235598511746d0 
-3.0594416051155093d-4 
0.0130829348363255d0 
-0.03311611551770621d0 
0.028204011935914153d0 
0.026285337989813584d0 
0.005874999130801584d0 
0.009243862802049918d0 
0.010428257250704398d0 
-0.028452029053535277d0 
0.004614069141301025d0 
0.004606639502716003d0 
0.019690094755994814d0 
-0.008694635247210147d0 
0.015195950104203604d0 
-0.03053000904011327d0 
0.032975685365285834d0 
0.018932058812605845d0 
-4.822778215718415d-5 
-0.002499462277767428d0 
0.008349579254486983d0 
-0.013590792432482068d0 
0.01062522167296942d0 
0.005496891128273784d0 
-0.01316941765185664d0 
0.005116016314316241d0 
-0.03014956192669871d0 
0.02927332296485563d0 
0.015083349943167068d0 
-5.392258537333383d-4 
-0.006423161854236525d0 
0.010977967271049494d0 
-9.178524005752919d-4 
0.014724337127627773d0 
0.0028961649788061816d0 
0.015433711584733611d0 
-0.034749689688200025d0 
0.028551469053174516d0 
0.013167601545934104d0 
-0.00988763098021428d0 
-0.014346027069588096d0 
-3.93495952302203d-4 
-0.011723523931638002d0 
-0.022310648941190614d0 
-0.003659278166844304d0 
-0.024260808089688446d0 
0.022055719606454804d0 
0.0012714250602225974d0 
-0.00728633317200611d0 
-0.01868789198194243d0 
0.005808540202719037d0 
0.006152012144244878d0 
-0.010960334858140186d0 
-0.008956123140001937d0 
0.03273983006639758d0 
0.024129492835454736d0 
0.011023824139809957d0 
0.0022252831236100334d0 
0.024480521609034245d0 
0.030218901827714806d0 
0.0014226344602361607d0 
0.014119473773486435d0 
-0.008973247990423427d0 
-0.009824212410621591d0 
-0.027234389057647412d0 
0.0030072784062245006d0 
0.022801613928571385d0 
-1.139551134526906d-4 
0.003123394669500623d0 
-0.017723945512604145d0 
-0.01176348191190213d0 
0.0039051849765798785d0 
0.024658890937713895d0 
-0.02307088101023891d0 
-0.019652558581777946d0 
-0.042806582977095164d0 
-0.01021365641054528d0 
0.0069590937800709796d0 
0.009377037806790112d0 
-0.019668658087175217d0 
0.013343549437774715d0 
0.026403653493477508d0 
-0.003571188745002063d0 
0.013113573267067968d0 
-0.005523791884999962d0 
0.02709765630131944d0 
0.01656322033127866d0 
-0.04326961463896373d0 
  


;;;----------------------------------------------------------------------
;;;                the lisp profiler on a large table
;;;----------------------------------------------------------------------


during the optimization of 
(setq nl-pro (fi-in-hi-table "~/mds/data/all-seq/latest-version/nl-pro.lisp"))
(make-master-mds-window (extract-hi-table-window nl-pro 0 100) :show-hi-table nil)



(prof:start-profiler)
:SAMPLING
USER(19): (prof:stop-profiler)
:SAVED
USER(20): (prof:show-flat-profile)

Sample represents 37.6 seconds of processor time (out of a total of 37.6)

Times below 1.0% will be suppressed.

  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 14.9  14.9    5.6    8.9                          ... "unidentified"
 14.2  29.0    5.3    5.3                          NTHCDR
  7.3  36.3    2.7    7.6                          EXCL::+_2OP
  6.9  43.2    2.6   21.3                          ... D-FROM-ARRAY
  6.8  49.9    2.5    2.6                          "new_other"
  6.0  56.0    2.3    7.7                          "new_double_float"
  5.7  61.7    2.2    5.1                          EXCL::*_2OP
  5.7  67.4    2.1    3.9                          EXCL::-_2OP
  3.8  71.2    1.4    1.5                          "aref"
  2.7  73.9    1.0    5.2                          SQUARE
  2.5  76.4    0.9   33.4                          (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  2.3  78.7    0.9    1.9                          "shift_left"
  2.1  80.7    0.8    0.8                          "prunebig"
  2.1  82.8    0.8    1.2                          "big_add_int"
  1.7  84.5    0.6    5.8                          DHAT-LL
  1.7  86.1    0.6    1.3                          "mathlib"
  1.5  87.6    0.6    7.3                          SQRT
  1.4  89.1    0.5    6.6                          EXCL::NAN-P
  1.2  90.3    0.5    0.5                          INC
  1.2  91.5    0.4    0.4                          DEC
  1.2  92.7    0.4    0.4                          "integer_multiply"
  1.1  93.8    0.4    5.8                          EXCL::DOUBLE-FLOAT-TO-COMPONENTS



(prof:show-call-graph)
Time profile of sampled pc values by function, children, and parents.

Total times below 1.0% will be suppressed.
Parent and child times less 2.0% of the node will be suppressed.

Sample represents 37.6 seconds of processor time (out of a total of 37.6)

  %     %                       Parent
 self total   total local  Function
 Time  Time    secs   %         Child

  0.0 100.0    37.6   0.0   "start"
               37.6 100.0        "initiate_stack_group"
-----------------------------------------------------
               37.6 100.0        "start"
  0.0 100.0    37.6   0.0   ... "initiate_stack_group"
               37.6 100.0        ... TK-INTERPRET
-----------------------------------------------------
               37.6 100.0        ... "initiate_stack_group"
  0.0 100.0    37.6   0.0   TK-INTERPRET
               37.6 100.0        EVAL
-----------------------------------------------------
               37.6 100.0        TK-INTERPRET
  0.0 100.0    37.6   0.0   ... EVAL
               37.6 100.0        ... HILLCLIMB-FROM-MDS-WINDOW
-----------------------------------------------------
               37.6 100.0        ... EVAL
  0.0 100.0    37.6   0.0   HILLCLIMB-FROM-MDS-WINDOW
               37.6 100.0        MDS-HI-TABLE
-----------------------------------------------------
               37.6 100.0        HILLCLIMB-FROM-MDS-WINDOW
  0.0 100.0    37.6   0.0   MDS-HI-TABLE
               37.6 100.0        RUN-MDS-FROM-DISTS
-----------------------------------------------------
               37.6 100.0        MDS-HI-TABLE
  0.0 100.0    37.6   0.0   ... RUN-MDS-FROM-DISTS
               37.6 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
-----------------------------------------------------
               37.6 100.0        ... RUN-MDS-FROM-DISTS
  0.0 100.0    37.6   0.0   ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
               37.6 100.0        ... MDS
-----------------------------------------------------
               37.6 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
  0.0 100.0    37.6   0.0   ... MDS
               37.6 100.0        ... CONJUGANT-GRADIENT-ITERATIONS
-----------------------------------------------------
               37.6 100.0        ... MDS
  0.0 100.0    37.6   0.0   CONJUGANT-GRADIENT-ITERATIONS
               33.4  88.8        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                2.8   7.5        LINMIN
                0.9   2.5        "unidentified"
-----------------------------------------------------
               33.4 100.0        CONJUGANT-GRADIENT-ITERATIONS
  2.5  88.8    33.4   2.8   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
               19.5  58.3        D-FROM-ARRAY
                5.3  15.9        DHAT-LL
                2.2   6.5        EXCL::+_2OP
                2.1   6.4        SQUARE
                1.8   5.3        "unidentified"
                1.1   3.2        EXCL::-_2OP
-----------------------------------------------------
               19.5  91.5        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                1.7   8.2        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  6.9  56.5    21.3  12.2   ... D-FROM-ARRAY
                7.0  32.8        SQRT
                2.4  11.5        SQUARE
                2.4  11.1        EXCL::-_2OP
                2.3  10.9        EXCL::+_2OP
                1.3   6.0        ... "mathlib"
                1.1   5.4        "aref"
                0.8   3.6        ... "unidentified"
                0.7   3.3        EXCL::*_2OP
-----------------------------------------------------
                5.3  60.0r       "unidentified"
                3.8  42.7r       "new_double_float"
                1.8  19.8r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.9  10.6r       CONJUGANT-GRADIENT-ITERATIONS
                0.8   9.3r       "newbignum"
                0.8   8.6r       D-FROM-ARRAY
                0.3   3.2r       EXCL::NAN-P
                0.2   2.5r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
 14.9  23.6     8.9  63.0   ... "unidentified"
                5.3  37.6        "unidentified"
                0.4   2.8        SQUARE
                0.4   2.6        EXCL::-_2OP
                0.4   2.6        EXCL::+_2OP
                0.3   2.3        EXCL::*_2OP
-----------------------------------------------------
                2.6  33.4        EXCL::+_2OP
                2.5  32.7        EXCL::*_2OP
                1.7  21.6        EXCL::-_2OP
                0.5   6.9        "mathlib"
                0.2   2.6        D-FROM-ARRAY
  6.0  20.4     7.7  29.6   "new_double_float"
                3.8  49.3        "unidentified"
                1.6  21.2        "new_other"
-----------------------------------------------------
                2.6  33.6        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                2.3  30.6        D-FROM-ARRAY
                2.2  28.8        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.4   4.8        "unidentified"
                0.2   2.1        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  7.3  20.2     7.6  36.1   EXCL::+_2OP
                2.6  33.9        "new_double_float"
                1.9  25.3        "integer_add"
                0.2   2.4        "bignum_add"
                0.2   2.3        "new_other"
-----------------------------------------------------
                7.0  96.1        D-FROM-ARRAY
                0.3   3.9        "unidentified"
  1.5  19.3     7.3   7.7   SQRT
                6.6  90.8        EXCL::NAN-P
-----------------------------------------------------
                6.6  99.2        SQRT
  1.4  17.7     6.6   8.2   EXCL::NAN-P
                5.7  85.9        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.3   4.2        "unidentified"
-----------------------------------------------------
                5.3  91.2        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.4   7.2        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  1.7  15.5     5.8  10.8   DHAT-LL
                5.1  87.3        NTHCDR
-----------------------------------------------------
                5.7  98.2        EXCL::NAN-P
  1.1  15.5     5.8   7.2   EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                2.6  43.9        EXCL::+_2OP
                2.0  35.2        ASH
                0.5   8.9        "unsigned_fixnum_or_bignum"
                0.2   2.9        "shift_left"
-----------------------------------------------------
                5.1  95.7        DHAT-LL
                0.2   4.3        "unidentified"
 14.2  14.2     5.3 100.0   NTHCDR
-----------------------------------------------------
                2.4  47.1        D-FROM-ARRAY
                2.1  41.1        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.4   7.7        "unidentified"
                0.2   4.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  2.7  13.8     5.2  19.7   SQUARE
                4.0  77.6        EXCL::*_2OP
                0.1   2.7        "new_double_float"
-----------------------------------------------------
                4.0  78.8        SQUARE
                0.7  13.8        D-FROM-ARRAY
                0.3   6.3        "unidentified"
  5.7  13.6     5.1  42.2   EXCL::*_2OP
                2.5  49.1        "new_double_float"
                0.3   6.2        "integer_multiply"
                0.1   2.6        "new_other"
-----------------------------------------------------
                2.4  60.6        D-FROM-ARRAY
                1.1  27.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.4   9.5        "unidentified"
                0.1   2.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  5.7  10.3     3.9  55.0   EXCL::-_2OP
                1.7  42.8        "new_double_float"
                0.1   2.3        "new_other"
-----------------------------------------------------
                2.8  93.8        F1DIM
                0.2   6.2        CONJUGANT-GRADIENT-ITERATIONS
  0.1   7.8     2.9   1.8   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                1.7  59.3        D-FROM-ARRAY
                0.4  14.2        DHAT-LL
                0.2   7.4        "unidentified"
                0.2   7.1        SQUARE
                0.2   5.3        EXCL::+_2OP
                0.1   2.7        EXCL::-_2OP
-----------------------------------------------------
                2.8 100.0        CONJUGANT-GRADIENT-ITERATIONS
  0.0   7.5     2.8   0.0   LINMIN
                1.7  60.5        BRENT
                1.1  39.5        MNBRAK
-----------------------------------------------------
                1.7  60.5        BRENT
                1.1  39.5        MNBRAK
  0.0   7.5     2.8   0.0   F1DIM
                2.8  97.5        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.1   2.2        "unidentified"
-----------------------------------------------------
                1.6  63.1        "new_double_float"
                0.3  13.6        "newbignum"
                0.2   6.8        EXCL::+_2OP
                0.1   5.1        EXCL::*_2OP
                0.1   4.7        "mathlib"
                0.1   3.4        EXCL::-_2OP
  6.8   6.9     2.6  98.6   "new_other"
-----------------------------------------------------
                2.0  90.0        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.2  10.0        "unidentified"
  0.9   6.0     2.3  14.2   ASH
                1.8  77.3        "shift_left"
                0.1   3.5        "prunebig"
                0.1   3.1        "fixnum_in_big"
-----------------------------------------------------
                1.9  97.3        EXCL::+_2OP
                0.1   2.7        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
  0.6   5.3     2.0  10.6   "integer_add"
                1.7  83.6        "bignum_add"
                0.1   3.1        "big_add_int"
-----------------------------------------------------
                1.8  91.4        ASH
                0.2   8.6        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
  2.3   5.1     1.9  44.5   "shift_left"
                0.5  25.5        "newbignum"
                0.4  20.0        "prunebig"
                0.1   6.8        "fixnum_in_big"
                0.0   2.3        "replace_temp_bignum"
-----------------------------------------------------
                1.7  90.0        "integer_add"
                0.2  10.0        EXCL::+_2OP
  0.9   4.9     1.8  18.1   "bignum_add"
                1.2  63.8        "big_add_int"
                0.3  16.2        "prunebig"
-----------------------------------------------------
                1.7 100.0        LINMIN
  0.0   4.6     1.7   0.0   BRENT
                1.7 100.0        F1DIM
-----------------------------------------------------
                1.1  74.4        D-FROM-ARRAY
                0.3  21.6        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.0   2.3        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  3.8   4.1     1.5  92.0   "aref"
                0.1   6.8        "unidentified"
-----------------------------------------------------
                0.5  33.1        "shift_left"
                0.4  29.6        "big_add_int"
                0.4  29.0        "unsigned_fixnum_or_bignum"
                0.1   4.1        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.0   2.4        "bignum_add"
  0.8   3.9     1.5  20.7   "newbignum"
                0.8  55.6        "unidentified"
                0.3  23.7        "new_other"
-----------------------------------------------------
                1.3 100.0        ... D-FROM-ARRAY
  1.7   3.4     1.3  48.6   "mathlib"
                0.5  41.8        "new_double_float"
                0.1   9.6        "new_other"
-----------------------------------------------------
                1.2  95.0        "bignum_add"
                0.1   5.0        "integer_add"
  2.1   3.3     1.2  63.1   "big_add_int"
                0.4  35.5        "newbignum"
-----------------------------------------------------
                1.1 100.0        LINMIN
  0.0   3.0     1.1   0.0   MNBRAK
                1.1 100.0        F1DIM
-----------------------------------------------------
                0.4  48.9        "shift_left"
                0.3  37.8        "bignum_add"
                0.1  10.0        ASH
                0.0   3.3        "integer_add"
  2.1   2.1     0.8 100.0   "prunebig"
-----------------------------------------------------
                0.5  81.9        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.1  18.1        EXCL::NAN-P
  0.4   1.7     0.6  26.4   "unsigned_fixnum_or_bignum"
                0.4  68.1        "newbignum"
                0.0   5.6        "new_other"
-----------------------------------------------------
                0.3  58.5        "unidentified"
                0.2  41.5        D-FROM-ARRAY
  1.2   1.2     0.5 100.0   INC
-----------------------------------------------------
                0.2  43.1        "unidentified"
                0.1  31.4        D-FROM-ARRAY
                0.1  25.5        DHAT-LL
  1.2   1.2     0.4 100.0   DEC
-----------------------------------------------------
                0.3  70.6        EXCL::*_2OP
                0.1  27.5        D-FROM-ARRAY
  1.2   1.2     0.4 100.0   "integer_multiply"
-----------------------------------------------------
USER(35): 


;;;----------------------------------------------------------------------
;;;                      MORE, JAN 2001
;;;----------------------------------------------------------------------


;; the old best

USER(119): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 4,070 msec user, 30 msec system
; cpu time (gc)     710 msec user, 0 msec system
; cpu time (total)  4,780 msec user, 30 msec system
; real time  7,528 msec
; space allocation:
;  9,390 cons cells, 0 symbols, 99,847,576 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0)
 (3.3529696984856354d0 -1.8827658113585406d0) (2.5062545266099945d0 -2.3375824915952723d0)
 (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0)
 (-0.5566778523664344d0 0.23693709352222894d0) (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL


;; the current production code best (close enuf)

(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 4,550 msec user, 40 msec system
; cpu time (gc)     850 msec user, 10 msec system
; cpu time (total)  5,400 msec user, 50 msec system
; real time  6,015 msec
; space allocation:
;  2,352 cons cells, 0 symbols, 99,727,144 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0)
 (3.3529696984856354d0 -1.8827658113585406d0) (2.5062545266099945d0 -2.3375824915952723d0)
 (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0)
 (-0.5566778523664344d0 0.23693709352222894d0) (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  ;; optimization method in metric-mds-conjugant-gradient-speed-optimization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      (let ((numerator 0.0d0)
		    (denominator 0.0d0))
		(loop for r from 1 below NUM_POINTS do
		      (loop for s from (+ r 1) to NUM_POINTS do
			    (let ((dtarget (dhat-ll target-dist-matrix r s))
				  (dmds    (d-from-array p r s NUM_DIMS)))
			      (if (not (dont-care-p dtarget))
				  (progn
				    (setq numerator   (+ numerator   (square (- dmds dtarget))))
				    (setq denominator (+ denominator (square dmds))))))))
		(sqrt (/ numerator denominator)))  ;; would be good to get for divide by zero
	      ))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args)))
	       (unmoveable-coords (mapcar #'inc (snoop-unmoveable-coords args))))
	   (^ (p df)
	      (let ((numerator-term-1-rs-loop 0.0d0)
		    (numerator-term-2-rs-loop 0.0d0)
		    (denominator-rs-loop      0.0d0))
		(loop for r from 1 below NUM_POINTS do
		      (loop for s from (+ r 1) to NUM_POINTS do
			    (let ((dtarget (dhat-ll target-dist-matrix r s))
				  (dmds    (d-from-array p r s NUM_DIMS)))
			      (if (not (dont-care-p dtarget))
				  (progn
				    (setq numerator-term-1-rs-loop 
				      (+ numerator-term-1-rs-loop (square dmds)))
				    (setq numerator-term-2-rs-loop 
				      (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
				    (setq denominator-rs-loop
				      numerator-term-1-rs-loop))))))
		
		(loop for a from 1 to NUM_POINTS do       ;; a is the point      
		      (loop for k from 1 to NUM_DIMS do   ;; k is the dimension  
			    (setf (fref df (+ (* (dec a) NUM_DIMS) k))
			      (if (and (or (eql 'all moveable-coords)
					   (member a moveable-coords))
				       (not (member a unmoveable-coords)))
				  (let ((numerator-term-1-s-loop  0.0d0)
					(numerator-term-2-s-loop  0.0d0))
				    (loop for s from 1 to NUM_POINTS do
					  (if (= a s)
					      0.0d0
					    (let ((dmds    (d-from-array p a s NUM_DIMS))
						  (dtarget (dhat-ll target-dist-matrix a s)))
					      (if (dont-care-p dtarget)
						  0.0d0
						(let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								  (coord-from-array p s k NUM_DIMS))))
						  (setq numerator-term-1-s-loop
						    (+ numerator-term-1-s-loop
						       (* (- 1.0d0 (/ dtarget dmds))
							  Xak-Xsk)))
						  (setq numerator-term-2-s-loop
						    (+ numerator-term-2-s-loop
						       Xak-Xsk)))))))
				    (* 0.5d0
				       (sqrt (/ numerator-term-1-rs-loop
						numerator-term-2-rs-loop))
				       (/ (- (* numerator-term-1-rs-loop
						2.0d0 numerator-term-1-s-loop)
					     (* numerator-term-2-rs-loop
						2.0d0 numerator-term-2-s-loop))
					  (square denominator-rs-loop))))
				0.0d0))))
		(values p df)))
	   )
	 args))


USER(19): (time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 1,720 msec user, 10 msec system
; cpu time (gc)     370 msec user, 10 msec system
; cpu time (total)  2,090 msec user, 20 msec system
; real time  2,353 msec
; space allocation:
;  2,352 cons cells, 0 symbols, 39,962,672 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0)
 (3.3529696984856354d0 -1.8827658113585406d0) (2.5062545266099945d0 -2.3375824915952723d0)
 (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0)
 (-0.5566778523664344d0 0.23693709352222894d0) (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL


WOW!  1.7 seconds from 4.5 seconds.  3x, that's pretty good.  It is also pretty bad that
I never saw that before.

and is 6x faster a table with 60 entries.



;;;----------------------------------------------------------------------
;;;                      TODO
;;;----------------------------------------------------------------------

put k inside the s loop (then we double the deriviative speed for 2d, triple for 3d etc)
memoize the dtarget and dmds distsances
would things be faster if i set the Dmds and D-target arrays to be type double?

later memoize the dtarget and dmds distances (see how many times called, and cost of checking if same input)
  -- no only rarely called with the same input.



;;;----------------------------------------------------------------------
;;;     calculate the dtarget and dmds distances and store in array
;;;----------------------------------------------------------------------

(defun dhat-ll (ll r s)
  ;; NOTE, for speed, this should be an aref in the future
  (if (> r s)   ;; to pick out of the upper triangle (we assume symetric matrix)
      (nth (dec r) (nth (dec s) ll))
    (nth (dec s) (nth (dec r) ll))))


(defun populate-d-target-cache (d-target-cache target-dist-ll num-points)
  (if (eql d-target-cache 'not-created-yet)
      (setq d-target-cache (make-array (list (inc num-points) (inc num-points)))))   ;; inc because numerical recepies is 1-based
  ;; later optimize here (by reading directly from ll)
  (loop for r from 1 to num-points do
	(loop for s from r to num-points do
	      (let ((d-target-rs (dhat-ll target-dist-ll r s)))
		(setf (aref d-target-cache r s) d-target-rs)
		(setf (aref d-target-cache s r) d-target-rs))))
  d-target-cache)

(defun populate-d-mds-cache (d-mds-cache p num-points num-dims)
  (if (eql d-mds-cache 'not-created-yet)
      (setq d-mds-cache (make-array (list (inc num-points) (inc num-points)))))   ;; inc because numerical recepies is 1-based
  ;; later optimize here?
  (loop for r from 1 to num-points do
	(loop for s from r to num-points do
	      (let ((d-mds-rs (d-from-array p r s num-dims)))
		(setf (aref d-mds-cache r s) d-mds-rs)
		(setf (aref d-mds-cache s r) d-mds-rs))))	
  d-mds-cache)

(defun metric-mds-global-norm-conjugant-gradient (target-dist-matrix stress-component-f &rest args)
  ;; with the global mds normalization
  ;; optimization method in metric-mds-conjugant-gradient-speed-optimization
  stress-component-f
  ;; ugly ugly, NUM_POINTS and NUM_DIMS are dynamically scoped variables, set in a let by conjugant-gradient-iterations
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (p)
	      ;;(ppl (list 'calling-f-with (list-array p)))
	      (let ((numerator 0.0d0)
		    (denominator 0.0d0))
		(loop for r from 1 below NUM_POINTS do
		      (loop for s from (+ r 1) to NUM_POINTS do
			    (let ((dtarget (dhat-ll target-dist-matrix r s))
				  (dmds    (d-from-array p r s NUM_DIMS)))
			      (if (not (dont-care-p dtarget))
				  (progn
				    (setq numerator   (+ numerator   (square (- dmds dtarget))))
				    (setq denominator (+ denominator (square dmds))))))))
		(sqrt (/ numerator denominator)))  ;; would be good to get for divide by zero
	      ))
	 (let ((moveable-coords 
		(if (listp (snoop-moveable-coords args))
		    (mapcar #'inc (snoop-moveable-coords args))     ;; inc because the numerical recepies is 1-based
		  (snoop-moveable-coords args)))
	       (unmoveable-coords (mapcar #'inc (snoop-unmoveable-coords args)))
	       (d-mds-cache 'not-created-yet)     ;; inc because numerical-recepies is one-based
	       (d-target-cache 'not-created-yet))
	   (^ (p df)
	      ;;(ppl (list 'calling-------df----------with (list-array p)))
	      (setq d-mds-cache (populate-d-mds-cache d-mds-cache p NUM_POINTS NUM_DIMS))
	      (setq d-target-cache (populate-d-target-cache d-target-cache target-dist-matrix NUM_POINTS))
	      (let ((numerator-term-1-rs-loop 0.0d0)
		    (numerator-term-2-rs-loop 0.0d0)
		    (denominator-rs-loop      0.0d0))
		(loop for r from 1 below NUM_POINTS do
		      (loop for s from (+ r 1) to NUM_POINTS do
			    (let ((dtarget (aref d-target-cache r s))
				  (dmds    (aref d-mds-cache    r s)))
			      (if (not (dont-care-p dtarget))
				  (progn
				    (setq numerator-term-1-rs-loop 
				      (+ numerator-term-1-rs-loop (square dmds)))
				    (setq numerator-term-2-rs-loop 
				      (+ numerator-term-2-rs-loop (square (- dmds dtarget))))
				    (setq denominator-rs-loop
				      numerator-term-1-rs-loop))))))
		
		(loop for a from 1 to NUM_POINTS do       ;; a is the point      
		      (loop for k from 1 to NUM_DIMS do   ;; k is the dimension  
			    (setf (fref df (+ (* (dec a) NUM_DIMS) k))
			      (if (and (or (eql 'all moveable-coords)
					   (member a moveable-coords))
				       (not (member a unmoveable-coords)))
				  (let ((numerator-term-1-s-loop  0.0d0)
					(numerator-term-2-s-loop  0.0d0))
				    (loop for s from 1 to NUM_POINTS do
					  (if (= a s)
					      0.0d0
					    (let ((dmds    (aref d-mds-cache a s))
						  (dtarget (aref d-target-cache a s)))
					      (if (dont-care-p dtarget)
						  0.0d0
						(let ((Xak-Xsk (- (coord-from-array p a k NUM_DIMS) 
								  (coord-from-array p s k NUM_DIMS))))
						  (setq numerator-term-1-s-loop
						    (+ numerator-term-1-s-loop
						       (* (- 1.0d0 (/ dtarget dmds))
							  Xak-Xsk)))
						  (setq numerator-term-2-s-loop
						    (+ numerator-term-2-s-loop
						       Xak-Xsk)))))))
				    (* 0.5d0
				       (sqrt (/ numerator-term-1-rs-loop
						numerator-term-2-rs-loop))
				       (/ (- (* numerator-term-1-rs-loop
						2.0d0 numerator-term-1-s-loop)
					     (* numerator-term-2-rs-loop
						2.0d0 numerator-term-2-s-loop))
					  (square denominator-rs-loop))))
				0.0d0))))
		(values p df)))
	   )
	 args))


;; --- the benchmark before this new hack -------

(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 1,810 msec user, 0 msec system
; cpu time (gc)     370 msec user, 0 msec system
; cpu time (total)  2,180 msec user, 0 msec system
; real time  2,325 msec
; space allocation:
;  2,352 cons cells, 0 symbols, 39,962,672 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0) (3.3529696984856354d0 -1.8827658113585406d0)
 (2.5062545266099945d0 -2.3375824915952723d0) (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0) (-0.5566778523664344d0 0.23693709352222894d0)
 (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL

;; ----- now with the new hack

(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 1,690 msec user, 10 msec system
; cpu time (gc)     300 msec user, 0 msec system
; cpu time (total)  1,990 msec user, 10 msec system
; real time  2,113 msec
; space allocation:
;  2,358 cons cells, 0 symbols, 36,868,512 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0) (3.3529696984856354d0 -1.8827658113585406d0)
 (2.5062545266099945d0 -2.3375824915952723d0) (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0) (-0.5566778523664344d0 0.23693709352222894d0)
 (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL

ugh, only saves 0.1s, with is about 5%.  why?
well, for one df is only called 1/15th of the number of times that f is called,
and this hack is not implemented for f yet.  but maybe it will, if we can
optimize the setting up of the arrays, and maybe cache the answers between
calls.


;; ------------- now only calc half of the array (and store in both symmetric halfs) --------------------

(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 1,580 msec user, 0 msec system
; cpu time (gc)     260 msec user, 0 msec system
; cpu time (total)  1,840 msec user, 0 msec system
; real time  2,015 msec
; space allocation:
;  2,358 cons cells, 0 symbols, 35,807,296 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0) (3.3529696984856354d0 -1.8827658113585406d0)
 (2.5062545266099945d0 -2.3375824915952723d0) (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0) (-0.5566778523664344d0 0.23693709352222894d0)
 (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0

saves another 0.1s, another 5%.  that makes sense because we are doing 1/2 of the work
to set up the cache (and all the work must be in setting up the cache, not using it, which makes sense).


;; ----------- optimize the setting of d-target-cache, take advantage of knowing ll is a list of lists ---------

(defun dhat-ll (ll r s)
  ;; NOTE, for speed, this should be an aref in the future
  (if (> r s)   ;; to pick out of the upper triangle (we assume symetric matrix)
      (nth (dec r) (nth (dec s) ll))
    (nth (dec s) (nth (dec r) ll))))

(defun populate-d-target-cache (d-target-cache target-dist-ll num-points)
  (if (eql d-target-cache 'not-created-yet)
      (setq d-target-cache (make-array (list (inc num-points) (inc num-points)))))   ;; inc because numerical recepies is 1-based
  (loop for r from 1 to num-points for l in target-dist-ll do
	(loop for s from r to num-points for e in (nthcdr (dec r) l) do
	      (let ((d-target-rs e)) ;;(dhat-ll target-dist-ll r s)))
		(setf (aref d-target-cache r s) d-target-rs)
		(setf (aref d-target-cache s r) d-target-rs))))
  d-target-cache)


(time (run-mds-from-dists #'metric-mds-global-norm-conjugant-gradient nil nil (hi-table-values foo) bar))
; cpu time (non-gc) 1,630 msec user, 0 msec system
; cpu time (gc)     310 msec user, 0 msec system
; cpu time (total)  1,940 msec user, 0 msec system
; real time  2,064 msec
; space allocation:
;  2,358 cons cells, 0 symbols, 35,807,296 other bytes
((3.7544066841428805d0 0.6636023876260472d0) (3.8712470481858867d0 -0.7030821413760799d0) (3.3529696984856354d0 -1.8827658113585406d0)
 (2.5062545266099945d0 -2.3375824915952723d0) (1.960939774344858d0 -1.7006135081546678d0) (0.7307861233663118d0 -2.7322070794205864d0)
 (-1.1667294292626031d0 -2.2337667376557118d0) (-1.3208501177586833d0 -0.5677872605384943d0) (-0.5566778523664344d0 0.23693709352222894d0)
 (-2.524508169266496d0 -0.1981715837074662d0) ...)
NIL
0.16583263270278387d0
MULTIPLE-END-CONDITIONS
NIL

no difference, well, the difference may only come on large arrays where the nthcdrs really effect things.


Sample represents 1.6 seconds of processor time (out of a total of 1.6)

Times below 1.0% will be suppressed.

  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 17.7  17.7    0.3    0.5                          "unidentified"
  9.4  27.1    0.2    0.2                          EXCL::*_2OP
  8.9  35.9    0.1    0.1                          "new_other"
  7.3  43.2    0.1    0.2                          EXCL::+_2OP
  5.2  48.4    0.1    0.2                          "aref"
  4.7  53.1    0.1    0.2                          SQUARE
  4.7  57.8    0.1    0.2                          EXCL::-_2OP
  4.7  62.5    0.1    0.1                          NTHCDR
  3.6  66.1    0.1    1.3                          (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  3.6  69.8    0.1    0.2                          "new_double_float"
  3.1  72.9    0.1    0.1                          "shift_left"
  2.6  75.5    0.0    0.0                          INC
  2.1  77.6    0.0    0.8                          ... D-FROM-ARRAY
  2.1  79.7    0.0    0.1                          DHAT-LL
  2.1  81.8    0.0    0.0                          DEC
  1.6  83.3    0.0    0.3                          SQRT
  1.6  84.9    0.0    0.3                          EXCL::NAN-P
  1.6  86.5    0.0    0.1                          ASH
  1.6  88.0    0.0    0.0                          "mathlib"
  1.6  89.6    0.0    0.0                          EXCL::/_2OP
  1.6  91.1    0.0    0.0                          "integer_multiply"
  1.0  92.2    0.0    1.3                          F1DIM
  1.0  93.2    0.0    0.2                          (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  1.0  94.3    0.0    0.2                          EXCL::DOUBLE-FLOAT-TO-COMPONENTS
  1.0  95.3    0.0    0.0                          "big_add_int"
  1.0  96.4    0.0    0.0                          "prunebig"
  1.0  97.4    0.0    0.0                          DONT-CARE-P


USER(29): (prof:show-call-graph)
Time profile of sampled pc values by function, children, and parents.

Total times below 1.0% will be suppressed.
Parent and child times less 2.0% of the node will be suppressed.

Sample represents 1.6 seconds of processor time (out of a total of 1.6)

  %     %                       Parent
 self total   total local  Function
 Time  Time    secs   %         Child

  0.0 100.0     1.6   0.0   ... "start"
                1.6 100.0        ... "start_reborn_lisp"
-----------------------------------------------------
                1.6 100.0        ... "start"
  0.0 100.0     1.6   0.0   ... "start_reborn_lisp"
                1.6  99.5        ... EVAL
-----------------------------------------------------
                1.6 100.0        ... "start_reborn_lisp"
  0.0  99.5     1.6   0.0   ... EVAL
                1.6 100.0        ... EXCL::TIME-A-FUNCALL
-----------------------------------------------------
                1.6 100.0        ... EVAL
  0.0  99.5     1.6   0.0   EXCL::TIME-A-FUNCALL
                1.6 100.0        "comp_to_interp"
-----------------------------------------------------
                1.6 100.0        EXCL::TIME-A-FUNCALL
  0.0  99.5     1.6   0.0   "comp_to_interp"
                1.6 100.0        EXCL::INTERPRETED-FUNCALL
-----------------------------------------------------
                1.6 100.0        "comp_to_interp"
  0.0  99.5     1.6   0.0   ... EXCL::INTERPRETED-FUNCALL
                1.6 100.0        ... RUN-MDS-FROM-DISTS
-----------------------------------------------------
                1.6 100.0        ... EXCL::INTERPRETED-FUNCALL
  0.0  99.5     1.6   0.0   ... RUN-MDS-FROM-DISTS
                1.6 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
-----------------------------------------------------
                1.6 100.0        ... RUN-MDS-FROM-DISTS
  0.0  99.5     1.6   0.0   ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
                1.6 100.0        ... MDS
-----------------------------------------------------
                1.6 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT
  0.0  99.5     1.6   0.0   ... MDS
                1.6 100.0        ... CONJUGANT-GRADIENT-ITERATIONS
-----------------------------------------------------
                1.6 100.0        ... MDS
  0.0  99.5     1.6   0.0   CONJUGANT-GRADIENT-ITERATIONS
                1.3  79.6        LINMIN
                0.2  14.7        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.1   4.7        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
-----------------------------------------------------
                1.3 100.0        CONJUGANT-GRADIENT-ITERATIONS
  0.0  79.2     1.3   0.0   LINMIN
                0.8  62.5        BRENT
                0.5  37.5        MNBRAK
-----------------------------------------------------
                0.8  62.5        BRENT
                0.5  37.5        MNBRAK
  1.0  79.2     1.3   1.3   F1DIM
                1.2  92.8        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.0   3.3        "unidentified"
-----------------------------------------------------
                1.2  94.0        F1DIM
                0.1   6.0        CONJUGANT-GRADIENT-ITERATIONS
  3.6  78.1     1.3   4.7   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.8  61.3        D-FROM-ARRAY
                0.1  10.0        "unidentified"
                0.1   8.7        DHAT-LL
                0.1   6.0        SQUARE
                0.1   4.7        EXCL::-_2OP
                0.0   3.3        EXCL::+_2OP
-----------------------------------------------------
                0.8  93.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.1   6.1        POPULATE-D-MDS-CACHE
  2.1  51.0     0.8   4.1   ... D-FROM-ARRAY
                0.3  31.6        SQRT
                0.2  22.4        "aref"
                0.1   9.2        SQUARE
                0.1   8.2        EXCL::*_2OP
                0.1   8.2        EXCL::-_2OP
                0.0   5.1        EXCL::+_2OP
                0.0   5.1        "mathlib"
                0.0   3.1        ... "unidentified"
-----------------------------------------------------
                0.8 100.0        LINMIN
  0.0  49.5     0.8   0.0   BRENT
                0.8 100.0        F1DIM
-----------------------------------------------------
                0.3  55.2r       "unidentified"
                0.1  25.9r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.1  22.4r       "new_double_float"
                0.1  20.7r       "aref"
                0.0   8.6r       F1DIM
                0.0   6.9r       "newbignum"
                0.0   5.2r       ... D-FROM-ARRAY
                0.0   3.4r       CONJUGANT-GRADIENT-ITERATIONS
                0.0   3.4r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.0   3.4r       EXCL::NAN-P
 17.7  30.2     0.5  58.6   "unidentified"
                0.3  35.6        "unidentified"
                0.1   6.7        EXCL::*_2OP
                0.0   5.6        INC
                0.0   3.3        SQUARE
                0.0   2.2        ASH
                0.0   2.2        NTHCDR
                0.0   2.2        DONT-CARE-P
                0.0   2.2        EXCL::-_2OP
-----------------------------------------------------
                0.5 100.0        LINMIN
  0.0  29.7     0.5   0.0   MNBRAK
                0.5 100.0        F1DIM
-----------------------------------------------------
                0.3  93.9        D-FROM-ARRAY
                0.0   3.0        "unidentified"
                0.0   3.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  1.6  17.2     0.3   9.1   SQRT
                0.3  90.9        EXCL::NAN-P
-----------------------------------------------------
                0.3 100.0        SQRT
  1.6  15.6     0.3  10.0   EXCL::NAN-P
                0.2  83.3        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.0   6.7        "unidentified"
-----------------------------------------------------
                0.2 100.0        CONJUGANT-GRADIENT-ITERATIONS
  1.0  14.6     0.2   7.1   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.1  32.1        EXCL::+_2OP
                0.1  21.4        POPULATE-D-MDS-CACHE
                0.0  10.7        EXCL::/_2OP
                0.0   7.1        "unidentified"
                0.0   7.1        COORD-FROM-ARRAY
                0.0   3.6        "new_double_float"
                0.0   3.6        "aref"
                0.0   3.6        SQRT
                0.0   3.6        EXCL::-_2OP
-----------------------------------------------------
                0.1  32.1        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.1  25.0        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.0  17.9        D-FROM-ARRAY
                0.0  17.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.0   3.6        "unidentified"
                0.0   3.6        F1DIM
  7.3  14.6     0.2  50.0   EXCL::+_2OP
                0.1  28.6        "new_double_float"
                0.0  17.9        "integer_add"
                0.0   3.6        "bignum_add"
-----------------------------------------------------
                0.1  40.7        SQUARE
                0.1  29.6        D-FROM-ARRAY
                0.1  22.2        "unidentified"
                0.0   3.7        F1DIM
                0.0   3.7        COORD-FROM-ARRAY
  9.4  14.1     0.2  66.7   EXCL::*_2OP
                0.1  22.2        "new_double_float"
                0.0  11.1        "integer_multiply"
-----------------------------------------------------
                0.1  29.6        EXCL::+_2OP
                0.1  25.9        EXCL::-_2OP
                0.1  22.2        EXCL::*_2OP
                0.0   7.4        "mathlib"
                0.0   3.7        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.0   3.7        SQUARE
                0.0   3.7        D-FROM-ARRAY
                0.0   3.7        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  3.6  14.1     0.2  25.9   "new_double_float"
                0.1  48.1        "unidentified"
                0.1  25.9        "new_other"
-----------------------------------------------------
                0.2 100.0        EXCL::NAN-P
  1.0  13.0     0.2   8.0   EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.1  40.0        ASH
                0.1  28.0        EXCL::+_2OP
                0.0  16.0        "unsigned_fixnum_or_bignum"
                0.0   8.0        "shift_left"
-----------------------------------------------------
                0.2  91.7        D-FROM-ARRAY
                0.0   4.2        F1DIM
                0.0   4.2        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  5.2  12.5     0.2  41.7   "aref"
                0.1  50.0        "unidentified"
                0.0   4.2        "calc_index"
                0.0   4.2        "new_other"
-----------------------------------------------------
                0.1  42.9        D-FROM-ARRAY
                0.1  42.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.0  14.3        "unidentified"
  4.7  10.9     0.2  42.9   SQUARE
                0.1  52.4        EXCL::*_2OP
                0.0   4.8        "new_double_float"
-----------------------------------------------------
                0.1  44.4        D-FROM-ARRAY
                0.1  38.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
                0.0  11.1        "unidentified"
                0.0   5.6        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  4.7   9.4     0.2  50.0   EXCL::-_2OP
                0.1  38.9        "new_double_float"
                0.0  11.1        "new_other"
-----------------------------------------------------
                0.1  41.2        "new_double_float"
                0.1  35.3        "newbignum"
                0.0  11.8        EXCL::-_2OP
                0.0   5.9        "aref"
                0.0   5.9        D-FROM-ARRAY
  8.9   8.9     0.1 100.0   "new_other"
-----------------------------------------------------
                0.1 100.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  2.1   6.8     0.1  30.8   DHAT-LL
                0.1  53.8        NTHCDR
                0.0  15.4        DEC
-----------------------------------------------------
                0.1  83.3        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.0  16.7        "unidentified"
  1.6   6.2     0.1  25.0   ASH
                0.1  75.0        "shift_left"
-----------------------------------------------------
                0.1  81.8        ASH
                0.0  18.2        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
  3.1   5.7     0.1  54.5   "shift_left"
                0.0  27.3        "newbignum"
                0.0  18.2        "prunebig"
-----------------------------------------------------
                0.0  40.0        "unsigned_fixnum_or_bignum"
                0.0  30.0        "big_add_int"
                0.0  30.0        "shift_left"
  0.0   5.2     0.1   0.0   "newbignum"
                0.1  60.0        "new_other"
                0.0  40.0        "unidentified"
-----------------------------------------------------
                0.1  77.8        DHAT-LL
                0.0  22.2        "unidentified"
  4.7   4.7     0.1 100.0   NTHCDR
-----------------------------------------------------
                0.1 100.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  0.0   3.1     0.1   0.0   POPULATE-D-MDS-CACHE
                0.1 100.0        D-FROM-ARRAY
-----------------------------------------------------
                0.0 100.0        "unidentified"
  2.6   2.6     0.0 100.0   INC
-----------------------------------------------------
                0.0 100.0        ... D-FROM-ARRAY
  1.6   2.6     0.0  60.0   "mathlib"
                0.0  40.0        "new_double_float"
-----------------------------------------------------
                0.0 100.0        EXCL::+_2OP
  0.0   2.6     0.0   0.0   "integer_add"
                0.0  80.0        "bignum_add"
                0.0  20.0        "big_add_int"
-----------------------------------------------------
                0.0  80.0        "bignum_add"
                0.0  20.0        "integer_add"
  1.0   2.6     0.0  40.0   "big_add_int"
                0.0  60.0        "newbignum"
-----------------------------------------------------
                0.0  80.0        "integer_add"
                0.0  20.0        EXCL::+_2OP
  0.5   2.6     0.0  20.0   "bignum_add"
                0.0  80.0        "big_add_int"
-----------------------------------------------------
                0.0  75.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
                0.0  25.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 0)
  1.6   2.1     0.0  75.0   EXCL::/_2OP
                0.0  25.0        EXCL::FLD_/
-----------------------------------------------------
                0.0  50.0        DHAT-LL
                0.0  25.0        COORD-FROM-ARRAY
                0.0  25.0        D-FROM-ARRAY
  2.1   2.1     0.0 100.0   DEC
-----------------------------------------------------
                0.0 100.0        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
  0.0   2.1     0.0   0.0   "unsigned_fixnum_or_bignum"
                0.0 100.0        "newbignum"
-----------------------------------------------------
                0.0 100.0        EXCL::*_2OP
  1.6   1.6     0.0 100.0   "integer_multiply"
-----------------------------------------------------
                0.0 100.0        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT 1)
  0.0   1.0     0.0   0.0   COORD-FROM-ARRAY
                0.0  50.0        DEC
                0.0  50.0        EXCL::*_2OP
-----------------------------------------------------
                0.0 100.0        "unidentified"
  1.0   1.0     0.0 100.0   DONT-CARE-P
-----------------------------------------------------
                0.0 100.0        "shift_left"
  1.0   1.0     0.0 100.0   "prunebig"
-----------------------------------------------------



;;;----------------------------------------------------------------------
;;;                      link
;;;----------------------------------------------------------------------

do not include the above.  for 10%, i do not think it is worth it right now
(though it maybe be more than 10% on larger arrays)





;;;----------------------------------------------------------------------
;;;                      back here, april 2002
;;;----------------------------------------------------------------------


(setq tab1-asl
  (read-hi-table-and-convert
   "mds/data/HI-2001-08/TAB1.txt"
   1
   0))

(loop for i from 1 to 3 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   tab1-asl
		   :starting-coordss 5)))))))



; cpu time (non-gc) 10,740 msec user, 130 msec system
; cpu time (gc)     2,130 msec user, 0 msec system
; cpu time (total)  12,870 msec user, 130 msec system
; real time  13,838 msec
; space allocation:
;  38,321 cons cells, 0 symbols, 550,488,208 other bytes
; cpu time (non-gc) 10,670 msec user, 80 msec system
; cpu time (gc)     2,070 msec user, 0 msec system
; cpu time (total)  12,740 msec user, 80 msec system
; real time  14,196 msec
; space allocation:
;  37,829 cons cells, 0 symbols, -3,744,485,464 other bytes
; cpu time (non-gc) 10,740 msec user, 40 msec system
; cpu time (gc)     2,030 msec user, 10 msec system
; cpu time (total)  12,770 msec user, 50 msec system
; real time  16,456 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 550,454,416 other bytes
(11.571045671299325d0 11.571045671299325d0 11.571045671299325d0)
USER(19): (apply #'nary-equal *)
T




;;;----------------------------------------------------------------------
;;;         only call d-mds if we a non-dont-care d-target
;;;----------------------------------------------------------------------

; cpu time (non-gc) 5,170 msec user, 10 msec system
; cpu time (gc)     1,300 msec user, 0 msec system
; cpu time (total)  6,470 msec user, 10 msec system
; real time  7,125 msec
; space allocation:
;  35,305 cons cells, 0 symbols, 229,309,200 other bytes
; cpu time (non-gc) 5,140 msec user, 0 msec system
; cpu time (gc)     460 msec user, 0 msec system
; cpu time (total)  5,600 msec user, 0 msec system
; real time  6,188 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 229,304,304 other bytes
; cpu time (non-gc) 5,190 msec user, 20 msec system
; cpu time (gc)     440 msec user, 0 msec system
; cpu time (total)  5,630 msec user, 20 msec system
; real time  6,235 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 229,304,304 other bytes
(11.571045671299325d0 11.571045671299325d0 11.571045671299325d0)


5.17 seconds from  10.7  seconds.  (wow, a trivial hack, taking 5 mins, without even having to think hard!)



;;;----------------------------------------------------------------------
;;;  only calc the d-target and d-mds once for each pair of points (not once for every dimension)
;;;----------------------------------------------------------------------


; cpu time (non-gc) 4,130 msec user, 0 msec system
; cpu time (gc)     390 msec user, 0 msec system
; cpu time (total)  4,520 msec user, 0 msec system
; real time  5,009 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 191,157,160 other bytes
; cpu time (non-gc) 4,080 msec user, 10 msec system
; cpu time (gc)     360 msec user, 0 msec system
; cpu time (total)  4,440 msec user, 10 msec system
; real time  5,019 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 191,157,160 other bytes
; cpu time (non-gc) 4,120 msec user, 10 msec system
; cpu time (gc)     350 msec user, 0 msec system
; cpu time (total)  4,470 msec user, 10 msec system
; real time  5,089 msec
; space allocation:
;  35,265 cons cells, 0 symbols, 191,157,160 other bytes
(11.571045671299325d0 11.571045671299325d0 11.571045671299325d0)


4.1 seconds from 5.17 seconds (another 20%, but i expected more, especially as we were running in 5d) 
this suggests that there is only another 0.25 seconds available for tab1 on this (as we have cut the work by 4/5ths and got a 1 second improvement)



let us test on tab2 also

(setq tab2-asl
  (read-hi-table-and-convert
   "mds/data/HI-2001-08/TAB2.txt"
   1
   0))

(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   tab2-asl
		   :starting-coordss 5)))))))

baseline

See the documentation for variable *GLOBAL-GC-BEHAVIOR* for more information.
; cpu time (non-gc) 201,590 msec (00:03:21.590) user, 720 msec system
; cpu time (gc)     15,630 msec user, 50 msec system
; cpu time (total)  217,220 msec (00:03:37.220) user, 770 msec system
; real time  234,481 msec (00:03:54.481)
; space allocation:
;  251,492 cons cells, 0 symbols, 1,020,110,832 other bytes
(125.32998670594318d0)



with the simple only calculating dmds if dtarget is not a true-dont-care

; cpu time (non-gc) 55,130 msec user, 120 msec system
; cpu time (gc)     2,760 msec user, 10 msec system
; cpu time (total)  57,890 msec user, 130 msec system
; real time  60,128 msec (00:01:00.128)
; space allocation:
;  250,473 cons cells, 0 symbols, 1,297,376,040 other bytes
(125.32998670594318d0)

wow, a 4-x speedup from 202s to 55s



USER(6): (hi-table-length tab2-asl)
115
USER(7): (hi-table-length tab1-asl)
33

tab2 is 3.5 times larger than tab1
and our speedup is
2x on tab1 and 4x on tab2




now add the calculating of d-target and d-mds outside the look for each coordinate

; cpu time (non-gc) 42,010 msec user, 10 msec system
; cpu time (gc)     2,490 msec user, 0 msec system
; cpu time (total)  44,500 msec user, 10 msec system
; real time  47,309 msec
; space allocation:
;  250,433 cons cells, 0 symbols, 1,065,077,712 other bytes
(125.32998670594318d0)

42s from 55s, about the same speedup as before, about 20%




;;;----------------------------------------------------------------------
;;;                 where is the time now spent?
;;;----------------------------------------------------------------------

  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 34.6  34.6   14.6   14.6                          NTHCDR
  6.8  41.4    2.9    6.3                          "unidentified"
  6.8  48.2    2.9    3.8                          "aref"
  5.0  53.2    2.1    3.8                          EXCL::*_2OP
  5.0  58.2    2.1    3.8                          EXCL::+_2OP
  4.3  62.5    1.8    1.8                          "new_other"
  4.1  66.7    1.7    2.9                          EXCL::-_2OP
  4.0  70.7    1.7   28.2                          (:INTERNAL
                                                    METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
  3.5  74.2    1.5   16.1                          DHAT-LL
  3.4  77.6    1.4    3.9                          "new_double_float"
  3.0  80.6    1.3    9.9                          ... D-FROM-ARRAY
  3.0  83.6    1.3    9.4                          (:INTERNAL
                                                    METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
  2.0  85.6    0.8    0.8                          DONT-CARE-P
  1.4  87.0    0.6    0.6                          DEC
  1.3  88.3    0.5    0.5                          "_inv_s_aref"


ok, this is clear, a huge chunk in nthcdr, which is the dhat-ll lookup i guess


convert the target distance ll to an array

; cpu time (non-gc) 27,500 msec user, 20 msec system
; cpu time (gc)     2,380 msec user, 0 msec system
; cpu time (total)  29,880 msec user, 20 msec system
; real time  33,791 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,065,131,096 other bytes
(125.32998670594318d0)


27.5s from 42s  -- reduction by about 1/3rd, which is what we would expect from the above profile where we were spending 35% of time in nthcdr



;;;----------------------------------------------------------------------
;;;                      where is time now spent?
;;;----------------------------------------------------------------------


  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 18.5  18.5    5.1    6.8                          "aref"
  8.9  27.4    2.5    4.8                          ... "unidentified"
  7.5  34.9    2.1    3.6                          EXCL::*_2OP
  6.7  41.6    1.9    1.9                          "new_other"
  6.6  48.3    1.8    3.6                          EXCL::+_2OP
  6.0  54.2    1.6   15.6                          (:INTERNAL
                                                    METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
  5.9  60.1    1.6    2.9                          EXCL::-_2OP
  5.9  66.0    1.6    4.1                          "new_double_float"
  5.2  71.1    1.4    9.9                          ... D-FROM-ARRAY
  5.1  76.2    1.4    7.9                          (:INTERNAL
                                                    METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
  3.3  79.5    0.9    0.9                          "calc_index"
  2.3  81.8    0.6    0.6                          DONT-CARE-P
  1.9  83.7    0.5    0.5                          LESS-THAN-TEN-P
  1.1  84.8    0.3    0.3                          "_inv_s_aref"
  1.1  85.9    0.3    1.9                          SQUARE
  1.1  87.0    0.3    0.5                          "shift_left"
  1.0  88.0    0.3   16.8                          F1DIM
  1.0  89.1    0.3    0.3                          "integer_multiply"



Time profile of sampled pc values by function, children, and parents.

Total times below 1.0% will be suppressed.
Parent and child times less 2.0% of the node will be suppressed.

Sample represents 27.6 seconds of processor time (out of a total of 27.6)

  %     %                       Parent
 self total   total local  Function
 Time  Time    secs   %         Child

  0.0 100.0    27.6   0.0   ... "start"
               27.6 100.0        ... "start_reborn_lisp"
-----------------------------------------------------
               27.6 100.0        ... "start"
  0.0 100.0    27.6   0.0   ... "start_reborn_lisp"
               27.6 100.0        ... EVAL
-----------------------------------------------------
               27.6 100.0        ... "start_reborn_lisp"
  0.0 100.0    27.6   0.0   EVAL
               27.6 100.0        LET
-----------------------------------------------------
               27.6 100.0        EVAL
  0.0 100.0    27.6   0.0   ... LET
               27.6 100.0        ... BLOCK
-----------------------------------------------------
               27.6 100.0        ... LET
  0.0 100.0    27.6   0.0   ... BLOCK
               27.6 100.0        ... EXCL::EVALUATE-A-TAGBODY
-----------------------------------------------------
               27.6 100.0        ... BLOCK
  0.0 100.0    27.6   0.0   ... EXCL::EVALUATE-A-TAGBODY
               27.6 100.0        ... EXCL::TIME-A-FUNCALL
-----------------------------------------------------
               27.6 100.0        ... EXCL::EVALUATE-A-TAGBODY
  0.0 100.0    27.6   0.0   EXCL::TIME-A-FUNCALL
               27.6 100.0        "comp_to_interp"
-----------------------------------------------------
               27.6 100.0        EXCL::TIME-A-FUNCALL
  0.0 100.0    27.6   0.0   "comp_to_interp"
               27.6 100.0        EXCL::INTERPRETED-FUNCALL
-----------------------------------------------------
               27.6 100.0        "comp_to_interp"
  0.0 100.0    27.6   0.0   ... EXCL::INTERPRETED-FUNCALL
               27.6 100.0        ... BATCH-MDS-KEYWORD-ARGS
-----------------------------------------------------
               27.6 100.0        ... EXCL::INTERPRETED-FUNCALL
  0.0 100.0    27.6   0.0   BATCH-MDS-KEYWORD-ARGS
               27.6  99.9        BATCH-MDS
-----------------------------------------------------
               27.6 100.0        BATCH-MDS-KEYWORD-ARGS
  0.0  99.9    27.6   0.0   BATCH-MDS
               27.5  99.7        RUN-MDS-FROM-DISTS
-----------------------------------------------------
               27.5 100.0        BATCH-MDS
  0.0  99.6    27.5   0.0   ... RUN-MDS-FROM-DISTS
               27.5 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC
-----------------------------------------------------
               27.5 100.0        ... RUN-MDS-FROM-DISTS
  0.0  99.6    27.5   0.0   ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC
               27.5 100.0        ... MDS
-----------------------------------------------------
               27.5 100.0        ... METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC
  0.0  99.6    27.5   0.0   ... MDS
               27.5  99.9        ... CONJUGANT-GRADIENT-ITERATIONS
-----------------------------------------------------
               27.5 100.0        ... MDS
  0.2  99.6    27.5   0.2   CONJUGANT-GRADIENT-ITERATIONS
               17.2  62.5        LINMIN
                7.9  28.8        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                1.1   3.8        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.6   2.2        "unidentified"
-----------------------------------------------------
               17.2 100.0        CONJUGANT-GRADIENT-ITERATIONS
  0.2  62.3    17.2   0.3   LINMIN
               13.0  75.3        BRENT
                4.0  23.3        MNBRAK
-----------------------------------------------------
               12.8  76.3        BRENT
                4.0  23.7        MNBRAK
  1.0  60.9    16.8   1.7   F1DIM
               14.5  86.4        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.7   4.2        "unidentified"
                0.4   2.4        EXCL::+_2OP
                0.4   2.3        "aref"
-----------------------------------------------------
               14.5  93.2        F1DIM
                1.1   6.8        CONJUGANT-GRADIENT-ITERATIONS
  6.0  56.4    15.6  10.6   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                8.7  56.1        D-FROM-ARRAY
                2.2  14.4        "aref"
                0.6   3.7        "unidentified"
                0.5   3.1        EXCL::+_2OP
                0.5   3.0        EXCL::-_2OP
                0.4   2.5        YI-FROM-ARRAY
                0.3   2.1        ZI-FROM-ARRAY
-----------------------------------------------------
               13.0 100.0        LINMIN
  0.1  46.9    13.0   0.1   BRENT
               12.8  99.1        F1DIM
-----------------------------------------------------
                8.7  88.1        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                1.1  11.5        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
  5.2  35.9     9.9  14.3   ... D-FROM-ARRAY
                1.7  17.0        SQRT
                1.7  16.7        EXCL::-_2OP
                1.6  16.5        SQUARE
                1.3  13.1        EXCL::+_2OP
                1.2  12.4        "aref"
                0.3   3.2        ... "unidentified"
                0.2   2.1        EXCL::*_2OP
-----------------------------------------------------
                7.9 100.0        CONJUGANT-GRADIENT-ITERATIONS
  5.1  28.7     7.9  17.6   (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                2.1  25.9        "aref"
                1.1  14.5        D-FROM-ARRAY
                0.6   7.5        EXCL::*_2OP
                0.5   6.6        EXCL::-_2OP
                0.4   4.9        EXCL::+_2OP
                0.3   3.5        COORD-FROM-ARRAY
                0.2   2.9        EXCL::/_2OP
                0.2   2.7        ZI-FROM-ARRAY
                0.2   2.4        YI-FROM-ARRAY
                0.2   2.4        "unidentified"
                0.2   2.1        DONT-CARE-P
-----------------------------------------------------
                2.2  33.1r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                2.1  30.2r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                1.3  18.7r       "aref"
                1.2  18.1r       D-FROM-ARRAY
                0.4   5.6r       F1DIM
                0.3   4.9r       CONJUGANT-GRADIENT-ITERATIONS
                0.2   3.1r       ZI-FROM-ARRAY
                0.2   3.0r       YI-FROM-ARRAY
 18.5  24.5     6.8  75.3   "aref"
                1.3  15.7        "aref"
                0.8  10.1        "calc_index"
                0.5   6.5        "unidentified"
                0.3   4.1        "new_other"
-----------------------------------------------------
                2.3  48.6r       "unidentified"
                1.5  30.1r       "new_double_float"
                0.7  14.8r       F1DIM
                0.6  12.7r       CONJUGANT-GRADIENT-ITERATIONS
                0.6  12.1r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.5  10.9r       "aref"
                0.3   6.5r       D-FROM-ARRAY
                0.2   4.0r       (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                0.1   2.3r       "newbignum"
  8.9  17.4     4.8  51.2   ... "unidentified"
                2.3  32.7        "unidentified"
                0.5   7.4        LESS-THAN-TEN-P
                0.3   4.0        DONT-CARE-P
                0.3   3.8        EXCL::*_2OP
                0.2   3.4        TRUE-DONT-CARE-P
                0.2   2.8        EXCL::-_2OP
                0.2   2.6        EXCL::+_2OP
-----------------------------------------------------
                1.2  28.9        EXCL::-_2OP
                1.2  28.3        EXCL::*_2OP
                1.1  26.9        EXCL::+_2OP
                0.1   3.4        "mathlib"
                0.1   2.9        D-FROM-ARRAY
                0.1   2.5        EXCL::FLD_/
                0.1   2.0        SQUARE
  5.9  14.9     4.1  39.2   "new_double_float"
                1.5  35.2        "unidentified"
                1.1  25.6        "new_other"
-----------------------------------------------------
                4.0 100.0        LINMIN
  0.0  14.5     4.0   0.0   MNBRAK
                4.0  99.5        F1DIM
-----------------------------------------------------
                1.6  43.7        SQUARE
                0.6  16.5        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                0.3   8.8        F1DIM
                0.3   7.8        YI-FROM-ARRAY
                0.3   7.5        "unidentified"
                0.2   5.7        D-FROM-ARRAY
                0.2   5.2        ZI-FROM-ARRAY
                0.1   2.3        COORD-FROM-ARRAY
  7.5  12.9     3.6  57.9   EXCL::*_2OP
                1.2  32.6        "new_double_float"
                0.2   6.5        "integer_multiply"
                0.1   3.1        "new_other"
-----------------------------------------------------
                1.3  36.3        D-FROM-ARRAY
                0.6  16.3        EXCL::DOUBLE-FLOAT-TO-COMPONENTS
                0.5  13.7        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.4  11.4        F1DIM
                0.4  10.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                0.2   5.2        "unidentified"
                0.1   2.8        CONJUGANT-GRADIENT-ITERATIONS
                0.1   2.1        LINMIN
  6.6  12.9     3.6  51.3   EXCL::+_2OP
                1.1  31.1        "new_double_float"
                0.5  12.7        "integer_add"
                0.1   3.6        "new_other"
-----------------------------------------------------
                1.7  57.0        D-FROM-ARRAY
                0.5  18.2        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 1)
                0.5  15.9        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.2   7.0        "unidentified"
  5.9  10.5     2.9  56.1   EXCL::-_2OP
                1.2  41.1        "new_double_float"
                0.1   2.5        "new_other"
-----------------------------------------------------
                1.6  83.4        D-FROM-ARRAY
                0.2  12.3        (:INTERNAL METRIC-MDS-GLOBAL-NORM-CONJUGANT-GRADIENT-HI-METRIC 0)
                0.1   4.3        "unidentified"
  1.1   7.1     1.9  15.6   SQUARE
                1.6  80.1        EXCL::*_2OP
                0.1   4.3        "new_double_float"
-----------------------------------------------------
                1.1  56.4        "new_double_float"
                0.3  17.8        "aref"
                0.1   6.9        EXCL::+_2OP
                0.1   5.9        EXCL::*_2OP
                0.1   5.4        "newbignum"
                0.1   4.0        EXCL::-_2OP
                0.1   3.0        D-FROM-ARRAY
  6.7   6.8     1.9  99.5   "new_other"
-----------------------------------------------------



;;;----------------------------------------------------------------------
;;;      try running x*x instead f square to save the function call
;;;----------------------------------------------------------------------

; cpu time (non-gc) 27,170 msec user, 50 msec system
; cpu time (gc)     2,320 msec user, 0 msec system
; cpu time (total)  29,490 msec user, 50 msec system
; real time  33,576 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,065,131,096 other bytes
(125.32998670594318d0)

might have saved a little, but it is in the noise (leave it anywhow, i don't see how it could harm)




;;;----------------------------------------------------------------------
;;;                      these earlier thoughts
;;;----------------------------------------------------------------------

2002-03-11

DONE
put dtarget into an array at the start (an array presized for our matrix) 
  faster to put in once i think than each time do an nthcdr
  ahhh -- maybe i only need to do this once for each optimization also

DONE
calc dmds only if dtarget is not a true-dont-care

put the yi and zi into a let so i only do them once

see if similar things in the f (as opposed to the df)

DONE
run the profiler to check this really is where i'm spending my time

another hack
  sqaush just called once and stored (and the dsquash as alan's hack)




;;;----------------------------------------------------------------------
;;;                  how long does the c code take?
;;;----------------------------------------------------------------------

(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (lapedes-dim-anneal 
		   tab2-asl
		   :starting-coordss 5
		   :starting-dimension 5
		   :final-dimension 5
		   :dim-anneal-f #'dim-anneal-wrapper
		   )))))))

; cpu time (non-gc) 440 msec user, 0 msec system
; cpu time (gc)     30 msec user, 0 msec system
; cpu time (total)  470 msec user, 0 msec system
; real time  14,874 msec
; space allocation:
;  219,311 cons cells, 0 symbols, 1,208,872 other bytes
(124.51031)

(125.32998670594318d0)

(make-master-mds-window
 tab2-asl
 :starting-coordss (nth 0 results))


the stress in lisp mds is 125.6, which then optimizes to 125.0



the c code is 15s, the lisp code is about 29 s (both wall clock time now)  (and some lisp to be gained by going to the latest version of the software)
so lisp is about 2x slower -- but at least now is in the ball park -- lisp is 10x faster than it was (wall time 234s)

close enuf, we can do the rest some other time
  (we should be close to converting ron to lisp -- just need to do all the dim anneal schedules?)




;;;----------------------------------------------------------------------
;;;        just another quick look around (it is hard to stop)
;;;----------------------------------------------------------------------

try to speed up d-from-array.
dont fall faref that does the dec, but aref, maybe 1% difference (not worth it at this ponit)

; cpu time (non-gc) 27,290 msec user, 70 msec system
; cpu time (gc)     2,070 msec user, 0 msec system
; cpu time (total)  29,360 msec user, 70 msec system
; real time  33,565 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,065,131,096 other bytes
(125.32998670594318d0)



;;;----------------------------------------------------------------------
;;;                      to finish propoerly
;;;----------------------------------------------------------------------

put a comment in the hillclimber
add the 3 speed hacks to the non hi optimizers

add dim anneal to the sequence optimzer?
and restricted dims to the sequence optimizer?
  (maybe not, maybe when i want these we just convert everything to a similarity matrix for hi)
  (the sequence stuff anyhow i am running with the hi so i can have thresholds, and regular sequence matrices are fast enuf for now anywhow)

;;;----------------------------------------------------------------------
;;;                      on the other optimzers
;;;----------------------------------------------------------------------

(setq similarity-matrix-test
  (read-hi-table-and-convert
   "mds/data/miscFromRon/PROmatrix.txt"
   1
   0))

(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   similarity-matrix-test
		   :starting-coordss 5)))))))


(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   similarity-matrix-test
		   :starting-coordss 5)))))))
; cpu time (non-gc) 6,220 msec user, 50 msec system
; cpu time (gc)     1,370 msec user, 10 msec system
; cpu time (total)  7,590 msec user, 60 msec system
; real time  8,910 msec
; space allocation:
;  23,828 cons cells, 0 symbols, 349,734,216 other bytes
(0.019150902620191007d0)


not much difference for this test case, so punt (see the notes above)



;;;----------------------------------------------------------------------
;;;      do the no access of dmds if dtarget is don't care again
;;;----------------------------------------------------------------------

(setq tab2-asl
  (read-hi-table-and-convert
   "mds/data/HI-2001-08/TAB2.txt"
   1
   0))

(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   tab2-asl
		   :starting-coordss 5)))))))

; cpu time (non-gc) 25,440 msec user, 90 msec system
; cpu time (gc)     2,960 msec user, 0 msec system
; cpu time (total)  28,400 msec user, 90 msec system
; real time  31,919 msec
; space allocation:
;  250,476 cons cells, 0 symbols, 1,065,135,992 other bytes
(125.32998670594318d0)

hmmm, last benchmark was 27.small, is this improvement because we have a fresh lisp (or have done a roboot?)



; cpu time (non-gc) 24,950 msec user, 60 msec system
; cpu time (gc)     1,800 msec user, 0 msec system
; cpu time (total)  26,750 msec user, 60 msec system
; real time  28,179 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,065,131,096 other bytes
(125.32998670594318d0)

small difference, about 0.4s, which is 2%



now add single calculation of the part in the f

; cpu time (non-gc) 25,230 msec user, 90 msec system
; cpu time (gc)     1,830 msec user, 0 msec system
; cpu time (total)  27,060 msec user, 90 msec system
; real time  28,390 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,061,044,696 other bytes
(125.32998670594318d0)

slower!



restart lisp and retry

; cpu time (non-gc) 25,210 msec user, 90 msec system
; cpu time (gc)     3,220 msec user, 30 msec system
; cpu time (total)  28,430 msec user, 120 msec system
; real time  30,145 msec
; space allocation:
;  250,476 cons cells, 0 symbols, 1,065,135,992 other bytes
(125.32998670594318d0)

benchmark the same

now with the dtarget gate on dmds, and with the single calc of part in f
; cpu time (non-gc) 25,100 msec user, 30 msec system
; cpu time (gc)     2,770 msec user, 0 msec system
; cpu time (total)  27,870 msec user, 30 msec system
; real time  29,544 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,061,044,696 other bytes
(125.32998670594318d0)

still the same, ok, take out the gate, and leave in the part (as the gate makes the code more complex, and the part makes the code simpler)

; cpu time (non-gc) 25,090 msec user, 70 msec system
; cpu time (gc)     2,610 msec user, 0 msec system
; cpu time (total)  27,700 msec user, 70 msec system
; real time  29,838 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,061,044,696 other bytes
(125.32998670594318d0)


put the part into the df as well (again to clean up the code)
; cpu time (non-gc) 24,840 msec user, 70 msec system
; cpu time (gc)     2,790 msec user, 0 msec system
; cpu time (total)  27,630 msec user, 70 msec system
; real time  29,283 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,055,452,696 other bytes
(125.32998670594318d0)

maybe a small difference, but nothing we care about at this point


clean up code more with the dmds-safe-reciprocal
; cpu time (non-gc) 25,080 msec user, 160 msec system
; cpu time (gc)     2,770 msec user, 0 msec system
; cpu time (total)  27,850 msec user, 160 msec system
; real time  29,405 msec
; space allocation:
;  250,476 cons cells, 0 symbols, -3,239,509,656 other bytes
(125.32998670550178d0)

difference is 4.4140335830888944d-10, this is ok, it is just the reorganization of the multiplies


reorganize again to make code simpler (again we might get slight numeric differences)
; cpu time (non-gc) 24,960 msec user, 80 msec system
; cpu time (gc)     2,690 msec user, 0 msec system
; cpu time (total)  27,650 msec user, 80 msec system
; real time  29,411 msec
; space allocation:
;  250,436 cons cells, 0 symbols, 1,053,580,168 other bytes
(125.32998670695352d0)

(- 125.32998670550178d0 125.32998670695352d0)
-1.451738285140891d-9

ok, this is just numerics



surprise -- it added 5% to simply the code further and not have many
   (setq numerator-term-1-s-loop
	(+ numerator-term-1-s-loop ...))
but just the one!



simplify the code further, to bring the <10 and non<10 common stuff in common
(might get code faster just because simpler)

; cpu time (non-gc) 23,190 msec user, 80 msec system
; cpu time (gc)     2,830 msec user, 0 msec system
; cpu time (total)  26,020 msec user, 80 msec system
; real time  27,398 msec
; space allocation:
;  250,436 cons cells, 0 symbols, -3,242,229,528 other bytes
(125.32998670695352d0)

yes, ~8% faster just by being simpler



now simplify the Yi, first get a baseline stress for letting one col adjust 

(loop for i from 1 to 1 collect 
      (progn
	(seed-random 467739585)
	(time
	 (setq stress
	   (nth 1 
		(setq results
		  (batch-mds-keyword-args 
		   tab2-asl
		   :starting-coordss 5
		   :adjustable-columns (list (position 'hk/1/68-sr (hi-table-antigens tab2-asl)))
		   :adjustable-rows    (list (position 'UT/5093/74-AG (hi-table-antigens tab2-asl)))
		   )))))))

; cpu time (non-gc) 36,570 msec user, 210 msec system
; cpu time (gc)     6,290 msec user, 20 msec system
; cpu time (total)  42,860 msec user, 230 msec system
; real time  46,713 msec
; space allocation:
;  306,685 cons cells, 0 symbols, 1,532,541,856 other bytes
(116.97346062965464d0)


(ppll (car (last (nth 0 results))))
((COL-AND-ROW-ADJUSTS
  (-1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 12.546309340385049d0 8.0d0 9.0d0 8.0d0 8.0d0 8.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 2.8796443504509193d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)))

we get a 2.9 for the antigen, and a 12.5 for the serum


; cpu time (non-gc) 33,260 msec user, 90 msec system
; cpu time (gc)     2,420 msec user, 0 msec system
; cpu time (total)  35,680 msec user, 90 msec system
; real time  37,670 msec
; space allocation:
;  305,110 cons cells, 0 symbols, 1,526,812,296 other bytes
(116.97346062965464d0)

((COL-AND-ROW-ADJUSTS
  (-1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7
   -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 -1.0d+7 12.546309340385049d0 8.0d0 9.0d0 8.0d0 8.0d0 8.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 2.8796443504509193d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))) 


good, exactly the same result (and the ag and sr have 5s in them. (and also a 10% speedup))
and a huge simplification of the code




