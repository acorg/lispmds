(in-package user)

(defun randomize-one-coords (coordss i amount)
  (replace-nth i (uniform-perturbs (nth i coordss) amount) (copy-list coordss)))

(defun multiple-optima-check-by-single-point-randomize (save &optional &key
								       (randomize-distance 20)
								       (num-randomizations 10)
								       (point-index-from 0)
								       (point-index-below (hi-table-length (table-from-save save))))
  ;; we need the stress, and we pick it up from the batch runs, maybe later figure the stress without needing batahruns
  (let ((table (table-from-save save))
	(original-coordss (starting-coords-from-save save))
	(original-stress (if (nth 1 (nth 0 (batch-runs-from-save save)))
                             (nth 1 (nth 0 (batch-runs-from-save save)))
                           (calc-stress-for-starting-coords-in-save save))))
    (loop for point-index from point-index-from below (min point-index-below (hi-table-length table)) collect
	  (let ((dist-stress-s
		 (time 
		  (loop for i below num-randomizations collect
			(let* ((perturbed-coordss (randomize-one-coords original-coordss point-index randomize-distance))
			       (batch-run 
				(batch-mds-keyword-args
				 table
				 :starting-coordss perturbed-coordss
				 :moveable-coords (list point-index))))
			  (list (e-dist (nth point-index original-coordss)
					(nth point-index (nth 0 batch-run)))
				(- (nth 1 batch-run) original-stress)
				(nth point-index original-coordss)
				(nth point-index (nth 0 batch-run))))))))
	    (let ((point-result
		   (list point-index 
			 (nth point-index (hi-table-antigens table))
			 (sort-nth 1 dist-stress-s)
			 )))
	      ;;(fi point-result "junk/tmp-mds-result" :append)
	      (print (list (nth 0 point-result)
			   (nth 1 point-result)
			   (mapcar (^ (l) (mapcar #'2dp l)) (mapcar #'butlast (mapcar #'butlast (nth 2 point-result))))))
	      point-result)))))


(defun multiple-optima-check-by-single-point-randomize-multiple-machines (save &optional &key
											 (randomize-distance 20)
											 (num-randomizations 10)
											 (num-points-per-batch-run 10)
											 gridware)
  (let* ((random-id (progn (seed-random -1 7) (krandom 467739585 7)))
	 (local-scratch-save-filename (format nil "mds/cl/batch-scratch/~a.save" random-id))
	 (remote-scratch-save-filename (format nil "mds/cl/batch-scratch/~a.save" random-id)))
    (write-save-form save local-scratch-save-filename)
    (if gridware (run-shell-command (format nil "scp -C ~a sfi:~a" local-scratch-save-filename remote-scratch-save-filename)))
    (cons 'list
	  (loop for i below (hi-table-length (table-from-save save)) by num-points-per-batch-run collect
		(batch-lisp `(multiple-optima-check-by-single-point-randomize
			      (fi-in ,remote-scratch-save-filename)
			      :randomize-distance ,randomize-distance
			      :num-randomizations ,num-randomizations
			      :point-index-from  ,i
			      :point-index-below ,(+ i num-points-per-batch-run))
			    :gridware gridware
			    )))))


#|
;; ----------------------- times 25 times on t9a ----------------------
;; about 6 seconds per randomization, so 1 min for 10 randomizations on one strain, about 6 hours for 10 each on each strain.
(multiple-optima-check-by-single-point-randomize-multiple-machines 
 seq-t9a-save
 :num-points-per-batch-run 12
 :num-randomizations 25
 :gridware t)

(fi (apply-append (eval *)) "mds/investigations/merge-hi-tables/seq-t9a-multiple-optima-check-by-single-point-randomize-raw-data.lisp")

(setq seq-t9a-multiple-optima-check (fi-in "mds/investigations/merge-hi-tables/seq-t9a-multiple-optima-check-by-single-point-randomize-raw-data.lisp"))
(setq seq-t9a-save (fi-in "mds/investigations/merge-hi-tables/seq-t9a.save"))
(setq table-asl (table-from-save seq-t9a-save))
(setq table (un-asl-hi-table (table-from-save seq-t9a-save)))

(setq num-connections-all-strains
  (append 
    (loop for hi-table-value-line in (hi-table-values table) collect
	  (length (collect #'numberp hi-table-value-line)))
    (loop for hi-table-value-line in (apply #'transpose (hi-table-values table)) collect
	  (length (collect #'numberp hi-table-value-line)))))

(setq possible-hemisphereing-strains
  (loop for (name nc dist-stress-list) in (mapcar (^ (ll num-connections name) 
						     (list name
							   num-connections
							   (loop for l in ll collect 
								 (let ((distance (nth 0 l))
								       (stress-delta (nth 1 l)))
								   (list distance
									 (/ stress-delta num-connections)
									 (if (nth 3 l) (nth 3 l)))))))
						  (nths 2 seq-t9a-multiple-optima-check)
						  num-connections-all-strains
						  (hi-table-antigens table-asl))
      when (collect (^ (dist-stress)
		       (and (> (nth 0 dist-stress) 1)
			    (< (nth 1 dist-stress) 1)))
		    dist-stress-list)
      collect (progn
		(newline)
		(ppl 
		 (list name nc (mapcar (^ (ds) (mapcar (^ (x) (if (integerp x) x (2dp x))) (flatten ds))) 
				       (collect 
					(^ (dist-stress)
					   (> (nth 0 dist-stress) 0.5))
					(sort-nth 1 dist-stress-list))))))))


(fll 
 (setq name-av-data
   (loop for (name num-connections data) in possible-hemisphereing-strains collect
	 (cons name (cons num-connections (mapcar #'2dp (mapcar #'av (apply #'transpose data))))))))

BI/15793/68-AG   4  3.39   0.28    5.27  -11.01
BI/808/69-AG     5  2.46   0.46    4.37  -10.33
BI/6449/71-AG    4  2.24   0.58    3.72  -13.35
GF/V728/85-AG    6  2.14   0.17    1.03   -0.41
SH/11/87-AG     11  5.58   0.43   -2.09    0.68
EN/427/88-AG     5  1.62   0.42     0.0    0.84
VI/1/88-AG      12  2.44   1.06    2.43    0.76
SP/36/89-AG      8  3.14   0.68   -0.89    2.94
WE/5/89-AG       7  3.12   0.31   -2.68    2.55
AS/1/91-AG       8  1.02  -0.06   -0.63    2.71
GE/5366/91-AG    8  2.16   0.36    0.03    0.57
FI/218/92-AG    13  4.85   2.29    -3.9    2.82
PE/1/92-AG       8  3.36   0.26    -1.9    2.56
SA/8/92-AG      12  6.26   3.56   -3.66    1.43
LY/1803/93-AG    5  0.98   0.08   -8.02    1.99
HK/2/94-AG      13  1.74    0.0   -8.51     3.3
JO/47/94-AG     12  1.58   0.55   -8.22    1.52
FI/381/95-AG    14  2.78   0.41   -7.99    7.39
GE/9509/95-AG    6   5.4    0.1   -7.32    1.71
GE/A9509/95-AG  17  5.06   0.75   -9.68    2.33
NL/47/95-AG     21  1.34   0.04   -7.92    7.19
SY/26/95-AG      7  5.21   0.72  -10.74    6.93
NL/91/96-AG     13  2.16   0.28   -7.44    4.55
HK/280/97-AG    10  2.74   0.25   -8.64    9.69
NE/491/97-AG    11  4.03  -0.14   -5.92    8.19
NL/427/98-AG     9  1.51   0.43   -6.08   12.78
MW/10/99-AG      8  1.19   0.13   -6.25   13.47
NL/3/00-AG       8   4.1    0.4   -4.71   11.08
NL/126/01-AG     6  1.24   0.16   -6.58   11.22
NL/1/02-AG      11  0.93   0.05   -7.54    9.78
SH/31/80-SR      4  1.33   0.04    3.65   -0.85
SY/5B/97-SR     18  1.02   0.38   -6.13   12.97



    t9a 25 iterations                       t9 10 iterations                  t9 10 iterations

name           num  dist stress
          connects        delta
BI/15793/68-AG   4  3.39   0.28       BI/15793/68-AG   4  3.33    0.2     BI/15793/68-AG   4  3.33    0.2
BI/808/69-AG     5  2.46   0.46       BI/808/69-AG     5  3.72  -0.01     BI/808/69-AG     5  3.72  -0.01     difference in t9a
                                      BI/17938/69-AG   6  1.12    0.4  	  BI/17938/69-AG   6  1.12    0.4
                                      BI/2668/70-AG    5   3.5    0.3  	  BI/2668/70-AG    5   3.5    0.3
BI/6449/71-AG    4  2.24   0.58       BI/6449/71-AG    4  4.74   0.15  	  BI/6449/71-AG    4  4.74   0.15     t9a larger hump
                                      BI/9459/74-AG    9  1.06   0.74  	  
                                      BI/5168/76-AG    5  0.86   0.01  	  BI/5168/76-AG    5  1.34   0.09     
GF/V728/85-AG    6  2.14   0.17       GF/V728/85-AG    6  2.21   0.04  	  GF/V728/85-AG    6  2.22   0.04     t9a larger hump
SH/11/87-AG     11  5.58   0.43   					  
EN/427/88-AG     5  1.62   0.42   					  
                                      OK/5/88-AG      13  5.22   1.62  	  
VI/1/88-AG      12  2.44   1.06       VI/1/88-AG      12  3.17   1.14  	                                      t9a smaller hump
                                      SP/34/89-AG      8  1.93   0.54  	  SP/34/89-AG      8  2.12   0.59
SP/36/89-AG      8  3.14   0.68       SP/36/89-AG      8  2.15   0.43  	  SP/36/89-AG      8  1.86   0.37     t9a larger hump
WE/5/89-AG       7  3.12   0.31       WE/5/89-AG       7  3.59   0.31  	  WE/5/89-AG       7  3.64   0.29
                                      WK/1/89-AG       9  1.57   0.62  	  WK/1/89-AG       9  1.59   0.62
AS/1/91-AG       8  1.02  -0.06                                                                               first very slight miss for t9a
GE/5366/91-AG    8  2.16   0.36       GE/5366/91-AG    8  2.01   0.02  	  GE/5366/91-AG    8  1.45   0.05     larger hump
FI/218/92-AG    13  4.85   2.29       FI/218/92-AG    13  1.01   0.49  	  
PE/1/92-AG       8  3.36   0.26       PE/1/92-AG       8  4.39   0.19  	  PE/1/92-AG       8  3.27   0.26     
SA/8/92-AG      12  6.26   3.56   
                                      LY/672/93-AG    15  2.27  -0.62  	  LY/672/93-AG    15  2.27  -0.63
LY/1803/93-AG    5  0.98   0.08       LY/1803/93-AG    5  1.74   0.09  	                                      tiny hump
HK/2/94-AG      13  1.74    0.0                                                                               zero hump
JO/47/94-AG     12  1.58   0.55       JO/47/94-AG     12  1.66   0.64  	  JO/47/94-AG     12  2.18   1.06     
FI/381/95-AG    14  2.78   0.41       FI/381/95-AG    14  3.22  -0.22  	  FI/381/95-AG    14  3.26  -0.22     t9a, not trapped
GE/9509/95-AG    6   5.4    0.1       GE/9509/95-AG    6  5.55   0.15  	  GE/9509/95-AG    6  5.23   0.19     t9a slightly closer to zero
GE/A9509/95-AG  17  5.06   0.75                                           GE/A9509/95-AG  17  5.05    0.9
NL/47/95-AG     21  1.34   0.04                                           NL/47/95-AG     21  1.77   0.23     t9a very close to zero
SY/26/95-AG      7  5.21   0.72  -    SY/26/95-AG      7  4.76   0.75     SY/26/95-AG      7  5.32   0.95     
NL/91/96-AG     13  2.16   0.28       NL/91/96-AG     13   2.5   0.05     NL/91/96-AG     13  2.54   0.05     t9a more gap
                                      SQ/1147/96-AG    5  7.68   0.48     SQ/1147/96-AG    5   7.7   0.63     this strain not in t9a
HK/280/97-AG    10  2.74   0.25       HK/280/97-AG    10  2.84   0.25     HK/280/97-AG    10  2.01    0.2     
NE/491/97-AG    11  4.03  -0.14       NE/491/97-AG    11  3.87   0.15     NE/491/97-AG    11  3.86   0.14     t9a missed this optimum
NL/427/98-AG     9  1.51   0.43                                           NL/427/98-AG     9  0.94   0.28
MW/10/99-AG      8  1.19   0.13       MW/10/99-AG      8  1.08   0.15           
NL/3/00-AG       8   4.1    0.4       NL/3/00-AG       8  4.02    0.4     NL/3/00-AG       8  3.97   0.38
NL/126/01-AG     6  1.24   0.16       NL/126/01-AG     6  1.11   0.04     NL/126/01-AG     6  1.32   0.07
NL/1/02-AG      11  0.93   0.05                                                                               close to zero (and this is sticking out)
SH/31/80-SR      4  1.33   0.04       SH/31/80-SR      4  1.38  -0.03     SH/31/80-SR      4  1.39  -0.03     close to zero
                                      SP/1/96-SR      20   2.4   0.52  	  SP/1/96-SR      20  2.42   0.52
SY/5B/97-SR     18  1.02   0.38       SY/5B/97-SR     18  2.42   0.35  	  SY/5B/97-SR     18  2.42   0.35




(fll 
 (sort-nth 
  3
  (setq name-av-data
    (loop for (name num-connections data) in possible-hemisphereing-strains collect
	  (cons name (cons num-connections (mapcar #'2dp (mapcar #'av (apply #'transpose data)))))))))

same as above, sort by stress

NE/491/97-AG    11  4.03  -0.14   -5.92    8.19    ;; also detected by blobs with force 0.5
AS/1/91-AG       8  1.02  -0.06   -0.63    2.71    ;; also detected by blobs with force 0.5
HK/2/94-AG      13  1.74    0.0   -8.51     3.3    ;; also detected by blobs with force 0.5
NL/47/95-AG     21  1.34   0.04   -7.92    7.19    ;; also detected by blobs with force 1.0
SH/31/80-SR      4  1.33   0.04    3.65   -0.85    ;; also detected by blobs with force 0.5
NL/1/02-AG      11  0.93   0.05   -7.54    9.78
LY/1803/93-AG    5  0.98   0.08   -8.02    1.99
GE/9509/95-AG    6   5.4    0.1   -7.32    1.71    ;; also detected by blobs with force 0.5  ;; also large distance
                                                                              ;; <<<<<<<<<<<<<<<<<<< gap here, maybe delete all above--just too close?
MW/10/99-AG      8  1.19   0.13   -6.25   13.47
NL/126/01-AG     6  1.24   0.16   -6.58   11.22    ;; also detected by blobs with force 1.0
GF/V728/85-AG    6  2.14   0.17    1.03   -0.41
HK/280/97-AG    10  2.74   0.25   -8.64    9.69
PE/1/92-AG       8  3.36   0.26    -1.9    2.56
BI/15793/68-AG   4  3.39   0.28    5.27  -11.01
NL/91/96-AG     13  2.16   0.28   -7.44    4.55
WE/5/89-AG       7  3.12   0.31   -2.68    2.55
GE/5366/91-AG    8  2.16   0.36    0.03    0.57
SY/5B/97-SR     18  1.02   0.38   -6.13   12.97
NL/3/00-AG       8   4.1    0.4   -4.71   11.08
FI/381/95-AG    14  2.78   0.41   -7.99    7.39
EN/427/88-AG     5  1.62   0.42     0.0    0.84
SH/11/87-AG     11  5.58   0.43   -2.09    0.68
NL/427/98-AG     9  1.51   0.43   -6.08   12.78
BI/808/69-AG     5  2.46   0.46    4.37  -10.33
JO/47/94-AG     12  1.58   0.55   -8.22    1.52
BI/6449/71-AG    4  2.24   0.58    3.72  -13.35
SP/36/89-AG      8  3.14   0.68   -0.89    2.94
SY/26/95-AG      7  5.21   0.72  -10.74    6.93
GE/A9509/95-AG  17  5.06   0.75   -9.68    2.33
VI/1/88-AG      12  2.44   1.06    2.43    0.76
FI/218/92-AG    13  4.85   2.29    -3.9    2.82
SA/8/92-AG      12  6.26   3.56   -3.66    1.43




(write-save-form
 (make-save-form
  :hi-table table-asl
  :starting-coordss 
  (make-coordss-plus-more
   (loop for coords in (starting-coords-from-save seq-t9a-save)
       for strain in (hi-table-antigens table-asl) collect
	 (if (assoc strain name-av-data)
	     (lastn 2 (assoc strain name-av-data))
	   coords))
   (col-bases (starting-coords-from-save seq-t9a-save))
   (row-adjusts (starting-coords-from-save seq-t9a-save))))
 "mds/investigations/validation/seq-t9a-hemisphering-strains.save")

(let ((name-av-data (firstn 8 (sort-nth 3 name-av-data))))
  (write-save-form
   (make-save-form
    :hi-table table-asl
    :starting-coordss 
    (make-coordss-plus-more
     (loop for coords in (starting-coords-from-save seq-t9a-save)
	 for strain in (hi-table-antigens table-asl) collect
	   (if (assoc strain name-av-data)
	       (lastn 2 (assoc strain name-av-data))
	     coords))
     (col-bases (starting-coords-from-save seq-t9a-save))
     (row-adjusts (starting-coords-from-save seq-t9a-save))))
   "mds/investigations/validation/seq-t9a-mostly-likely-hemisphering-strains.save"))

(eval (blank-save seq-t9a-save))


<IMG src=seq-t9a-multiple-optima-by-randomization-01.gif alt=seq-t9a-multiple-optima-by-randomization-01.gif>  <IMG src=seq-t9a-multiple-optima-by-randomization.gif alt=seq-t9a-multiple-optima-by-randomization.gif>

Left to right:
  

<HR>
|#


