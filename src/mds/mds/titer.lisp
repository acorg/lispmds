(in-package user)

;;;---------------------------------------------------------------
;;;                             MISC
;;;---------------------------------------------------------------

(defun hypotenuse-length-from-coords (l1 l2)
  (e-dist l1 l2))

(defun hypotenuse-length-from-lengths (&rest lengths)
  (hypotenuse-length-from-coords (loop for i below (length lengths) collect 0) lengths))

(defun add-coords (l1 l2)
  (loop for e1 in l1 for e2 in l2 collect (+ e1 e2)))

(defun subtract-coords (l1 l2)
  (add-coords l1 (mapcar #'- l2)))

(defun generate-random-2d-point (center radius)
  (add-coords 
   center
   (polar-to-rectangular radius (random-in-range 0 (* pi 2)))))

(defun plot-serum (antiserum &key (refresh t))
  (g-plot antiserum
          :style 'scatter :x-min -20 :y-min -20 :x-max 20 :y-max 20
          :refresh refresh))

(defun plot-sera (antisera)
  (plot-serum (car antisera))
  (loop for antiserum in (cdr antisera) do (plot-serum antiserum :refresh nil)))


;;;---------------------------------------------------------------
;;;                      GENERATING ANTISERA
;;;---------------------------------------------------------------

(defun gen-antisera (center nums-at-radii)
  (loop for (num radius) in nums-at-radii append
        (loop for i below num collect
              (generate-random-2d-point center radius))))

(defvar *nums-at-radiii*)
(setq *nums-at-radii* '((0 0) (0 1) (0 2) (1 3) (8 4) (64 5) (512 6) (2048 7)))

#|
(setq farris (gen-antisera '(0 0) (firstn 5 *nums-at-radii*)))
(on-ability farris '(1 1) 4)
0.12264366306656645
? (on-ability farris '(0 0) 4)
0.018000000000000016
;;woops, i see i need to generate points randomly in the space
|#

;;;---------------------------------------------------------------
;;;                           TITER
;;;---------------------------------------------------------------

(defun on-ability (points reference-point reference-antigenicity)
  ;;this is the "volume"
  ;;or when thresholded and coarsed is the titer
  (loop for point in points sum
        (min 1.0
             (expt 10 (- reference-antigenicity
                         (hypotenuse-length-from-coords reference-point point)
                         3)))))

(defun titer (antigen sera antigenicity &key (required-num-bound 0.5))
  (max 0
       (expt 2.0
	     (ceiling (/ (log (on-ability sera antigen antigenicity) 2.0)
			 required-num-bound)))))

#|
(setq farris (gen-antisera '(0 0) (firstn 5 *nums-at-radii*)))
|#

;;;---------------------------------------------------------------
;;;                 GENERATING SERA RANDOMLY 
;;;---------------------------------------------------------------

(defun generate-point (center radius)
  (loop for i below 10000000000 do
        (let ((x (random-in-range (- radius) radius))
              (y (random-in-range (- radius) radius)))
          (if (<= (hypotenuse-length-from-lengths x y) radius)
            (return (add-coords center (list x y)))))
        finally (error "point generation num trials exceeded")))

(defun generate-points (num-points center radius)
  (loop for i below num-points collect
        (generate-point center radius)))

;;(g-plot (generate-points 1000 '(0 0) 5) :style 'dot :x-min -5 :y-min -5 :x-max 5 :y-max 5)
;;we see from this that we do indeed generate within a cirle


;;;----------------------------------------------------------------
;;;                GENERATE ANTISERA TAKE 2
;;;----------------------------------------------------------------
            
(defun gen-antisera (num-ab center radius)
  (generate-points num-ab center radius))

(defun gen-antisera-uniform-density (center radius &key (density 1))
  (gen-antisera (* density (* pi (square radius))) center radius))

(defun make-ag-sr-set (name antigen antigen-antigenicity 
		       &key (serum-density 1) (standard-antigenicity 5) 
			    (standard-num-antigens 1.0) (distance-affinity-exponent 10)
			    (serum-generation-f #'gen-antisera-uniform-density))
  (list name
	antigen
	(* standard-num-antigens (expt distance-affinity-exponent (- standard-antigenicity antigen-antigenicity)))
	antigen-antigenicity
	(funcall serum-generation-f antigen antigen-antigenicity :density serum-density)))


;;;----------------------------------------------------------------------
;;;                             PLAYING
;;;----------------------------------------------------------------------
#|
(g-plot (generate-points 1000 '(5 10) 5) :style 'dot :x-min -20 :y-min -20 :x-max 20 :y-max 20)

(setq farris-sera 
      (gen-antisera-uniform-density (setq farris-antigen '(0 0)) (setq farris-antigenicity 4)))
(setq logan-sera 
      (gen-antisera-uniform-density (setq logan-antigen '(5 0)) (setq logan-antigenicity 4)))
(setq zimmerman-sera 
      (gen-antisera-uniform-density (setq zimmerman-antigen '(7 7)) (setq zimmerman-antigenicity 6)))

(progn
  (plot-sera farris-sera)
  (plot-sera logan-sera :refresh nil)
  (plot-sera zimmerman-sera :refresh nil))

? (titer logan-antigen logan-sera logan-antigenicity)
128
? (titer farris-antigen farris-sera farris-antigenicity)
128
? (titer zimmerman-antigen zimmerman-sera zimmerman-antigenicity)
2048
|#

;;replaced below
(defun make-faux-hi-table (antigen-names antisera-names antigens antisera antigenicities)
  (make-hi-table
   antigen-names
   antisera-names
   (loop for antigen in antigens for antigenicity in antigenicities collect
         (loop for antiserum in antisera collect
               (titer antigen antiserum antigenicity)))))

#|
(make-faux-hi-table
 '(farris logan zimmerman)  '(farris logan zimmerman) 
 (list farris-antigen logan-antigen zimmerman-antigen)
 (list farris-sera logan-sera zimmerman-sera)
 (list farris-antigenicity logan-antigenicity zimmerman-antigenicity))

FARRIS                     128    1    0
LOGAN                        0  128    0
ZIMMERMAN                    0    1 2048


-----------------now more realistic data

(progn
(setq farris-sera    (gen-antisera-uniform-density (setq farris-antigen    '(0 0)) (setq farris-antigenicity    4)))
(setq logan-sera     (gen-antisera-uniform-density (setq logan-antigen     '(2 0)) (setq logan-antigenicity     4)))
(setq zimmerman-sera (gen-antisera-uniform-density (setq zimmerman-antigen '(4 0)) (setq zimmerman-antigenicity 5)))

(progn
  (plot-sera farris-sera)
  (plot-sera logan-sera :refresh nil)
  (plot-sera zimmerman-sera :refresh nil))

(setq hi-table (make-faux-hi-table
                '(farris logan zimmerman)  '(farris logan zimmerman) 
                (list farris-antigen logan-antigen zimmerman-antigen)
                (list farris-sera logan-sera zimmerman-sera)
                (list farris-antigenicity logan-antigenicity zimmerman-antigenicity)))

(pp-hi-table hi-table)
)
FARRIS                      32  256   32
LOGAN                       64   64  128
ZIMMERMAN                  128  256  512

and again
FARRIS                     128   64   32
LOGAN                       64   32   16
ZIMMERMAN                   32  256  512

;;;now increase density by 10
(progn
(setq farris-sera    (gen-antisera-uniform-density (setq farris-antigen    '(0 0)) (setq farris-antigenicity    4) :density 10))
(setq logan-sera     (gen-antisera-uniform-density (setq logan-antigen     '(2 0)) (setq logan-antigenicity     4) :density 10))
(setq zimmerman-sera (gen-antisera-uniform-density (setq zimmerman-antigen '(4 0)) (setq zimmerman-antigenicity 5) :density 10))

(progn
  (plot-sera farris-sera)
  (plot-sera logan-sera :refresh nil)
  (plot-sera zimmerman-sera :refresh nil))

(setq hi-table (make-faux-hi-table
                '(farris logan zimmerman)  '(farris logan zimmerman) 
                (list farris-antigen logan-antigen zimmerman-antigen)
                (list farris-sera logan-sera zimmerman-sera)
                (list farris-antigenicity logan-antigenicity zimmerman-antigenicity)))

(pp-hi-table hi-table)
)

FARRIS       4096  8192  4096
LOGAN        4096  4096  8192
ZIMMERMAN    8192 32768 65536

;;i made proportion bound to num-bound (and value changed from 0.5 to 10
;;and make titer /10 and * 10.0 (should have been at least 10 not 10.0)
;;but some other problem
;;also i don't understand density because dont get the multiplication of table 
above
;;with the ones above that
FARRIS                    20.0 20.0 20.0
LOGAN                     20.0 20.0 20.0
ZIMMERMAN                 20.0 20.0 20.0


|#

#|
where next
 - initially divide titers by 10, so have 10x the Ab, and multiply by 10 in 
the end (so CDC numbers)
 - set up some "real" data
 - set up for showing lapedes
 - also do in other num dims, and other metrics
 - is this assymetry pattern what we see?
    (eg if higher homo, then assymetric with greater in lower triangle, and 
greater in 
        general across the row, and assymetry greater when ags are closer)
 - can i get non homo higher in column
 - ah, nice, I can also have a "zimmerman-4" -- same place, different 
antigenicity
 - ahh, nice, i also don't believe a 2 fold! (eg farris and logan ag in logan 
sera)

-also show lapedes the UI (check not stepping on toes)
-and have my HI order

- how does all this tell me what to do with MDS?

|#



;;;---------------------------------------------------------------
;;;                   TITER TAKE 2, ADD IN NUM AG
;;;---------------------------------------------------------------

(defun on-ability (antigen serum antigen-antigenicity distance-affinity-exponent &key (metric #'e-dist))
  ;;this is the expected number of antigens of this antigenicicy that would be bound by this serum
  (loop for antibody in serum sum
        (min 1.0
             (expt distance-affinity-exponent
		   (- antigen-antigenicity
		      (funcall metric antigen antibody)
		      3)))))

(defun titer (antigen-coord num-antigens serum antigen-antigenicity 
	      &key (proportion-antigens-bound-threshold 0.5) (distance-affinity-exponent 10) actual-values
		   (metric #'e-dist))
  (let* ((num-antigens-serum-could-bind (on-ability antigen-coord serum antigen-antigenicity distance-affinity-exponent
						    :metric metric))
	 (serum-power (/ (/ num-antigens-serum-could-bind num-antigens)
			 proportion-antigens-bound-threshold)))
    (if actual-values
	serum-power
      (HA-titerize serum-power))))

(defun HA-titerize (serum-power)
  (round (max 0 (* 10 (expt 2.0 (ceiling (log (/ (progn (print (list 'actual serum-power)) serum-power) 10) 2)))))))



;;;----------------------------------------------------------------------
;;;                      MAKING HI TABLES TAKE 2
;;;----------------------------------------------------------------------

(defun make-faux-hi-table (ag-sr-sets &rest titer-keyword-args)
  (let ((antigens (nths 0 ag-sr-sets))
	(antisera (nths 0 ag-sr-sets)))
    (make-hi-table
     antigens
     antisera
     (loop for (ig0 ag-coords num-ags ag-antigenicity) in ag-sr-sets collect
	   (loop for (ig0 ig1 ig2 ig3 antiserum) in ag-sr-sets collect
		 (apply #'titer ag-coords num-ags antiserum ag-antigenicity titer-keyword-args))))))
		      
#|
(setq ag-sr-sets
  (loop for args in '((farris    (0 0) 4 :serum-density 10)
		      (logan     (5 0) 4 :serum-density 10)
		      (zimmerman (5 5) 6 :serum-density 10))
      collect (apply #'make-ag-sr-set args)))

(plot-sera (nths 4 ag-sr-sets))

(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t))

RRIS GAAN MMAN
FARRIS                      13    1    2
LOGAN                        1   15   13
ZIMMERMAN                  190 1831 7774


(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 2))

                          RRIS GAAN MMAN
FARRIS                      38   10   13
LOGAN                       11   39   32
ZIMMERMAN                 1233 3201 12208

(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 1.1))
                          RRIS GAAN MMAN
FARRIS                      86   67  125
LOGAN                       67   86  146
ZIMMERMAN                 6812 7947 20216

|#


;;;----------------------------------------------------------------------
;;;                      GENERATE SERA FROM MODEL
;;;----------------------------------------------------------------------

(defun gen-sera-from-model (antigen)
  ;;load "classes-no-graphics"
  (init)
  (inject-ag 1 'dead antigen)
  (mapcar #'receptor (b-cells idvl)))

(defun sample-num-at-r (r &optional (density 1))
  (let ((hash-index (list *receptor-cardinality*
			  *receptor-length*
			  r
			  density)))
    (if (null (gethash hash-index *poisson-inverse-cdf-tables*))
	(setf (gethash hash-index *poisson-inverse-cdf-tables*)
	  (let ((mu (* density (nth r *expected-num-at-dists*))))
	    (make-poisson-cdf-inverse mu *poisson-inverse-cdf-probability-limit*))))
    (random-from-inverse-cdf (gethash hash-index *poisson-inverse-cdf-tables*))))

(defun gen-serum-from-model-abstractly (antigen antigenicity &key density)
  (loop for r to antigenicity append
	(loop for j below (sample-num-at-r r density) collect
	      (mutate-exactly-n r antigen *receptor-cardinality*))))

#|
(setq ag-sr-sets
  (loop for args in (list (list 'farris '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				5 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000)
			  (list 'logan  '(1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				5 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000)
			  (list 'zimmerman '(1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				5 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000))			
      collect (apply #'make-ag-sr-set args)))

(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 10))
                          RRIS GAAN MMAN
FARRIS                      42   24   17
LOGAN                       36   46   30
ZIMMERMAN                   27   45   65
(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 2))
                          RRIS GAAN MMAN
FARRIS                     172  161  129
LOGAN                      164  201  160
ZIMMERMAN                  157  190  203
(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 1.1))
                          RRIS GAAN MMAN
FARRIS                     484  491  461
LOGAN                      481  506  474
ZIMMERMAN                  479  501  489

;;---------now with different antigenicities
(setq ag-sr-sets
  (loop for args in (list (list 'farris '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				5 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000)
			  (list 'logan  '(1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				5 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000)
			  (list 'zimmerman '(1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				6 :serum-generation-f #'gen-serum-from-model-abstractly :serum-density 1000))			
      collect (apply #'make-ag-sr-set args)))

(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 10))
                          RRIS GAAN MMAN
FARRIS                      39   18   23
LOGAN                       28   42   42
ZIMMERMAN                 1298 1232 2479
(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 2))
                          RRIS GAAN MMAN
FARRIS                     177  136  161
LOGAN                      164  170  197
ZIMMERMAN                 2951 2876 4227
(pp-hi-table (make-faux-hi-table ag-sr-sets :actual-values t :distance-affinity-exponent 1.1))
                          RRIS GAAN MMAN
FARRIS                     500  466  573
LOGAN                      496  479  588
ZIMMERMAN                 5408 5196 6599
|#
