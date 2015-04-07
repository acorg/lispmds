(in-package user)

(defvar *poisson-inverse-gaussian-noise*)
;; this barfs in cmucl on solaris (not on linux), we need make-poisson-cdf-inverse-long, but not called anyhow
;; so comment out for now
#-:cmu   
(setq *poisson-inverse-gaussian-noise*
  (make-poisson-cdf-inverse 
   100
   (- 1 (expt 10 -7))))

;;this is for linear noise
;;(defun add-gaussian-noise (value amount)
;;  ;;not guassian right now!, but uniform
;;  (+ value
;;     (- (* amount 
;;	   (knuth-random *cell-plot-generator*))
;;	(/ amount 2))))

;; this function is very poor.  too much of a step, and is not symmetric.
;; make a better cdf, maybe using the normal rnorm below, or the numerical cdf algorithm (well explained online), or table lookup
(defun gaussian-noise (&optional generator)
  ;;returns number in {-1,1} (can be more or less but this is ~3sds)
  ;;this is not perfect, because biased low
  ;;I should really calc from the normal formula, then i know the sd's etc but OK for now
  (* 3.3
     (- (/ (random-from-inverse-cdf 
	    *poisson-inverse-gaussian-noise* generator)
	   100)
	1)))

(defun add-gaussian-noise (value amount &optional generator)
  (+ value
     (* amount
	(gaussian-noise generator))))


;;;----------------------------------------------------------------------
;;;                   spherical 2d gaussian noise
;;;----------------------------------------------------------------------

(defun sperhical-gaussian-noise-2d (&optional generator)
  (let ((r (gaussian-noise generator))            ;; positive and negative
	(theta (* pi (knuth-random generator))))  ;; 0 to 180 degrees because r is positive and negative
    (polar-to-rectangular r theta)))
		
(defun add-sperhical-gaussian-noise-2d (xy amount &optional generator)
  (let ((noise (sperhical-gaussian-noise-2d generator)))
    (list
     (+ (nth 0 xy) (* amount (nth 0 noise)))
     (+ (nth 1 xy) (* amount (nth 1 noise))))))
	 
;;(gnuplot-correlation (loop for i below 1000 collect (add-sperhical-gaussian-noise-2d '(1 2) 0.5)))


;;;----------------------------------------------------------------------
;;;  later (Nov 2005) add the correct calculation of normal (from R)
;;;----------------------------------------------------------------------

(defun rnorm (x &optional &key (mean 0) (sd 1))
  ;;     The normal distribution has density
  ;;
  ;;       f(x) = 1/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))
  (let ((xx (knuth-random)))
    (* (/ 1 (* (sqrt (* 2 pi)) sd))
       (exp (- (/ (square (- x mean)) (* 2 (square sd))))))))

