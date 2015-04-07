(in-package user)

(defun chi-squared-term (observed expected)
  (/ (square (- observed expected))
     expected))

(defun chi-squared (observations mu lower upper)
  (let ((bins (sort-hist observations (^ (a b) (< (car a) (car b))) :fill t))
	(min (apply #'min observations))
	(max (apply #'max observations))
	(n (length observations)))
    (+ (chi-squared-term (loop for bin from min to lower sum (assoc-value-1 bin bins))
			 (loop for bin from min to lower sum (* n (poisson-term-long mu bin))))
       (loop for bin from (inc lower) to (dec upper) sum
	     (chi-squared-term (assoc-value-1 bin bins)
			       (* n (poisson-term-long mu bin))))
       (chi-squared-term (loop for bin from upper to max sum (assoc-value-1 bin bins))
			 (loop for bin from upper to max sum (* n (poisson-term-long mu bin)))))))
	     

#|
(setq mu (nth 5 *expected-num-at-dists*))
(load "~/im/lazy-repertoire-data/lazy-repertiore-test-s-10000.lisp")
(setq @5s (loop for i below 10 collect (nths 5 (nths i s-10000))))

(mapcar (^ (x) (chi-squared x mu 25 45)) @5s)   
(setq 10k-20dof '(17.491 19.953 11.572 20.496 17.932 25.333 14.641 15.395 12.568 10.374))
;;chi-squared for these 20 degrees of freedom for alpha=0.05 is 37.652

(mapcar (^ (x) (chi-squared x mu 20 50)) @5s)
(setq 10k-30dof '(23.134 29.286 31.924 43.785 25.842 30.934 27.058 18.378 19.085 19.956)) 
;;chi-squared for these 30 degrees of freedom for alpha=0.05 is 43.773

(load "~/im/lazy-repertoire-data/lazy-repertiore-test-s-100000.lisp")
|#

(defun stream-read (filename &optional filter)
  (with-open-file (in filename)
    (let (read)
      (loop for i from 0 until (eql 'eof (setq read (read in nil 'eof)))
	  collect (progn
		    (if (zerop (mod i 1000))
			(print i))
		    (if filter
		      (funcall filter read)
		    read))))))

#|
(setq mu (nths 5 *num-at-dists*))
(setq foo (stream-read "~/im/lazy-repertoire-data/lazy-repertiore-test-s-100000.lisp" (^ (x) (nths 5 x))))
(setq @5s (loop for i below (length (car foo)) collect (nths i foo)))

(mapcar (^ (x) (chi-squared x mu 25 45)) @5s)   
(setq 100k-20dof '(25.227 14.726 9.055 17.222 23.626 13.976 15.258 24.541 14.474 36.673))
;;chi-squared for these 20 degrees of freedom for alpha=0.05 is 37.652

(mapcar (^ (x) (chi-squared x mu 20 50)) @5s)
(setq 100k-30dof '(31.555 24.441 14.64 25.623 32.658 24.182 25.686 31.697 20.081 45.015))
;;chi-squared for these 30 degrees of freedom for alpha=0.05 is 43.773

(mapcar (^ (x) (chi-squared x mu 15 55)) @5s)
(setq 100k-40dof '(39.468 44.451 31.719 31.736 45.252 39.947 35.958 48.346 28.221 54.58))
;;chi-squared for these 40 degrees of freedom for alpha=0.05 is ?


now, on the first 10k of the 100k
(mapcar (^ (x) (chi-squared (firstn 10000 x) mu 25 45)) @5s)   
(setq 10k-of-100k-20dof '(22.807 15.406 14.386 31.713 15.583 14.531 24.849 26.551 28.339 29.556))
;;chi-squared for these 20 degrees of freedom for alpha=0.05 is 37.652

(mapcar (^ (x) (chi-squared (firstn 10000 x) mu 20 50)) @5s)
(setq 10k-of-100k-30dof '(31.975 21.718 27.991 41.884 26.467 25.554 37.952 35.962 44.451 33.521))
;;chi-squared for these 30 degrees of freedom for alpha=0.05 is 43.773

(loop for data-set in (list (list 10k-20dof 10k-30dof)
			    (list 10k-of-100k-20dof 10k-of-100k-30dof)
			    (list 100k-20dof 100k-30dof 100k-40dof)) do
      (loop for data in data-set do
	    (format t "~{ ~7f~}~%" data))
      (format t "~%"))

(g-plots (list 10k-20dof 10k-30dof) 
	 :x-title "Antigen" :y-title "Chi Squared" :latex t :x-min 0)
(g-plots (list 10k-of-100k-20dof 10k-of-100k-30dof) 
	 :x-title "Antigen" :y-title "Chi Squared" :latex t :x-min 0)
(g-plots (list 100k-20dof 100k-30dof 100k-40dof)
	 :x-title "Antigen" :y-title "Chi Squared" :latex t :x-min 0)

(g-plots (list 10k-20dof 10k-of-100k-20dof 100k-20dof) 
	 :x-title "Antigen" :y-title "Chi Squared" :latex t :x-min 0)
(g-plots (list 10k-30dof 10k-of-100k-30dof 100k-30dof) 
	 :x-title "Antigen" :y-title "Chi Squared" :latex t :x-min 0)
|#