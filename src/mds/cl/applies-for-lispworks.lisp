(in-package user)

;;;----------------------------------------------------------------------
;;;                      for lispworks
;;;           because it has a problem with long applies
;;;----------------------------------------------------------------------

(defun apply-min (l)
  ;; because lispworks will not apply on a long list
  (let (min)
    (loop for e in l do
	  (if (or (null min)
		  (< e min))
	      (setq min e))
	  finally (return min))))

(defun apply-max (l)
  ;; because lispworks will not apply on a long list
  (let (max)
    (loop for e in l do
	  (if (or (null max)
		  (> e max))
	      (setq max e))
	finally (return max))))

(defun apply-transpose (lists)
  ;;this is v. slow on a 3 lists of length 6000 (~30seconds)
  (if (not (apply #'equal-n (mapcar #'length lists)))
      (error "All lists are not equal length"))
  ;;(loop for i below (length (car lists)) collect (nths i lists)))
  ;;this mapcar is much faster than the loop above (because of the nths above)
  ;;on 2 lists of 6000 50ms vs 2000ms (on vaio) (i guess the timing above was old!
  (if (null lists)
      nil
    (apply #'mapcar (^ (&rest args) (apply #'list args)) lists)))

(defun apply-append (lists)
  (loop for list in lists append list))

(defun apply-+ (l)
  (loop for e in l sum e))

