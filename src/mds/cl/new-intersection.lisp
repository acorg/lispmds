(in-package user)

;;;--------------------------------------------------------------------------
;;;                             NEW RON TABLE
;;;--------------------------------------------------------------------------

(defun ron-table-term (d n k s i j)
  (let ((e (- (+ i s) (+ d d j)))
	(u (- (+ d j) s)))
    (* (nCr s d)
       (nCr (- s d) e)
       (nCr (- n s) u)
       (expt (- k 1) u)
       (expt (- k 2) e))))

(defun new-ron-table (n k s i j)
  (loop for d to s sum (ron-table-term d n k s i j)))

(defun new-ron-table-list (n k s i j)
  (loop for d to s collect (ron-table-term d n k s i j)))

(defun new-ron-table-list-and-dIs (n k s i j)
  (loop for d to s collect
	(let ((e (- (+ i s) (+ d d j)))
	      (u (- (+ d j) s)))
	  (list (ron-table-term d n k s i j)
		d			;dI
		(- s d e)		;dJ
		e			;dIdJ
		u			;dIJ
		(- n s u)		;sIJ
		))))

(defun full-ron-table (n k s f)
  (loop for i to n collect
	(loop for j to n collect
	      (funcall f n k s i j))))

;;;--------------------------------------------------------------------------
;;;                         MUTATION PROBABILITIES
;;;--------------------------------------------------------------------------

(defun mutation-probabilities (n k s i j)
  (let ((total-at-ij (new-ron-table n k s i j)))
    (apply 
     #'plus
     (loop for d to s collect
	   (let ((e (- (+ i s) (+ d d j)))
		 (u (- (+ d j) s)))
	     (map-all (^ (x)
			 (if (zerop total-at-ij)
			     0
			   (* (/ (ron-table-term d n k s i j) total-at-ij)
			      x)))
		      (map-all (^ (x) 
				  (float (/ x 
					    (* n (- k 1))
					    )))
			       (list (list d
					   (* d (- k 2))
					   (* (- n s u) (- k 1)))
				     (list e
					   (+ (* e (- k 3)) (* u (- k 2)))
					   (* (- s d e) (- k 2)))
				     (list u
					   e
					   (- s d e))))))))))