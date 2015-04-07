(in-package user)

(defun ron-table-entry (sep i j n k)
  (let ((sames (/ (- j (- sep i)) 2))
	(diffs (/ (+ j (- sep i)) 2)))
    (loop for a from (floor sames) downto 0
	for b from (floor diffs) downto 0
	for c from (- (ceiling diffs) (floor diffs)) to n by 2
	when (and (<= a (- n sep))
		  (<= (+ b c) sep))
	sum (* (nCr (- n sep) a)
	       (expt (- k 1) a)
	       (nM sep b c (- sep (+ b c)))
	       (expt (- k 2) c)))))

(defun intersection-area (r0 r1 sep n k)
  (loop for i to r0 sum
	(loop for j to r1 sum
	      (ron-table-entry sep i j n k))))

(defun intersection-expanded (r0 r1 sep n k)
  (loop for i to r0 collect
	(loop for j to r1 collect
	      (ron-table-entry sep i j n k))))

(defun ron-table (sep n k &optional (max-dist (min n (max 10 (/ n 2)))))
  (loop for i to max-dist collect
	(loop for j to max-dist collect
	      (ron-table-entry sep i j n k))))

(defun pp-ron-table (ron-table)
  (format nil "~{~%~{ ~5d~} ~}" ron-table))


(defun num-at-dist (d n k)
  (* (nCr n d) (expt (- k 1) d)))

(defun area-of-sphere (r n k)
  (loop for i to r sum
	(num-at-dist i n k)))

;;;-------------------------------------------------------------------------
;;;                      PROPORTION IN INTERSECTION
;;;-------------------------------------------------------------------------

(defun proportion-in-intersection (ring n k sep memory-radius)
  (/ (loop for i to memory-radius 
	 sum (ron-table-entry sep ring i n k))
     (float (num-at-dist ring n k))))

(defun proportions-in-intersection (n k sep prime-radius memory-radius)
  (loop for i to prime-radius collect
	(proportion-in-intersection i n k sep memory-radius)))

(defun all-proportions-in-intersection (n k prime-radius memory-radius)
  (loop for i to (+ prime-radius memory-radius) collect
	(proportions-in-intersection n k i prime-radius memory-radius)))

'(fi-tk-list (lisp->tk (all-proportions-in-intersection 20 4 5 6))
	    "~/im/intersection-proportions-4-20")


#|
(g-plot (loop for sep from 0 to 20 collect (intersection-area 5 5 sep 20 4)))

(defun area-of-sphere-2 (r n k)
  (loop for i from 0 to r sum
      (loop for j from 0 to r sum
	    (ron-table-entry 0 i j n k))))

(pp-ron-table (ron-table 3 20 4))

(defun ron-table-entry-diags (sep i j n k)
  (let ((r (/ (- j (- sep i)) 2))
	(l (/ (+ j (- sep i)) 2)))
    (if (and (integerp r)
	     (integerp l))
	(* (nCr (- n sep) r)
	   (expt (- k 1) r)
	   (nCr sep l))
      0)))

(defun ron-table-diags (sep n k)
  (loop for i below *max-dist* collect
	(loop for j below *max-dist* collect
	      (ron-table-entry-diags sep i j n k))))

(pp-ron-table (ron-table-diags 3 20 2))

(defun sub-tables (a b)
  (loop for aa in a for bb in b collect
	(mapcar #'- aa bb)))

(pp-ron-table (sub-tables (ron-table 3 20 4) (ron-table-diags 3 20 4)))

(defun binomial-series-expt (n p k)
  (mapcar (^ (e) (round (* (expt 2 n) e))) (binomial-pd-expt n p k)))

(defun binomial-pd-expt (n p k)
  (loop for i from 0 to n collect (* (expt (dec k) i) (binomial-term n p i))))

(binomial-series-expt 17 .5 4)
(binomial-series 3 .5)

(defun cross (f a b)
  (loop for aa in a collect
	(loop for bb in b collect
	      (funcall f aa bb))))

(cross #'* 
       (binomial-series-expt 17 .5 4)
       (binomial-series 3 .5))



(defun ron-table-entry (sep i j n k)
  (let ((sames (/ (- j (- sep i)) 2))
	(diffs (/ (+ j (- sep i)) 2)))
    (loop for a from (floor sames) downto 0
	for b from (floor diffs) downto 0
	for c from (- (ceiling diffs) (floor diffs)) to n by 2
	when (and (<= a (- n sep))
		  (<= (+ b c) sep))
	collect (list a b c))))

(defun hdb (a b)
  (logcount (logxor a b)))

(defun inter (a b dims)
  (loop for i below (expt 2 dims) 
      when (and (= 0 (hdb i a))
		(= 2 (hdb i b)))
      sum 1))

(defun make-all-points ()
  (let (all-points)
    (loop for i below 4 do
	  (loop for j below 4 do
		(loop for k below 4 do
		      (loop for l below 4 do
			    (loop for m below 4 do
				  (push (list i j k l m) all-points))))))
    all-points))

(defvar *all-points*)
(setq *all-points* (make-all-points))

(defun inter (a b x y)
  (loop for p in *all-points*
      when (and (= x (distance p a))
		(= y (distance p b)))
      sum 1))

(defun ron-table-actual (a b)
  (loop for i below *max-dist* collect
	(loop for j below *max-dist* collect
	      (inter a b i j))))

(equal (ron-table-actual '(0 0 0 0 0) '(0 0 0 1 1)) (ron-table 2 5 4))
|#




#|
(defun Rij (n s i j)
  (ron-table-entry s i j n 2))

(defun av-fitness (n s)
  (/ (loop for i to n sum
	(loop for j to n sum
	      (* (min i j)
		 (Rij n s i j))))
     (float (expt 2 n))))

USER(28): (loop for i to 10 sum (* i (binomial-term 10 .5 i)))
5.0
USER(29): (loop for i to 10 sum (* (min i (- 10 i)) (binomial-term 10 .5 i)))
3.7695312
USER(30): (av-fitness 10 10)
3.7695312

so, the best for n=2 is max separation
|#