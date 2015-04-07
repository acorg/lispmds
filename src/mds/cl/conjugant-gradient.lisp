(in-package user)

(defvar NUM_POINTS)
(defvar NUM_DIMS)

(defun conjugant-gradient-iterations (l perturbation-f fitness-f   ;; perturbation-f is in this case a list of moveable coords
				      &key (max-trials 100)
					   (max-climbs 100)
					   (acceptance-f #'>=)
					   optimum-acceptance-f
					   initial-dribble-f
					   incremental-dribble-f
					   (incremental-dribble-modulus 1))  ;;1 is every trial, 20 is every 20th
  ;;horrible binding of dynamically scoped variables for func and dfunc
  (setq NUM_POINTS (length (deconstruct-coordss-plus-more l)))
  (setq NUM_DIMS (length (car (deconstruct-coordss-plus-more l))))

  (let* ((p (coordss-to-array l))
	 (num-dims (length (car l)))
	 (num-points (length (deconstruct-coordss-plus-more l)))
	 (n (length p))
	 (ftol 1.0e-4)
	 (iter 0)
	 fret
	 (func fitness-f)
	 (dfunc perturbation-f)
	 (itmax max-climbs) 
	 (eps 1.0E-10))
  ;;(defun frprmn (p n ftol iter fret func dfunc &key (itmax 200) (eps 1.0E-10))
  ;; iter and fret are returns
  ;;(declare (type (simple-array double-float (*)) p)) (declare (type fixnum n))
  ;;(declare (type double-float ftol)) (declare (type fixnum iter))
  ;;(declare (type double-float fret)) (declare (type fixnum nmax))
  ;;(declare (type fixnum itmax)) (declare (type double-float eps))
  (prog
      ((g (make-array (list n) :element-type 'double-float))
       (h (make-array (list n) :element-type 'double-float))
       (xi (make-array (list n) :element-type 'double-float)) 
       (gam 0.0d0) (dgg 0.0d0) (gg 0.0d0) (j 0) (fp 0.0d0)
       )
    ;;(declare (type (simple-array double-float (*)) g))
    ;;(declare (type (simple-array double-float (*)) h))
    ;;(declare (type (simple-array double-float (*)) xi))
    ;;(declare (type double-float gam)) (declare (type double-float dgg))
    ;;(declare (type double-float gg)) (declare (type fixnum j))
    ;;(declare (type double-float fp)) 
    (setf fp (funcall func p))
    (if initial-dribble-f (funcall initial-dribble-f fp l))  ;; for lisp integration
    (if (= itmax 0)     ;; derek added this optimization, because i call sometimes with 
	(setq fret fp)  ;; no iterations, and do not want to call the dfunc in that case
      (progn
	(multiple-value-setq (p xi) (funcall dfunc p xi))
	(fdo ((j 1 (+ j 1))) ((> j n) nil)
	     (tagbody 
	       (fset-nm (fref g j) (- (fref xi j))) 
	       (fset-nm (fref h j) (fref g j))
	       (fset-nm (fref xi j) (fref h j))
	       ))
	(fdo ((iter 1 (+ iter 1))) ((> iter itmax) nil)
	     (tagbody
	       ;;(ppl (mapcar (^ (x) (if (numberp x) (dps x 6) x)) (flatten (list fp (array-to-coordss p num-points num-dims)))))
	       (multiple-value-setq (p xi n fret) (linmin p xi n fret func))

	       (if incremental-dribble-f 
		   (if (zerop (mod iter incremental-dribble-modulus))
		       (funcall incremental-dribble-f fret 'num-trials-not-set 'num-climbs-not-set 
				(array-to-coordss p num-points num-dims))))

	       (if optimum-acceptance-f
		   (if (funcall optimum-acceptance-f fret)
		       (go end_label)))

	       (if (<= (* 2.0 (abs (+ fret (- fp)))) (* ftol (+ (+ (abs fret) (abs fp)) eps))
		       )
		   (go end_label)
		 )
	       (setf fp (funcall func p))   ;; there is an optimization here, to set fp to fret (in the c code)
	       (multiple-value-setq (p xi) (funcall dfunc p xi)) 
	       (setf gg 0.0)
	       (setf dgg 0.0)
	       (fdo ((j 1 (+ j 1))) ((> j n) nil)
		    (tagbody 
		      (setf gg (+ gg (expt (fref g j) 2)))
		      (setf dgg (+ dgg (* (+ (fref xi j) (fref g j)) (fref xi j))))
		      ))
	       (if (= gg 0.0) (go end_label)) 
	       (setf gam (/ dgg gg))
	       (fdo ((j 1 (+ j 1))) ((> j n) nil)
		    (tagbody 
		      (fset-nm (fref g j) (- (fref xi j)))
		      (fset-nm (fref h j) (+ (fref g j) (* gam (fref h j))))
		      (fset-nm (fref xi j) (fref h j))
		      ))))))
    ;;(error "FRPR maximum iterations exceeded")   ;;derek: commented out because this is called once on setup with iters=0
    (go end_label)
   end_label
    (return (values (array-to-coordss p num-points num-dims) fret 'multiple-end-conditions))
    )))


;;;----------------------------------------------------------------------
;;;                      de- and constructors
;;;   (these do not really belong here, but here they are for now...)
;;;----------------------------------------------------------------------

(defun deconstruct-coordss-plus-more (coordss-plus-more)
  (let (coordss more)
    (if (and (listp (car (car (last coordss-plus-more))))
	     (caar (car (last coordss-plus-more)))
	     (symbolp (caar (car (last coordss-plus-more)))))
	(progn
	  (setq coordss (butlast coordss-plus-more))
	  (setq more (car (last coordss-plus-more))))
      (setq coordss coordss-plus-more))
    (let* ((col-and-row-adjusts (assoc-value-1 'col-and-row-adjusts more))
	   (col-bases (first-half col-and-row-adjusts))
	   (row-adjusts (second-half col-and-row-adjusts)))
      (values
       coordss
       more
       col-bases
       row-adjusts))))

(defun reconstruct-coordss-plus-more (coordss more)
  (append coordss (list more)))

(defun make-extras-hack (numbers)
  (list (list (list 'col-and-row-adjusts numbers))))

(defun make-coordss-plus-more (coords col-bases row-adjusts)
  (append coords
	  (make-extras-hack (append col-bases row-adjusts))))


#|
(defun coordss-to-array (coordss)
  (let ((flat-coordss (flatten coordss)))
    (make-array (length flat-coordss) :initial-contents flat-coordss)))
|#
(defun coordss-to-array (coordss-plus-more)
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more coordss-plus-more)
    (let ((flat-coordss (append (flatten coordss) (collect #'numberp (flatten more)))))
      (make-array (length flat-coordss) :initial-contents flat-coordss))))

#|
(defun array-to-coordss (array num-dims)
  (loop for i from 0 below (length array) by num-dims collect
	(loop for j from i to (+ i (dec num-dims)) collect
	      (aref array j))))
|#
(defun array-to-coordss (array num-coordss num-dims)
  (append
   (loop for i from 0 below (* num-coordss num-dims) by num-dims collect
	 (loop for j from i to (+ i (dec num-dims)) collect
	       (aref array j)))
    (make-extras-hack (loop for i from (* num-coordss num-dims) below (length array) collect (aref array i)))))

(defun extract-col-bases-from-coordss (coordss-more)
  ;; when i make this production code, the col-bases and row-adjusts should be alist identified after the coords
  ;; right now they are names with the keyword test, and are bundled together, row and col should be separate
  (let ((col-and-row-adjusts (assoc-value-1 'col-and-row-adjusts coordss-more)))
    (firstn (/ (length col-and-row-adjusts) 2) col-and-row-adjusts)))

(defun extract-row-adjusts-from-coordss (coordss-more)
  ;; when i make this production code, the col-bases and row-adjusts should be alist identified after the coords
  ;; right now they are names with the keyword test, and are bundled together, row and col should be separate
  (let ((col-and-row-adjusts (assoc-value-1 'col-and-row-adjusts coordss-more)))
    (nthcdr (/ (length col-and-row-adjusts) 2) col-and-row-adjusts)))

(defun coordss (coordss-plus-more)
  (deconstruct-coordss-plus-more coordss-plus-more))

(defun col-bases (coordss-plus-more)
  (extract-col-bases-from-coordss (nth-value 1 (deconstruct-coordss-plus-more coordss-plus-more))))

(defun row-adjusts (coordss-plus-more)
  (extract-row-adjusts-from-coordss (nth-value 1 (deconstruct-coordss-plus-more coordss-plus-more))))



(defun coordss-plus-more-subset (coordss-plus-more subset-indices)
  (make-coordss-plus-more
   (multiple-nth subset-indices (coordss     coordss-plus-more))
   (multiple-nth subset-indices (col-bases   coordss-plus-more))
   (multiple-nth subset-indices (row-adjusts coordss-plus-more))))
   