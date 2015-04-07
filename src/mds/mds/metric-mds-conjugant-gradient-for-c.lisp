(in-package user)

(defvar *conjugant-gradient-hash*)
(setq *conjugant-gradient-hash* (make-hash-table :test #'equal))

(defvar *NUMERIC_DONT_CARE* -7777777)

(defvar *metric-conjugant-gradient-stress-f*)  ;; name the stress fuction so we can (temporarily) (nasty hack)
                                               ;; so we can switch on it in mds

(defun metric-mds-conjugant-gradient (target-dists stress-component-f &rest args)
  stress-component-f
  (apply #'mds
	 (setq *metric-conjugant-gradient-stress-f* 
	   (^ (coordss &optional old-coordss (initial-or-next 'initial))   ;; initial or next is a bit of an ugly hack for 
	      old-coordss                                                  ;; shoehorning in conjugant-gradient
	      ;; this is the fitness function, it must return fitness, but also have it return
	      ;; 3 other values, new fitness, new coords, and (depreciated) num-trials
	      (let ((stream (gethash target-dists *conjugant-gradient-hash*)))
		(if (not stream)
		    (progn
		      (setq stream (process-open "/home/dsmith/junk/misc/mds" nil nil))
		      (setf (gethash target-dists *conjugant-gradient-hash*) stream)
		      ;; send the target distances (just the upper triangle)
		      (tk-put stream "~d" (length coordss))
		      (tk-put stream "~{~f ~}"   ;; maybe split this up into many smaller peices?
			      (subst *NUMERIC_DONT_CARE* 'dont-care target-dists))
		      ;; send the moveable coords (very very nasty hack here, snooping into args to pick it up)
		      ;; (this can be removed, this whole progn, when we have better integration into lisp)
		      (tk-put stream "~{~d ~}"
			      (let ((moveable-coords (snoop-moveable-coords args)))
				(cond ((eql 'all (snoop-moveable-coords args)) 
				       (loop for i below (length coordss) collect 1))
				      ((listp moveable-coords) 
				       (loop for i below (length coordss) collect (bool->bit (member i moveable-coords))))
				      (t (error "unexpected value for moveable coords: ~a" moveable-coords)))))))
		(if (eql initial-or-next 'initial)
		    (progn
		      (tk-put stream "~d" (length (car coordss)))
		      (tk-put stream "~{ ~{ ~f ~} ~}" coordss)
		      (c-conjugate-gradient-decode (tk-get stream) (length (car coordss))))
		  (progn
		    (tk-put stream "-2")   ;; indicating next value
		    (c-conjugate-gradient-decode (tk-get stream) (length (car coordss))))))))
	args))

(defun c-conjugate-gradient-decode (string dimensions)
  ;; string has the form
  ;;   fitness for the input coords; fitness for next coords, next coords
  (let ((atoms (string-to-atoms string)))
    (values
     (nth 0 atoms)
     (nth 1 atoms)
     (if (eql (nth 2 atoms) -7777777)
	 (refold dimensions (nthcdr 3 atoms))
       (refold dimensions (nthcdr 2 atoms)))
     (if (eql (nth 2 atoms) -7777777)
	 -7777777
       nil))))

(defun refold (n l)
  (let ((sofar l))
    (loop for i below (ceiling (/ (length l) n)) collect
	  (let ((first-pair (firstn 2 sofar)))
	    (setq sofar (nthcdr 2 sofar))
	    first-pair))))


(defun snoop-moveable-coords (args)
  (nth (inc (position :moveable-coords args)) args))

#|

;;--------------------------- testing basic functionality -------------------------

(setq 345hi
  (make-hi-table
   '(a b c)
   '(a b c)
   '((0 3 5)
     (0 0 4)
     (0 0 0))))

(batch-mds 345hi
	   2
	   100
	   100)

(make-strain-selection-window 345hi t nil 2)   ;;right angle 345 triangle


;;--------------------------  testing dont-care  ----------------------------

;; 2 right triangles
;; 
;;
;;         a
;;
;;    c    b    d
;;



(setq 345hi2
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 5)
     (0 0 4 4)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2 t nil 2)  ;; no dont-cares   

(setq 345hi2-dc
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 5)
     (0 0 4 dont-care)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2-dc t nil 2)  ;;should be able to reconstruct perfectly

(setq 345hi2-dc-ambiguous
  (make-hi-table
   '(a b c d)
   '(a b c d)
   '((0 3 5 dont-care)
     (0 0 4 dont-care)
     (0 0 0 8)
     (0 0 0 0))))

(make-strain-selection-window 345hi2-dc-ambiguous t nil 2)  ;;should be ambiguous for d
                                                            ;; d is anywhere so long as it is 8 from a


exit is not sorted
it should close the c pipe
and remove the target matrix from the hash table
(we need to be careful with a restart though, from different coords)
sort out the gg==0 case  (just print a warning (one we may never see))
send the data gradually thru the pipe instead of all at once?
|#
