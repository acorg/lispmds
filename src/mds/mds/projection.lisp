(in-package user)

#|
to rotate by theta, we multiply by the vector
cos  -sin
sin   cos

to rotate by -theta, we multiply by
cos   sin
-sin  cos
(because cos (-theta) = cos(theta)
     and sin (theta)  = - sin(theta))

note, when we do the swap-y below (currently not used)
we multiply the 2nd row of this matrix by -1
(and we would need to undo that to unrotate)

the mds-to-canvas-coords projection works by:
getting canvas-x by projecting onto *canvas-basis-vector-0*
    and canvas-y by projecting onto *canvas-basis-vector-1*
thus having *canvas-basis-vector-0* multiplied by -1
flips the y-axis
(the purpose of doing this is to make the third point that
defines the plane always below the first 2)     

(note VdotW is |V|*|W|*cos(angle between V and W))
|#


;;;----------------------------------------------------------------------
;;;                      LIST VECTOR OPERATIONS 
;;;----------------------------------------------------------------------

(defun lvector-dot-product (v1 v2)
  (loop for e1 in v1 for e2 in v2 sum (* e1 e2)))

(defun lvector-norm (v)
  (let ((lvector-dot-product-v-v (lvector-dot-product v v)))
    (if (zerop lvector-dot-product-v-v)
	(progn
	  (format t "Warning, projection not possible from the chosen basis vectors~%")
	  v)
      (let ((scale (/ 1.0 (sqrt lvector-dot-product-v-v))))
	(loop for e in v collect (* e scale))))))

(defun lvector-difference (v1 v2)
  (loop for e1 in v1 for e2 in v2 collect (- e1 e2)))

(defun lvector-sum (v1 v2)
  (loop for e1 in v1 for e2 in v2 collect (+ e1 e2)))

(defun lvector-scalar-multiply (scalar vector)
  (loop for e in vector collect (* scalar e)))


;;;----------------------------------------------------------------------
;;;                         BASIS VECTORS
;;;----------------------------------------------------------------------

(defun 2-basis-lvectors (p0 p1 p2)
  (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	 (p2-p0 (lvector-difference p2 p0))
	 (basis1 (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0)))))
    (list basis0 basis1)))

(defun 2-basis-lvectors-ignoring-p2 (p0 p1 p2)
  (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	 (basis1 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0)))
    (list basis0 basis1)))

'(defun 2-basis-lvectors-swap-y (p0 p1 p2)
  (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	 (p2-p0 (lvector-difference p2 p0))
	 (basis1 (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0)))))
    (list basis0 (lvector-scalar-multiply -1 basis1))))

;; not used as of 2002-06-11
(defun 2-basis-lvectors-swap-y (mds-window p0 p1 p2)
  (let* ((p0-projected (mds-to-canvas-coords mds-window p0))
	 (p1-projected (mds-to-canvas-coords mds-window p1))
	 (p2-projected (mds-to-canvas-coords mds-window p2))
	 (p2-projected-y-on-p0-p1-line (linear-interpolation (nth 0 p2-projected) 
							     (nth 0 p0-projected) (nth 1 p0-projected)
							     (nth 0 p1-projected) (nth 1 p1-projected)))
	 (p2-above-p0p1? (> (nth 1 p2-projected) p2-projected-y-on-p0-p1-line)))
    ;;(print (if p2-above-p0p1? 'above '---below))
    (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	   (p2-p0 (lvector-difference p2 p0))
	   (basis1 (lvector-scalar-multiply 
		    (if p2-above-p0p1? 1 -1)
		    (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0))))))
      (list basis0 basis1))))

'(defun 2-basis-lvectors-swap-y (p0 p1 p2)
  (let* ((p0-projected (mds-to-canvas-coords p0))
	 (p1-projected (mds-to-canvas-coords p1))
	 (p2-projected (mds-to-canvas-coords p2))
	 (p2-projected-y-on-p0-p1-line (linear-interpolation (nth 0 p2-projected) 
							     (nth 0 p0-projected) (nth 1 p0-projected)
							     (nth 0 p1-projected) (nth 1 p1-projected)))
	 (p2-above-p0p1? (> (nth 1 p2-projected) p2-projected-y-on-p0-p1-line))
	 (p1-toright-p0? (> (nth 0 p1) (nth 0 p0))))
    (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	   (p2-p0 (lvector-difference p2 p0))
	   (basis1 (lvector-scalar-multiply 
		    (if p1-toright-p0? 
			(* 1 (if p2-above-p0p1? 1 -1))
		      (* -1 (if p2-above-p0p1? -1 1)))
		    (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0))))))
      (list basis0 basis1))))

(defun project-coordss (coordss basis-vectors)
  (loop for coords in coordss collect
	(loop for basis-vector in basis-vectors collect
	      (lvector-dot-product coords basis-vector))))
	      
