(in-package user)

;;;----------------------------------------------------------------------
;;;               PLOT COORDSS AS DISTANCES TO V2 and E
;;;----------------------------------------------------------------------


(defun get-last-mds-coordss () (assoc-value-1 'mds-coordss *selections* :test (^ (a b) (eql a (nth 1 b)))))

(defun mds-to-canvas-coords (vaccine1)
  (let* ((mds-coordss (get-last-mds-coordss))
	 (epidemic (nth (nth 0 *basis-vector-point-indices*) mds-coordss))
	 (vaccine2 (nth (nth 1 *basis-vector-point-indices*) mds-coordss)))
    (mds-to-canvas-coords-hack
     (if (or (equal vaccine1 epidemic)
	     (equal vaccine1 vaccine2))
	 vaccine1
       (list (+ (nth 0 vaccine2) (e-dist epidemic vaccine1))
	     (+ (nth 1 epidemic) (e-dist vaccine2 vaccine1)))))))

(defun mds-to-canvas-coords-without-basis-vector-translation-hack (mds-coords)
  (let ((x (if *canvas-basis-vector-0* (lvector-dot-product mds-coords *canvas-basis-vector-0*) (nth 0 mds-coords)))
        (y (if *canvas-basis-vector-1* (lvector-dot-product mds-coords *canvas-basis-vector-1*) (nth 1 mds-coords))))
    (list (+ *canvas-x-coord-translation*
	     (* *canvas-x-coord-scale* *basis-vector-x-coord-scale* x))
	  (+ *canvas-y-coord-translation*
	     (* *canvas-y-coord-scale* *basis-vector-y-coord-scale* y)))))

(defun mds-to-canvas-coords-without-basis-vector-translation-tk (tk vaccine1)
  (let* ((mds-coordss (get-mds-coordss tk))
	 (canvas-epidemic (mds-to-canvas-coords-without-basis-vector-translation-hack (nth (nth 0 *basis-vector-point-indices*) mds-coordss)))
	 (canvas-vaccine2 (mds-to-canvas-coords-without-basis-vector-translation-hack (nth (nth 1 *basis-vector-point-indices*) mds-coordss)))
	 (canvas-vaccine1 (mds-to-canvas-coords-without-basis-vector-translation-hack vaccine1)))
    (if (or (equal canvas-vaccine1 canvas-epidemic)
	    (equal canvas-vaccine1 canvas-vaccine2))
	canvas-vaccine1
      (list (+ (nth 0 canvas-vaccine2) (e-dist canvas-epidemic canvas-vaccine1))
	    (+ (nth 1 canvas-epidemic) (e-dist canvas-vaccine2 canvas-vaccine1))))))
  
(defun mds-to-canvas-coords-hack (mds-coords)
  (let ((x (if *canvas-basis-vector-0* (lvector-dot-product mds-coords *canvas-basis-vector-0*) (nth 0 mds-coords)))
        (y (if *canvas-basis-vector-1* (lvector-dot-product mds-coords *canvas-basis-vector-1*) (nth 1 mds-coords))))
    (list (+ *canvas-x-coord-translation* *basis-vector-x-coord-translation* 
	     (* *canvas-x-coord-scale* *basis-vector-x-coord-scale* x))
	  (+ *canvas-y-coord-translation* *basis-vector-y-coord-translation* 
	     (* *canvas-y-coord-scale* *basis-vector-y-coord-scale* y)))))

(defun mds-to-canvas-coords-tk (tk vaccine1)
  (let* ((mds-coordss (get-mds-coordss tk))
	 (canvas-epidemic (mds-to-canvas-coords-hack (nth (nth 0 *basis-vector-point-indices*) mds-coordss)))
	 (canvas-vaccine2 (mds-to-canvas-coords-hack (nth (nth 1 *basis-vector-point-indices*) mds-coordss)))
	 (canvas-vaccine1 (mds-to-canvas-coords-hack vaccine1)))
    (if (or (equal canvas-vaccine1 canvas-epidemic)
	    (equal canvas-vaccine1 canvas-vaccine2))
	canvas-vaccine1
      (list (+ (nth 0 canvas-vaccine2) (e-dist canvas-epidemic canvas-vaccine1))
	    (+ (nth 1 canvas-epidemic) (e-dist canvas-vaccine2 canvas-vaccine1))))))

(defun mds-to-canvas-coords-without-basis-vector-translation-tk (tk vaccine1)
  (mds-to-canvas-coords-without-basis-vector-translation-hack vaccine1))
(defun mds-to-canvas-coords-tk (tk vaccine1)
  (mds-to-canvas-coords-hack vaccine1))


(defun visualize-mds-coordss (tk new-or-update? coordss coords-names coords-colors stress &optional title)
  (if (or (= (length (car coordss)) 2)
	  *canvas-basis-vector-0*)
      nil
    (error "Visualization only works on 2d projection, so either use 2d MDS or define basis vectors for a projection into 2d"))
  (if *basis-vector-point-indices*
      (if (eql new-or-update? 'update-changes-only)
	  (loop for coords in coordss for old-coords in (get-mds-coordss tk) for index from 0 do
		(if (and (not (equal coords old-coords)) (member index *basis-vector-point-indices*))
		    (return (progn
			      ;;a point used to compute the basis vectors has changed
			      ;;all the basis vectors must be recomputed, and all points updated
			      (setq new-or-update? 'update)
			      (apply #'set-basis-vectors-from-point-indices tk coordss *basis-vector-point-indices*)
			      ))))
	(apply #'set-basis-vectors-from-point-indices tk coordss *basis-vector-point-indices*)))
  (let ((num-dimensions (length (car coordss))))
    (case new-or-update?
      (new 
       (progn
	 ;;LINK the order here and get-dimension-pair-from-tk-id
	 (tk-put tk "mkText ~a 10 25 stress sw black" stress)
	 (loop for coords in coordss 
	     for coords-name in coords-names
	     for coords-color in coords-colors
	     for i from 0 do
	       ;;LINK re ordering to change-point-by-mouse below
	       (tk-put tk "mkCircle .c ~{~d ~} 2" (mds-to-canvas-coords-tk tk coords))  
	       (tk-put tk "mkText ~s ~{~d ~} text se ~a" 
		       coords-name
		       (mds-to-canvas-coords-tk tk coords)
		       coords-color))
	 (loop for dimension below num-dimensions 
	     for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
	       (tk-put tk "mkLine ~{~d ~} axis" (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis))))
	 '(loop for axis in (get-basis-vectors-as-axes-lines) do
	   (tk-put tk "mkLine ~{~d ~} basis" (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis))))
	 (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) for x from 180 by 30 do
	       (tk-put tk "mkText ~s ~d 25 dimensionPair sw black" (dimension-pair-to-integer dimension-pair) x))
	 (if title
	     (tk-put tk "mkText ~s ~d 25 documentation sw black" 
		     title (+ 180 (* 30 (length (combs 2 (series 0 (dec num-dimensions))))) 30)))))
      (update 
       (let ((item 1))
	 (if stress (tk-put tk "setTextText ~d ~a" item stress))
	 (setq item (+ item 1))
	 (loop for coords in coordss do
	       (tk-put tk "setIdXYR ~d ~{~d ~} 2" item (mds-to-canvas-coords-tk tk coords))
	       (setq item (+ item 1))
	       (tk-put tk "setTextXY ~d ~{~d ~}" item (mds-to-canvas-coords-tk tk coords))
	       (setq item (+ item 1)))
	 (loop for dimension below num-dimensions for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
	       (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis)))
	       (setq item (+ item 1)))
	 '(loop for axis in (get-basis-vectors-as-axes-lines) do
	   (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis)))
	   (setq item (+ item 1)))
	 (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) do
	       dimension-pair
	       (setq item (+ item 1)))
	 (if title
	     (progn (tk-put tk "setTextText ~d ~a" item title)
		    (setq item (+ item 1))))))
      (update-changes-only 
       (let ((item 1))
	 (if stress (tk-put tk "setTextText ~d ~a" item stress))
	 (setq item (+ item 1))
	 (loop for coords in coordss for old-coords in (get-mds-coordss tk) do
	       (if (not (equal coords old-coords))
		   (tk-put tk "setIdXYR ~d ~{~d ~} 2" item (mds-to-canvas-coords-tk tk coords)))
	       (setq item (+ item 1))
	       (if (not (equal coords old-coords))
		   (tk-put tk "setTextXY ~d ~{~d ~}" item (mds-to-canvas-coords-tk tk coords)))
	       (setq item (+ item 1)))
	 (loop for dimension below num-dimensions for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
	       (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis)))
	       (setq item (+ item 1)))
	 '(loop for axis in (get-basis-vectors-as-axes-lines) do
	   (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (x) (mds-to-canvas-coords-tk tk x)) axis)))
	   (setq item (+ item 1)))
	 (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) do
	       dimension-pair
	       (setq item (+ item 1)))
	 (if title
	     (progn (tk-put tk "setTextText ~d ~a" item title)
		    (setq item (+ item 1))))))
      (t (error "end of case"))))
  (set-mds-coordss tk coordss))

(defun set-basis-vectors-from-point-indices (tk coordss p0-index p1-index p2-index)
  (let* ((basis-vectors (2-basis-lvectors-swap-y
			 (nth p0-index coordss)
			 (nth p1-index coordss)
			 (nth p2-index coordss)))
	 (rotated-basis-vectors (rotate-coordss basis-vectors 
						(- (* 0.75 pi)) 
						0 0 ;;center not used in rotate-coordss
						0 1
						)))
    (set-basis-vectors rotated-basis-vectors)
    (let ((p0-position
	   (mds-to-canvas-coords-without-basis-vector-translation-tk tk (nth p0-index coordss)))
	  (user-translation (get-user-translation)))
      (set-basis-translation (mapcar #'- user-translation p0-position)))
    '(let ((basis-scale (/ 0.5 (e-dist (nth p0-index coordss) (nth p1-index coordss)))))
      (set-basis-scale (list basis-scale basis-scale)))))

(defun set-basis-vectors-from-point-indices (tk coordss p0-index p1-index p2-index)
  (let* ((basis-vectors (2-basis-lvectors
			 (nth p0-index coordss)
			 (nth p1-index coordss)
			 (nth p2-index coordss))))
    ;;(print (list basis-vectors coordss))
    (set-basis-vectors basis-vectors)))
