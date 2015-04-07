 (in-package user)

;;;----------------------------------------------------------------------
;;;                        COORD TRANSFORMATION
;;;----------------------------------------------------------------------

#|
(defvar *canvas-width*)
(defvar *canvas-height*)

(defvar *canvas-x-coord-translation*)
(defvar *canvas-y-coord-translation*)
(defvar *canvas-x-coord-scale*)
(defvar *canvas-y-coord-scale*)
(defvar *canvas-basis-vector-0*)
(defvar *canvas-basis-vector-1*)
(defvar *first-dimension*)
(defvar *second-dimension*)
(defvar *basis-vector-point-indices*)
(defvar *basis-vector-point-indices-backup*)
(defvar *basis-vector-x-coord-translation*)
(defvar *basis-vector-y-coord-translation*)
(defvar *basis-vector-x-coord-scale*)
(defvar *basis-vector-y-coord-scale*)
(defvar *translate-to-fit-mds-window*)
(defvar *scale-to-fit-mds-window*)
|#

(defun reset-canvas-coord-transformations (tk)
  (set-canvas-width tk 530) ;;550
  (set-canvas-height tk 450) ;;350)
  (set-canvas-x-coord-translation tk 150.0)
  (set-canvas-y-coord-translation tk 150.0)
  (set-canvas-x-coord-scale tk 150.0)
  (set-canvas-y-coord-scale tk 150.0)
  (set-canvas-basis-vector-0 tk nil)
  (set-canvas-basis-vector-1 tk nil)
  (set-first-dimension tk 0)
  (set-second-dimension tk 1)
  (set-basis-vector-point-indices tk '(0 1 2))  ;;set this to nil to not have basis vectors set by points
  (set-basis-vector-point-indices-backup tk nil)
  (set-basis-vector-x-coord-translation tk 0)
  (set-basis-vector-y-coord-translation tk 0)
  (set-basis-vector-x-coord-scale tk 1)
  (set-basis-vector-y-coord-scale tk 1)
  (set-translate-to-fit-mds-window tk t)
  (set-scale-to-fit-mds-window tk t))

;; (reset-canvas-coord-transformations)


(defun set-canvas-coord-transformations (tk &key 
					    canvas-width
					    canvas-height
					    canvas-x-coord-translation
					    canvas-y-coord-translation
					    canvas-x-coord-scale
					    canvas-y-coord-scale
					    canvas-basis-vector-0
					    canvas-basis-vector-1
					    first-dimension
					    second-dimension
					    basis-vector-point-indices
					    basis-vector-point-indices-backup
					    basis-vector-x-coord-translation
					    basis-vector-y-coord-translation
					    basis-vector-x-coord-scale
					    basis-vector-y-coord-scale
					    translate-to-fit-mds-window
					    scale-to-fit-mds-window)
  (set-canvas-width tk canvas-width)
  (set-canvas-height tk canvas-height)
  (set-canvas-x-coord-translation tk canvas-x-coord-translation)
  (set-canvas-y-coord-translation tk canvas-y-coord-translation)
  (set-canvas-x-coord-scale tk canvas-x-coord-scale)
  (set-canvas-y-coord-scale tk canvas-y-coord-scale)
  (set-canvas-basis-vector-0 tk canvas-basis-vector-0)
  (set-canvas-basis-vector-1 tk canvas-basis-vector-1)
  (set-first-dimension tk first-dimension)
  (set-second-dimension tk second-dimension)
  (set-basis-vector-point-indices tk basis-vector-point-indices)
  (set-basis-vector-point-indices-backup tk basis-vector-point-indices-backup)
  (set-basis-vector-x-coord-translation tk basis-vector-x-coord-translation)
  (set-basis-vector-y-coord-translation tk basis-vector-y-coord-translation)
  (set-basis-vector-x-coord-scale tk basis-vector-x-coord-scale)
  (set-basis-vector-y-coord-scale tk basis-vector-y-coord-scale)
  (set-translate-to-fit-mds-window tk translate-to-fit-mds-window)
  (set-scale-to-fit-mds-window tk scale-to-fit-mds-window))


(defun get-canvas-coord-transformations (tk)
  (list 
   :canvas-width (get-canvas-width tk)
   :canvas-height (get-canvas-height tk)
   :canvas-x-coord-translation (get-canvas-x-coord-translation tk)
   :canvas-y-coord-translation (get-canvas-y-coord-translation tk)
   :canvas-x-coord-scale (get-canvas-x-coord-scale tk)
   :canvas-y-coord-scale (get-canvas-y-coord-scale tk)
   :canvas-basis-vector-0 (get-canvas-basis-vector-0 tk)
   :canvas-basis-vector-1 (get-canvas-basis-vector-1 tk)
   :first-dimension (get-first-dimension tk)
   :second-dimension (get-second-dimension tk)
   :basis-vector-point-indices (get-basis-vector-point-indices tk)
   :basis-vector-point-indices-backup (get-basis-vector-point-indices-backup tk)
   :basis-vector-x-coord-translation (get-basis-vector-x-coord-translation tk)
   :basis-vector-y-coord-translation (get-basis-vector-y-coord-translation tk)
   :basis-vector-x-coord-scale (get-basis-vector-x-coord-scale tk)
   :basis-vector-y-coord-scale (get-basis-vector-y-coord-scale tk)
   :translate-to-fit-mds-window (get-translate-to-fit-mds-window tk)
   :scale-to-fit-mds-window (get-scale-to-fit-mds-window tk)))


(defun get-basis-vectors (tk)
  (list (get-canvas-basis-vector-0 tk) (get-canvas-basis-vector-1 tk)))

(defun set-basis-vectors (tk 2-basis-vectors)
  (set-canvas-basis-vector-0 tk (nth 0 2-basis-vectors))
  (set-canvas-basis-vector-1 tk (nth 1 2-basis-vectors)))

(defun set-canvas-basis-vectors-from-canvas-coord-transforamtions (tk canvas-coord-transformations)
  (set-basis-vectors 
   tk 
   (list 
    (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)
    (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations))))

(defun mds-to-canvas-coords (tk mds-coords)
  (let ((x (if (get-canvas-basis-vector-0 tk) ;; (and *basis-vector-point-indices* *canvas-basis-vector-0*) 
	       ;; Derek, 11/1/00, I'm not sure if I need both of the above
	       ;; Derek, 12/1/00. Well, things are a bit squirley, and we should only have 
	       ;; the canvas-basis-vectors.  This is because when the basis-vector-point-indices are
	       ;; nil, this indicates that the basis vectors are not moved as the mds proceeds.
	       ;; (which is set by click right on the basis-vector pairs).  This disables both
	       ;; the scaling and translating to fit the mds visualization window, and also 
	       ;; the rotation adjustment.  However, the original rotation still happens.  And
	       ;; when we rotate, with mouse-right, we achieve that by adjusting the basis-vectors.
	       (lvector-dot-product mds-coords (get-canvas-basis-vector-0 tk)) 
	     (nth 0 mds-coords)))
        (y (if (get-canvas-basis-vector-1 tk) ;; (and *basis-vector-point-indices* *canvas-basis-vector-1*)
	       (lvector-dot-product mds-coords (get-canvas-basis-vector-1 tk)) 
	     (nth 1 mds-coords))))
    (list (+ (get-canvas-x-coord-translation tk) (get-basis-vector-x-coord-translation tk) 
	     (* (get-canvas-x-coord-scale tk) (get-basis-vector-x-coord-scale tk) x))
	  (+ (get-canvas-y-coord-translation tk) (get-basis-vector-y-coord-translation tk) 
	     (* (get-canvas-y-coord-scale tk) (get-basis-vector-y-coord-scale tk) y)))))

(defun mds-to-canvas-scale-scalar (tk scalar)
  (* (get-canvas-x-coord-scale tk) (get-basis-vector-x-coord-scale tk) scalar))

(defun mds-to-canvas-coords-given-canvas-coord-transformations (canvas-coord-transformations mds-coords)
  ;; canvas-coord-transformations come from saves
  (let ((x (if (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)
	       (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)) 
	     (nth 0 mds-coords)))
        (y (if (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)
	       (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)) 
	     (nth 1 mds-coords))))
    (list (+ (snoop-keyword-arg :canvas-x-coord-translation canvas-coord-transformations) 
	     (snoop-keyword-arg :basis-vector-x-coord-translation canvas-coord-transformations) 
	     (* (snoop-keyword-arg :canvas-x-coord-scale canvas-coord-transformations)
		(snoop-keyword-arg :basis-vector-x-coord-scale canvas-coord-transformations)
		x))
	  (+ (snoop-keyword-arg :canvas-y-coord-translation canvas-coord-transformations)
	     (snoop-keyword-arg :basis-vector-y-coord-translation canvas-coord-transformations) 
	     (* (snoop-keyword-arg :canvas-y-coord-scale canvas-coord-transformations)
		(snoop-keyword-arg :basis-vector-y-coord-scale canvas-coord-transformations)
		y)))))

(defun mds-to-canvas-coords-given-canvas-coord-transformations-no-scale (canvas-coord-transformations mds-coords)
  ;; canvas-coord-transformations come from saves
  (if canvas-coord-transformations
      (let ((x (if (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)
		   (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)) 
		 (nth 0 mds-coords)))
	    (y (if (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)
		   (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)) 
		 (nth 1 mds-coords))))
	(list (+ (snoop-keyword-arg :canvas-x-coord-translation canvas-coord-transformations) 
		 (snoop-keyword-arg :basis-vector-x-coord-translation canvas-coord-transformations) 
		 (* (sign (snoop-keyword-arg :canvas-x-coord-scale canvas-coord-transformations))
                    (sign (snoop-keyword-arg :basis-vector-x-coord-scale canvas-coord-transformations))
                    x))
	      (+ (snoop-keyword-arg :canvas-y-coord-translation canvas-coord-transformations)
		 (snoop-keyword-arg :basis-vector-y-coord-translation canvas-coord-transformations) 
		 (* (sign (snoop-keyword-arg :canvas-y-coord-scale canvas-coord-transformations))
                    (sign (snoop-keyword-arg :basis-vector-y-coord-scale canvas-coord-transformations))
                    y))))
    ;; a save that has never been opened in the gui might not have canvas-coord-transformations
    mds-coords))

(defun mds-to-canvas-coordss-given-canvas-coord-transformations-no-scale (canvas-coord-transformations mds-coordss)
  (mapcar 
   (^ (mds-coords) 
      (mds-to-canvas-coords-given-canvas-coord-transformations-no-scale
       canvas-coord-transformations
       mds-coords))
   mds-coordss))

(defun mds-to-canvas-coordss-given-canvas-coord-transformations (canvas-coord-transformations mds-coordss)
  (mapcar 
   (^ (mds-coords) 
      (mds-to-canvas-coords-given-canvas-coord-transformations
       canvas-coord-transformations
       mds-coords))
   mds-coordss))

(defun mds-to-canvas-coords-given-canvas-coord-transformations-rotation-only (canvas-coord-transformations mds-coords)
  ;; canvas-coord-transformations come from saves
  (let ((x (if (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)
	       (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-0 canvas-coord-transformations)) 
	     (nth 0 mds-coords)))
        (y (if (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)
	       (lvector-dot-product mds-coords (snoop-keyword-arg :canvas-basis-vector-1 canvas-coord-transformations)) 
	     (nth 1 mds-coords))))
    (list x y)))

(defun flip-x-in-canvas-coord-transformations (canvas-coord-transformations)
  (if canvas-coord-transformations
      (subst-keyword-arg 
       :CANVAS-BASIS-VECTOR-0
       canvas-coord-transformations
       (mapcar #'- (snoop-keyword-arg :CANVAS-BASIS-VECTOR-0 canvas-coord-transformations)))
    nil))

(defun flip-y-in-canvas-coord-transformations (canvas-coord-transformations)
  (if canvas-coord-transformations
      (subst-keyword-arg 
       :CANVAS-BASIS-VECTOR-1
       canvas-coord-transformations
       (mapcar #'- (snoop-keyword-arg :CANVAS-BASIS-VECTOR-1 canvas-coord-transformations)))
    nil))

(defun flip-bottom-y-to-top-y-in-canvas-coord-transformations (canvas-coord-transformations)
  (if canvas-coord-transformations
      (let ((canvas-coord-transformations (subst-keyword-arg
                                           :canvas-y-coord-translation
                                           canvas-coord-transformations
                                           (+ (snoop-keyword-arg :canvas-y-coord-translation canvas-coord-transformations)
                                              (snoop-keyword-arg :canvas-height              canvas-coord-transformations)))))
        (subst-keyword-arg 
         :CANVAS-BASIS-VECTOR-1
         canvas-coord-transformations
         (mapcar #'- (snoop-keyword-arg :CANVAS-BASIS-VECTOR-1 canvas-coord-transformations))))
    nil))



#|
'(:CANVAS-WIDTH 599 :CANVAS-HEIGHT 729 :CANVAS-X-COORD-TRANSLATION 251.0d0 :CANVAS-Y-COORD-TRANSLATION
  347.00000000000006d0 :CANVAS-X-COORD-SCALE -19.63455797757935d0 :CANVAS-Y-COORD-SCALE -19.63455797757935d0
  :CANVAS-BASIS-VECTOR-0 (0.862052414062216d0 0.5068189999737442d0) :CANVAS-BASIS-VECTOR-1
  (0.5068189999737437d0 -0.8620524140622163d0) :FIRST-DIMENSION 0 :SECOND-DIMENSION 1 :BASIS-VECTOR-POINT-INDICES
  NIL :BASIS-VECTOR-POINT-INDICES-BACKUP (0 1 2) :BASIS-VECTOR-X-COORD-TRANSLATION 0
  :BASIS-VECTOR-Y-COORD-TRANSLATION 0 :BASIS-VECTOR-X-COORD-SCALE 1 :BASIS-VECTOR-Y-COORD-SCALE 1
  :TRANSLATE-TO-FIT-MDS-WINDOW NIL :SCALE-TO-FIT-MDS-WINDOW NIL)
|#




#|
;; this did not work because the basis vectors were also doing x and y flips
;; and i could not figure it out.  so instead, below, i just do a matrix invert
(defun un-rotate (xy)
  ;; we don't do this very often, just when we move a point with the mouse in the UI.
  ;; so its ok that we are not super efficient
  ;; if we reinitroduce the swap-y in the creation of the basis-vectors, then we need to
  ;; modify this--one way would be to make the flip (doing -1 on the 2nd basis vector)
  ;; and rotation separate operations
  ;; NOTE: there seem to be occations when this does not work (when the point "warps" to
  ;; another part of the space, but i'm not sure what is causing it
  (let ((anti-canvas-basis-vector-0 (list (nth 0 *canvas-basis-vector-0*) (- (nth 1 *canvas-basis-vector-0*))))
	(anti-canvas-basis-vector-1 (list (- (nth 0 *canvas-basis-vector-1*)) (nth 1 *canvas-basis-vector-1*)))) 
    (list (lvector-dot-product xy anti-canvas-basis-vector-0)
	  (lvector-dot-product xy anti-canvas-basis-vector-1))))
|#

(defun un-rotate (mds-window xy)
  ;; we don't do this very often, just when we move a point with the mouse in the UI.
  ;; so its ok that we are not super efficient
  ;; this should work with swap-y on the basis vectors too (as we are now assuming nothing)
  (if (not (= 2 (length (get-canvas-basis-vector-0 mds-window))))
      (progn (format t "Moving points by the mouse only works reliably in 2D~%")
	     xy)
    (let ((anti-canvas-basis (list-array (matrix:invert-matrix (array-list (list (get-canvas-basis-vector-0 mds-window) (get-canvas-basis-vector-1 mds-window)))))))
      (let ((anti-canvas-basis-vector-0 (nth 0 anti-canvas-basis))
	    (anti-canvas-basis-vector-1 (nth 1 anti-canvas-basis)))
	(list (lvector-dot-product xy anti-canvas-basis-vector-0)
	      (lvector-dot-product xy anti-canvas-basis-vector-1))))))
	  
(defun canvas-to-mds-coords (mds-window canvas-coords)
  ;; NOTE, this only works correctly now for 2D.  for >2D, we could say we are are moving
  ;; in the plane of the canvas, but we need to figure out how to generate the mds coords
  (let ((mds-coords 
	 (list (/ (- (nth 0 canvas-coords) 
		     (+ (get-canvas-x-coord-translation mds-window) (get-basis-vector-x-coord-translation mds-window))) 
		  (* (get-canvas-x-coord-scale mds-window) (get-basis-vector-x-coord-scale mds-window)))
	       (/ (- (nth 1 canvas-coords) 
		     (+ (get-canvas-y-coord-translation mds-window) (get-basis-vector-y-coord-translation mds-window))) 
		  (* (get-canvas-y-coord-scale mds-window) (get-basis-vector-y-coord-scale mds-window))))))
    ;; note on (un) rotation.
    ;; even when the basis-vector-point-indices are nil, we still do rotation via the
    ;; canvas basis vector coordss
    (let ((2d-mds-coords (if t ;;*basis-vector-point-indices*
                             (un-rotate mds-window mds-coords)
                           mds-coords)))
      (append
       2d-mds-coords
       (loop for i from 2 below (length (get-canvas-basis-vector-0 mds-window)) collect 0.0))))) ;; don't know what to do, so make zeros
      

(defun canvas-to-mds-scale-factor (mds-window)
  (if (not (and (eql (abs (get-canvas-x-coord-scale mds-window)) (abs (get-canvas-y-coord-scale mds-window)))
		(eql (abs (get-basis-vector-x-coord-scale mds-window)) (abs (get-basis-vector-y-coord-scale mds-window)))))
      (error "the x and y scales are different, but i expected them to be the same"))
  (/ 1 (abs (* (get-canvas-x-coord-scale mds-window) (get-basis-vector-x-coord-scale mds-window)))))

(defun dimension-pair-to-integer (dimension-pair)
  (format nil "~{~d~}" dimension-pair))

(defun make-standard-vectors-as-axes-lines (num-dimensions)
  (let ((origin (zero-base num-dimensions)))
    (loop for dimension below num-dimensions collect
	  (list origin (loop for i below num-dimensions collect (bool->bit (= i dimension)))))))
 
(defun make-unit-vectors (num-dimensions)
  (loop for dimension below num-dimensions collect
	(loop for i below num-dimensions collect (bool->bit (= i dimension)))))
 

;;;----------------------------------------------------------------------
;;;                          CANVAS SIZE
;;;----------------------------------------------------------------------

(defun set-canvas-size-info (tk geometry)
  "Gives lisp info about the canvas size (called from tk)"
  (setq geometry (string geometry))
  (let* ((width (read-from-string (substring-before-char #\X geometry)))
	 (intermediate (if (substring-before-char #\- (substring-after-char #\X geometry))
			   (substring-before-char #\- (substring-after-char #\X geometry))
			 (if (substring-before-char #\+ (substring-after-char #\X geometry))
			     (substring-before-char #\+ (substring-after-char #\X geometry))
			   (error "unexpected case when resizing window, email dsmith@santafe.edu"))))
	 (height (read-from-string
		  (if (substring-before-char #\- intermediate)
		      (substring-before-char #\- intermediate)
		    (if (substring-before-char #\+ intermediate)
			(substring-before-char #\+ intermediate)
		      intermediate)))))
    (set-canvas-width tk width)
    (set-canvas-height tk height)
    (list width height)
  ))


;;;----------------------------------------------------------------------
;;;                      VISUALIZE MDS COORDS
;;;----------------------------------------------------------------------

(defun make-mds-visualization-window (&optional name width height)
  (let ((tk (tk-open)))
    (tk-put tk "set bitmapDir ~s" (uw-sfnr "bitmaps" :assertIsDir t))
    (tk-put tk "source ~s" (uw-sfnr "mds/mds-visualization.tk" :assertIsFile t))
    (if (not *ugly-hack-to-write-to-file-instead-of-wish*)
	(sleep 1))  ;; to let the source happen
    (if name (tk-put tk "wm title . {~s}" name))
    (tk-put tk "wm geometry . ~dx~d" width height)
    (if (not *ugly-hack-to-write-to-file-instead-of-wish*)
	(sleep 1))  ;; let the resize happen
    ;;(make-stream-alist-item tk 'item-id nil)
    tk))
 
(defun translate-to-fit-mds-window (mds-window coordss)
  ;;rescale so the points fit in the window
  ;;(error "" "")
  (if (get-translate-to-fit-mds-window mds-window)
      (let* ((canvas-coordss (mapcar (^ (coords) (mds-to-canvas-coords mds-window coords)) coordss))
	     (x-min (apply-min (nths 0 canvas-coordss)))
	     (x-max (apply-max (nths 0 canvas-coordss)))
	     (y-min (apply-min (nths 1 canvas-coordss)))
	     (y-max (apply-max (nths 1 canvas-coordss))))
	(set-canvas-x-coord-translation
	 mds-window
	 (+ (get-canvas-x-coord-translation mds-window)
	    (- (round (/ (get-canvas-width mds-window) 2))
	       (round (av (list x-max x-min))))))
	(set-canvas-y-coord-translation
	 mds-window
	 (+ 15
	    (get-canvas-y-coord-translation mds-window)
	    (- (round (/ (get-canvas-height mds-window) 2))
	       (round (av (list y-max y-min)))))))))

(defun scale-to-fit-mds-window (mds-window coordss)
  (if (get-scale-to-fit-mds-window mds-window)
      ;;rescale so the points fit in the window
      (let* ((canvas-coordss (mapcar (^ (coords) (mds-to-canvas-coords mds-window coords)) coordss))
	     (x-min (apply-min (nths 0 canvas-coordss)))
	     (x-max (apply-max (nths 0 canvas-coordss)))
	     (y-min (apply-min (nths 1 canvas-coordss)))
	     (y-max (apply-max (nths 1 canvas-coordss)))
	     (x-spread (if (zerop (- x-max x-min)) 0.00000001 (- x-max x-min)))
	     (y-spread (if (zerop (- y-max y-min)) 0.00000001 (- y-max y-min))))
	(let ((scale (* 0.75
			(min (/ (get-canvas-height mds-window) y-spread)
			     (/ (get-canvas-width mds-window)  x-spread)))))
	  (set-canvas-x-coord-scale mds-window (* (get-canvas-x-coord-scale mds-window) scale))
	  (set-canvas-y-coord-scale mds-window (* (get-canvas-y-coord-scale mds-window) scale))))))



(defun center-radius-tk-params (tk coords coords-dot-size)
  (format nil "~{~d ~}~d" 
	  (mapcar (^ (x) (coerce x 'single-float)) (mds-to-canvas-coords tk coords))
	  coords-dot-size))

(defun polygon-tk-params (tk coords coords-shape)
  (format nil "{~{~d ~}}"
	  (mapcar 
	   (^ (x) (coerce x 'single-float))
	   (map-append (^ (polygon-offsets)
			  (if (eql 'polygon-scaled (nth 0 coords-shape))
			      (mds-to-canvas-coords 
			       tk
			       (mapcar #'+ coords polygon-offsets))
			    (mapcar #'+ 
				    (mds-to-canvas-coords tk coords)
				    polygon-offsets)))
		       (nth 1 coords-shape)))))
  
(defun visualize-mds-coordss-new (tk coordss coords-names coords-colors stress 
				  passed-in-coordss
				  more
				  row-adjusts
				  col-adjusts
				  num-dimensions
				  &key title
				       coords-name-sizes
				       coords-dot-sizes
                                       coords-transparencies
				       coords-name-colors
				       scale-to-fit-mds-window
				       show-error-lines
				       show-stress-components
				       show-scale-with-length
				       unmoveable-coords
				       coords-outline-colors
				       coords-shapes
				       procrustes-data
				       coords-full-names
				       blob-steps
				       blob-power
                                       raise-points
                                       lower-points)

  coords-transparencies  ;; do nothing for display with these for now in mds map-window, just keep the values for passing to pymol

  (let ((item 1)
	(disconnected-points (get-disconnected-points (get-table-window-for-mds-window tk))))
    (if scale-to-fit-mds-window
	(scale-to-fit-mds-window tk coordss))
    (translate-to-fit-mds-window tk coordss)
    ;;LINK the order here and get-dimension-pair-from-tk-id
    (tk-put tk "mkText \"~7a\" 10 25 stress sw black ~a" (if (numberp stress) (dps stress 4) stress) (tk-font 11))
    (setq item (+ item 1))

    (set-mds-window-run-indicator-canvas-item tk item)
    (tk-put tk "mkText \"~a\" 60 25 run-indicator sw black ~a" "Idle" (tk-font 11))
    (setq item (+ item 1))

    (set-hi-table-prediction-error-id tk item)
    ;;(tk-put tk "mkText \"~a\" 150 25 prediction-report sw black ~a" "no predictions" (tk-font 11))
    (tk-put tk "mkText \"~a\" 150 25 prediction-report sw black ~a" "" (tk-font 11))
    (show-table-prediction-summary-in-mds-window tk passed-in-coordss)
    (setq item (+ item 1))

    (set-mds-window-first-coords-canvas-item tk item)
    (loop for coords in coordss 
	for coords-name in coords-names
	for coords-shape in coords-shapes
	for coords-name-size in coords-name-sizes
	for coords-name-color in coords-name-colors
	for coords-dot-size in coords-dot-sizes
	for coords-color in coords-colors
	for coords-outline-color in coords-outline-colors
	for i from 0 do
	  ;;for a linux problem?
	  (if (not *ugly-hack-to-write-to-file-instead-of-wish*)
	      (if (zerop (mod i 6))  ;; was 10 before i added the set itemColor  2001-10-28
		  (sleep 0.08)))
	  ;;LINK re ordering to change-point-by-mouse below
	  (if (not (and (listp coords-shape)
			(eql (car coords-shape) 'color-graded-polygon-scaled)))
	      (progn
		(tk-put tk "~a .c ~a ~a ~a"
			(cond ((eql coords-shape 'circle)    "mkCircleColor")
			      ((eql coords-shape 'rectangle) "mkRectangleColor")
			      ((eql coords-shape 'triangle)  "mkTriangleColor")
			      ((eql coords-shape 'down-triangle)  "mkDownTriangleColor")
			      ((listp coords-shape) "mkPolygonColor")
			      (t (error "unexpected shape")))
			(if (eql coords-outline-color 'not-set)
			    'black
			  coords-outline-color)   ;; use {} to specify none for fill or outline
			(if (member i disconnected-points)
			    "#eeeeee"               ;; LINK color also in reset-point-color and mds-visualization.tk and below
			  (if (member i unmoveable-coords)
			      "#777777"             
			    coords-color))
			(if (listp coords-shape)
			    (polygon-tk-params tk coords coords-shape)
			  (center-radius-tk-params tk coords coords-dot-size)))
		(setq item (+ item 1)))
	    (let ((constant-stress-radial-data (get-constant-stress-radial-data (get-table-window-for-mds-window tk))))
	      (loop for blob-step below blob-steps do
		    (progn
		      (tk-put tk "mkPolygonColor .c ~a ~a ~a"
			      (if ;; outlines for sera only
				  (and (not (= -1.0d+7 (nth i col-adjusts)))   ;; hack way to determine if serum
				       (zerop blob-step))
				  "#777777"
				"{}")
			      (if (member i disconnected-points)
				  "#eeeeee"               ;; LINK color also in reset-point-color and mds-visualization.tk and below
				(if (member i unmoveable-coords)
				    "#777777"             
				  (if (equal "{}" coords-color)
				      coords-color
				    (multiple-value-bind (h s v)
					(apply #'rgb-to-r-hsv (tk-color-to-rgb coords-color))
				      (hsv-tk-color
				       h
				       s
				       (max 0 (- v (* blob-step (/ v blob-steps)))))))))
			      (polygon-tk-params 
			       tk
			       coords
			       (list 'polygon-scaled
				     (calc-constant-stress-shape-from-index 
				      i
				      constant-stress-radial-data
				      :stress-delta (* (expt (float (/ (- blob-steps blob-step) blob-steps)) blob-power) 
						       (nth 1 coords-shape))))))  ;; the stress-delta
		      (setq item (+ item 1))
		      ))))

	  ;; (tk-put tk "set itemColor(~d) ~a" item coords-color)  removed 20041011, have the mkCircle etc. color set it

	  ;; crosses in center of blobs hack 
	  ;; (tk-put tk "mkCross .c ~a ~a" (center-radius-tk-params tk coords 2) 1)
	  ;; dots in center of blobs hack 
	  ;; (tk-put tk "mkCircleColor .c ~a ~a ~a" "black" "black" (center-radius-tk-params tk coords 0.5))
	  ;; dots for antigens, crosses for sera
	  ;;(if (= -1.0d+7 (nth i col-adjusts))   ;; hack way to determine if not serum
	  ;;    (tk-put tk "mkCircleColor .c ~a ~a ~a" "black" "black" (center-radius-tk-params tk coords 0.5))
	  ;;  (tk-put tk "mkCross .c ~a ~a" (center-radius-tk-params tk coords 2) 1))
	  (tk-put tk 
                  ;;"mkText ~s ~{~d ~} strainName se ~a ~a" ;;to get only names, make anchor center 
                  "mkText ~s ~{~d ~} strainName n ~a ~a" ;;to get only names, make anchor center 
                  ;;and color gray90 for the circle (above)
		  (coords-name-plus-adjusts coords-name (nth i row-adjusts) (nth i col-adjusts))
                  (let ((canvas-coords (mds-to-canvas-coords tk coords)))
                    (list (nth 0 canvas-coords)
                          (+ 1
                             coords-dot-size
                             (nth 1 canvas-coords))))
		  (if (member i disconnected-points)
		      "#eeeeee"                 ;; LINK color also in reset-point-color and mds-visualization.tk and above
		    (if (member i unmoveable-coords)
			"#777777"               ;; LINK color also in reset-point-color and mds-visualization.tk and above
		      coords-name-color))
		  (tk-font coords-name-size))
	  ;; (tk-put tk "set itemColor(~d) ~a" item coords-color)  removed 20041011, have the mkCircle etc. color set it
	  (setq item (+ item 1))
	  )
    (if (> num-dimensions 2)
	(progn
	  (set-mds-window-first-axis-canvas-item tk item)
	  (loop for dimension below num-dimensions 
	      for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
		(tk-put tk "mkLine ~{~d ~} axis" (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
		(setq item (+ item 1)))))
    '(loop for axis in (get-basis-vectors-as-axes-lines) do
      (tk-put tk "mkLine ~{~d ~} basis" (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
      (setq item (+ item 1)))
    (set-mds-window-first-dimension-pairs-canvas-item tk item)
    (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) for x from 475 by 20 do
	  (tk-put tk "mkText ~s ~d 25 dimensionPair sw black ~a" (dimension-pair-to-integer dimension-pair) x (tk-font 11))
	  (setq item (+ item 1)))
    (tk-put tk "mkText ~s ~d 25 documentation sw black ~a" 
	    (if title title "")    ;; so that we have a predictable number of items, whether there is a title or not
	    (+ 180 (* 30 (length (combs 2 (series 0 (dec num-dimensions))))) 30)
	    (tk-font 11))
    (setq item (+ item 1))
    (if show-scale-with-length
	(let ((length (if (listp show-scale-with-length)
			  (nth 0 show-scale-with-length)
			show-scale-with-length))
	      (tic-interval (if (listp show-scale-with-length)
				(nth 1 show-scale-with-length)
			      show-scale-with-length)))
	  (let ((mds-length (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
					       (mds-to-canvas-coords tk (list length 0))))
		(mds-tic-interval (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
						     (mds-to-canvas-coords tk (list tic-interval 0)))))
	    ;; make the ruler
	    mds-length ;; to stop compiler warning
	    ;;(tk-put tk "mkLine 10 30 ~d 30 scale" (+ 10 mds-length))
	    ;;(setq item (+ item 1))
	    ;;(loop for x from 10 to (* 1.01 (+ 10 mds-length)) by mds-tic-interval do   ;; the * 0.01 is just to get the last tic
	    ;;     (tk-put tk "mkLine ~d 30 ~d 34 scale" x x)
	    ;;     (setq item (+ item 1)))
		       
	    ;; make the grid lines
	    ;; the * 2 for the number of scale lines is a guess at a safeish upper bound on how many we will need
	    ;; the * 2 for the length of the lines corresponds to making the mds window 2x larger and still seeing the 
	    ;;   lines all the way to the outside
	    (set-num-vertical-grid-lines   tk (* 2 (ceiling (/ (get-canvas-width tk) mds-tic-interval))))
	    (set-num-horizontal-grid-lines tk (* 2 (ceiling (/ (get-canvas-height tk) mds-tic-interval))))
	    (loop for x from 0 to (inc (* mds-tic-interval (get-num-vertical-grid-lines tk))) by mds-tic-interval do
	    ;;(loop for x from 0 to (inc (* mds-tic-interval (get-num-vertical-grid-lines tk))) by (* 2.5 mds-tic-interval) do
		  ;;(tk-put tk "mkColoredLine ~d 30 ~d ~d grid #eeeeee" x x (* 2 (get-canvas-height tk)))   ;; the orignial
		  (tk-put tk "mkColoredLine ~d 30 ~d ~d grid #bbbbbb" x x (* 2 (get-canvas-height tk)))   ;; for gui
		  ;;(tk-put tk "mkColoredLine ~d 30 ~d ~d grid #555555" x x (* 2 (get-canvas-height tk)))   ;; for presentations
		  (setq item (+ item 1)))
	    (loop for y from 30 to (inc (* mds-tic-interval (get-num-horizontal-grid-lines tk))) by mds-tic-interval do
	    ;;(loop for y from 30 to (inc (* mds-tic-interval (get-num-horizontal-grid-lines tk))) by (* 2.5 mds-tic-interval) do
		  ;;(tk-put tk "mkColoredLine 0 ~d ~d ~d grid #eeeeee" y (* 2 (get-canvas-width tk)) y)     ;; the orignal
		  (tk-put tk "mkColoredLine 0 ~d ~d ~d grid #bbbbbb" y (* 2 (get-canvas-width tk)) y)     ;; for gui
		  ;;(tk-put tk "mkColoredLine 0 ~d ~d ~d grid #555555" y (* 2 (get-canvas-width tk)) y)     ;; for presentations
		  (setq item (+ item 1)))

            
            '(let* ((x-lower 100)
                   (y-middle 50)
                   (x-upper (+ x-lower 0.0001 (* mds-tic-interval 4)))
                   (y-lower (- y-middle 3))
                   (y-upper (+ y-middle 3))
                   (y-middle (av (list y-lower y-upper))))
              (tk-put tk "mkColoredLine ~d ~d ~d ~d scalebar #bbbbbb" x-lower y-middle x-upper y-middle)
              (tk-put tk "mkText ~s ~d ~d scalebar sw #bbbbbb ~a" "Antigenic distance" x-lower (- y-upper 5) (tk-font 11))
              (loop for x from x-lower to x-upper by mds-tic-interval for i from 0 do
                    (progn
                      (tk-put tk "mkColoredLine ~d ~d ~d ~d scalebar #bbbbbb" x y-upper x y-middle)
                      (tk-put tk "mkText ~s ~d ~d scalebar n #bbbbbb ~a" 
                              (anything->string i)
                              x (+ y-upper 2) (tk-font 11))
                      '(tk-put tk "mkText ~s ~d ~d scalebar n #bbbbbb ~a" 
                              (string-append (anything->string (round (expt 2 i))) "-fold")
                              x (+ y-upper 2) (tk-font 11))
                      )))
            
            '(let* ((x-lower 140)
                   (y-middle 430)
                   (x-upper (+ x-lower 0.0001 (* mds-tic-interval 1)))
                   (y-lower (- y-middle 2))
                   (y-upper (+ y-middle 2))
                   (y-middle (av (list y-lower y-upper))))
              (tk-put tk "mkColoredLine ~d ~d ~d ~d scalebar #bbbbbb" x-lower y-middle x-upper y-middle)
              (tk-put tk "mkText ~s ~d ~d scalebar n #bbbbbb ~a" "1.0" (+ x-lower (* 0.5 mds-tic-interval)) (+ 3 y-upper) (tk-font 11))
              (loop for x from x-lower to x-upper by mds-tic-interval for i from 0 do
                    (progn
                      (tk-put tk "mkColoredLine ~d ~d ~d ~d scalebar #bbbbbb" x y-upper x y-lower)
                      )))

	    )))

    ;; make rectangle for selecting points (make it once and reuse because of the error line problem below)
    (tk-put tk "mkSelectionRectangle .c ~{~d ~} ~d #000000" 
	    '(-10000 -10000)
	    0)
    (setq item (+ item 1))

    (set-first-error-line-item tk item)  ;; so update-error-lines can be called from move-point-by-mouse
    ;; but problem if i make a ruler before i toggle error lines
    ;; (ok we no longer have the ruler)
    ;; or if we make an elastic rectangle for selecting 
    ;;   (tmp fix by makeing the rectangle first, then just resizing it)
    ;; we should really have a way to get the tk-id when we make
    ;; the first error lines
	       
    (if show-error-lines
	(if (get-error-lines-made-p tk)  
	    (error "i would have expected that error lines could not have been made at this point")
	  (progn
	    (make-error-lines tk coordss more)
	    (set-error-lines-made-p tk t))))
    (tk-put tk "set-canvasToMdsScaleFactor-and-update-rules ~d" (canvas-to-mds-scale-factor tk))
    
    (if procrustes-data
	(draw-arrows-on-mds-window
	 tk
	 coords-full-names
	 coordss
	 (nths 0 procrustes-data)
	 (nths 1 procrustes-data)
	 ;; ignore arrow color for now, needs to be a color for each of the master, not slave entries
	 ;; and no need to implement it now , as we've not used.
	 :set-procrustes-data nil
	 ))
    
    (if raise-points (raise-strains-from-strain-names tk raise-points :ignore-if-name-does-not-exist t))
    (if lower-points (lower-strains-from-strain-names tk lower-points :ignore-if-name-does-not-exist t))
    ))


(defun visualize-mds-coordss-update (tk coordss coords-names coords-colors stress 
				     passed-in-coordss
				     more
				     row-adjusts
				     col-adjusts
				     num-dimensions
				     &key title
					  coords-name-sizes
					  coords-dot-sizes
                                          coords-transparencies
					  coords-name-colors
					  scale-to-fit-mds-window
					  show-error-lines
					  show-stress-components
					  show-scale-with-length
					  unmoveable-coords
					  coords-outline-colors
					  coords-shapes
					  bypass-color-update-inhibition
                                          raise-points
                                          lower-points
					  )

  coords-transparencies  ;; do nothing for display with these for now in mds map-window, just keep the values for passing to pymol

  (let ((item 1))
    (if stress (tk-put tk "setTextText ~7a ~a" item (if (numberp stress) (dps stress 4) stress)))
    (setq item (+ item 1))

    (setq item (+ item 1)) ;; the run-indicator

    (show-table-prediction-summary-in-mds-window tk passed-in-coordss)
    (setq item (+ item 1))

    (if scale-to-fit-mds-window
	(scale-to-fit-mds-window tk coordss))
    (translate-to-fit-mds-window tk coordss)

    ;;move the coordss to their new positions (and also now scaled to fit in the window)
    (loop for coords in coordss 
	for coords-name in coords-names
	for coords-shape in coords-shapes
	for coords-color in coords-colors
	for old-coords-color in (get-coords-colors (get-table-window-for-mds-window tk))
	for coords-dot-size in coords-dot-sizes
	for i from 0 do
	  (if (listp coords-shape)
	      (tk-put tk "setIdXYs ~d ~a" item (polygon-tk-params tk coords coords-shape))
            (if (or (equal coords-shape 'triangle) (equal coords-shape 'down-triangle))  ;; triangle is implemented as polygon
                ;; known error here, and in update-changes-only below.  if we change the shape of points in the gui to triangle, and don't
                ;; launch a new window, this code tries to move points as if they are triangles, when they are actually what they were before,
                ;; which, if that was anything other than blobs, will results in an error like
                ;; "!...EXC... wrong # coordinates: expected 0 or 4, got 6 (when trying to eval 'setIdXYs 68 {181.74658 413.1826 189.74658 413.1826 185.74658 405.1826 }')."
                ;; because the canvas item is not a polygon
                (tk-put tk "setIdXYs ~d ~a" item (polygon-tk-params tk coords 
                                                                    (list 'polygon
                                                                          (if (equal coords-shape 'triangle)
                                                                              (list (list (- coords-dot-size) (+ coords-dot-size))
                                                                                    (list (+ coords-dot-size) (+ coords-dot-size))
                                                                                    (list 0                   (- coords-dot-size)))
                                                                            (list (list (- coords-dot-size) (- coords-dot-size))
                                                                                  (list (+ coords-dot-size) (- coords-dot-size))
                                                                                  (list 0                   (+ coords-dot-size)))))))
              (tk-put tk "setIdXYR_NoRGiven ~d ~{~d ~}" item  (mds-to-canvas-coords tk coords))))

	  ;; set coords color
	  (if (or bypass-color-update-inhibition
		  (not (equal coords-color old-coords-color)))   ;; why do we have this?, when i figure it out i can avoid the bypass
	      (tk-put tk "setIdFill ~d ~a" item coords-color))   ;; which was introduced when we set the color from the new plot-spec setting
	  ;; (tk-put tk "set itemColor(~d) ~a" item coords-color)  removed 20041011, have the mkCircle etc. color set it

	  ;; we could do shw-stress-coponents here, but do all at once below so we can set the range
	  (setq item (+ item 1))
	  (tk-put tk "setTextXY ~d ~{~d ~}" item (let ((canvas-coords (mds-to-canvas-coords tk coords)))
                                                   (list (nth 0 canvas-coords)
                                                         (+ 1
                                                            coords-dot-size
                                                            (nth 1 canvas-coords)))))
	  (tk-put tk "setTextText ~d ~s" item 
		  (coords-name-plus-adjusts coords-name (nth i row-adjusts) (nth i col-adjusts)))
	  (setq item (+ item 1)))
    (if show-stress-components
	(set-point-colors-based-on-stress tk coordss more :average-stress show-stress-components))
    (if (> num-dimensions 2)
	(loop for dimension below num-dimensions for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
	      (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
	      (setq item (+ item 1))))
    '(loop for axis in (get-basis-vectors-as-axes-lines) do
      (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
      (setq item (+ item 1)))
    (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) do
	  ;;dimension-pair
	  (setq item (+ item 1)))
    (if title
	(tk-put tk "setTextText ~d ~s" item title))
    (setq item (+ item 1))
    (if show-scale-with-length
	(let ((length (if (listp show-scale-with-length)
			  (nth 0 show-scale-with-length)
			show-scale-with-length))
	      (tic-interval (if (listp show-scale-with-length)
				(nth 1 show-scale-with-length)
			      show-scale-with-length)))
	  (let ((mds-length (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
					       (mds-to-canvas-coords tk (list length 0))))
		(mds-tic-interval (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
						     (mds-to-canvas-coords tk (list tic-interval 0)))))

	    ;; scale
	    mds-length ;; to stop compiler warning
	    ;;(tk-put tk "setIdXYXY ~d 10 30 ~d 30" item (+ 10 mds-length))
	    ;;(setq item (+ item 1))
	    ;;(loop for x from 10 to (* 1.01 (+ 10 mds-length)) by mds-tic-interval do   ;; the * 0.01 is just to get the last tic
	    ;;     (tk-put tk "setIdXYXY ~d ~d 30 ~d 34" item x x)
	    ;;     (setq item (+ item 1)))
		       
	    ;; grid lines
	    (loop for x from 0 to (inc (* mds-tic-interval (get-num-vertical-grid-lines tk))) by mds-tic-interval do
		  (tk-put tk "setIdXYXY ~d ~d 30 ~d ~d" item x x (* 2 (get-canvas-height tk)))
		  (setq item (+ item 1)))
	    (loop for y from 30 to (inc (* mds-tic-interval (get-num-horizontal-grid-lines tk))) by mds-tic-interval do
		  (tk-put tk "setIdXYXY  ~d 0 ~d ~d ~d" item y (* 2 (get-canvas-width tk)) y)
		  (setq item (+ item 1)))
	    )))

    (setq item (+ item 1))   ;; the selectionRectangle

    ;;(set-first-error-line-item tk item)  ;; so update-error-lines can be called from move-point-by-mouse
    (if show-error-lines
	(setq item (update-error-lines tk coordss more item)))
    (tk-put tk "set-canvasToMdsScaleFactor-and-update-rules ~d" (canvas-to-mds-scale-factor tk))

    (if raise-points (raise-strains-from-strain-names tk raise-points :ignore-if-name-does-not-exist t))
    (if lower-points (lower-strains-from-strain-names tk lower-points :ignore-if-name-does-not-exist t))

    ))


(defun visualize-mds-coordss-update-changes-only (tk coordss coords-names coords-colors stress 
						  passed-in-coordss
						  more
						  row-adjusts
						  col-adjusts
						  num-dimensions
						  &key title
						       coords-name-sizes
						       coords-dot-sizes
                                                       coords-transparencies
						       coords-name-colors
						       scale-to-fit-mds-window
						       show-error-lines
						       show-stress-components
						       show-scale-with-length
						       unmoveable-coords
						       coords-outline-colors
						       coords-shapes
						       bypass-color-update-inhibition
                                                       raise-points
                                                       lower-points
						       )

  coords-transparencies  ;; do nothing for display with these for now in mds map-window, just keep the values for passing to pymol

  (let ((item 1))
    (if stress (tk-put tk "setTextText ~7a ~a" item (if (numberp stress) (dps stress 4) stress)))
    (setq item (+ item 1))

    (setq item (+ item 1)) ;; the run-indicator

    (show-table-prediction-summary-in-mds-window tk passed-in-coordss)
    (setq item (+ item 1))

    (let* ((old-coordss (get-mds-coordss tk))
	   (old-row-adjusts (extract-row-adjusts-from-coordss 
			     (nth-value 1 (deconstruct-coordss-plus-more old-coordss))))
	   (old-col-adjusts (extract-col-bases-from-coordss 
			     (nth-value 1 (deconstruct-coordss-plus-more old-coordss)))))
      (loop for coords in coordss 
	  for old-coords in old-coordss 
	  for coords-name in coords-names
	  for coords-shape in coords-shapes
	  for coords-color in coords-colors
	  for old-coords-color in (get-coords-colors (get-table-window-for-mds-window tk))
          for coords-dot-size in coords-dot-sizes
	  for i from 0
	  do
	    (if (not (equal coords old-coords))
		(if (listp coords-shape)
		    (tk-put tk "setIdXYs ~d ~a" 
			    item
			    (polygon-tk-params tk coords coords-shape))

                  (if (or (equal coords-shape 'triangle) (equal coords-shape 'down-triangle))  ;; triangle is implemented as polygon
                      (tk-put tk "setIdXYs ~d ~a" item (polygon-tk-params tk coords 
                                                                          (list 'polygon
                                                                                (if (equal coords-shape 'triangle)
                                                                                    (list (list (- coords-dot-size) (+ coords-dot-size))
                                                                                          (list (+ coords-dot-size) (+ coords-dot-size))
                                                                                          (list 0                   (- coords-dot-size)))
                                                                                  (list (list (- coords-dot-size) (- coords-dot-size))
                                                                                        (list (+ coords-dot-size) (- coords-dot-size))
                                                                                        (list 0                   (+ coords-dot-size)))))))
                    (tk-put tk "setIdXYR_NoRGiven ~d ~{~d ~}" item  (mds-to-canvas-coords tk coords)))))

	    ;; set coords color
	    (if (or bypass-color-update-inhibition
		    (not (equal coords-color old-coords-color)))
		(tk-put tk "setIdFill ~d ~a" item coords-color))
	    ;;(tk-put tk "set itemColor(~d) ~a" item coords-color)   removed 20041011, have the mkCircle etc. color set it

	    ;; we could do shw-stress-coponents here, but do all at once below so we can set the range
	    (setq item (+ item 1))
	    (if (not (equal coords old-coords))
		(tk-put tk "setTextXY ~d ~{~d ~}" item (let ((canvas-coords (mds-to-canvas-coords tk coords)))
                                                         (list (nth 0 canvas-coords)
                                                               (+ 1
                                                                  coords-dot-size
                                                                  (nth 1 canvas-coords))))))
	    (if (or (not (eql (nth i row-adjusts) (nth i old-row-adjusts)))  ;; eql here so that if no adjusts, then nil, and eql
		    (not (eql (nth i col-adjusts) (nth i old-col-adjusts))))
		(tk-put tk "setTextText ~d ~s" item 
			(coords-name-plus-adjusts coords-name (nth i row-adjusts) (nth i col-adjusts))))
	    (setq item (+ item 1))))
    (if show-stress-components
	(set-point-colors-based-on-stress tk coordss more :average-stress show-stress-components))
    (if (> num-dimensions 2)
	(loop for dimension below num-dimensions for axis in (make-standard-vectors-as-axes-lines num-dimensions) do
	      (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
	      (setq item (+ item 1))))
    '(loop for axis in (get-basis-vectors-as-axes-lines) do
      (tk-put tk "setIdXYXY ~d ~{~d ~}" item (flatten (mapcar (^ (coords) (mds-to-canvas-coords tk coords)) axis)))
      (setq item (+ item 1)))
    (loop for dimension-pair in (combs 2 (series 0 (dec num-dimensions))) do
	  ;;dimension-pair
	  (setq item (+ item 1)))
    (if title
	(tk-put tk "setTextText ~d ~s" item title))
    (setq item (+ item 1))
    (if show-scale-with-length
	(let ((length (if (listp show-scale-with-length)
			  (nth 0 show-scale-with-length)
			show-scale-with-length))
	      (tic-interval (if (listp show-scale-with-length)
				(nth 1 show-scale-with-length)
			      show-scale-with-length)))
	  (let ((mds-length (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
					       (mds-to-canvas-coords tk (list length 0))))
		(mds-tic-interval (hypotenuse-length (mds-to-canvas-coords tk (list 0 0))
						     (mds-to-canvas-coords tk (list tic-interval 0)))))
	    ;; scale
	    mds-length ;; to stop compiler warning
	    ;;(tk-put tk "setIdXYXY ~d 10 30 ~d 30" item (+ 10 mds-length))
	    ;;(setq item (+ item 1))
	    ;;(loop for x from 10 to (* 1.01 (+ 10 mds-length)) by mds-tic-interval do   ;; the * 0.01 is just to get the last tic
	    ;;     (tk-put tk "setIdXYXY ~d ~d 30 ~d 34" item x x)
	    ;;     (setq item (+ item 1)))
		       
	    ;; grid lines
	    (loop for x from 0 to (inc (* mds-tic-interval (get-num-vertical-grid-lines tk))) by mds-tic-interval do
		  (tk-put tk "setIdXYXY ~d ~d 30 ~d ~d" item x x (* 2 (get-canvas-height tk)))
		  (setq item (+ item 1)))
	    (loop for y from 30 to (inc (* mds-tic-interval (get-num-horizontal-grid-lines tk))) by mds-tic-interval do
		  (tk-put tk "setIdXYXY  ~d 0 ~d ~d ~d" item y (* 2 (get-canvas-width tk)) y)
		  (setq item (+ item 1)))
	    )))

    (setq item (+ item 1))   ;; the selectionRectangle

    ;;(set-first-error-line-item tk item)  ;; so update-error-lines can be called from move-point-by-mouse
    (if show-error-lines
	(setq item (update-error-lines tk coordss more item)))
    ;;(tk-put tk "set canvasToMdsScaleFactor ~d" (canvas-to-mds-scale-factor tk))
    (tk-put tk "set-canvasToMdsScaleFactor-and-update-rules ~d" (canvas-to-mds-scale-factor tk))

    (if raise-points (raise-strains-from-strain-names tk raise-points :ignore-if-name-does-not-exist t))
    (if lower-points (lower-strains-from-strain-names tk lower-points :ignore-if-name-does-not-exist t))

    ))


(defun visualize-mds-coordss (tk new-or-update? coordss coords-names coords-colors stress 
			      &key title
				   (coords-name-sizes (loop for n in coords-names collect (progn n 10)))
				   (coords-dot-sizes  (if (and (get-table-window-for-mds-window tk)
                                                               (get-coords-dot-sizes (get-table-window-for-mds-window tk)))
                                                          (get-coords-dot-sizes (get-table-window-for-mds-window tk))
                                                        (loop for n in coords-names collect (progn n 4))))
                                   (coords-transparencies (loop for n in coords-names collect (progn n 0.0)))
				   (coords-name-colors coords-colors)
				   (scale-to-fit-mds-window 
				    (if (get-table-window-for-mds-window tk)
					(get-scale-to-fit-mds-window (get-table-window-for-mds-window tk))
				      t))
				   (show-error-lines (get-show-error-lines (get-table-window-for-mds-window tk)))
				   (show-stress-components (get-show-stress-components tk))
				   (show-scale-with-length '(7 1))
				   unmoveable-coords
				   (coords-outline-colors (if (get-table-window-for-mds-window tk)
							      (get-coords-outline-colors (get-table-window-for-mds-window tk))
							    t))
				   (coords-shapes (if (get-coords-shapes tk)
						      (get-coords-shapes tk)  ;; get for mds-window if set (blob plot)
						    (if (get-table-window-for-mds-window tk)  ;; otherwise get for table-window if set
							(get-coords-shapes (get-table-window-for-mds-window tk)))))
                    				   ;; default for coords-shapes is also set further down -- not any more, now set in make-master-mds-window (2004-08-24)
				   bypass-color-update-inhibition
				   (procrustes-data (get-procrustes-data tk))
				   coords-full-names
				   (blob-steps 20)
				   (blob-power 3)
                                   (raise-points (get-raise-points tk))
                                   (lower-points (get-lower-points tk))
				   )
  
  coords-transparencies  ;; do nothing for display with these for now in mds map-window, just keep the values for passing to pymol

  ;;(if (not coords-shapes)
  ;;    (setq coords-shapes
  ;;	(loop for name in (hi-table-antigens (get-hi-table (get-table-window-for-mds-window tk))) collect
  ;;	      (if (sr-name-p name)
  ;;		  'rectangle
  ;;		'circle))))

  (let ((passed-in-coordss coordss))
    (multiple-value-bind (coordss more)
	(deconstruct-coordss-plus-more coordss)
      (let ((col-adjusts (extract-col-bases-from-coordss more))
	    (row-adjusts (extract-row-adjusts-from-coordss more)))

	;; this slows things down, and needs to be coordinated globally with whether we are showing predictions
	;; in tk-interface we should set a slot on the object for if we are showing predictions, and if we 
	;; are showing them on the fly
	;; and should have the speed up for memoized 
	;;(let ((table-window (get-table-window-for-mds-window tk)))
	;;  (if (get-display-errors-in-lower-triangle table-window)
	;;      (show-table-predictions table-window passed-in-coordss)))

	;; now in the mds-visualization
	;;(ppl (list (list (mapcar #'2dp col-adjusts)
	;;		 (mapcar #'2dp row-adjusts))))
	
	;;CAREFUL if if change the number of items created, or the order because
	;;the function get-dimension-pair-from-tk-id depends on recalculating the order and number
	;;actually, need to be careful with anything with <anything>from-tk-id
	(if (or (= (length (car coordss)) 2)
		(get-canvas-basis-vector-0 tk))
	    nil
	  (error "Visualization only works on 2d projection, so either use 2d MDS or define basis vectors for a projection into 2d"))

	(if (get-basis-vector-point-indices tk)
	    (if (eql new-or-update? 'update-changes-only)
		(loop for coords in coordss for old-coords in (get-mds-coordss tk) for index from 0 do
		      (if (and (not (equal coords old-coords)) (member index (get-basis-vector-point-indices tk)))
			  (return (progn
				    ;;a point used to compute the basis vectors has changed
				    ;;all the basis vectors must be recomputed, and all points updated
				    (setq new-or-update? 'update)
				    (apply #'set-basis-vectors-from-point-indices tk coordss (get-basis-vector-point-indices tk))
				    ))))
	      (apply #'set-basis-vectors-from-point-indices tk coordss (get-basis-vector-point-indices tk))))
	
	(let ((num-dimensions (length (car coordss))))
	  (case new-or-update?
	    (new 
	     (visualize-mds-coordss-new
	      tk coordss coords-names coords-colors stress 
	      passed-in-coordss
	      more
	      row-adjusts
	      col-adjusts
	      num-dimensions
	      :title title
	      :coords-name-sizes coords-name-sizes
	      :coords-dot-sizes coords-dot-sizes
	      :coords-name-colors coords-name-colors
	      :scale-to-fit-mds-window scale-to-fit-mds-window
	      :show-error-lines show-error-lines
	      :show-stress-components show-stress-components
	      :show-scale-with-length show-scale-with-length
	      :unmoveable-coords unmoveable-coords
	      :coords-outline-colors coords-outline-colors
	      :coords-shapes coords-shapes
	      :procrustes-data procrustes-data       ;; right now only called for new (so will not animate, by choice)
	      :coords-full-names coords-full-names   ;; right now only called for new (so will not animate, by choice)
	      :blob-steps blob-steps
	      :blob-power blob-power
              :raise-points raise-points
              :lower-points lower-points))
	    (update 
	     (visualize-mds-coordss-update
	      tk coordss coords-names coords-colors stress 
	      passed-in-coordss
	      more
	      row-adjusts
	      col-adjusts
	      num-dimensions
	      :title title
	      :coords-name-sizes coords-name-sizes
	      :coords-dot-sizes coords-dot-sizes
	      :coords-name-colors coords-name-colors
	      :scale-to-fit-mds-window scale-to-fit-mds-window
	      :show-error-lines show-error-lines
	      :show-stress-components show-stress-components
	      :show-scale-with-length show-scale-with-length
	      :unmoveable-coords unmoveable-coords
	      :coords-outline-colors coords-outline-colors
	      :coords-shapes coords-shapes
	      :bypass-color-update-inhibition bypass-color-update-inhibition
              :raise-points raise-points
              :lower-points lower-points))
	    (update-changes-only 
	     (visualize-mds-coordss-update-changes-only
	      tk coordss coords-names coords-colors stress 
	      passed-in-coordss
	      more
	      row-adjusts
	      col-adjusts
	      num-dimensions
	      :title title
	      :coords-name-sizes coords-name-sizes
	      :coords-dot-sizes coords-dot-sizes
	      :coords-name-colors coords-name-colors
	      :scale-to-fit-mds-window scale-to-fit-mds-window
	      :show-error-lines show-error-lines
	      :show-stress-components show-stress-components
	      :show-scale-with-length show-scale-with-length
	      :unmoveable-coords unmoveable-coords
	      :coords-outline-colors coords-outline-colors
	      :coords-shapes coords-shapes
	      :bypass-color-update-inhibition bypass-color-update-inhibition
              :raise-points raise-points
              :lower-points lower-points))
	    (t (error "end of case"))))
	(set-mds-coordss tk passed-in-coordss)))))


;;;----------------------------------------------------------------------
;;;                      write on a map window
;;;----------------------------------------------------------------------

(defun add-circle-to-map-window-centered-on-xy (tk x y &optional &key radius (outline-color "black") (fill-color "{}"))
  (tk-put tk "mkCircleColor .c ~a ~a ~d ~d ~d"
          outline-color
          fill-color
          x 
          y
          radius))

(defun add-circle-to-map-window-centered-on-name (tk name &optional &key radius (outline-color "black") (fill-color "{}"))
  (let ((mds-coords (mds-to-canvas-coords
                     tk
                     (let* ((table-window (get-table-window-for-mds-window tk))
                            (coordss (coordss (get-mds-coordss tk)))
                            (table (get-hi-table table-window))
                            (strains (hi-table-antigens table)))
                       (if (position name strains)
                           (nth (position name strains) coordss)
                         (error "No such point ~a" name))))))
    (add-circle-to-map-window-centered-on-xy
     tk
     (nth 0 mds-coords)
     (nth 1 mds-coords)
     :radius        (mds-to-canvas-scale-scalar tk radius)
     :outline-color outline-color
     :fill-color    fill-color
     )))

(defun add-circles-to-map-window-centered-on-names (tk names &optional &key radius (outline-color "black") (fill-color "{}"))
  (loop for name in names collect
        (add-circle-to-map-window-centered-on-name
         tk
         name
         :radius        radius
         :outline-color outline-color
         :fill-color    fill-color
         )))


;;;----------------------------------------------------------------------
;;;                      error lines
;;;----------------------------------------------------------------------

(defun adjust-table-value (table-value col-base row-adjust)
  ;; same adjust as in the conj-gradient optimization
  (- col-base (+ table-value row-adjust)))

;; this supersedes the above as it takes the col and row adjusts (so we can do the <10 properly)
;; and we need them anyhow to do the negative distance properly
(defun error-line-endpoint (target-value from-coords to-coords similarity-table-p target-col-base target-row-adjust)
  (let* ((actual-distance (e-dist from-coords to-coords))
	 (target-distance (if (true-dont-care-p target-value)
			      (error "assumption violation, did not expect a true dont-care here")
			    (if similarity-table-p
				(if (thresholdp target-value)
				    ;; note the dec here on the threshold number.  this is because in metric-mds-conjugant-gradiet
				    ;; we do a dec in f and df on the threshold.  ie we treat a <10 really as a <5.  maybe we should
				    ;; really treat it as a <7.5 (or the geometric equivalent).  it is not a <10, because of rounding.
				    ;; it really does not matter most likely.
				    (adjust-table-value (dec (threshold-number target-value)) target-col-base target-row-adjust)
				  (adjust-table-value target-value target-col-base target-row-adjust))
			      (if (thresholdp target-value)
				  (error "assumption violation, did not expect a thresholded value in a non-ag-sr-table")
				target-value))))
	 (half-error (/ (- actual-distance target-distance) 2))   ;; +ve if actual is too long (so we need to move closer)
	 (half-error-non-if-threshold-and-too-far (if (and (plusp half-error) (thresholdp target-value)) 0 half-error)))
                ;; do not show the error line when beyond threshold
    (values
     (coords-on-way-to-point half-error-non-if-threshold-and-too-far from-coords to-coords actual-distance)
     (if (minusp half-error)
	 'too-close
       (if ;;(and (not (less-than-ten-p target-distance)) (minusp target-distance))  ;; if <10, and plus, then error is zero, so ignore
	   (minusp target-distance)
	   'negative-target
	 'too-far)))))

(defun coords-on-way-to-point (distance-to-move from-point to-point &optional distance-between-points)
  ;; +ve distance means towards the to-point, -ve means away from it
  ;; distance-between-points is an optimization, it is not needed
  (if (not distance-between-points)
      (setq distance-between-points (e-dist from-point to-point)))
  (if (and (zerop distance-between-points)
	   (not (zerop distance-to-move)))
      (format t "~%Warning: drawing error line, but do not know which direction to draw (direction points are conincident).")
    (loop for from-coord in from-point
	for to-coord in to-point collect
	  (+ from-coord
	     (* (- to-coord from-coord)
		(/ distance-to-move distance-between-points))))))
  
#|
;; this file is not the place, but this is where the only calls are for now, so have here for now (same with f below)
(defun log-hi-table-values-adjust-rows-and-cols (log-hi-table-values column-bases row-adjusts)
;;  UGLY, hack for now to work only on values as the sera is not set on the working-hi-table i pass in
;;  (make-hi-table
;;   (hi-table-antigens log-hi-table)
;;   (hi-table-sera log-hi-table)
   (loop for row in log-hi-table-values for r from 0 collect
	 (loop for log-titer in row for c from 0 collect
	       (if (dont-care-p log-titer)
		   log-titer
		 (+ (+ log-titer (- 7 (nth c column-bases)))
		    (nth r row-adjusts))))))

;; this file is not the place, but this is where the only calls are for now, so have here for now (same with f above)
(defun log-hi-table-values-to-distance-matrix (log-hi-table-values &optional column-bases row-adjusts)
  ;; do this on hi-table-values because this next call wants to set sera, and there is no sera set,
  ;; and i don't want to set sera, because of efficiency reasons (though i've not really checked to see if there are any)
  (setq log-hi-table-values (log-hi-table-values-adjust-rows-and-cols log-hi-table-values column-bases row-adjusts))
  (f-elements
   (^ (log-titer)
      (if (dont-care-p log-titer)
	  log-titer
	(progn
	  (if (minusp (- 7 log-titer))   ;; ugh, some will be the percentage errors, not real hi values
	      (format t "negative distance from log-titer ~d~%" log-titer))
	  (max 0                   ;; don't know what to do with negative distances
	       (- 7 log-titer)))))
   log-hi-table-values))
|#

(defun log-hi-table-values-to-distance-matrix (log-hi-table-values &optional column-bases row-adjusts)
  (loop for row in log-hi-table-values 
	for row-index below (length log-hi-table-values)
        for row-adjust in row-adjusts collect
	(loop for element in row 
	      for col-index below (length row)
	      for col-base in column-bases collect
	      (if (or (dont-care-p element)        ;; dont-care or <10 (take care of <10 later) (SHOULD DEPEND ON COL ADJ)
		      (<= col-index row-index))    ;; the lower triangle, which contains the error percentages
		  element
		(let ((adjusted-element (adjust-table-value element col-base row-adjust)))
		  (if (minusp adjusted-element)
		      (format t "negative target ~d~%" adjusted-element))
		  adjusted-element)))))
  
(defvar *off-screen-coordinate*)
(setq *off-screen-coordinate* -1234567)

(defun filter-connetion-error-prediction-line-data-leaving-only-displaying-data (connetion-error-prediction-line-data)
  (let ((filtered-nth-1
	 (filter 
	  (^ (name-data) (null (nth 1 name-data)))
	  (loop for (name data) in (nth 1 connetion-error-prediction-line-data) collect
		(list name
		      (loop for (coords color) in data
			  when (and coords
				    ;; just use one coord below so we don't have to add the (minor) complexity of figuring out #dims
				    (not (eql (car coords) *off-screen-coordinate*)))
			  collect (list coords color)))))))
    (if filtered-nth-1
	(list
	 (nth 0 connetion-error-prediction-line-data)
	 filtered-nth-1)
      nil)))

(defun calc-error-coords-and-class (make-or-update 
				    tk
				    coordss
				    coordss-more
				    &optional &key 
					      item
					      selected-subset
					      (set-error-connection-prediction-line-data t))
  ;; note, we look at the working-table-value, and the original-table-value, because we are now
  ;; changing working-table-values.  when we create error lines we do so from the original table
  ;; (because we do not want to not create error lines because of a dont-care that will be there
  ;; later when we reconnect a point (we have to deal with this because we dont create error lines
  ;; when there is a dont-care (as an optimization) and we dont-create and destroy error lines
  ;; on the fly (thinking that is an optimization).
  
  (let* ((show-error-lines (get-show-error-lines (get-table-window-for-mds-window tk)))
	 (prediction-lines-only (eql 'prediction-lines-only show-error-lines))
	 (connection-lines-only (eql 'connection-lines-only show-error-lines))
	 (off-screen-coords (loop for i below (length (car coordss)) collect *off-screen-coordinate*))
	 
	 (connetion-error-prediction-line-data
	  (if connection-lines-only
	      ;; right now a major and too much code duplicated switch here, later integrate
	      (let* ((similarity-table-p (similarity-table-p (get-hi-table (get-table-window-for-mds-window tk))))
		     (strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window tk))))
		     (working-table-values (hi-table-values (get-hi-table-working-copy (get-table-window-for-mds-window tk))))
		     (original-table-values (hi-table-values (get-hi-table (get-table-window-for-mds-window tk))))
		     (col-bases (extract-col-bases-from-coordss coordss-more))
		     (row-adjusts (extract-row-adjusts-from-coordss coordss-more))
		     (disconnected-points (get-disconnected-points (get-table-window-for-mds-window tk)))
		     (components
		      (loop for i from 0 for i-coords in coordss collect
			    (loop for j from 0 for j-coords in coordss collect
				  (let* ((working-target-table-value   
					  (nth (max i j) (nth (min i j) working-table-values)))    ;; (pick out of upper triangle)    
					 (original-target-table-value   
					  (nth (max i j) (nth (min i j) original-table-values)))  ;; (pick out of upper triangle)    
					 (original-target-table-value-is-true-dont-care   ;; by true i mean not a <10 but a dont-care
					  (true-dont-care-p original-target-table-value))
					 (working-target-table-value-is-true-dont-care
					  (true-dont-care-p working-target-table-value)))

				    (multiple-value-bind (stress-component stress-class)
					(if (and selected-subset
						 (not (member i selected-subset))
						 (not (member j selected-subset)))
					    (values
					     'not-set
					     'not-set)
					  (if working-target-table-value-is-true-dont-care
					      (values
					       'not-set
					       'not-set)
					    (stress-component working-target-table-value 
							      i-coords
							      j-coords
							      similarity-table-p
							      (nth (max i j) col-bases)
							      (nth (min i j) row-adjusts))))

				      (list working-target-table-value   
					    original-target-table-value   
					    original-target-table-value-is-true-dont-care
					    working-target-table-value-is-true-dont-care
					    stress-component
					    stress-class))))))
		     (max-stress-component (apply-max (filter (^ (x) (eql 'not-set x)) 
							      (nths 4 (apply-append components))))))
				
		(loop for i from 0 for i-coords in coordss for components-row in components for strain-name in strain-names collect
		      (list
		       strain-name
		       (loop for j from 0 for j-coords in coordss for (working-target-table-value   
								       original-target-table-value   
								       original-target-table-value-is-true-dont-care
								       working-target-table-value-is-true-dont-care
								       stress-component
								       stress-class) in components-row collect

			     (if (and (not (= i j))
				      (not original-target-table-value-is-true-dont-care))
				 (prog1
				     (multiple-value-bind (error-coords error-class)
					 (if (and selected-subset
						  (not (member i selected-subset))
						  (not (member j selected-subset)))
					     (values
					      off-screen-coords
					      'not-relevant)
					   (if (or (member i disconnected-points)
						   (member j disconnected-points))
					       (values
						off-screen-coords
						'not-relevant)
					     (if (if prediction-lines-only
						     working-target-table-value-is-true-dont-care
						   (not working-target-table-value-is-true-dont-care))

						 (let ((actual-distance (e-dist i-coords j-coords)))

						   ;; take 8, remove <10s carefully unless they are within range, and color code with shading if too far or close
						   (if (thresholdp working-target-table-value)
						       (if (< actual-distance 
							      ;; note the dec below.  LINK to the error line part of this function.
							      ;; and to metric-mds-conjugant-gradient, stress-component, and error-line-endpoint
							      ;; and calculate-errors
							      (adjust-table-value (dec (threshold-number working-target-table-value)) 
										  (nth (max i j) col-bases) 
										  (nth (min i j) row-adjusts)))
							   (values
							    (coords-on-way-to-point (/ actual-distance 2) i-coords j-coords actual-distance)
							    (list stress-class (/ stress-component max-stress-component)))
							 (values
							  off-screen-coords
							  'not-relevant))
						     (values
						      (coords-on-way-to-point (/ actual-distance 2) i-coords j-coords actual-distance)
						      (list stress-class (/ stress-component max-stress-component)))))

					       (values
						off-screen-coords
						'not-relevant))))

				       (let ((canvas-error-coords
					      (flatten 
					       (mapcar (^ (coords) (mds-to-canvas-coords tk coords))
						       (list
							(if (equal off-screen-coords error-coords)
							    error-coords
							  i-coords)
							error-coords))))
					     (canvas-error-class-color
					      (cond ((eql error-class 'too-close)       "#ff0000")
						    ((eql error-class 'too-far)         "#0000ff")
						    ((eql error-class 'negative-target) "#00ff00")
						    ((eql error-class 'connection)      "#dddddd")
						    ((eql error-class 'not-relevant)    "#ffff00")
						    ((listp error-class)
						     (hsv-tk-color 
						      (case (nth 0 error-class)
							(too-far          0.667)
							(too-close        0.0)
							(negative-target  0.333))
						      (nth 1 error-class)
						      0.95))
						    (t (error "unexpected error-class ~a" error-class)))))
					 (tk-put tk "~a ~{~d ~} ~a" 
						 (case make-or-update
						   (make "mkErrorLine")
						   (update (format nil "setIdXYXYFill ~d" item))
						   (t (error "Unexpected case")))
						 canvas-error-coords
						 canvas-error-class-color)
					 (list
					  error-coords
					  canvas-error-class-color)
					 ))
				   (if item (setq item (inc item))) 
				   ))))))

	    (let* ((similarity-table-p (similarity-table-p (get-hi-table (get-table-window-for-mds-window tk))))
		   (strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window tk))))	     
		   (working-table-values (hi-table-values (get-hi-table-working-copy (get-table-window-for-mds-window tk))))
		   (original-table-values (hi-table-values (get-hi-table (get-table-window-for-mds-window tk))))
		   (col-bases (extract-col-bases-from-coordss coordss-more))
		   (row-adjusts (extract-row-adjusts-from-coordss coordss-more))
		   (disconnected-points (get-disconnected-points (get-table-window-for-mds-window tk))))
	      (loop for i from 0 for i-coords in coordss for strain-name in strain-names collect
		    (list
		     strain-name
		     (loop for j from 0 for j-coords in coordss collect
			   (let* ((working-target-table-value   
				   (nth (max i j) (nth (min i j) working-table-values)))    ;; (pick out of upper triangle)    
				  (original-target-table-value   
				   (nth (max i j) (nth (min i j) original-table-values)))  ;; (pick out of upper triangle)    
				  (original-target-table-value-is-true-dont-care   ;; by true i mean not a <10 but a dont-care
				   (true-dont-care-p original-target-table-value))
				  (working-target-table-value-is-true-dont-care
				   (true-dont-care-p working-target-table-value)))
			     (if (and (not (= i j))
				      (not original-target-table-value-is-true-dont-care))
				 (prog1
				     (multiple-value-bind (error-coords error-class)
					 (if (and selected-subset
						  (not (member i selected-subset))
						  (not (member j selected-subset)))
					     (values
					      off-screen-coords
					      'not-relevant)
					   (if (or (member i disconnected-points)
						   (member j disconnected-points))
					       (values
						off-screen-coords
						'not-relevant)
					     (if (if prediction-lines-only
						     working-target-table-value-is-true-dont-care
						   (not working-target-table-value-is-true-dont-care))
						 (error-line-endpoint (if prediction-lines-only
									  original-target-table-value
									working-target-table-value)
								      i-coords
								      j-coords
								      similarity-table-p
								      (nth (max i j) col-bases)
								      (nth (min i j) row-adjusts))
					       (values
						off-screen-coords
						'not-relevant))))
				       (let ((canvas-error-coords
					      (flatten 
					       (mapcar (^ (coords) (mds-to-canvas-coords tk coords))
						       (list
							(if (equal off-screen-coords error-coords)
							    error-coords
							  i-coords)
							error-coords))))
					     (canvas-error-class-color
					      (hsv-tk-color 
					       (case error-class
						 (too-far          0.667)
						 (too-close        0.0)
						 (negative-target  0.333)
						 (not-relevant     0.85) ; should never see this as should be moved off screen
						 (t (error "unexpected error-class ~a" error-class)))
					       0.5
					       1.0)
					      ;;(case error-class
					      ;;  (too-close       "#ff0000")
					      ;;  (too-far         "#0000ff")
					      ;;  (negative-target "#00ff00")
					      ;;  (not-relevant    "#ffff00") ; should never see this as should be moved off screen
					      ;;  (t (error "unexpected error-class ~a" error-class)))
					      ))
					 (tk-put tk "~a ~{~d ~} ~a" 
						 (case make-or-update
						   (make "mkErrorLine")
						   (update (format nil "setIdXYXYFill ~d" item))
						   (t (error "Unexpected case")))
						 canvas-error-coords
						 canvas-error-class-color)
					 (list
					  error-coords
					  canvas-error-class-color)

					 ))
				   (if item (setq item (inc item))) 
				   ))))))))))
    (if (and set-error-connection-prediction-line-data
	     show-error-lines)
	(set-error-connection-prediction-line-data
	 tk
	 (filter-connetion-error-prediction-line-data-leaving-only-displaying-data
	  (list
	   show-error-lines
	   connetion-error-prediction-line-data))))
    )
  item)



(defun make-error-lines (tk coordss coordss-more &optional &key selected-subset)
  (calc-error-coords-and-class 'make tk coordss coordss-more :selected-subset selected-subset))

(defun update-error-lines (tk coordss coordss-more item &optional &key selected-subset)
  (let ((saved-error-lines-selected-subset (get-error-lines-selected-subset tk)))
    (if selected-subset
	(if (not (equal selected-subset saved-error-lines-selected-subset))
	    (format t "assumption violation in update-error-lines"))
      (setq selected-subset saved-error-lines-selected-subset))
    (let ((item (calc-error-coords-and-class 'update tk coordss coordss-more :item item :selected-subset selected-subset)))
      item)))

(defun hide-error-lines (tk coordss coordss-more item)
  (let* ((original-target-distances 
	  (if (similarity-table-p (get-hi-table (get-table-window-for-mds-window tk)))
	      (log-hi-table-values-to-distance-matrix
	       (hi-table-values
		(get-hi-table (get-table-window-for-mds-window tk)))
	       (extract-col-bases-from-coordss coordss-more)
	       (extract-row-adjusts-from-coordss coordss-more))
	    (hi-table-values (get-hi-table (get-table-window-for-mds-window tk))))))
    (loop for i from 0 for i-coords in coordss do
	  (loop for j from 0 for j-coords in coordss do
		(let ((original-target-distance   ;; pick out of the upper triangle
		       (nth (max i j) (nth (min i j) original-target-distances))))
		  i-coords
		  j-coords
		  (if (and (not (= i j))
			   (not (true-dont-care-p original-target-distance)))    ;; HORRIBLE boolean, simplify!
		      (progn
			(tk-put tk "mvIdXY ~d ~{~d ~}" item '(-1000000 -1000000))
			(setq item (+ item 1))))))))
  item)

(defun set-line-state (mds-window desired-state &optional &key selected-subset-canvas-ids)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (existing-state (get-show-error-lines table-window))
	 (selected-subset (get-mds-point-indices-from-mds-window-canvas-ids mds-window selected-subset-canvas-ids))
	 (previously-selected-subset (get-error-lines-selected-subset mds-window)))
    (set-show-error-lines table-window desired-state)
    (set-error-lines-selected-subset mds-window selected-subset)
    (multiple-value-bind (coordss more)
	(deconstruct-coordss-plus-more (get-mds-coordss mds-window))

      ;; make "error" lines if they are not already made
      (if (and (member desired-state '(error-lines-only prediction-lines-only connection-lines-only))
	       (not (get-error-lines-made-p mds-window)))
	  (progn
	    (make-error-lines mds-window coordss more :selected-subset selected-subset)
	    (set-error-lines-made-p mds-window t))
	(if (and (eql desired-state existing-state)
		 (equal selected-subset previously-selected-subset))
	    'do-nothing  ;; (later pop up a tk window saying that there is nothing to do)
	  (progn
	    (cond ((member desired-state '(error-lines-only prediction-lines-only connection-lines-only))
		   (update-error-lines mds-window coordss more (get-first-error-line-item mds-window) :selected-subset selected-subset))
		  ((null desired-state)
		   (hide-error-lines mds-window coordss more (get-first-error-line-item mds-window)))
		  (t  (error "assumption violation, unecpected desired line state ~a" desired-state)))))))))


;;;----------------------------------------------------------------------
;;;                      set color based on stress
;;;----------------------------------------------------------------------

(defun gui-set-show-stress-components (mds-window state)
  (set-show-stress-components mds-window state)
  (if (get-show-stress-components mds-window)
      (multiple-value-bind (coordss more)
	  (deconstruct-coordss-plus-more (get-mds-coordss mds-window))
	(set-point-colors-based-on-stress mds-window coordss more :average-stress (get-show-stress-components mds-window)))
    (loop for i from (get-mds-window-first-coords-canvas-item mds-window) by 2 
	for loop-terminator below (get-mds-num-coordss mds-window) do
	  (progn
	    loop-terminator ;; to stop compiler warning
	    (reset-point-color mds-window i)))))

(defun reset-point-color (tk item)
  (let ((point (get-mds-point-index-from-mds-window-canvas-id tk item))
	(unmoveable-coords (get-unmoveable-coords (get-table-window-for-mds-window tk)))
	(disconnected-coords (get-disconnected-points (get-table-window-for-mds-window tk))))
    (tk-put tk "setIdFill ~d ~a" 
	    item
	    (if (member point unmoveable-coords)
		"#777777"                                ;; LINK color also in visualize-mds-coords and mds-visualization.tk
	      (if (member point disconnected-coords)
		  "#eeeeee"                              ;; LINK color also in visualize-mds-coords and mds-visualization.tk  
		(nth point
		     (get-coords-colors 
		      (get-table-window-for-mds-window tk))))))))

(defun stress-component (target-value from-coords to-coords similarity-table-p target-col-base target-row-adjust)
  ;; identical to error-line-endpoint, other than we do not calc the endpoints, just return the abs half-error
  (let* ((actual-distance (e-dist from-coords to-coords))
	 (target-distance (if (true-dont-care-p target-value)
			      (error "assumption violation, did not expect a true dont-care here")
			    (if similarity-table-p
				(if (thresholdp target-value)
				    ;; note the dec below.  LINK with error-line-endpoint, the error line master function, and 
				    ;; metric-mds-conjugant-gradient
				    (adjust-table-value (dec (threshold-number target-value)) target-col-base target-row-adjust)
				  (adjust-table-value target-value target-col-base target-row-adjust))
			      (if (thresholdp target-value)
				  (error "assumption violation, did not expect a thresholded value in a non-ag-sr-table")
				target-value))))
	 (half-error (/ (- actual-distance target-distance) 2))   ;; +ve if actual is too long (so we need to move closer)
	 (half-error-non-if-threshold-and-too-far (if (and (plusp half-error) (thresholdp target-value)) 0 half-error)))
                ;; do not show the error line when beyond threshold
    (values
     (abs half-error-non-if-threshold-and-too-far)
     (if (minusp half-error)
	 'too-close
       (if ;;(and (not (less-than-ten-p target-distance)) (minusp target-distance))  ;; if <10, and plus, then error is zero, so ignore
	   (minusp target-distance)
	   'negative-target
	 'too-far)))))

(defun calc-point-stress (tk point-index coordss coordss-more &optional &key average-stress)
  (let* ((similarity-table-p (similarity-table-p (get-hi-table (get-table-window-for-mds-window tk))))
	 (table-values (hi-table-values (get-hi-table-working-copy (get-table-window-for-mds-window tk))))
	 (col-bases (extract-col-bases-from-coordss coordss-more))
	 (row-adjusts (extract-row-adjusts-from-coordss coordss-more))
	 (disconnected-points (get-disconnected-points (get-table-window-for-mds-window tk)))
	 (point-coords (nth point-index coordss)))
    (let ((stress-components
	   (filter #'null
		   (loop for j from 0 for j-coords in coordss collect
			 (let ((target-table-value   ;; (pick out of upper triangle)
				(nth (max point-index j) (nth (min point-index j) table-values))))
			   (if (and (not (= point-index j))
				    (not (true-dont-care-p target-table-value))
				    (not (or (member point-index disconnected-points)
					     (member j disconnected-points))))
			       (multiple-value-bind (stress-component error-class)
				   (stress-component target-table-value
						     point-coords
						     j-coords
						     similarity-table-p
						     (nth (max point-index j) col-bases)
						     (nth (min point-index j) row-adjusts))
				 error-class
				 stress-component)
			     nil))))))
      (if (eql average-stress 'average)
	  (if stress-components
	      (av stress-components)
	    0)
	(apply-+ stress-components)))))

(defun set-point-colors-based-on-stress (mds-window coordss more &key (color-ag-sr-differently 'not-set) average-stress)
  
  (if (eql 'not-set color-ag-sr-differently)
      (setq color-ag-sr-differently
	(ag-sr-table-p (get-hi-table (get-table-window-for-mds-window mds-window)))))      

  (let* ((point-stresses 
	  (loop for point below (get-mds-num-coordss mds-window) collect
		(calc-point-stress mds-window point coordss more :average-stress average-stress)))
	 (point-stresses-in-range-0-to-1 (scale-zero-based-data-so-max-is-1 
					  point-stresses
					  :if-max-less-than-this-value-return-original-list SINGLE-FLOAT-EPSILON))
	 (antigens (hi-table-antigens (get-hi-table-working-copy (get-table-window-for-mds-window mds-window)))))

    (if (not color-ag-sr-differently)

	(loop for point-stress-in-range-0-to-1 in point-stresses-in-range-0-to-1
	    for canvas-item from (get-mds-window-first-coords-canvas-item mds-window) by 2 collect
	      (tk-put mds-window "setIdFill ~d ~a" 
		      canvas-item
		      (stress-component-to-shade-of-red point-stress-in-range-0-to-1)))
      
      (let ((ag-names (collect #'ag-name-p antigens))
	    (sr-names (collect #'sr-name-p antigens))
	    ag-point-stresses
	    sr-point-stresses)
	(loop for name in antigens
	    for point-stress in point-stresses do
	      (cond ((ag-name-p name) (push-end point-stress ag-point-stresses))
		    ((sr-name-p name) (push-end point-stress sr-point-stresses))
		    (t (error "expected only ag or sr names but got ~a" name))))
	(let ((ag-point-stresses-in-range-0-to-1 (scale-zero-based-data-so-max-is-1 
						  ag-point-stresses
						  :if-max-less-than-this-value-return-original-list SINGLE-FLOAT-EPSILON))
	      (sr-point-stresses-in-range-0-to-1 (scale-zero-based-data-so-max-is-1 
						  sr-point-stresses
						  :if-max-less-than-this-value-return-original-list SINGLE-FLOAT-EPSILON)))
	  (loop for name in antigens
	      for canvas-item from (get-mds-window-first-coords-canvas-item mds-window) by 2 collect
		(tk-put mds-window "setIdFill ~d ~a" 
			canvas-item
			(if (ag-name-p name)
			    (stress-component-to-shade-of-blue 
			     (nth (position name ag-names) ag-point-stresses-in-range-0-to-1))
			  (stress-component-to-shade-of-red
			   (nth (position name sr-names) sr-point-stresses-in-range-0-to-1))))))))
    ))


(defun stress-component-to-shade-of-red (stress-component-in-range-0-to-1)
  (rgb-tk-color #xff
		(round (* 255 (- 1 stress-component-in-range-0-to-1)))
		(round (* 255 (- 1 stress-component-in-range-0-to-1)))))

(defun stress-component-to-shade-of-blue (stress-component-in-range-0-to-1)
  (rgb-tk-color (round (* 255 (- 1 stress-component-in-range-0-to-1)))
		(round (* 255 (- 1 stress-component-in-range-0-to-1)))
		#xff))



#|
testing

test on tab23-stressed (one point that has high stress)
(read in from input-ui)

(setq merged-hi-with-seq-0.75-lt10s
  (add-lt10s-based-on-sequence-distance
   merged-hi-5-to-lt10
   nl-pro
   0.75))

(setq hi-of-sequenced-strains-wo-rc-coordss 
    (lowest-stress-coordss-from-batch-runs 
     "~/mds/investigations/less-than-ten/merged-hi-with-seq-0.75-lt10s-asl-output"))



(make-master-mds-window
 merged-hi-with-seq-0.75-lt10s-asl
 :starting-coordss
 hi-of-sequenced-strains-wo-rc-coordss )

 
|#

;;;----------------------------------------------------------------------
;;;                         removing names
;;;----------------------------------------------------------------------

(defun restore-all-names (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-coords-names-working-copy table-window (get-coords-names table-window))
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))

(defun remove-all-names (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-coords-names-working-copy
     table-window
     (loop for i below (hi-table-length (get-hi-table table-window)) collect
	   ""))
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))

(defun restore-name (mds-window point-canvas-id &optional (display-update t))
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (point-index (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id)))
    (set-coords-names-working-copy 
     table-window
     (replace-nth 
      point-index
      (nth point-index (get-coords-names table-window))  ;; note original here
      (get-coords-names-working-copy table-window)))     ;; and working-copy here
    (if display-update
	(visualize-mds-coordss mds-window 'update 
			       (get-mds-coordss mds-window)   
			       (get-coords-names-working-copy table-window)
			       (get-coords-colors table-window)
			       nil))))

(defun remove-name (mds-window point-canvas-id &optional (display-update t))
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (point-index (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id)))
    (set-coords-names-working-copy 
     table-window
     (replace-nth 
      point-index
      ""
      (get-coords-names-working-copy table-window)))
    (if display-update
	(visualize-mds-coordss mds-window 'update 
			       (get-mds-coordss mds-window)   
			       (get-coords-names-working-copy table-window)
			       (get-coords-colors table-window)
			       nil))))

(defun display-update (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window)   
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))


;;------------------------- utilities for raising names -----------------------------
;;  (right now, 2002-02-06, not integrated with the ui), just used in investigations
;;   to bring colored dots to the front of white dots)

(defun raise-strains-from-strain-indices (mds-window strain-indices)
  (loop for strain-index in strain-indices do
	(tk-put mds-window ".c raise ~d" (get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index))
	(tk-put mds-window ".c raise ~d" (get-mds-window-name-canvas-id-from-strain-index mds-window strain-index))))

(defun lower-strains-from-strain-indices (mds-window strain-indices)
  (loop for strain-index in strain-indices do
	(tk-put mds-window ".c lower ~d" (get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index))
	(tk-put mds-window ".c lower ~d" (get-mds-window-name-canvas-id-from-strain-index mds-window strain-index))))

(defun raise-strains-from-strain-names (mds-window strain-names &optional &key ignore-if-name-does-not-exist)
  (raise-strains-from-strain-indices 
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for strain-name in strain-names 
	 when (if ignore-if-name-does-not-exist
		  (member strain-name all-strain-names)
		t)
	 collect (position strain-name all-strain-names)))))

(defun lower-strains-from-strain-names (mds-window strain-names &optional &key ignore-if-name-does-not-exist)
  (lower-strains-from-strain-indices 
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for strain-name in strain-names 
	 when (if ignore-if-name-does-not-exist
		  (member strain-name all-strain-names)
		t)
	 collect (position strain-name all-strain-names)))))


(defun raise-selected-points-from-mds-window (mds-window canvas-ids)
  (let ((names (get-point-names-from-mds-window-canvas-ids mds-window canvas-ids)))
    (set-raise-points mds-window names)
    (set-lower-points mds-window (my-set-difference (get-lower-points mds-window) names))  ;; remove the lowering list, if present
    (raise-strains-from-strain-names mds-window names)))

(defun lower-selected-points-from-mds-window (mds-window canvas-ids)
  (let ((names (get-point-names-from-mds-window-canvas-ids mds-window canvas-ids)))
    (set-lower-points mds-window names)
    (set-raise-points mds-window (my-set-difference (get-raise-points mds-window) names))  ;; remove from the raising list, if present
    (lower-strains-from-strain-names mds-window names)))


;;------------------------- utilities for selecting names -----------------------------
;;  (right now, 2002-02-06, not integrated with the ui), just used in investigations
;;   to select strains to procrustes a subset)

(defun select-strains-from-strain-indices (mds-window strain-indices)
  (loop for strain-index in strain-indices do
	(tk-put mds-window "processPointForSelection ~d" (get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index))))

(defun select-strains-from-strain-names (mds-window strain-names &optional &key ignore-if-name-does-not-exist)
  (select-strains-from-strain-indices 
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for strain-name in strain-names 
	 when (if ignore-if-name-does-not-exist
		  (member strain-name all-strain-names)
		t)
	 collect (position strain-name all-strain-names)))))


;;------------------------- utilities for coloring dots -----------------------------
;;  (right now, 2002-02-06, not integrated with the ui), just used in investigations

(defun color-dots-from-strain-indices (mds-window strain-indices-and-colors)
  (loop for (strain-index strain-color) in strain-indices-and-colors do
	(tk-put mds-window "setIdFill ~d ~a" 
		(get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index)
		strain-color)))

(defun color-dots-from-strain-names (mds-window strain-names-and-colors)
  (color-dots-from-strain-indices 
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for (strain-name strain-color) in strain-names-and-colors collect
	   (list (position strain-name all-strain-names) strain-color)))))


;;;----------------------------------------------------------------------
;;;                            scale-line
;;;----------------------------------------------------------------------

#|
TODO:
  i'd like to be able to pick the ruler up, and move it around, and have it scale in that position 
    (ideally move an end too, so it is not horizontal)
  at least move it closer to the stress (will look better) as its home
  but a distance on it, and maybe make it look like a ruler with tics
  get the scale line to resize when we scale the coords (with the mouse), and on a randomize the coords
  maybe have a 4-fold ruler as well, and an 8 and 16, or be able to click on it, and the length changes
    (add a scale on the ruler, with ticks, and make the screen length always be about the same?)
  make the length 7 scale something we pass in (so it is only length 7 for HI-metric)
|#

(defun hypotenuse-length (xy1 xy2)
  (sqrt (+ (square (- (nth 0 xy1) (nth 0 xy2))) 
	   (square (- (nth 1 xy1) (nth 1 xy2))))))


;;;----------------------------------------------------------------------
;;;                      row and col adjusts
;;;----------------------------------------------------------------------

(defun adjust-from-canvas-id (mds-window canvas-id inc-or-dec &optional (display-update t))
  (let* ((adjust-f (case inc-or-dec
		     (inc #'inc)
		     (dec #'dec)
		     (t (error "expected inc or dec but got ~a" inc-or-dec))))
	 (table-window (get-table-window-for-mds-window mds-window))
	 (position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id))
	 (strain-name (nth position (hi-table-antigens (get-hi-table table-window)))))
    (cond ((ag-name-p strain-name) 
	   (set-mds-row-adjusts-by-one-adjust 
	    mds-window
	    position
	    (funcall adjust-f (nth position (get-mds-row-adjusts table-window))))
	   (set-mds-row-adjusts-by-one-adjust 
	    table-window
	    position
	    (funcall adjust-f (nth position (get-mds-row-adjusts table-window)))))
	  ((sr-name-p strain-name) 
	   (set-mds-col-bases-by-one-adjust 
	    mds-window
	    position
	    (funcall adjust-f (nth position (get-mds-col-bases table-window))))
	   (set-mds-col-bases-by-one-adjust 
	    table-window
	    position
	    (funcall adjust-f (nth position (get-mds-col-bases table-window)))))
	  (t (format t "~%Warning: tried to change adjust by clicking right on a strain name, but no adjusts possible")))
    (if display-update
	(visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
			       (get-coords-names-working-copy table-window)
			       (get-coords-colors table-window)
			       nil))))

(defun strain-names-from-positions (positions hi-table)
  (multiple-nth positions (hi-table-antigens hi-table)))
	

(defun set-automatic-adjust-from-canvas-id (mds-window canvas-id &optional (display-update t))
  display-update ;; later, add something to indicate that we are doing automatic adjust on this strain
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id))
	 (strain-name (nth position (hi-table-antigens (get-hi-table table-window)))))
    (cond ((ag-name-p strain-name) (set-adjustable-rows    table-window (cons position (get-adjustable-rows    table-window))))
	  ((sr-name-p strain-name) (set-adjustable-columns table-window (cons position (get-adjustable-columns table-window))))
	  (t (format t "~%Warning: tried to make adjust automatic clicking on a strain name, but this is only implemented for hi tables right now")))
    (print (strain-names-from-positions (append (get-adjustable-rows table-window) (get-adjustable-columns table-window))
					(get-hi-table table-window)))))

(defun unset-automatic-adjust-from-canvas-id (mds-window canvas-id &optional (display-update t))
  display-update ;; later, add something to indicate that we are doing automatic adjust on this strain
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id))
	 (strain-name (nth position (hi-table-antigens (get-hi-table table-window)))))
    (cond ((ag-name-p strain-name) (set-adjustable-rows    table-window (remove position (get-adjustable-rows    table-window))))
	  ((sr-name-p strain-name) (set-adjustable-columns table-window (remove position (get-adjustable-columns table-window))))
	  (t (format t "~%Warning: tried to make adjust non-automatic clicking on a strain name, but this is only implemented for hi tables right now")))
    (print (strain-names-from-positions (append (get-adjustable-rows table-window) (get-adjustable-columns table-window))
					(get-hi-table table-window)))))


;;;----------------------------------------------------------------------
;;;          anotating the name with the row and col adjusts
;;;----------------------------------------------------------------------

;; for reporting row and col adjusts
(defun coords-name-plus-adjusts (coords-name row-adjust col-adjust)
  ;; do some manipulations here so the output looks good
  ;;   make the col an adjust (from a basis of 7.0)
  ;;   and get the signs intuitive, high reactivity is +, high avidity is +
  ;;   show nothing if there is no adjust from the baselines, or if row- and col-adjusts are null

  ;; to ease calling this function (for when there are no col or row adjusts
  (if (null row-adjust) (setq row-adjust 0))
  (if (null col-adjust) (setq col-adjust 7))

  (let ((row-adjust-real row-adjust)  ;;(- row-adjust))  
	(col-adjust-real col-adjust))    ;; print out the basis (- col-adjust 7)))    ;; ugly hard coded 7.0
    ;;i don't think this assertion should be true
    ;;(if (not (or (zerop col-adjust-real)
    ;;		 (zerop row-adjust-real)))
    ;;	(error "Assertion failed, I expected at least the row or col adjust to be zero, but row-adjust is ~d and col-adjust is ~d"
    ;;	       row-adjust-real col-adjust-real))
    (cond ((ag-name-p coords-name)
	   (if (zerop (dps row-adjust-real 1))
	       coords-name
	     (format nil "~a(~3,1f)" coords-name row-adjust-real)))
	  ((sr-name-p coords-name)
	   (if (zerop (dps col-adjust-real 1))
	       coords-name
	     (format nil "~a(~3,1f)" coords-name col-adjust-real)))
	  (t coords-name))))


;;;----------------------------------------------------------------------
;;;                      procrustes lines
;;;----------------------------------------------------------------------

(defun remove-procrustes-data (mds-window)
  (set-procrustes-data mds-window nil)
  ;; and the next line as this is how i've done things elsewhere, and the new window gets
  ;; most of its information from the table window
  (set-procrustes-data (get-table-window-for-mds-window mds-window) nil)
  )


;;;----------------------------------------------------------------------
;;;                      output distances
;;;----------------------------------------------------------------------

#|
not separated into separate function, as for large table we overflow the memory capacity of windows allegro
(defun output-names-and-distances (mds-window filename)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (format out ";; MDS distances for ~a~%;; This file created on ~a~%~%" 
	    (hi-table-name (get-hi-table (get-table-window-for-mds-window mds-window)))
	    (time-and-date))
    (fll (names-and-distances mds-window) out)))


;; don't do any sorting (ron will let excel do it for now)
;;(defun names-and-distances (mds-window)
;;  (let* ((unsorted-result
;;	  (let ((names (all-comparisons (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window))) #'list))
;;		(distances (all-comparisons (coordss (get-mds-coordss mds-window)) #'e-dist)))
;;	    (mapcar (^ (l) 
;;		       (let ((names (nth 0 l))
;;			     (distance (nth 1 l)))
;;			 (append
;;			  (sort-strains names)
;;			  (list (coerce distance 'single-float)))))
;;		    (transpose names distances)))))
;;    (sort-nth 0
;;	      (reverse
;;	       (sort-nth 1
;;			 unsorted-result
;;			 #'strain-sort-f))
;;	      #'strain-sort-f)))

(defun names-and-distances (mds-window)
  (let* ((unsorted-result
	  (let ((names (all-comparisons (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window))) #'list))
		(distances (all-comparisons (coordss (get-mds-coordss mds-window)) #'e-dist)))
	    (mapcar (^ (l) 
		       (let ((names (nth 0 l))
			     (distance (nth 1 l)))
			 (append
			  (sort-strains names)
			  (list (coerce distance 'single-float)))))
		    (transpose names distances)))))
    unsorted-result))
|#

(defun pp-sorted-pair-and-distance-s (triples &optional &key filename (if-exists-action :error))
  (with-open-file (out filename :direction :output 
		   :if-exists if-exists-action
		   :if-does-not-exist :create)
    (loop for (a-name b-name distance) in triples do
	  (format out "~{~30a ~}~10f~%" (sort-strains (list a-name b-name)) distance))))
  

(defun output-names-and-distances (mds-window filename)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (let ((hi-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	  (coordss (coordss (get-mds-coordss mds-window))))
      (format out ";; Map distances for ~a~%;; This file created at ~a~%~%" 
	      (hi-table-name hi-table)
	      (time-and-date))
      (format out ";; Strain        Strain         Map distance~%~%")
      (loop for (a-name . rest-names) on (hi-table-antigens hi-table) 
	  for (a-coords . rest-coordss) on coordss do
	    (loop for b-name in rest-names 
		for b-coords in rest-coordss do
		  (format out "~{~16a ~}~10f~%"
			  (sort-strains (list a-name b-name))
			  (dps (e-dist a-coords b-coords) 8)))))))


(defun output-names-and-distances-from-table (mds-window filename)
  ;; table and map distances (combining the two above)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (let* ((hi-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	   (similarity-table-p (similarity-table-p hi-table))
	   (mds-coordss (get-mds-coordss mds-window))
	   (col-bases (col-bases mds-coordss)))
      (format out ";; Table distances for ~a~%;; This file created at ~a~%~%" 
	      (hi-table-name hi-table)
	      (time-and-date))
      (format out ";; Strain        Strain        Table distance~%~%")
      (loop for (a-name . rest-names) on (hi-table-antigens hi-table) 
	  for row-values in (hi-table-values hi-table) 
	  for antigen-number from 0 do
	    (loop for b-name in rest-names 
		for value in (nthcdr (inc antigen-number) row-values) 
		for serum-number from (inc antigen-number) do
		  (format out "~{~16a ~}~10f~%" 
			  (sort-strains (list a-name b-name))
			  (if similarity-table-p 
			      (cond ((numberp value) (- (nth serum-number col-bases) value))
				    ((thresholdp value) (read-from-string 
							 (format nil ">~d" 
								 (coerce (nth serum-number col-bases) 'single-float))))
				    ((true-dont-care-p value) '*)
				    (t value))
			    value)))))))


#|
;; in shephered plot
(defun target-actual-s-from-save (save)
  (let* ((hi-table (table-from-save save))
	 (similarity-table-p (similarity-table-p hi-table))
	 (starting-coordss (starting-coordss-from-save save))
	 (coordss (coordss starting-coordss))
	 (col-bases (col-bases starting-coordss)))
    (loop for (a-name . rest-names) on (hi-table-antigens hi-table) 
	for (a-coords . rest-coordss) on coordss 
	for row-values in (hi-table-values hi-table) 
	for antigen-number from 0 append
	  (loop for b-name in rest-names 
	      for b-coords in rest-coordss 
	      for value in (nthcdr (inc antigen-number) row-values) 
	      for serum-number from (inc antigen-number) 
	      when (not (true-dont-care-p value))
	      collect (append
		       (sort-strains (list a-name b-name))
		       (list 
			(if similarity-table-p 
			    (cond ((numberp value) (coerce (- (nth serum-number col-bases) value) 'single-float))
				  ((thresholdp value) (read-from-string 
						       (format nil ">~d" 
							       (coerce (nth serum-number col-bases) 'single-float))))
				  (t (coerce value 'single-float)))
			  (coerce value 'single-float))
			(coerce (e-dist a-coords b-coords) 'single-float)))))))
|#


(defun output-names-and-distances-from-table-and-map (mds-window filename)
  ;; table and map distances (combining the two above)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (let* ((hi-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	   (similarity-table-p (similarity-table-p hi-table))
	   (mds-coordss (get-mds-coordss mds-window))
	   (coordss (coordss mds-coordss))
	   (col-bases (col-bases mds-coordss)))
      (format out ";; Table and map distances for ~a~%;; This file created at ~a~%~%" 
	      (hi-table-name hi-table)
	      (time-and-date))
      (format out ";; Strain        Strain        Table distance   Map distance~%~%")
      (loop for (a-name . rest-names) on (hi-table-antigens hi-table) 
	  for (a-coords . rest-coordss) on coordss 
	  for row-values in (hi-table-values hi-table) 
	  for antigen-number from 0 do
	    (loop for b-name in rest-names 
		for b-coords in rest-coordss 
		for value in (nthcdr (inc antigen-number) row-values) 
		for serum-number from (inc antigen-number) do
		  (format out "~{~16a ~}~10f      ~10f~%" 
			  (sort-strains (list a-name b-name))
			  (if similarity-table-p 
			      (cond ((numberp value) (- (nth serum-number col-bases) value))
				    ((thresholdp value) (read-from-string 
							 (format nil ">~d" 
								 (coerce (nth serum-number col-bases) 'single-float))))
				    ((true-dont-care-p value) '*)
				    (t value))
			    value)
			  (dps (e-dist a-coords b-coords) 8)))))))


;;;----------------------------------------------------------------------
;;;                    output distance matrix
;;;----------------------------------------------------------------------

(defun output-distance-matrix (mds-window filename)
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (format out ";; MDS distance matrix for ~a~%;; This file created at ~a~%~%" 
	    (hi-table-name (get-hi-table (get-table-window-for-mds-window mds-window)))
	    (time-and-date))
    (fll (distance-table-from-mds-window mds-window) :stream out)))

(defun distance-table-from-mds-window (mds-window)
  (let ((names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window))))
	(distance-matrix (coordss-to-full-distance-matrix (coordss (get-mds-coordss mds-window)) 
							  (^ (a b)
							     (dps (e-dist a b) 6)))))
    (make-hi-table
     names
     names
     distance-matrix)))
  

;;;----------------------------------------------------------------------
;;;                    output phylip matrix
;;;----------------------------------------------------------------------

(defun output-phylip-matrix-from-mds-window (mds-window filename &key enths normalize-to-max-distance-of-one)
  (output-phylip-matrix 
   (get-hi-table (get-table-window-for-mds-window mds-window))
   (coordss (get-mds-coordss mds-window))
   filename
   :enths enths
   :normalize-to-max-distance-of-one normalize-to-max-distance-of-one))

(defun output-phylip-matrix (table coordss filename &key 
						    enths
						    (if-exists :supersede)
						    normalize-to-max-distance-of-one
						    table-is-distance-matrix)
  (if (and coordss table-is-distance-matrix)
      (error "either supply coords from which we calculate the distnace matrix, or supply the distance matrix"))
  (with-open-file (out filename :direction :output 
		   :if-exists if-exists
		   :if-does-not-exist :create)
    (if enths
	(progn
	  (setq table (hi-table-enths table enths))
	  (setq coordss (enths enths coordss))))
    (format out "    ~d~%" (hi-table-length table))
    (fll (phylip-table table coordss 
		       :normalize-to-max-distance-of-one normalize-to-max-distance-of-one 
		       :table-is-distance-matrix table-is-distance-matrix) 
	 :stream out)))

(defun phylip-table (table coords &key
				  (max-name-length 10)
				  normalize-to-max-distance-of-one
				  table-is-distance-matrix)
  (if (and coords table-is-distance-matrix)
      (error "either supply coords from which we calculate the distnace matrix, or supply the distance matrix"))
  (let* ((names (mapcar (^ (name-with-ag-sr) 
			   (read-from-string 
			    (let* ((name (string (remove-ag-sr-from-name name-with-ag-sr)))
				   (sr-name-p (sr-name-p name-with-ag-sr)))
			      (if sr-name-p
				  (setq name (string-append "S" name)))
			      (substring 
			       name
			       0
			       (dec (min max-name-length (length name)))))))
			(hi-table-antigens table)))
	 (distance-matrix (if table-is-distance-matrix
			      (hi-table-values table)
			    (coordss-to-full-distance-matrix (firstn (length names) coords) #'e-dist))))
    (if normalize-to-max-distance-of-one
	(setq distance-matrix
	  (let ((max-distance (apply-max (flatten distance-matrix))))
	    (f-elements (^ (x) (/ x max-distance)) distance-matrix))))
    (mapcar #'cons names (f-elements (^ (x) (format nil "~9,4f" x)) distance-matrix))))


;;;----------------------------------------------------------------------
;;;                      output names
;;;----------------------------------------------------------------------

(defun output-names (mds-window filename point-canvas-ids)  ;; null point-canvas-ids means no points selected, so output all names
  (if (null point-canvas-ids)
      ;; nothing selected, so take everything
      (setq point-canvas-ids
	(loop for i below (hi-table-length (get-hi-table (get-table-window-for-mds-window mds-window))) collect 
	      (get-mds-window-dot-canvas-id-from-strain-index mds-window i))))
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (let* ((table-window (get-table-window-for-mds-window mds-window))
	   (table (get-hi-table table-window))
	   (strains (hi-table-antigens table))  ;; assuming square table here
	   (point-indices (loop for point-canvas-id in (my-sort point-canvas-ids) collect  ;; sort so same order as original table
				(get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id))))
      (loop for point-index in point-indices collect
	    (format out "~a~%" (nth point-index strains))))))
		  
		  
;;;----------------------------------------------------------------------
;;;                      output names and coordss
;;;----------------------------------------------------------------------

(defun output-names-and-coordss-from-mds-window (mds-window filename point-canvas-ids)  ;; null point-canvas-ids means no points selected, so output all names
  (if (null point-canvas-ids)
      ;; nothing selected, so take everything
      (setq point-canvas-ids
	(loop for i below (hi-table-length (get-hi-table (get-table-window-for-mds-window mds-window))) collect 
	      (get-mds-window-dot-canvas-id-from-strain-index mds-window i))))
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (mds-coordss-unscaled (mds-to-canvas-coordss-given-canvas-coord-transformations-no-scale
                                (get-canvas-coord-transformations mds-window)
                                (coordss (get-mds-coordss mds-window))))
	 (table (get-hi-table table-window))
	 (strains (hi-table-antigens table))  ;; assuming square table here
	 (point-indices (loop for point-canvas-id in (my-sort point-canvas-ids) collect  ;; sort so same order as original table
			      (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id))))
    (output-names-and-coordss
     filename
     (multiple-nth point-indices strains)
     (multiple-nth point-indices mds-coordss-unscaled))))

(defun output-names-and-coordss-from-save (save filename)
  (output-names-and-coordss
   filename
   ;; (hi-table-antigens (table-from-save save))  replaced by efficiency hack below
   (hi-table-antigens-table-from-save-non-expanding-hack save)
   (coordss (starting-coordss-from-save save))))
		  
		  
;;;----------------------------------------------------------------------
;;;            output names, coordss, and bases/adjusts
;;;----------------------------------------------------------------------

(defun output-names-coordss-and-bases-adjusts-from-mds-window (mds-window filename point-canvas-ids)  ;; null point-canvas-ids means no points selected, so output all names
  (if (null point-canvas-ids)
      ;; nothing selected, so take everything
      (setq point-canvas-ids
	(loop for i below (hi-table-length (get-hi-table (get-table-window-for-mds-window mds-window))) collect 
	      (get-mds-window-dot-canvas-id-from-strain-index mds-window i))))
  (let* ((table-window (get-table-window-for-mds-window mds-window))
         (mds-coordss-unscaled (mds-to-canvas-coordss-given-canvas-coord-transformations-no-scale
                                (get-canvas-coord-transformations mds-window)
                                (coordss (get-mds-coordss mds-window))))
	 (col-bases   (col-bases   (get-mds-coordss mds-window)))
	 (row-adjusts (row-adjusts (get-mds-coordss mds-window)))
	 (table (get-hi-table table-window))
	 (strains (hi-table-antigens table))  ;; assuming square table here
	 (point-indices (loop for point-canvas-id in (my-sort point-canvas-ids) collect  ;; sort so same order as original table
			      (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id))))
    (output-names-and-coordss
     filename
     (multiple-nth point-indices strains)
     (multiple-nth point-indices mds-coordss-unscaled)
     :bases   (multiple-nth point-indices col-bases)
     :adjusts (multiple-nth point-indices row-adjusts))))

(defun output-names-coordss-and-bases-adjusts-from-save (save filename)
  (output-names-and-coordss
   filename
   (hi-table-antigens (table-from-save save))
   (coordss (starting-coordss-from-save save))
   :bases   (col-bases   (starting-coordss-from-save save))
   :adjusts (row-adjusts (starting-coordss-from-save save))))
		  
		  
;;;----------------------------------------------------------------------
;;;                output pymol format for 3D viewing
;;;----------------------------------------------------------------------

(defun output-pymol-format-from-mds-window (mds-window filename point-canvas-ids &optional &key (if-exists :error))
  ;; null point-canvas-ids means no points selected, so output all names
  (print point-canvas-ids)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (coordss-plus-more (get-mds-coordss mds-window))
	 (table (get-hi-table table-window))
	 (strains (hi-table-antigens table))  ;; assuming square table here
	 (coords-colors (get-coords-colors table-window))
	 (procrustes-data (get-procrustes-data mds-window))
	 (point-indices (loop for point-canvas-id in (my-sort point-canvas-ids) collect  ;; sort so same order as original table
			      (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id))))
    (pymol-format
     (let ((save (make-save-form 
		  :hi-table         table
		  :starting-coordss coordss-plus-more
		  :coords-colors    coords-colors
		  :procrustes-data  procrustes-data)))
       (if point-canvas-ids
	   (subset-save-form save (multiple-nth point-indices strains))
	 save))
     :filename  filename
     :if-exists if-exists)))


;;;----------------------------------------------------------------------
;;;                   OUTPUT HI TABLE EXTRACT
;;;----------------------------------------------------------------------

(defun collect-common (original new &key action-f)   ;; action-f should handle nil pass in case e-new is not in original
  (loop for e-new in new collect
	(funcall action-f (position e-new original))))

(defun output-table-extract (mds-window filename point-canvas-ids &optional &key include-all-sera include-all-antigens exclude-all-sera)
  (if (and include-all-sera exclude-all-sera)
      (error "It makes no sense to specify to both include and exclude all sera"))
  (if (null point-canvas-ids)
      ;; nothing selected, so take everything
      (setq point-canvas-ids
	(loop for i below (hi-table-length (get-hi-table (get-table-window-for-mds-window mds-window))) collect 
	      (get-mds-window-dot-canvas-id-from-strain-index mds-window i))))
  (with-open-file (out filename :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (let* ((table-window (get-table-window-for-mds-window mds-window))
	   (table (get-hi-table table-window))
	   (point-indices 
	    (let ((selected-point-indices
		   (loop for point-canvas-id in (my-sort point-canvas-ids) collect  ;; sort so same order as original table
			 (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id)))
		  (additional-point-indices
		   (if include-all-sera
		       (if (not (ag-sr-table-p table))
			   (error "Only ask to include all sera for an HI table in raw (not pannel) format")
			 (series (length (hi-table-antigens-short table)) (dec (hi-table-length table))))
		     (if include-all-antigens
			 (if (not (ag-sr-table-p table))
			     (error "Only ask to include all antigens for an HI table in raw (no pannel) format")
			   (series 0 (dec (length (hi-table-antigens-short table)))))
		       nil)))
		  (excluded-point-indices
		   (if exclude-all-sera
		       (if (not (ag-sr-table-p table))
			   (error "Only ask to exclude all sera for an HI table in raw (not pannel) format")
			 (series (length (hi-table-antigens-short table)) (dec (hi-table-length table))))
		     nil)))
	      (my-sort (set-difference (union selected-point-indices additional-point-indices) excluded-point-indices))))
           (point-names (multiple-nth point-indices (hi-table-antigens table)))
	   (table-extract (extract-hi-table-by-indices table point-indices point-indices))
           (pre-merge-table-extracts (filter #'null  ;; remove table that have become empty
                                             (loop for pre-merge-table in (get-pre-merge-tables table-window) collect
                                                   (extract-hi-table 
                                                    pre-merge-table 
                                                    (my-intersection (hi-table-antigens pre-merge-table) (mapcar #'remove-ag-sr-from-name (collect #'ag-name-p point-names)))
                                                    (my-intersection (hi-table-antisera pre-merge-table) (mapcar #'remove-ag-sr-from-name (collect #'sr-name-p point-names)))))))
	   (coordss (get-mds-coordss mds-window))
	   (coordss-extract (new-starting-coords-matching-old-coordss
			     coordss
			     (hi-table-antigens table)
			     (hi-table-antigens table-extract)
			     :col-bases (collect-common 
					 (hi-table-antigens table)
					 (hi-table-antigens table-extract)
					 :action-f (^ (position)
						      (if (null position)
							  (error "expected the extract antigens to be in the original table")
							(nth position (col-bases coordss)))))))
           (raise-points-extract (my-intersection (get-raise-points mds-window) (hi-table-antigens table-extract)))
           (lower-points-extract (my-intersection (get-lower-points mds-window) (hi-table-antigens table-extract))))
      (format out 
	      ";; MDS configuration file (version 0.6).~%;; Created for ~a at ~a~%~%"
      	      (hi-table-name table)
      	      (time-and-date))
      (print  ;; had previously used (format out "~s" (make-save-form ...)) but changed 2004-09-05 by Derek, see write-save-form for reason
       (make-save-form :hi-table table-extract
                       :pre-merge-tables pre-merge-table-extracts
		       :starting-coordss coordss-extract
		       :mds-dimensions (get-mds-num-dimensions table-window)
		       :coords-colors (multiple-nth point-indices (get-coords-colors table-window))
		       :coords-dot-sizes (multiple-nth point-indices (get-coords-dot-sizes table-window))
		       :coords-transparencies (multiple-nth point-indices (get-coords-transparencies table-window))
		       :coords-names (multiple-nth point-indices (get-coords-names table-window))
		       :coords-names-working-copy (multiple-nth point-indices (get-coords-names-working-copy table-window))
		       :coords-name-sizes (multiple-nth point-indices (get-coords-name-sizes table-window))
		       :coords-name-colors (multiple-nth point-indices (get-coords-name-colors table-window))
		       :coords-shapes (if (null (get-coords-shapes table-window))
					  'not-passed ;; bit ugly, as if we change make-save-form this breaks
					(multiple-nth point-indices (get-coords-shapes table-window)))
		       ;; these two do not work, maybe not because of this, but maybe passing these in
		       ;;   to make-master-mds-window does not work (at lease we don't get the colors in tk
		       ;;   and we seem to get movement when we optimize
		       ;;:moveable-coords (let ((moveable-coords (get-moveable-coords table-window)))
		       ;;		  (if (eql 'all moveable-coords)
		       ;;		      moveable-coords
		       ;;		    (reverse (intersection moveable-coords point-indices))))
		       ;;:unmoveable-coords (reverse (intersection (get-unmoveable-coords table-window) point-indices))
		       :canvas-coord-transformations (get-canvas-coord-transformations mds-window)
                       :raise-points raise-points-extract
                       :lower-points lower-points-extract
		       )
       out)
      )))


#|
(defun subset-save-form (save-form subset-strains)
  (let* ((table (extract-table-from-save save-form))
	 (table-subset (extract-hi-table-by-indices table subset-strains subset-strains))
	 (best-coordss (extract-best-coordss-from-save-or-starting-coords-if-no-batch-runs save-form))
	 (best-coordss-subset (
    
	 (table-extract )
	 (coordss (get-mds-coordss mds-window))
	 (coordss-extract (new-starting-coords-matching-old-coordss
			   coordss
			   (hi-table-antigens table)
			   (hi-table-antigens table-extract)
			   :col-bases (collect-common 
				       (hi-table-antigens table)
				       (hi-table-antigens table-extract)
				       :action-f (^ (position)
						    (if (null position)
							(error "expected the extract antigens to be in the original table")
						      (nth position (col-bases coordss))))))))

    (format out "~s" (make-save-form :hi-table table-extract
				     :starting-coordss coordss-extract
				     :mds-dimensions (get-mds-num-dimensions table-window)
				     :coords-colors (multiple-nth point-indices (get-coords-colors table-window))
				     :coords-dot-sizes (multiple-nth point-indices (get-coords-dot-sizes table-window))
				     :coords-names (multiple-nth point-indices (get-coords-names table-window))
				     :coords-names-working-copy (multiple-nth point-indices (get-coords-names-working-copy table-window))
				     :coords-name-sizes (multiple-nth point-indices (get-coords-name-sizes table-window))
				     :coords-name-colors (multiple-nth point-indices (get-coords-name-colors table-window))
				     ;; these two do not work, maybe not because of this, but maybe passing these in
				     ;;   to make-master-mds-window does not work (at lease we don't get the colors in tk
				     ;;   and we seem to get movement when we optimize
				     ))))
|#

;;;----------------------------------------------------------------------
;;;                          mds-plot-spec
;;;----------------------------------------------------------------------

#|
pre release:
  >>>  test on windows popping up error messages
  split so ron can read in parts
  (note to alan about international strain names)
  done in table window, then not cost of drawing all names (still some ui smoothing, but let's take a pass)

release notes:
  mds/mds/plot-specs/all-strains-take-1-sequence-clade.plot-spec

later 
   add shape
   add ordering
   warn if we set parameters and also get them in the spec list  (for now just override)
   we don't check for typos, it is hard.  (maybe have something on the top saying "disable checking")
   the gui is not quite right (nothing happens, it is next plot made that stuff happens in.)
     but for a large plot, we don't want to make another.

   needs to handle lapedes filenames (ok, should not be a problem, as we convert before we see them)
     ok, but i could test and convert if they are in lapedes format?, or alan could have to write them
     or i provide a converter tool 
|#



(defun quoted-color-from-name (name)
  (format nil "~a" (color-from-name name)))

(defun shape-from-name (name)
  (if (ag-name-p name)
      'circle
    (if (sr-name-p name)
	'rectangle
      'circle)))

(defvar *plot-spec-defaults*)
(setq *plot-spec-defaults*
  `(;;(starting-coords   random)
    ;;(avidity-adjust    0)
    ;;(reactivity-basis  7654321)
    (dot-size          4)
    (transparency      0.0)
    (color             ,#'color-from-name)
    (name              ,#'identity)
    (name-working-copy ,#'identity)
    (name-color        ,#'color-from-name)
    (name-size         10)
    (outline-color     black)
    (shape             ,#'shape-from-name)
    ;;(fixed             nil)
    ;;(connected         t)
    ))


(defun generate-plot-spec (antigens &optional &key
					     ;;starting-coords   
					     ;;avidity-adjust    
					     ;;reactivity-basis  
					     coords-dot-sizes
                                             coords-transparencies
					     coords-names
					     coords-name-sizes
					     coords-names-working-copy
					     coords-name-colors
					     coords-outline-colors
					     coords-shapes
					     ;;fixed             
					     ;;connected         
					      coords-colors)
  (setq coords-shapes (mapcar #'anything->string coords-shapes))
  (loop for antigen in antigens for i from 0 collect
	(cons antigen
	      (loop for spec-name in '(;;:sc
				       ;;:aa
				       ;;:rb
				       :co 
				       :ds
                                       :tr
				       :nm
				       :wn
				       :ns
				       :nc
				       :oc
				       :sh
				       ;;:fd
				       ;;:ct
				       )
		  for spec-full-data in (list 
					 ;;starting-coords   
					 ;;avidity-adjust    
					 ;;reactivity-basis  
					 coords-colors
					 coords-dot-sizes
                                         coords-transparencies
					 coords-names
					 coords-names-working-copy
					 coords-name-sizes
					 coords-name-colors
					 coords-outline-colors
					 coords-shapes
					 ;;fixed             
					 ;;connected
					 ) append
		    (if (and spec-full-data
			     (not (eql 'not-set (nth i spec-full-data))))
			(list spec-name (nth i spec-full-data)))))))

	
(defun parse-plot-spec (&optional &key
				  ;;sc (starting-coords sc)  
				  ;;aa (avidity-adjust aa)   
				  ;;rb (reactivity-basis rb) 
				  co (color co)
				  ds (dot-size ds)         
                                  tr (transparency tr)
				  nm (name nm)             
				  (wn name) (name-working-copy wn)
				  ns (name-size ns)        
				  (nc color) (name-color nc)       
				  oc (outline-color oc)    
				  sh (shape sh)
				 ;;fd (fixed fd)            
				 ;;ct (connected ct)        
				 )
  (loop for arg-name in '(;;starting-coords   
			  ;;avidity-adjust    
			  ;;reactivity-basis  
			  dot-size          
                          transparency
			  name              
			  name-size         
			  name-working-copy 
			  name-color        
			  outline-color     
			  shape
			  ;;fixed             
			  ;;connected         
			  color)
      for arg-value in (list ;;starting-coords   
			     ;;avidity-adjust    
			     ;;reactivity-basis  
			     dot-size          
                             transparency
			     name              
			     name-size         
			     name-working-copy 
			     name-color        
			     outline-color     
			     shape
			     ;;fixed             
			     ;;connected         
			     color)
      when arg-value
      collect (list arg-name arg-value)))

(defun functionp-careful-re-eval (data)
  ;; be careful here, we are a bit hackey,
  ;; we want to check that we have a function, but we have a symbol, and it needs to be evealed to 
  ;; be a function.  we could eval everything, strings and numbers and functions would be fine, but we'd have to 
  ;; quote symbols (ie the name and the not-set for the :oc)
  ;; so instead, we are doing a careful check before we do the eval to check we will not get an error
  ;; this a bit of an inconsistency
  (handler-case 
      (functionp (eval data))
     (error (condition) 
       (let ((error-string '(apply #'format nil (condition-format-control condition) (condition-format-arguments condition))))
	 ;; comment out the above error string, we don't use it anyhow, and it causes problems on windows with functions not defined
	 ;; (format t "~a~%" error-string)
	 nil))))

(defun process-plot-specs (table-window antigens specs)
  (let* ((table (get-hi-table table-window))
         (ag-sr-table-p (ag-sr-table-p table))
         (ag-sr-plot-spec-p (and (listp specs)
                                 (listp (nth 0 specs))
                                 (ag-or-sr-name-p (car (nth 0 specs)))))
         (ag-sr-suffix-plot-spec    (and      ag-sr-table-p (not ag-sr-plot-spec-p)))
         (un-ag-sr-suffix-plot-spec (and (not ag-sr-table-p)     ag-sr-plot-spec-p) ))
    (let ((spec-alists))

      ;; parse spec lines and store in sparse spec lists
      (loop for spec in specs 
          unless (and un-ag-sr-suffix-plot-spec (sr-name-p (nth 0 spec)))   ;; remove sr if going from ag-sr-plotspec to non-ag-sr-table
          do
            (let ((antigen (nth 0 spec))
                  (spec-item-data-list (cdr spec)))
              (loop for (item data) in (apply #'parse-plot-spec spec-item-data-list) do

                    ;; before we write lists to the plotspec we encode them into a string so they will go on one line
                    ;; (we put on one line as the plot-spec format is human readable and modifyable
                    ;; so we need to uncode when we get them back in here
                    (if (and (stringp data)
                             (not (zerop (length data)))
                             (or (eql #\( (aref data 0))
                                 (eql 'shape item)))  ;; special case shape, maybe others?
                        (setq data (read-from-string data)))

                    ;; the function calling we have here is
                    ;; a bit restrictive, as we are only calling with the name
                    ;; we could also imagine calling with the full spec-item-data-list
                    ;; and for that we would have to write special functons
                    ;; we can test if the fucntion takes one are then call with the ag-name
                    ;; for now just call with the name
                    (if (functionp-careful-re-eval data)
                        (setq data (funcall (eval data) antigen)))  

                    (let ((spec-alist (assoc-value-1 item spec-alists))
                          (antigen (cond (ag-sr-suffix-plot-spec    (suffix-as-ag antigen))
                                         (un-ag-sr-suffix-plot-spec (remove-ag-sr-from-name antigen))
                                         (t                         antigen))))
                      (if spec-alist
                          (push (list antigen data) (cadr (assoc item spec-alists)))  ;; direct into spec-alists so we can do the more efficient push
                        (push (list item (list (list antigen data))) spec-alists))))))     ;;  instead of push-end 
      ;; (ppl spec-alists)

      ;; convert sparse spec lists into full spec lists, with defaults applied
      (let ((spec-full-lists
             (loop for spec-alist in spec-alists collect
                   (let* ((alist-name (car spec-alist))
                          (alist-body (cadr spec-alist))
                          (alist-default (if (assoc-value-1 'default alist-body)
                                             (assoc-value-1 'default alist-body)
                                           (assoc-value-1 alist-name *plot-spec-defaults*))))
                     (list alist-name
                           (sparse-alist-to-full-list antigens alist-body alist-default))))))
	  
        ;; set the plot-specs in table-window for those specs specified
        (set-plot-specs table-window spec-full-lists)))))


#|
;; specs
(ppl 
 (process-plot-specs 
  '(more another hk/1/68 not-in-list)
  '((hk/1/68 :sc (4.5 3.2) :aa 0 :rb max-in-col :ds 4 :na hk68 :ns 4 :nw "" :nc "#774422" :oc black :fd nil :ct t :co blue)
    (another :sc (2 4) :aa 3)
    (more :starting-coords (4 2) :color red))))

;; with default
(ppl 
 (process-plot-specs 
  '(more another hk/1/68 not-in-list)
  '((hk/1/68 :sc (4.5 3.2) :aa 0 :rb max-in-col :ds 4 :na hk68 :ns 4 :nw "" :nc "#774422" :oc black :fd nil :ct t :co blue)
    (another :sc (2 4) :aa 3)
    (more :starting-coords (4 2) :color red)
    (default :color pink))))

;; remove some specs for now
(ppl 
 (process-plot-specs 
  '(more another hk/1/68 not-in-list)
  '((hk/1/68  :ds 4 :na hk68 :ns 4 :nw "" :nc "#774422" :oc black :co blue)
    (another  :ds 2)
    (more  :color red)
    (default :color pink))))

(make-master-mds-window 
 last-hi-table
 :starting-coordss last-mds-coordss
 :plot-spec '((vi/3a/75-sr :color blue)))

(make-master-mds-window 
 last-hi-table
 :starting-coordss last-mds-coordss
 :plot-spec '((vi/3a/75-sr :color blue)
	      (default :color red :dot-size 2)
	      (vi/3a/75-sr :dot-size 5)))

(make-master-mds-window 
 last-hi-table
 :starting-coordss last-mds-coordss
 :plot-spec '((vi/3a/75-sr :color blue :dot-size 5)
	      (default :color red :dot-size 2)))
|#

(defun sparse-alist-to-full-list (order alist default)
  (loop for id in order collect
	(let ((data (assoc-value-1 id alist)))
	  (if data
	      data
	    (if (functionp default)
		(funcall default id)
	      default)))))

(defun set-plot-specs (table-window spec-lists)
  (loop for spec-name in '(;;starting-coords   
			   ;;avidity-adjust    
			   ;;reactivity-basis  
			   dot-size          
                           transparency
			   name              
			   name-size         
			   name-working-copy 
			   name-color        
			   outline-color     
			   shape
			   ;;fixed             
			   ;;connected         
			   color)
      for spec-name-set-function in (list 
				     ;;starting-coords   
				     ;;avidity-adjust    
				     ;;reactivity-basis  
				     #'set-coords-dot-sizes
				     #'set-coords-transparencies
				     #'set-coords-names
				     #'set-coords-name-sizes
				     #'set-coords-names-working-copy
				     #'set-coords-name-colors
				     #'set-coords-outline-colors
				     #'set-coords-shapes
				     ;;fixed             
				     ;;connected         
				     #'set-coords-colors) do
	(if (assoc-value-1 spec-name spec-lists)
	    (funcall spec-name-set-function table-window (assoc-value-1 spec-name spec-lists)))))
				    
    
(defun set-plot-spec-set-value-ids (table-window get-f set-f new-value ids)
  (funcall set-f
   table-window
   (loop for id from 0 
       for old-value in (funcall get-f table-window) collect
	 (if (or (eql ids 'all) 
		 (member id ids))
	     new-value
	   old-value))))

(defun set-plot-spec-data-from-mds-window (mds-window get-f set-f new-data-value selected-point-indices)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-plot-spec-set-value-ids
     table-window
     get-f set-f
     new-data-value
     (if selected-point-indices
	 (get-mds-point-indices-from-mds-window-canvas-ids mds-window selected-point-indices)
       'all))
    (visualize-mds-coordss mds-window 'update-changes-only
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil
			   :bypass-color-update-inhibition t)))

#|
(make-master-mds-window 
 last-hi-table
 :starting-coordss last-mds-coordss
 :plot-spec '((vi/3a/75-sr :color blue)))

|#


;;---------------------- reading in plot-specs ---------------------------

#|
;; do something like this, maybe even pass the whole line, maybe even have :ignore-rest to have extra params on a line?
(defun fi-in-plot-spec (plot-spec-filename)
  (fi-in-readline 
   plot-spec-filename
   :comment-char #\;
   :line-process-f 
   (^ (line-as-string)
      (let* ((line-as-list (space-delimited-string-to-list line-as-string))
	     (antigen (car line-as-list)))
	(loop for e in line-as-list collect
	      (if (functionp e)
		  (funcall e antigen)
		e))))))
|#

(defun fi-in-plot-spec (plot-spec-filename)
  (if (save-file-p plot-spec-filename)
      (plot-spec-from-save (fi-in plot-spec-filename))
    (fi-in-readline 
     plot-spec-filename
     :comment-char #\;
     :line-process-f #'space-delimited-string-to-list)))

(defun input-plot-spec (table-window plot-spec-filename)
  (process-plot-specs 
   table-window
   (hi-table-antigens (get-hi-table table-window))
   (fi-in-plot-spec plot-spec-filename)))
			 

(defun plot-spec-subset (plot-spec names)
  (loop for plot-spec-line in plot-spec
      when (member (car plot-spec-line) names)
      collect plot-spec-line))


;;-------------------- writing out plot spec in human readable (not lisp form) ------------------

(defun generate-plot-spec-from-mds-window (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (generate-plot-spec
     (hi-table-antigens (get-hi-table table-window))
     :coords-colors             (get-coords-colors table-window)
     :coords-dot-sizes          (get-coords-dot-sizes table-window)
     :coords-transparencies     (get-coords-transparencies table-window)
     :coords-names              (get-coords-names table-window)
     :coords-names-working-copy (get-coords-names-working-copy table-window)
     :coords-name-sizes         (get-coords-name-sizes table-window)
     :coords-name-colors        (get-coords-name-colors table-window)
     :coords-outline-colors     (get-coords-outline-colors table-window)
     :coords-shapes             (get-coords-shapes table-window)
     )))

(defvar *last-plot-spec*)
(defun save-plot-spec-from-mds-window (mds-window filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out 
	    ;; ";; Plot specification file (version 0.0).~%;; Created for ~a at ~a~%~%"
            ;; 0.1 has an additional :sh or :shape field
	    ";; Plot specification file (version 0.1).~%;; Created for ~a at ~a~%~%"
	    (hi-table-name (get-hi-table (get-table-window-for-mds-window mds-window)))
	    (time-and-date))    
    (fll (setq *last-plot-spec* (generate-plot-spec-from-mds-window mds-window))
	 :stream out
	 :use~s t)))


;;;----------------------------------------------------------------------
;;;                        save hi table 
;;;----------------------------------------------------------------------

(defun save-hi-table-from-mds-window (mds-window filename 
				      &optional &key 
						currently-selected-point-canvas-ids
						include-all-sera
						include-all-antigens)
  (let ((hi-table (get-hi-table (get-table-window-for-mds-window mds-window))))
    (if currently-selected-point-canvas-ids
	(let* ((selected-point-indices
		(loop for point-canvas-id in (my-sort currently-selected-point-canvas-ids) collect  ;; sort so same order as original table
		      (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id)))
	       (additional-point-indices
		(if include-all-sera
		    (if (not (ag-sr-table-p hi-table))
			(error "Only ask to include all sera for an HI table in raw (not pannel) format")
		      (series (length (hi-table-antigens-short hi-table)) (dec (hi-table-length hi-table))))
		  (if include-all-antigens
		      (if (not (ag-sr-table-p hi-table))
			  (error "Only ask to include all antigens for an HI table in raw (not pannel) format")
			(series 0 (dec (length (hi-table-antigens-short hi-table)))))
		    nil)))
	       (point-indices 
		(my-sort (union selected-point-indices additional-point-indices))))
	  (setq hi-table (extract-hi-table-by-indices hi-table point-indices point-indices))))
    (if (ag-sr-table-p hi-table)
	(setq hi-table (un-asl-hi-table hi-table)))
    (with-open-file (out filename :direction :output :if-exisits :supersede)
      (pp-hi-table hi-table 'full 4 nil :stream out))))


;;;----------------------------------------------------------------------
;;;                      lapedes format out
;;;----------------------------------------------------------------------

(defun save-lapedes-format-from-mds-window (mds-window filename)
  (let ((hi-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	(starting-coordss (get-mds-coordss mds-window)))
    (fi (hi-table-to-lapedes hi-table :starting-coordss starting-coordss)
	filename
	:supersede t :write-outer-list-elements-individually t :include-newline nil)))



;;;----------------------------------------------------------------------
;;;                  SETTING UP A TEST CASE
;;;----------------------------------------------------------------------

(defun coordss-to-full-distance-matrix (coordss &optional (distance-metric-f #'e-dist))
  (loop for row in coordss collect
	(loop for column in coordss collect
	      (funcall distance-metric-f row column))))

#|
(defun coordss-to-hi-table (coordss 
			    &optional 
			    (antigens (loop for i below (length coordss) collect (number->string i)))
			    (antisera (loop for i below (length coordss) collect (number->string i))))
  (make-hi-table
   antigens
   antisera
   (coordss-to-full-distance-matrix coordss)))
|#
(defun coordss-to-hi-table (coordss 
			    &optional 
			    antigens
			    antisera)
  (setq coordss (deconstruct-coordss-plus-more coordss))
  (if (null antigens) (setq antigens (loop for i below (length coordss) collect (number->string i))))
  (if (null antisera) (setq antisera (loop for i below (length coordss) collect (number->string i))))
  (make-hi-table
   antigens
   antisera
   (coordss-to-full-distance-matrix coordss)))


#|
(setq coordss '((0 0.5) (0.1 0.1) (1 1)(0.5 0.5) (0.1 0.1) (1 1)(0 0) (0.1 0.1) (1.5 1.5)))
(visualize-mds-coordss (make-mds-visualization-window) coordss)

(pp-hi-table (setq foo (coordss-to-hi-table coordss)))

(make-strain-selection-window foo t)


(pp-hi-table (setq bar (coordss-to-hi-table (loop for i from 0 to 1 by 0.1 collect (list i i)))))
(make-strain-selection-window bar t)

|#

(defun make-and-visualize-random-coordss (n &optional (dimensions 2) 
					  &key (coordss (make-random-coordss n dimensions))
					       upper-square 
					       (coords-names (loop for i below n collect (number->string i)))
					       coords-colors)
  ;; (reset-canvas-coord-transformations)   not needed, i think, is called in make-strain-selection-window
  (let* ((random-table (if upper-square
			   (coordss-to-upper-square-hi-table coordss coords-names coords-names)
			 (coordss-to-hi-table coordss coords-names coords-names)))
	 (coords-colors (if coords-colors
			    (if (listp coords-colors)
				coords-colors
			      (loop for i below n collect coords-colors))
			  (determine-coords-colors 'pair-if-paired-table n random-table)))
	 (table-window (make-strain-selection-window random-table t nil dimensions coords-colors coordss))
	 (mds-window (make-mds-visualization-window 
		      (get-hi-table-name table-window)
		      (get-canvas-width table-window)
		      (get-canvas-height table-window))))
    ;;(set-basis-vectors (apply #'2-basis-lvectors (firstn 3 coordss)))
    ;;(set-basis-vectors (apply #'2-basis-lvectors-swap-y (firstn 3 coordss)))
    (set-table-window-for-mds-window mds-window table-window)
    (set-basis-vectors mds-window (firstn 2 (make-unit-vectors dimensions)))
    (set-mds-f-for-mds-window mds-window 'no-choice-see-function-make-and-visualize-random-coordss)
    (set-stress-component-f-for-mds-window mds-window 'no-choice-see-function-make-and-visualize-random-coordss)
    (set-comparison-f-for-mds-window mds-window 'no-choice-see-function-make-and-visualize-random-coordss)
    (set-mds-coordss mds-window coordss)
    (visualize-mds-coordss mds-window 'new
			   coordss coords-names coords-colors
			   "OriginalConfiguration")
    (values random-table
	    coordss)))

;;(make-and-visualize-random-coordss 10)

;;;----------------------------------------------------------------------
;;;               INTERFACE VIA MDS VISUALIZATION ONLY
;;;----------------------------------------------------------------------

(defun make-mds-visualization (hi-table &optional (dimensions 2) 
			       &key coordss
				    (coords-colors 'random-all-same)
				    coords-dot-sizes 
				    coords-name-sizes
				    (mds-f 'metric-mds)
				    (stress-component-f 'square)
				    (comparison-f nil))
  (let* ((table-window (make-strain-selection-window hi-table nil nil dimensions 
						     coords-colors coordss 
						     coords-dot-sizes coords-name-sizes
						     nil))) 
    (tk-put table-window "tolisp \"'eof\"")
    (mds-hi-table table-window mds-f stress-component-f comparison-f nil 0 0)))

(defun mds-ui-merged-hi-tables (hi-tables &optional (dimensions 2) 
				&key coordss
				     (mds-f 'metric-mds)
				     (stress-component-f 'square)
				     (comparison-f nil)
				     (merge-multiple-values-f #'make-multiples-list))
  ;;this is a wrapper to add colors
  (make-mds-visualization (merge-hi-tables hi-tables merge-multiple-values-f)
			  dimensions
			  :coordss coordss
			  :coords-colors (merged-hi-tables-colors hi-tables)
			  :mds-f mds-f
			  :stress-component-f stress-component-f
			  :comparison-f comparison-f))


;;;----------------------------------------------------------------------
;;;                      STREAM ALIST
;;;----------------------------------------------------------------------

(defun make-stream-alist-item (stream-number alist-key alist-item)
  (let ((stream-alist (nth 4 (assoc stream-number *tk-streams*))))
    (setf (nth 4 (assoc stream-number *tk-streams*)) (cons (list alist-key alist-item) stream-alist))))
    

;;;----------------------------------------------------------------------
;;;                     MOVE POINTS BY MOUSE
;;;----------------------------------------------------------------------

(defun move-point-by-mouse-delta (mds-window new-point-canvas-id canvas-x-delta canvas-y-delta)
  (let ((previous-canvas-coords
	 (mds-to-canvas-coords
	  mds-window
	  (nth (get-mds-point-index-from-mds-window-canvas-id mds-window new-point-canvas-id)
	       (get-mds-coordss mds-window)))))
    (move-point-by-mouse mds-window 
			 new-point-canvas-id
			 (list (+ (nth 0 previous-canvas-coords) canvas-x-delta)
			       (+ (nth 1 previous-canvas-coords) canvas-y-delta)))))  
  
(defun move-point-by-mouse (mds-window new-point-canvas-id new-point-canvas-coords)
  ;;decoding the canvas id and canvas coords of which point and new-point-coords resp. is hardcoded and tied in with their 
  ;;setting in visualize-mds-coordss above
  (let ((which-point (get-mds-point-index-from-mds-window-canvas-id mds-window new-point-canvas-id))
	(new-point-coords (canvas-to-mds-coords mds-window new-point-canvas-coords)))
    ;;(prin1 (nth which-point (get-mds-coordss mds-window)))
    (set-mds-coordss-by-one-point mds-window which-point new-point-coords))
  (if (get-show-error-lines (get-table-window-for-mds-window mds-window))
      (update-error-lines 
       mds-window
       (nth-value 0 (deconstruct-coordss-plus-more (get-mds-coordss mds-window)))
       (nth-value 1 (deconstruct-coordss-plus-more (get-mds-coordss mds-window)))
       (get-first-error-line-item mds-window)))
  ;; this slows things down, and needs to be coordinated globally with whether we are showing predictions
  ;; in tk-interface we should set a slot on the object for if we are showing predictions, and if we 
  ;; are showing them on the fly
  ;; and should have the speed up for memoized 
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (if (get-display-errors-in-lower-triangle table-window)
	(show-table-predictions table-window (get-mds-coordss mds-window))))
  )

(defun move-all-coordss (mds-window new-coordss &optional (new-stress "Unknown"))
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (visualize-mds-coordss 
     mds-window 'update
     new-coordss
     (get-coords-names-working-copy table-window) ;;(hi-table-antigens (get-hi-table table-window))
     (get-coords-colors table-window)
     new-stress)))

#|
superseded when we annotated coords with row and col adjusts
(defun randomize-all-coordss (mds-window &optional (range-0-1 t))
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (move-all-coordss
     mds-window
     (make-random-coordss (hi-table-length (get-hi-table table-window)) 
			  (get-mds-num-dimensions mds-window)
			  (if range-0-1
			      nil
			    (hi-table-max-value (get-hi-table table-window)))))))
|#
(defun randomize-all-coordss (mds-window &optional (range-0-1 nil))
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (move-all-coordss
     mds-window
     (make-random-coordss (hi-table-length (get-hi-table table-window)) 
			  (get-mds-num-dimensions mds-window)
			  (if range-0-1
			      nil
			    (hi-table-max-value (get-hi-table table-window)))
			  (if (similarity-table-p (get-hi-table table-window))
			      (get-mds-coordss mds-window)
			    nil)
			  (get-hi-table table-window)))))

(defun randomize-coords (mds-window point-canvas-id &optional &key 
							      (range-0-1 t)
							      (display-update t)
							      (amount 4))
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (which-point (get-mds-point-index-from-mds-window-canvas-id mds-window point-canvas-id))
	 (old-coordss (nth which-point (get-mds-coordss mds-window))))
    (set-mds-coordss-by-one-point
     mds-window
     which-point
     (uniform-perturbs old-coordss amount))  
    (if display-update
	(visualize-mds-coordss mds-window 'update 
			       (get-mds-coordss mds-window)   
			       (get-coords-names-working-copy table-window)
			       (get-coords-colors table-window)
			       nil))))

(defun starting-configuration-all-coordss (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (move-all-coordss
     mds-window
     (get-mds-starting-coordss table-window))))

(defun randomize-all-coordss-a-little (mds-window &optional &key 
							    (amount 0.01))
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more (get-mds-coordss mds-window))
    (move-all-coordss
     mds-window
     (reconstruct-coordss-plus-more
      (f-lists (^ (x) (uniform-perturb x amount)) 
	       coordss)
      more))))

;;---------------  makeing points movable or unmoveable --------------------

(defun make-point-unmoveable (mds-window canvas-id)
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(point-index  (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (set-unmoveable-coords 
     table-window
     (union (list point-index)
	    (get-unmoveable-coords table-window)))
    (get-unmoveable-coords table-window)))

(defun make-point-moveable (mds-window canvas-id)
  ;; this is really remove from the unmoveable list, not add it to the moveable-list
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(point-index  (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (set-unmoveable-coords 
     table-window
     (remove point-index
	     (get-unmoveable-coords table-window)))
    (get-unmoveable-coords table-window)))


;;;----------------------------------------------------------------------
;;;                        IGNORING POINTS
;;;----------------------------------------------------------------------

#|
link:
  act on groups of points
  careful about reactivating some, we get others partially reactivated too (but could not as option)

later (cleaning up the table widnow)
  displaying just the HI table when in HI-metric mode
  (with option for displaying as hi titers)
  (maybe later add changing titers when we do the row/col adjusts)
  and vertical and horizontal scroll bars
|#

#|
this is disconnecting by making entries dont-care, but now we disconnect by having a point on a list of disconnected points
and dealing with it in the optimization in the same way we deal with unmoveable points

(defun disconnect-point (mds-window canvas-id)
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (if (member position (get-disconnected-points table-window))
	(error "assumption violation, did not expect to be able to disconnect a point already disconnected")
      (set-disconnected-points table-window (cons position (get-disconnected-points table-window))))
    (change-table-value 
     table-window
     (get-hi-table-id-from-indices table-window position position) 
     'dont-care
     'not-set
     'ortho-values)
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)
    ))

(defun reconnect-point (mds-window canvas-id)
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (if (not (member position (get-disconnected-points table-window)))
	(error "assumption violation, did not expect to be able to reconnect a point not disconnected")
      (set-disconnected-points table-window (remove position (get-disconnected-points table-window))))
    (reset-table-value 
     table-window
     (get-hi-table-id-from-indices table-window position position) 
     'ortho-values
     :ag-to-exclude-resetting (get-disconnected-points table-window)    ;; to get the UI feel right
     :sr-to-exclude-resetting (get-disconnected-points table-window))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))
|#

(defun disconnect-point (mds-window canvas-id  &optional (display-update t))
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (if (member position (get-disconnected-points table-window))
	'do-nothing-point-is-already-disconnected
      (progn
	(set-disconnected-points table-window (cons position (get-disconnected-points table-window)))
	(if display-update
	    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
				   (get-coords-names-working-copy table-window)
				   (get-coords-colors table-window)
				   nil))
	))))

(defun reconnect-point (mds-window canvas-id &optional (display-update t))
  (let ((table-window (get-table-window-for-mds-window mds-window))
	(position (get-mds-point-index-from-mds-window-canvas-id mds-window canvas-id)))
    (if (not (member position (get-disconnected-points table-window)))
	'do-nothing-point-is-already-connected
      (progn
	(set-disconnected-points table-window (remove position (get-disconnected-points table-window)))
	(if display-update
	    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
				   (get-coords-names-working-copy table-window)
				   (get-coords-colors table-window)
				   nil))))))


;;;----------------------------------------------------------------------
;;;                      PINTING POINT COORDS
;;;----------------------------------------------------------------------

(defun print-canvas-and-mds-coords (mds-window canvas-coords)
  ;;decoding the canvas id and canvas coords of which point and new-point-coords resp. is hardcoded and tied in with their 
  ;;setting in visualize-mds-coordss above
  (let ((mds-coords (canvas-to-mds-coords mds-window canvas-coords)))
    (format t "~%Canvas and MDS coords:      ~6,2f  ~6,2f         ~6,2f  ~6,2f"
	    (nth 0 canvas-coords)
	    (nth 1 canvas-coords)
	    (nth 0 mds-coords)
	    (nth 1 mds-coords))))

;;;----------------------------------------------------------------------
;;;                            ROTATE
;;;----------------------------------------------------------------------

(defun disable-basis-vector-point-indices (tk)
  ;;keeping the basis vectors referenced to point indices is good for comparing 
  ;;visualizations, but does not allow us to rotate to look around a 3 or more D image
  ;;so have a way to toggle the locking of basis vectors to points off and on
  ;;note: the first optmization iteration after the disable will move all the 
  ;;points, as this functions diables rotation, scale and translation that was in effect
  (set-scale-to-fit-mds-window tk nil)        ;;piggyback on this switch
  (set-translate-to-fit-mds-window tk nil)    ;;piggyback on this switch
  (if (get-basis-vector-point-indices tk)
      (progn
	(set-basis-vector-point-indices-backup tk (get-basis-vector-point-indices tk))
	(set-basis-vector-point-indices tk nil))
    'basis-vector-point-indices-already-disabled))

(defun enable-basis-vector-point-indices (tk)
  ;;see disbale-basis-vector-point-indices
  (set-scale-to-fit-mds-window tk t)        ;;piggyback on this switch
  (set-translate-to-fit-mds-window tk t)    ;;piggyback on this switch
  (if (get-basis-vector-point-indices tk)
      'basis-vector-point-dices-already-enabled
    (progn
      (set-basis-vector-point-indices tk (get-basis-vector-point-indices-backup tk))
      (set-basis-vector-point-indices-backup tk nil))))

(defun enable-auto-scale-and-translate-but-not-rotate (tk)
  ;;see disbale-basis-vector-point-indices
  (set-scale-to-fit-mds-window tk t)        ;;piggyback on this switch
  (set-translate-to-fit-mds-window tk t)    ;;piggyback on this switch
  (if (get-basis-vector-point-indices tk)
      ;; disable rotation
      (progn
	(set-basis-vector-point-indices-backup tk (get-basis-vector-point-indices tk))
	(set-basis-vector-point-indices tk nil))
    'basis-vector-point-indices-already-disabled))

(defun rotate-all-coordss (mds-window degrees center-x center-y)
  (error "disused fucntion, we now use rotate-all-coordss-centered")
  (let* ((table-window (get-table-window-for-mds-window mds-window)))
    (set-basis-vectors mds-window (rotate-coordss mds-window
						  (get-basis-vectors mds-window) 
						  (* degrees (/ pi 180))
						  center-x center-y
						  (get-first-dimension mds-window) (get-second-dimension mds-window)))
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))

#|
(defun rotate-all-coordss-centered (mds-window degrees canvas-center-x canvas-center-y 
				    &key point-indices-subset)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (mds-center (canvas-to-mds-coords mds-window (list canvas-center-x canvas-center-y)))
	 (mds-center-x (nth 0 mds-center))
	 (mds-center-y (nth 1 mds-center)))

    ;; translate the mds-coords so the center of rotation is mds-coords origin
    (set-mds-coordss mds-window
		     (translate-mds-coordss (get-mds-coordss mds-window) (get-first-dimension mds-window) (get-second-dimension mds-window) 
					    (- mds-center-x) (- mds-center-y)))

    ;; change the canvas-translation so the above translation does not change the canvas-coords
    (let ((canvas-translation-coords (f-list #'- (mds-to-canvas-coords mds-window '(0 0)) (mds-to-canvas-coords mds-window mds-center))))
      (set-canvas-x-coord-translation mds-window (- (get-canvas-x-coord-translation mds-window) (nth 0 canvas-translation-coords)))
      (set-canvas-y-coord-translation mds-window (- (get-canvas-y-coord-translation mds-window) (nth 1 canvas-translation-coords))))

    (if (not point-indices-subset)
	;; rotate all coords by rotating the basis vectors
	(set-basis-vectors mds-window (rotate-coordss mds-window
						      (get-basis-vectors mds-window)
						      (degrees-to-radians degrees)
						      0 0
						      (get-first-dimension mds-window) (get-second-dimension mds-window)))
      ;; just rotate some coordss
      (let* ((mds-coords (get-mds-coordss mds-window))
	     (new-rotated-coords
	      (rotate-coordss mds-window
			      (multiple-nth point-indices-subset mds-coords)
			      (degrees-to-radians degrees)
			      0 0
			      (get-first-dimension mds-window) (get-second-dimension mds-window))))
	(set-mds-coordss mds-window
			 (replace-multiple-nth point-indices-subset new-rotated-coords mds-coords))))

    ;; update the display
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))
|#

;; 2004-03-05 (replace the above), 
;; conditionally remove the centering when >3d, as we do not know how to to the translation from canvas to mds coords for >2d
;; without thinking hard, and don't feel like thinking hard about this right now
;; what we get is roation about the reference frame rather than the cursor for >2d
(defun rotate-all-coordss-centered (mds-window degrees canvas-center-x canvas-center-y 
				    &key point-indices-subset)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (mds-center (canvas-to-mds-coords mds-window (list canvas-center-x canvas-center-y)))
	 (mds-center-x (nth 0 mds-center))
	 (mds-center-y (nth 1 mds-center)))

    (if (= 2 (get-mds-num-dimensions mds-window))  ;; see note at top of this function
	(progn
	  ;; translate the mds-coords so the center of rotation is mds-coords origin
	  (set-mds-coordss mds-window
			   (translate-mds-coordss (get-mds-coordss mds-window) (get-first-dimension mds-window) (get-second-dimension mds-window) 
						  (- mds-center-x) (- mds-center-y)))

	  ;; change the canvas-translation so the above translation does not change the canvas-coords
	  (let ((canvas-translation-coords (f-list #'- (mds-to-canvas-coords mds-window '(0 0)) (mds-to-canvas-coords mds-window mds-center))))
	    (set-canvas-x-coord-translation mds-window (- (get-canvas-x-coord-translation mds-window) (nth 0 canvas-translation-coords)))
	    (set-canvas-y-coord-translation mds-window (- (get-canvas-y-coord-translation mds-window) (nth 1 canvas-translation-coords))))
	  ))

    (if (not point-indices-subset)
	;; rotate all coords by rotating the basis vectors
	(set-basis-vectors mds-window (rotate-coordss mds-window
						      (get-basis-vectors mds-window)
						      (degrees-to-radians degrees)
						      0 0
						      (get-first-dimension mds-window) (get-second-dimension mds-window)))
      ;; just rotate some coordss
      (let* ((mds-coords (get-mds-coordss mds-window))
	     (new-rotated-coords
	      (rotate-coordss mds-window
			      (multiple-nth point-indices-subset mds-coords)
			      (degrees-to-radians degrees)
			      0 0
			      (get-first-dimension mds-window) (get-second-dimension mds-window))))
	(set-mds-coordss mds-window
			 (replace-multiple-nth point-indices-subset new-rotated-coords mds-coords))))

    ;; update the display
    (visualize-mds-coordss mds-window 'update 
			   (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window)
			   nil)))

(defun rotate-coordss (mds-window coordss radians center-x center-y 
		       &optional (first-dimension (get-first-dimension mds-window)) (second-dimension (get-second-dimension mds-window)))
  (let-list ((center-x center-y) (canvas-to-mds-coords mds-window (list center-x center-y)))
	    (setq center-x 0
		  center-y 0)
	    (let ((sina (sin radians))
		  (cosa (cos radians)))
	      (loop for coords in coordss collect
		    (let ((x (- (nth first-dimension coords)  center-x))
			  (y (- (nth second-dimension coords) center-y)))
		      (append (firstn first-dimension coords)
			      (list (+ center-x (* x cosa) (* y sina)))
			      (firstn (- second-dimension first-dimension 1) (nthcdr (inc first-dimension) coords))
			      (list (+ center-y (* y cosa) (- (* x sina))))
			      (nthcdr (inc second-dimension) coords)))))))

(defun set-dimension-pair-from-dimensionPair (mds-window canvas-id)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (dimension-pair (get-dimension-pair-from-tk-id table-window canvas-id)))
    (set-first-dimension  mds-window (nth 0 dimension-pair))
    (set-second-dimension mds-window (nth 1 dimension-pair))))

(defun set-dimension-pair-from-dimensionPair (mds-window canvas-id)
  (let* ((dimension-pair (get-dimension-pair-from-tk-id mds-window canvas-id)))
    (set-first-dimension  mds-window (nth 0 dimension-pair))
    (set-second-dimension mds-window (nth 1 dimension-pair))))

(defun set-dimension-pair-from-axis (mds-window canvas-id)
  ;;not used
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (axis (get-axis-from-tk-id table-window canvas-id)))
    (print 'have-on-implement-move-axis-yet)))


;;;----------------------------------------------------------------------
;;;                            ZOOM
;;;----------------------------------------------------------------------

(defun zoom-all-coordss (mds-window percentage)
  (error "we don't use zoom-all-coordss anymore, we use zoom-all-coordss-centered")
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-canvas-x-coord-scale mds-window (add-percentage percentage (get-canvas-x-coord-scale mds-window)))
    (set-canvas-y-coord-scale mds-window (add-percentage percentage (get-canvas-y-coord-scale mds-window)))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
			   (get-coords-names-working-copy table-window) 
			   (get-coords-colors table-window)
			   nil)))

(defun zoom-coordss (coordss scale)
  (loop for coords in coordss collect
	(loop for x in coords collect
	      (* x scale))))		   

(defun zoom-all-coordss-centered (mds-window scale canvas-center-x canvas-center-y
				  &key point-indices-subset)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (mds-center (canvas-to-mds-coords mds-window (list canvas-center-x canvas-center-y)))
	 (mds-center-x (nth 0 mds-center))
	 (mds-center-y (nth 1 mds-center)))

    ;; translate the mds-coords so the center of zoom is mds-coords origin
    (set-mds-coordss mds-window
		     (translate-mds-coordss (get-mds-coordss mds-window) (get-first-dimension mds-window) (get-second-dimension mds-window) 
					    (- mds-center-x) (- mds-center-y)))

    ;; change the canvas-translation so the above translation does not change the canvas-coords
    (let ((canvas-translation-coords (f-list #'- (mds-to-canvas-coords mds-window '(0 0)) (mds-to-canvas-coords mds-window mds-center))))
      (set-canvas-x-coord-translation mds-window (- (get-canvas-x-coord-translation mds-window) (nth 0 canvas-translation-coords)))
      (set-canvas-y-coord-translation mds-window (- (get-canvas-y-coord-translation mds-window) (nth 1 canvas-translation-coords))))

    (if (not point-indices-subset)

	;; zoom all coords by zooming the scale 
	(progn
	  (set-canvas-x-coord-scale mds-window (* (get-canvas-x-coord-scale mds-window) scale))
	  (set-canvas-y-coord-scale mds-window (* (get-canvas-y-coord-scale mds-window) scale)))

      ;; zoom some coords
      (let* ((mds-coords (get-mds-coordss mds-window))
	     (new-zoomed-coords
	      (zoom-coordss (multiple-nth point-indices-subset mds-coords) scale)))
	(set-mds-coordss mds-window
			 (replace-multiple-nth point-indices-subset new-zoomed-coords mds-coords))))

    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window)
			   (get-coords-names-working-copy table-window) 
			   (get-coords-colors table-window)
			   nil)))


;;;----------------------------------------------------------------------
;;;                          TRANSLATE
;;;----------------------------------------------------------------------

(defun translate-all-coordss (mds-window x-translate y-translate)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (set-canvas-x-coord-translation mds-window (+ x-translate (get-canvas-x-coord-translation mds-window)))
    (set-canvas-y-coord-translation mds-window (+ y-translate (get-canvas-y-coord-translation mds-window)))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window) 
			   (get-coords-colors table-window) nil)))

(defun translate-mds-coordss (coordss first-dimension second-dimension first-delta second-delta)
  (multiple-value-bind (coordss more)
      (deconstruct-coordss-plus-more coordss)
    (reconstruct-coordss-plus-more
     (loop for coords in coordss collect
	   (let ((first-replacement
		  (replace-nth first-dimension
			       (+ (nth first-dimension coords) first-delta)
			       coords)))
	     (replace-nth second-dimension
			  (+ (nth second-dimension first-replacement) second-delta)
			  first-replacement)))
     more)))

(defun center-coordss (mds-window new-canvas-x-center new-canvas-y-center)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 ;; THIS ONLY WORKS FOR 2D (see canvas-to-mds-coords) 
	 (new-mds-center (canvas-to-mds-coords mds-window (list new-canvas-x-center new-canvas-y-center)))
	 (new-mds-x-center (nth 0 new-mds-center))
	 (new-mds-y-center (nth 1 new-mds-center)))
    (print "centering coords only works reliably for 2d right now")
    ;;(setq *canvas-x-coord-translation* (- *canvas-x-coord-translation* new-canvas-x-center))
    ;;(setq *canvas-y-coord-translation* (- *canvas-y-coord-translation* new-canvas-y-center))
    ;;(set-mds-coordss mds-window (translate-mds-coordss (get-mds-coordss mds-window) (- new-mds-x-center) (- new-mds-y-center)))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window) 
			   (get-coords-colors table-window) nil)))


;;;----------------------------------------------------------------------
;;;                            FLIP
;;;----------------------------------------------------------------------

(defun flip-all-coordss (mds-window x-or-y?)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (case x-or-y?
      (x (set-canvas-x-coord-scale mds-window (- (get-canvas-x-coord-scale mds-window))))
      (y (set-canvas-y-coord-scale mds-window (- (get-canvas-y-coord-scale mds-window))))
      (t (error "neither x nor y supplied to flip-all-coordss")))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window) ;;(hi-table-antigens (get-hi-table table-window))
			   (get-coords-colors table-window) nil)))


(defun flip-coordss (mds-window x-or-y? &optional &key point-indices-subset)
  (if (not point-indices-subset)
      (flip-all-coordss mds-window x-or-y?)
    (let* ((table-window (get-table-window-for-mds-window mds-window))
	   (mds-coords (get-mds-coordss mds-window))
	   (new-flipped-coords
	    (flip-some-coordss (multiple-nth point-indices-subset mds-coords)
			       x-or-y?)))
      (set-mds-coordss mds-window
		       (replace-multiple-nth point-indices-subset new-flipped-coords mds-coords))
      (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window) 
			     (get-coords-names-working-copy table-window) ;;(hi-table-antigens (get-hi-table table-window))
			     (get-coords-colors table-window) nil))))

(defun flip-some-coordss (coordss x-or-y?)
  ;; this needs some extra work to remove the canvas rotation, then put it back in
  ;; it also only works for 2d
  ;; but it is enuf for now, i just want the ability to flop (2003-03-09)
  (let ((av-x (av (nths 0 coordss)))
	(av-y (av (nths 1 coordss))))
    (loop for (x y) in coordss collect
	  (if (eql x-or-y? 'x)
	      (list (+ (- (- x av-x)) av-x) y)
	    (list x (+ (- (- y av-y)) av-y))))))
    

;;;----------------------------------------------------------------------
;;;                         FILL WINDOW
;;;----------------------------------------------------------------------

(defun fill-window (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (visualize-mds-coordss mds-window 'update (get-mds-coordss mds-window) 
			   (get-coords-names-working-copy table-window)
			   (get-coords-colors table-window) nil)))


;;;----------------------------------------------------------------------
;;;                         HILLCLIMB
;;;----------------------------------------------------------------------

(defun hillclimb-from-mds-window-multiple (mds-window new-window? 
                                           &optional (num-climbs 100) (incremental-dribble-modulus 1))
  (loop for i below 20 do (hillclimb-from-mds-window mds-window new-window? num-climbs incremental-dribble-modulus)))

;; the mp package is not how multiprocessing is done in cmucl, so for now, as we do not have graphics with cmucl
;; anyhow. comment out for cmucl
#-:cmu
(defun hillclimb-from-mds-window (mds-window new-window? 
				  &optional (num-climbs 100) (incremental-dribble-modulus 1))
  ;; run in background so we can do a stop
  (lisp-system-independent-process-run-function
   (format nil "background-mds-hi-table-~a" (gensym))
   (^ () 
      (let ((mds-window
	     (nth-value 1 
			(mds-hi-table (get-table-window-for-mds-window mds-window)
				      (get-mds-f-for-mds-window mds-window)
				      (get-stress-component-f-for-mds-window mds-window)
				      (get-comparison-f-for-mds-window mds-window)
				      :existing-mds-window (if new-window?
							       nil
							     mds-window)
				      :new-window-canvas-coord-transformations (if new-window? (get-canvas-coord-transformations mds-window)) ;; copy from seeding window
                                      :raise-points (get-raise-points mds-window)
                                      :lower-points (get-lower-points mds-window)
				      :num-trials 250     ;;the max num trials on each climb
				      :num-climbs num-climbs
				      :incremental-dribble-modulus incremental-dribble-modulus))))
	;; "Running... is set in mds-hi-table.  not set in this function, as we might not have the mds-window if it is a new-window
	(if *stop-optimization*
	    (set-mds-window-run-indicator-text mds-window "Running...stopped")
	  (if new-window?
	      (set-mds-window-run-indicator-text mds-window "Idle")
	    (set-mds-window-run-indicator-text mds-window "Running...done")))))))
			  
(defun set-mds-window-run-indicator-text (mds-window text)
  (if (get-mds-window-run-indicator-canvas-item mds-window)  ;; first call fails, so to save sending a nil to tk as the item, send nothing
      (tk-put mds-window "setTextText ~d ~a" (get-mds-window-run-indicator-canvas-item mds-window) text)))

(defun stop-optimization (mds-window)
  (setq *stop-optimization* t))


;;;----------------------------------------------------------------------
;;;                       XGOBI from MDS window
;;;----------------------------------------------------------------------

(defun xgobi-from-mds-window (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (xgobi (get-hi-table table-window)
	   :starting-coordss (get-mds-coordss mds-window))))

(defun xgvis-from-mds-window (mds-window)
  (let ((table-window (get-table-window-for-mds-window mds-window)))
    (xgvis (get-hi-table table-window)
	   :dims (get-mds-num-dimensions table-window)
	   :starting-coordss (get-mds-coordss mds-window))))


;;;----------------------------------------------------------------------
;;;                    BASIS VECTORS FROM POINTS
;;;----------------------------------------------------------------------

(defun set-and-revisualize-basis-vectors-from-tk-item-point-ids (mds-window tk-item-p0 tk-item-p1 tk-item-p2)
  (let ((p0-index (get-mds-point-index-from-mds-window-canvas-id mds-window tk-item-p0))
	(p1-index (get-mds-point-index-from-mds-window-canvas-id mds-window tk-item-p1))
	(p2-index (get-mds-point-index-from-mds-window-canvas-id mds-window tk-item-p2))
	(table-window (get-table-window-for-mds-window mds-window)))
    (set-basis-vectors-from-point-indices mds-window (get-mds-coordss mds-window) p0-index p1-index p2-index)
    (set-basis-vector-point-indices mds-window (list p0-index p1-index p2-index))
    (visualize-mds-coordss
     mds-window 'update (get-mds-coordss mds-window) 
     (get-coords-names-working-copy table-window) ;;(hi-table-antigens (get-hi-table table-window))
     (get-coords-colors table-window)
     nil)))

(defun set-basis-vectors-from-point-indices (mds-window coordss p0-index p1-index p2-index)
  (let ((basis-vectors (2-basis-lvectors ;;2-basis-lvectors-swap-y
			(nth p0-index coordss)
			(nth p1-index coordss)
			(nth p2-index coordss))))
    ;;(print (list (list (nth p0-index coordss) (nth p1-index coordss) (nth p2-index coordss)) basis-vectors))
    (set-basis-vectors mds-window basis-vectors)))


;;;----------------------------------------------------------------------
;;;                      image conversion
;;;----------------------------------------------------------------------

(defun tk-image-convert-ps-to-gif (filename &optional (gif-filename "") &key scale-geometry)
  (if (not (running-on-windows-p))
      (run-shell-command (format nil "mogrify ~a -format gif ~a ~a" 
				 (if scale-geometry
				     (format nil "-scale ~a" scale-geometry)
				   "")
				 filename
				 gif-filename
				 )
			 :wait t)))

(defun tk-image-convert-ps-to-png (filename &optional (png-filename "") &key scale-geometry)
  (if (not (running-on-windows-p))
      (run-shell-command (format nil "mogrify -format png ~a ~a" 
				 (if scale-geometry
				     (format nil "-scale ~a" scale-geometry)
				   "")
				 filename
				 png-filename
				 )
			 :wait t)))

(defun tk-image-convert-ps-to-pdf (filename &optional pdf-filename)
  (if (not (running-on-windows-p))
      (run-shell-command (format nil "ps2pdf ~a ~a" 
				 filename
				 (if pdf-filename
				     pdf-filename
				   (if (equal ".ps" (substring filename (- (length filename) 3)))
				       (string-append (substring filename 0 (- (length filename) 4)) ".pdf")
				     (string-append filename ".pdf"))))
			 :wait t)))

;; rename the above to have less specific names
(defun ps-to-gif (filename &optional (gif-filename "") &key scale-geometry) 
  (tk-image-convert-ps-to-gif filename gif-filename :scale-geometry scale-geometry))
(defun ps-to-png (filename &optional (png-filename "") &key scale-geometry)
  (tk-image-convert-ps-to-png filename png-filename :scale-geometry scale-geometry))
(defun ps-to-pdf (filename &optional pdf-filename)      (tk-image-convert-ps-to-pdf filename pdf-filename))


(defun gif-from-canvas (tk filename-unpostfixed)
  (tk-put tk ".c postscript -file ~a.ps" filename-unpostfixed)
  (sleep 1)
  (tk-image-convert-ps-to-gif (format nil "~a.ps" filename-unpostfixed))
  (sleep 1))

(defun png-from-canvas (tk filename-unpostfixed &optional &key use-encapsulated-postscript)
  (tk-put tk ".c postscript -file ~a.~a" filename-unpostfixed (if use-encapsulated-postscript "eps" "ps"))
  (sleep 1)
  (tk-image-convert-ps-to-png (format nil "~a.~a" filename-unpostfixed (if use-encapsulated-postscript "eps" "ps")))
  (sleep 1))

(defun pdf-from-canvas (tk filename-unpostfixed &optional &key use-encapsulated-postscript)
  (tk-put tk ".c postscript -file ~a.~a" filename-unpostfixed (if use-encapsulated-postscript "eps" "ps"))
  (sleep 1)
  (tk-image-convert-ps-to-pdf-postfixed-name (format nil "~a.~a" filename-unpostfixed (if use-encapsulated-postscript "eps" "ps")))
  (sleep 1))

(defun ps-pdf-and-gif-from-canvas (tk ps-filename pdf-filename gif-filename)
  (tk-put tk ".c postscript -file ~a" ps-filename)
  (if (not *ugly-hack-to-write-to-file-instead-of-wish*)
      (progn
	(sleep 1)
	(tk-image-convert-ps-to-pdf ps-filename pdf-filename)
	(sleep 1)
	(tk-image-convert-ps-to-gif ps-filename gif-filename)
	(sleep 1))))

(defun ps-pdf-and-png-from-canvas (tk ps-filename pdf-filename gif-filename)
  (tk-put tk ".c postscript -file ~a" ps-filename)
  (if (not *ugly-hack-to-write-to-file-instead-of-wish*)
      (progn
	(sleep 1)
	(tk-image-convert-ps-to-pdf ps-filename pdf-filename)
	(sleep 1)
	(tk-image-convert-ps-to-png ps-filename gif-filename)
	(sleep 1))))



;;;----------------------------------------------------------------------
;;;              hacky programatic stuff in the window
;;; (done to support the timeseries work for the strain slection meeting)
;;;----------------------------------------------------------------------

(defun run-command-in-mds-window-by-indices (mds-window strain-indices command)
  (loop for strain-index in strain-indices do
	(tk-put mds-window command (get-mds-window-dot-canvas-id-from-strain-index mds-window strain-index))
	(tk-put mds-window command (get-mds-window-name-canvas-id-from-strain-index mds-window strain-index))))

(defun run-command-in-mds-window-by-names (mds-window strain-names command)
  (run-command-in-mds-window-by-indices
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for strain-name in strain-names collect
	   (position strain-name all-strain-names)))
   command))

(defun run-command-in-mds-window-excluding-names (mds-window strain-names command)
  (run-command-in-mds-window-by-indices
   mds-window 
   (let ((all-strain-names (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
     (loop for strain-name in all-strain-names
	 when (not (member strain-name strain-names))
	 collect (position strain-name all-strain-names)))
   command))


(defun move-strains-out-of-view-by-names (mds-window strain-names) (run-command-in-mds-window-by-names mds-window strain-names ".c move ~d  1000000  1000000"))
(defun move-strains-into-view-by-names   (mds-window strain-names) (run-command-in-mds-window-by-names mds-window strain-names ".c move ~d -1000000 -1000000"))

(defun move-strains-out-of-view-excluding-names (mds-window strain-names) (run-command-in-mds-window-excluding-names mds-window strain-names ".c move ~d  1000000  1000000"))
(defun move-strains-into-view-excluding-names   (mds-window strain-names) (run-command-in-mds-window-excluding-names mds-window strain-names ".c move ~d -1000000 -1000000"))





;;;----------------------------------------------------------------------
;;;                      operating on groups
;;;----------------------------------------------------------------------

#|
       pathName addtag tag searchSpec ?arg arg ...?
              enclosed x1 y1 x2 y2
                     Selects  all  the  items completely enclosed
                     within the rectangular region given  by  x1,
                     y1,  x2, and y2.  X1 must be no greater then
                     x2 and y1 must be no greater than y2.
              overlapping x1 y1 x2 y2
                     Selects all the items that  overlap  or  are
                     enclosed within the rectangular region given
                     by x1, y1,  x2,  and  y2.   X1  must  be  no
                     greater  then  x2  and y1 must be no greater
                     than y2.

       pathName dtag tagOrId ?tagToDelete?
              For  each of the items given by tagOrId, delete the
              tag given by tagToDelete from  the  list  of  those
              associated  with the item.  If an item doesn't have
              the tag tagToDelete then the item is unaffected  by
              the  command.   If  tagToDelete  is omitted then it
              defaults to tagOrId.  This command returns an empty
              string.


what to do.
  click left on a point and it becomes selected (and all others deselected)
  click shift-left on a point and it is added to the list of selected points
  motion with click-left, or shift-click-left, draws a rectangle, and anything 
    overlapping are included

  then any operation on one of the selected items (all in a particular color)
    is an operation on all of them (eg disconnecting or freezing)





Moving/freezing/disconnecting individual points:
-----------------------------------------------
   on the the point itself (the circle, which will be highlighed when selected)
     click-and-drag-left to move a point
     control-left  toggles freezing a point
     control-right toggles disconnecting a point


Avidity/Reactivity:
------------------
   on the NAME of the point (name will highlight if selected):
     shift-left to decrease avidity/reactivity
     shift-right to increase avidity/reactivity


Moving all points:
-----------------
  click right on the "01" to disable autoscaling

  then
    click-and-drag-left to translate
    click-and-drag-right horizontally to rotate
    click-and-drag-shift-right vertically to scale

  (double-click right on the "01" to enable autoscaling later if u want)


Selecting groups of points:
--------------------------
  click-and-drag-shift-left selects points and prints their names
    (but they go to the lisp-listener so you don't see them yet)
    Note, the point's name needs to be completely within the rectangle
    for the point to be selected.


Printing coordinates:
--------------------
  shift-left writes the coordinates of the cursor
    (but writes them to the lisp-listener so you don't see them...)


Making a ruler:
--------------
   click-and-drag-shift-left to make a ruler
   control-x with the ruler selected (by having the cursor over the line
     and the line becoming red) to delete the ruler


Printing distances:
------------------
  the icon above the printer icon writes all distances between points
    to C:/program files/acl60/mds-distances (assuming you installed lisp
    in the usual folder.
  

|#
