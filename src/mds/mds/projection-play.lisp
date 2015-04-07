(in-package user)

(defun rotate (xy)
  (list (2dp (lvector-dot-product xy *canvas-basis-vector-0*))
	(2dp (lvector-dot-product xy *canvas-basis-vector-1*))))

(defun anti-rotate (xy)
  (list (2dp (lvector-dot-product xy anti0))
	(2dp (lvector-dot-product xy anti1))))
  
(defun make-anti ()
  (setq anti0 (mapcar #'2dp (list (nth 0 *canvas-basis-vector-0*) (- (nth 1 *canvas-basis-vector-0*)))))
  (setq anti1 (mapcar #'2dp (list (- (nth 0 *canvas-basis-vector-1*)) (nth 1 *canvas-basis-vector-1*)))))

(defun theta-r ()
  (mapcar #'2dp (list (acos (nth 0 *canvas-basis-vector-0*))
		      (- (asin (nth 1 *canvas-basis-vector-0*)))
		      (asin (nth 0 *canvas-basis-vector-1*))
		      (acos (nth 1 *canvas-basis-vector-1*)))))

(defun theta-ar ()
  (mapcar #'2dp (list (acos (nth 0 anti0))
		      (asin (nth 1 anti0))
		      (- (asin (nth 0 anti1)))
		      (acos (nth 1 anti1)))))


;;----------------fake-----------------

(defun make-r-vectors (theta)
  (setq *r0* (list (cos theta) (- (sin theta))))
  (setq *r1* (list (sin theta) (cos theta))))

(defun make-anti-r ()
  (setq *ar0* (mapcar #'2dp (list (nth 0 *r0*) (- (nth 1 *r0*)))))
  (setq *ar1* (mapcar #'2dp (list (- (nth 0 *r1*)) (nth 1 *r1*)))))

(defun r (xy)
  (list (2dp (lvector-dot-product xy *r0*))
	(2dp (lvector-dot-product xy *r1*))))

(defun anti-r (xy)
  (list (2dp (lvector-dot-product xy *ar0*))
	(2dp (lvector-dot-product xy *ar1*))))
  

;;------------------------------------------

mds to canvas coords
  rotate
  scale
  translate

so for canvas to mds
  translate
  scale
  rotate

;;------------- an error case --------------

;; here we have a problem (just happened to come across these basis vectors)

(setq cbv0 *canvas-basis-vector-0*)
(-0.28913565601771274d0 -0.9572881344815712d0)
USER(19): (setq cbv1 *canvas-basis-vector-1*)
(-0.9572881344815716d0 0.28913565601771224d0)

USER(22): (mds-to-canvas-coords '(0 0))
(395.0d0 268.0d0)
USER(23): (canvas-to-mds-coords (mds-to-canvas-coords '(0 0)))
(0.0d0 0.0d0)
USER(24): (canvas-to-mds-coords (mds-to-canvas-coords '(1 1)))   
(-0.2792288793158117d0 -1.3863734103610164d0)                    <------------ here is the production problem
USER(25): (canvas-to-mds-coords (mds-to-canvas-coords '(1 0)))
(-0.8328011448384136d0 -0.5535722655226034d0)
USER(26): (canvas-to-mds-coords (mds-to-canvas-coords '(0 1)))
(0.5535722655226023d0 -0.8328011448384145d0)


USER(29): (rotate '(0 0))
(0.0 0.0)
USER(30): (rotate '(1 1))
(-1.25 -0.67)
USER(31): (make-anti)
(0.96 0.29)
USER(32): (anti-rotate '(1 1))
(0.67 1.25)
USER(33): (anti-rotate (rotate '(1 1)))
		       (anti-rotate (rotate '(1 1)))
(-0.28 -1.39)
USER(34): (anti-rotate (rotate '(1 1)))
(-0.28 -1.39)                                                        <-------- my simplification of above production problem



USER(39): (theta-r)
(1.86 1.28 -1.28 1.28)
USER(40): (theta-ar)
(1.87 1.29 -1.29 1.28)
;; so the angles are the same


USER(41): (theta-r)
(1.86 1.28 -1.28 1.28)
;; ahh, these first two angles should not be different

USER(42): (firstn 2 (theta-r))
(1.86 1.28)
USER(43): (apply #'+ (firstn 2 (theta-r)))
3.1399999
;; ahh, they add up to PI

(setq cbv0 *canvas-basis-vector-0*)
(-0.28913565601771274d0 -0.9572881344815712d0)
USER(19): (setq cbv1 *canvas-basis-vector-1*)
(-0.9572881344815716d0 0.28913565601771224d0)
;; here we see the problem in the canvas vectors
;; 0,0 is minus 1,1
;; ah and 1,0 which is normally minus 0,1 is the same
;; OK, so really *canvas-basis-vector-1* is -1 what it "should be"

;; ahh, maybe it is not v1 that is wrong, because
;; theta-r has 0,0 giving a different angle than the others.

(setq anti1 (lvector-scalar-multiply -1 anti1))
USER(46): anti0
(-0.29 0.96)
USER(47): anti1
(-0.96 -0.29)
USER(48): (theta-ar)
(1.87 1.29 1.29 1.87)
;;yeah, don't expect this to work, we do not have the same angle everywhere

(anti-rotate (rotate '(1 1)))
(-0.28 1.39)
;;still does not work, as i would expect

(make-anti)


;;;;;;;;;;;;;;;;------------  let me see if i can figure out why --------------------------

(defun 2-basis-lvectors (p0 p1 p2)
  (let* ((basis0 (lvector-norm (lvector-difference p1 p0)))
	 (p2-p0 (lvector-difference p2 p0))
	 (basis1 (lvector-norm (lvector-difference p2-p0 (lvector-scalar-multiply (lvector-dot-product p2-p0 basis0) basis0)))))
    (list basis0 basis1)))

(defun thetar (basis0 basis1)
  (mapcar #'2dp (list (acos (nth 0 basis0))
		      (asin (- (nth 1 basis0)))
		      (asin (nth 0 basis1))
		      (acos (nth 1 basis1)))))

(defun hack-make-anti ()
  (let ((theta (acos (nth 0 *canvas-basis-vector-0*))))
    ;; i was going to make a reverse matrix from just the element 0 0 but then
    ;; i realize we need the full inversion of the matrix
    ;; ahh--do matrix invert!
    (setq anti0 (mapcar #'2dp (list (nth 0 *canvas-basis-vector-0*) (- (nth 1 *canvas-basis-vector-0*)))))
    (setq anti1 (mapcar #'2dp (list (- (nth 0 *canvas-basis-vector-1*)) (nth 1 *canvas-basis-vector-1*))))))


(apply #'thetar (2-basis-lvectors '(0 0) '(1 0) '(0 1)))
(0.0 0.0 0.0 0.0)
USER(62): (apply #'thetar (2-basis-lvectors '(0 0) '(1 0) '(0 -1)))
(0.0 0.0 0.0 3.14)
USER(63): (apply #'thetar (2-basis-lvectors '(0 0) '(10 0) '(0 -1)))
(0.0 0.0 0.0 3.14)
USER(64): (apply #'thetar (2-basis-lvectors '(0 0) '(-1 0) '(0 -1)))
(3.14 0.0 0.0 3.14)


USER(66): (apply #'thetar (2-basis-lvectors '(0 0) '(-1.1 0.1) '(0.1 -1.1)))
(3.05 -0.09 -0.09 3.05)
USER(67): (apply #'thetar (2-basis-lvectors '(0 0) '(1.1 0.1) '(0.1 -1.1)))
(0.09 -0.09 0.09 3.05)
USER(68): (apply #'thetar (2-basis-lvectors '(0 0) '(1.1 0.1) '(0.1 1.1)))
(0.09 -0.09 -0.09 0.09)

USER(78): (apply #'thetar (2-basis-lvectors '(0 0) '(1.1 -0.1) '(0.1 1.1)))
(0.09 0.09 0.09 0.09)
USER(79): (apply #'thetar (2-basis-lvectors '(0 0) '(1.1 -0.1) '(-0.1 1.1)))
(0.09 0.09 0.09 0.09)
USER(80): (apply #'thetar (2-basis-lvectors '(0 0) '(1.1 0.1) '(-0.1 1.1)))
(0.09 -0.09 -0.09 0.09)


;;------------------------------ now by matrix inversion -------------------------------

(defun make-anti ()
  (let ((a (array-list (list *canvas-basis-vector-0* *canvas-basis-vector-1*))))
    (let ((a-inverse (list-array (gaussj a 2 (make-array '(2 2) :initial-contents '((0 0) (0 0))) 2))))
      (setq anti0 (nth 0 a-inverse))
      (setq anti1 (nth 1 a-inverse))
      a-inverse)))

(setq *canvas-basis-vector-0* '(-0.28913565601771274d0 -0.9572881344815712d0))
(setq *canvas-basis-vector-1* '(-0.9572881344815716d0 0.28913565601771224d0))
(make-anti)

anti0
(-0.9572881344815712d0 -0.28913565601771224d0)
USER(25): anti1
(0.28913565601771274d0 -0.9572881344815715d0)

USER(23): (anti-rotate (rotate '(1 1)))
(1.39 0.28)
;; OUCH, still does not work!


(defun foo (al l)
  (list (2dp (lvector-dot-product l (nth 0 al)))
	(2dp (lvector-dot-product l (nth 1 al)))))



(defun num-rows (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 0))

(defun invert-matrix (matrix &optional (destructive T))
  "Find the inverse of a matrix.  By default this operation is
  destructive.  If you want to preserve the original matrix, call this
  function with an argument of NIL to destructive."
  (let ((result (if destructive matrix (copy-matrix matrix)))
        (size (num-rows matrix))
        (temp 0))
    (dotimes (i size result)
      (setf temp (aref result i i))
      (dotimes (j size)
        (setf (aref result i j)
              (if (= i j)
                  (/ (aref result i j))
                  (/ (aref result i j) temp))))
      (dotimes (j size)
        (unless (= i j)
          (setf temp (aref result j i)
                (aref result j i) 0)
          (dotimes (k size)
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))))))))


(defun make-anti ()
  (let ((a-inverse (list-array (invert-matrix (array-list (list *canvas-basis-vector-0* *canvas-basis-vector-1*))))))
    (setq anti0 (nth 0 a-inverse))
    (setq anti1 (nth 1 a-inverse))
    a-inverse))


(list-array (invert-matrix (array-list (list *canvas-basis-vector-0* *canvas-basis-vector-1*))))
((-0.2891356560177125d0 -0.9572881344815712d0) (-0.9572881344815714d0 0.28913565601771274d0))
USER(90): (make-anti)
((-0.2891356560177125d0 -0.9572881344815712d0) (-0.9572881344815714d0 0.28913565601771274d0))
USER(91): anti0
(-0.2891356560177125d0 -0.9572881344815712d0)
USER(92): anti1
(-0.9572881344815714d0 0.28913565601771274d0)
USER(93): (rotate '(1 2))
(-2.2 -0.38)
USER(94): (anti-rotate (rotate '(1 2)))
(1.0 2.0)


fuck!
the numerical recipies matrix inversion was wrong!