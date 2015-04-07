(in-package user)

(defun scatter (base p)
  (loop for e in base collect
        (if (< (random .9999999) p)
          (mod (+ 1 e) 2)
          e)))

(defun scatter-code (p so-far req-length)
  (if (= req-length (length so-far))
    so-far
    (let ((next (scatter (car (last so-far)) p)))
      (scatter-code p (reverse (cons next (reverse so-far))) req-length))))

(defun list-xor (a b)
  (loop for e in a for f in b collect (mod (+ e f) 2)))

(defun nary-list-xor (&rest args)
  (apply #'mapcar (^ (&rest args) (mod (apply #'+ args) 2)) args))

(defun nD-scatter-f (&rest codes)
  (^ (&rest coords)
     (apply #'nary-list-xor 
            (mapcar #'nth coords codes))))

(defun nD-scatter (codes &rest coords)
  (apply #'nary-list-xor 
	 (mapcar #'nth coords codes)))

(defun make-scatter-code-f (num-sensors num-entries p num-dims)
  (apply #'nD-scatter-f 
	 (loop for i below num-sensors collect
	       (scatter-code p (list (random-base num-dims 2)) num-entries))))


#|
(setq code (scatter-code 0.01 (list (zero-base 100)) 1000))
(g-plot (mapcar (^ (x) (hd x (nth (floor (/ (length code) 2)) code))) code))

(setq one-d (scatter-code 0.01 (list (zero-base 100)) 1000))
(setq another-d (scatter-code 0.01 (list (zero-base 100)) 1000))
(setq 2d-scatter (nD-scatter-f one-d another-d))
(funcall 2d-scatter 3 4)
(let ((some-point (funcall 2d-scatter 200 400)))
  (loop for slice below 1000 by 100 do
	(let ((code (loop for i below 1000 collect
			  (funcall 2d-scatter slice i))))
	  (apply #'g-plot
		 (mapcar (^ (x) (hd x some-point)) code)
		 (if (= 0 slice)
		     nil
		   '(:refresh nil))))))
	
(let ((some-point (funcall 2d-scatter 200 400)))
  (loop for slice from 100 to 500 by 25 do
	(let ((code (loop for i below 1000 collect
			  (funcall 2d-scatter slice i))))
	  (apply #'g-plot
		 (mapcar (^ (x) (hd x some-point)) code)
		 (if (= 100 slice)
		     nil
		   '(:refresh nil))))))

(let ((some-point (funcall 2d-scatter 700 300)))
  (loop for slice from 550 to 850 by 25 do
	(let ((code (loop for i below 1000 collect
			  (funcall 2d-scatter slice i))))
	  (apply #'g-plot
		 (mapcar (^ (x) (hd x some-point)) code)
		 :element-name (format nil "@~d" slice)
		 (if (= 550 slice)
		     nil
		   '(:refresh nil))))))



(make-scatter-code-f 3 100 0.01 256)
|#


