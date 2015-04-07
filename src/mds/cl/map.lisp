(in-package user)

(define (map-append f &rest args)
  (apply-append (apply #'mapcar f args)))

(define (map-union f &rest args)
    ;;this is not map-append with remove duplicates, map-append is apply append to mapcar
    ;;and this is apply union to mapcar (but union is not naturally nary so we have the futzing)
  (let ((result (apply #'mapcar f args)))
    (loop for e on result 
          when (not (member (car e) (cdr e) :test #'equal))
	append (car e))))

(defun map-all (f l)
  (loop for e in l collect
	(if (consp e)
	    (map-all f e)
	  (funcall f e))))

(defun map-all2 (f l0 l1)
  ;;would be nice to have just one map-all and it takes &rest l's
  (loop for e0 in l0 for e1 in l1 collect
	(if (consp e0)
	    (map-all2 f e0 e1)
	  (funcall f e0 e1))))

(defun map^2 (f l)
  (loop for (e . rest) on l until (null rest) collect
	(mapcar (^ (x) (funcall f e x)) rest)))


(defun map-apply (f &rest args)
  ;; call apply on the function
  ;;  to avoid writing (mapcar (^ (l) (apply #'foo l)) ll)
  (apply #'mapcar (^ (l) (apply f l)) args))


;;;------------------------------------------------------------------------
;;;             TRY SOME MAPS FOR FUNCTIONS (^ (x) (f x y))
;;;------------------------------------------------------------------------

;;this needs to be cleaned up to get a compile on the function
(defmacro mapcar-mx (f x &rest args)
  `(mapcar (^ (&rest argss)
	      (apply ,f ,x argss))
	   ,@args))

(defmacro mapcar-xm (f x &rest args)
  `(mapcar (^ (&rest argss)
	      (apply ,f (append argss (list ,x))))
	   ,@args))

#|
(mapcar-mx #'list 3 '(2 3 4))
|#


#|
(mapcar #'cdr '((0 1 2) (a b c)))
(map-append #'cdr '((0 1 2) (a b c)))
(map-union #'cdr '((0 1 2) (a b c)))
(map-union #'cdr '((0 1 2) (a b c) (d b c)))
|#

(define (filter f l)
  (loop for e in l when (not (funcall f e)) collect e))

(define (collect f l)
    (loop for e in l when (funcall f e) collect e))

#|
(filter (^ (a) (> a 3)) '(0 1 2 3 4 5 6))   ;(0 1 2 3)
(collect (^ (a) (> a 3)) '(0 1 2 3 4 5 6))  ;(4 5 6)
|#

(defun alist-group (f l)
  ;; could be better if we could pass lists, not just a list, and if we could pass in the assoc test fucntion
  (let ((alist nil))
    (loop for e in l do
	  (let ((key (funcall f e)))
	    (if (assoc key alist :test #'equal)
		(push-end e (cdr (assoc key alist :test #'equal)))
	      (push-end (list key e) alist))))
    alist))

#|
(alist-group #'inc '(0 1 2 1 1 1 3))
((1 0) (2 1 1 1 1) (3 2) (4 3))
|#

;;;------------------------------------------------------------------------
;;;                      COLLECT UNIQUE
;;;------------------------------------------------------------------------

(defun collect-uniques (num-required generation-thunk &key (test #'eql))
  (let (so-far 
	(trials 0))
    (loop for i from 1
	until (= (length so-far) num-required) do
	  (let ((this-one (funcall generation-thunk)))
	    (if (not (member this-one so-far :test test))
		(push this-one so-far))
	    (setq trials i)))
    (if (not (= num-required
		(length (remove-duplicates so-far :test #'equal))))
	(print "Maybe you need :test #'equal in collect-uniques"))
    (values (reverse so-far)
	    trials)))
;;(collect-uniques 10 (^ () (krandom 10)))


;;;------------------------------------------------------------------------
;;;                      ALL COMPARISONS
;;;------------------------------------------------------------------------

(defun all-comparisons (l comparator)
  (loop for (f . r) on l 
        until (null r)
        append (loop for e in r 
                      collect (funcall comparator e f))))

(defun all-comparisons-full (l comparator)
  (loop for (f . r) on l 
        until (null r)
        collect (loop for e in r 
		    collect (funcall comparator e f))))

(defun all-comparisons-square (l comparator)
  (loop for e1 in l collect
	(loop for e2 in l collect
	      (funcall comparator e1 e2))))



;;;------------------------------------------------------------------------
;;;                         SERIES
;;;------------------------------------------------------------------------

(defun series (low high &optional (step 1))
  (if (> low high)
    nil
    (cons low (series (+ low step) high step))))

(defun replicate-into-list (e length)
  (loop for i below length collect e))