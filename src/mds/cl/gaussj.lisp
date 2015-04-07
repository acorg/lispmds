(in-package user)

THIS IS THE LISP OF THE NUMERICAL RECEPIES OF GAUSSJ WHICH I WANTED TO USE FOR
MATRIX INVERSION, BUT IT GIVES THE WRONG ANSWERS SOMETIMES (OR I USE IT INCORECTLY)

(defun gaussj (a n 
	       ;; np 
	       b m
	       ;; mp
	       ;; &key (nmax 50))
	       )
  (declare (type (simple-array double-float (* *)) a))
  (declare (type fixnum n))
  ;; (declare (type fixnum np))
  (declare (type (simple-array double-float (* *)) b))
  (declare (type fixnum m))
  ;;(declare (type fixnum mp)) 
  (declare (type fixnum nmax))
  (prog
      ((ipiv (make-array n :element-type 'fixnum))  ;;nmax :element-type 'fixnum))
       (indxr (make-array n :element-type 'fixnum)) ;;nmax :element-type 'fixnum))
       (indxc (make-array n :element-type 'fixnum)) ;;nmax :element-type 'fixnum)) 
       (ll 0) (pivinv 0.0d0)
       (dum 0.0d0) (l 0) (icol 0) (irow 0) (k 0) (big 0.0d0) (i 0) (j 0)
       )
    (declare (type (simple-array fixnum (*)) ipiv))
    (declare (type (simple-array fixnum (*)) indxr))
    (declare (type (simple-array fixnum (*)) indxc))
    (declare (type fixnum ll))
    (declare (type double-float pivinv))
    (declare (type double-float dum))
    (declare (type fixnum l))
    (declare (type fixnum icol))
    (declare (type fixnum irow))
    (declare (type fixnum k))
    (declare (type double-float big))
    (declare (type fixnum i))
    (declare (type fixnum j))
    (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref ipiv j) 0)))
    (fdo ((i 1 (+ i 1))) ((> i n) nil)
	 (tagbody (setf big 0.0)
	   (fdo ((j 1 (+ j 1))) ((> j n) nil)
		(tagbody
		  (cond
		   ((/= (fref ipiv j) 1)
		    (fdo ((k 1 (+ k 1))) ((> k n) nil)
			 (tagbody
			   (cond
			    ((= (fref ipiv k) 0)
			     (cond
			      ((>= (abs (fref a j k)) big) (setf big (abs (fref a j k)))
							   (setf irow j) (setf icol k)
							   )))
			    ((> (fref ipiv k) 1) (error "Singular matrix"))
			    )))))))
	   (fset (fref ipiv icol) (+ (fref ipiv icol) 1))
	   (cond
	    ((/= irow icol)
	     (fdo ((l 1 (+ l 1))) ((> l n) nil)
		  (tagbody (setf dum (fref a irow l))
		    (fset (fref a irow l) (fref a icol l)) (fset (fref a icol l) dum)
		    ))
	     (fdo ((l 1 (+ l 1))) ((> l m) nil)
		  (tagbody (setf dum (fref b irow l))
		    (fset (fref b irow l) (fref b icol l)) (fset (fref b icol l) dum)
		    ))))
	   (fset (fref indxr i) irow) (fset (fref indxc i) icol)
	   (if (= (fref a icol icol) 0.0) (error "Singular matrix."))
	   (setf pivinv (/ 1.0 (fref a icol icol))) (fset (fref a icol icol) 1.0)
	   (fdo ((l 1 (+ l 1))) ((> l n) nil)
		(tagbody (fset (fref a icol l) (* (fref a icol l) pivinv)))
		)
	   (fdo ((l 1 (+ l 1))) ((> l m) nil)
		(tagbody (fset (fref b icol l) (* (fref b icol l) pivinv)))
		)
	   (fdo ((ll 1 (+ ll 1))) ((> ll n) nil)
		(tagbody
		  (cond
		   ((/= ll icol) (setf dum (fref a ll icol)) (fset (fref a ll icol) 0.0)
				 (fdo ((l 1 (+ l 1))) ((> l n) nil)
				      (tagbody
					(fset (fref a ll l) (+ (fref a ll l) (* (* -1 (fref a icol l)) dum)))
					))
				 (fdo ((l 1 (+ l 1))) ((> l m) nil)
				      (tagbody
					(fset (fref b ll l) (+ (fref b ll l) (* (* -1 (fref b icol l)) dum)))
					))))))))
    (fdo ((l n (+ l (- 1)))) ((> l 1) nil)
	 (tagbody
	   (cond
	    ((/= (fref indxr l) (fref indxc l))
	     (fdo ((k 1 (+ k 1))) ((> k n) nil)
		  (tagbody (setf dum (fref a k (fref indxr l)))
		    (fset (fref a k (fref indxr l)) (fref a k (fref indxc l)))
		    (fset (fref a k (fref indxc l)) dum)
		    ))))))
    (return (values a n 
		    ;; np 
		    b m
		    ;; mp
		    ))
    ))

