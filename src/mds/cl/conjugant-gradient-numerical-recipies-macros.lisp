(in-package user)

; macros.l - all the basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Confidential Trade Secret and Copyright (c) University of ;;;;;;;;
;;;;;;;;Waikato, Hamilton, New Zeland 1993 - all rights reserved;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(in-package :senac)
(eval-when (compile load eval)
   (export '(do1 do! map-defvar double-cdr double-mapcar push 
             fexport fproclaim fuse-package fin-package fdo arithmetic-if
             computed-goto assigned-goto)))

; macros:
;	rexpt
;	fexport
;	fproclaim
;	fuse-package 
;	fin-package
;	map-defvar
;	do1 
;	do!
;	double-cdr
;	putprop
;	defprop
;	array-cl
;	store-cl
;	apply!

;	rfref
;	rfset
;	fref
;	fset

;	while
;       fdo
;	reset-vble - a defun
;       arithmetic-if
;	computed-goto
;	assigned-goto
;	eqv
;	constant-list
;       Fortran intrinsic functions imax, dabs,...
;----------------------------------------------------------------------------

(eval-when (compile load eval) (proclaim '(special $verbose)))
;----------------------------------------------------------------------------
#+aclpc (defmacro rexpt (x y) `(realpart (expt ,x ,y)))
#-aclpc (defmacro rexpt (x y) `(expt ,x ,y))

(defmacro fexport (x) `(eval-when (compile load eval) (export ,x)))

(defmacro fproclaim (x) `(eval-when (compile load eval) (proclaim ,x)))

(defmacro fin-package (x)
  `(prog nil 
     (defpackage ,x)
     (in-package ,x)))

(defmacro fuse-package (x) `(eval-when (compile load eval) ,x))
;-------------------------------------------------------------------------

(defmacro apply! (fun args) (eval `(cons ,fun ,args)))

(defmacro map-defvar (&rest l)
   `(progn ,@(mapcar #'(lambda (x y) (list 'defvar x y))
                     (car (odd-even l)) 
                     (cadr (odd-even l)))))
;-----------------------------------------------------------------------------

(defmacro do! (var init step end &rest body)
   `(do ((,var ,init ,step)) (,end) ,@body))

; the body is an unquoted list of the terms of the actual body
(defmacro do1 (var end body)
   `(do ((,var 1 (1+ i))) ((> ,var ,end)) ,@body))

(defmacro double-cdr (lis)
   `(mapcar #'cdr (cdr ,lis)))

;; derek removed these next 2
;; (defun putprop (a b c) (setf (get a c) b))

;; (defmacro defprop (sym prop ind)
;;  `(putprop ',sym ',prop ',ind))

(defmacro def (name body) `(defun ,name ,(cadr body) ,(caddr body)))

(defmacro array-cl (name type &rest dims)
 `(set ',name 
        (make-array ',(mapcar #'eval dims) 
           :element-type ,(cond ((equal type 'fixnum-block) ''integer)
                                ((equal type 'flonum-block)  ''flonum) ;###
                                ((equal type t) t)))))

(defmacro store-cl (name-indices val)
  `(setf (aref ,(car name-indices) ,@(cdr name-indices)) ,val))
;-----------------------------------------------------------------------------

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))

(defmacro fset-nm (a b)     ;; rename fset as fset-nm (as in numerical-recipies, as fset is an internal function in openmcl)
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro rfref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) (reverse indices))))

(defmacro rfset (a b) 
  `(setf (rfref ,(second a) ,@(cddr a)) ,b))


;----------------------------------------------------------------------------

;; commented out by DJS Jan03 2000, acl60 on windows complains
;;#-aclpc (defmacro while (con &rest body)
;;            `(loop (if (not ,con) (return t)) ,@body))
;------------------------------------------------------------------

(defun comment (s) (when $verbose (princ s) (terpri)))

;----------------------------------------------------------------------------
(defun reset-vble (pred-clause) ; ((> i 4) nil)
   `(,(first pred-clause)
          (setq ,(second (first pred-clause)) ,(third (first pred-clause)))
          ,(second pred-clause)))

(defmacro fdo (do-vbles predicate-clause &rest body)
   `(prog nil
          (setq ,(caar do-vbles) ,(cadar do-vbles)) 
          loop
          (return
          (cond ,(reset-vble predicate-clause)
                ,(cons 't 
                       (append 
                        (append body `((setq ,(caar do-vbles) ,(caddar do-vbles))))
                        '((go loop))))))))

;----------------------------------------------------------------------------
(defun constant-list (x n)
  (do  ((i 1 (1+ i)) 
        (ret nil (cons x ret))) 
       ((> i n) ret)))
     

;----------------------------------------------------------------------------

;; macro for a lisp equivalent of Fortran arithmetic IFs
(defmacro arithmetic-if (pred s1 s2 s3)
   `(cond ((< ,pred 0) ,s1)
          ((= ,pred 0) ,s2)
          (t ,s3)))

;; macro for a lisp equivalent of Fortran computed GOTOs
(defmacro computed-goto (tag-lst i)
   `(let ((tag ,(nth (1- (eval i)) tag-lst)))
       (if tag (go tag) nil)))

;; macro for a lisp equivalent of Fortran assigned GOTOs
(defmacro assigned-goto (i &optional tag-lst)
   `(if ,tag-lst
        (if (member ,i ,tag-lst) 
            (go ,i)
            (error "bad statement number in assigned goto"))
        (go ,i)))

;-----------------------------------------------------------------------------       
; set up a list of intrinsic function names
;real xxx
(defvar intrinsic_function_names
  '(int ifix idint real float sngl dble cmplx ichar char aint dint
    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
    idim dim ddim dprod max max0 amax1 dmax1 amax0 amax1 min min0 amini dmini
    amini min1 len index lge lgt lle llt aimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh))

; some macros for intrinsic functions
(defmacro int (x)
   `(floor ,x))
(defmacro ifix (x)
   `(floor ,x))
(defmacro idfix (x)
   `(floor ,x))

(defmacro real_ (x)
   `(coerce ,x 'float))

(defmacro sngl (x)
   `(coerce ,x 'float))

(defmacro cmplx (x &optional y)
   `(complex ,x ,(if y y 0)))

(defmacro ichar (c)
   `(char-int ,c))
(defmacro fchar (i)  ;intrinsic function char
   `(char-int ,i))

(defmacro aint (x)
   `(float (truncate ,x)))
(defmacro dint (x)
   `(coerce (truncate ,x) 'double-float))
(defmacro anint (x)
   `(float (round ,x)))
(defmacro dnint (x)
   `(coerce (round ,x) 'double-float))
(defmacro nint (x)
   `(round ,x))
(defmacro idnint (x)
   `(round ,x))

#-aclpc (defmacro iabs (x) `(abs ,x))
(defmacro dabs (x)
   `(abs ,x))
(defmacro cabs (x)
   `(abs ,x))

(defmacro amod (x y)
  `(mod ,x ,y))
(defmacro dmod (x y)
  `(mod ,x ,y))

(defmacro fsign (x y)   ;; was sign, changed by derek so as not to conflict with common lisp function
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))
(defmacro isign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))
(defmacro dsign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))

(defmacro idim (x y)
  `(abs (- ,x ,y)))
(defmacro dim (x y)
  `(abs (- ,x ,y)))
(defmacro ddim (x y)
  `(abs (- ,x ,y)))

(defmacro dprod (x y)
  `(coerce (* ,x ,y) `double-float))

(defmacro max0 (&rest x)
  `(funcall #'max ,@x))
(defmacro amax1 (&rest x)
  `(funcall #'max ,@x))
(defmacro dmax1 (&rest x)
  `(funcall #'max ,@x))
(defmacro amax0 (&rest x)
  `(round (funcall #'max ,@x)))
(defmacro max1 (&rest x)
  `(float (funcall #'max ,@x)))

(defmacro min0 (&rest x)
  `(funcall #'min ,@x))
(defmacro amin1 (&rest x)
  `(funcall #'min ,@x))
(defmacro dmin1 (&rest x)
  `(funcall #'min ,@x))
(defmacro amin0 (&rest x)
  `(round (funcall #'min ,@x)))
(defmacro min1 (&rest x)
  `(float (funcall #'min ,@x)))

(defmacro len (s)
   `(length ,s))

(defmacro index (s1 s2)
 (declare (ignore s1 s2))
   `(error "macro for intrinsic INDEX not yet implemented"))

(defmacro lge (s1 s2)
   `(string>= ,s1 ,s2))
(defmacro lgt (s1 s2)
   `(string> ,s1 ,s2))
(defmacro lle (s1 s2)
   `(string<= ,s1 ,s2))
(defmacro llt (s1 s2)
   `(string< ,s1 ,s2))

(defmacro aimag (c)
   `(imagpart ,c))
(defmacro conjg (c)
   `(conjugate ,c))

(defmacro dsqrt (x)
   `(sqrt ,x))
(defmacro csqrt (x)
   `(sqrt ,x))

(defmacro dexp (x)
   `(exp ,x))
(defmacro cexp (x)
   `(exp ,x))

(defmacro alog (x)
   `(log ,x))
(defmacro dlog (x)
   `(log ,x))
(defmacro clog (x)
   `(log ,x))
(defmacro alog10 (x)
   `(log ,x 10))
(defmacro dlog10 (x)
   `(log ,x 10))

(defmacro dsin (x)
   `(sin ,x))
(defmacro csin (x)
   `(sin ,x))

(defmacro dcos (x)
   `(cos ,x))
(defmacro ccos (x)
   `(cos ,x))

(defmacro dtan (x)
   `(tan ,x))
(defmacro ctan (x)
   `(tan ,x))

(defmacro dasin (x)
   `(asin ,x))
(defmacro dacos (x)
   `(acos ,x))
(defmacro datan (x)
   `(atan ,x))
(defmacro atan2 (x y)
   `(atan (/ ,x ,y)))
(defmacro datan2 (x y)
   `(atan (/ ,x ,y)))

(defmacro dsinh (x)
   `(sinh ,x))
(defmacro dcosh (x)
   `(cosh ,x))
(defmacro dtanh (x)
   `(tanh ,x))

;----------------------------------------------------------------------------- ; end of macros.l

(provide "f2cl_macros")
