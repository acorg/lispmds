(in-package user)

(defmacro ^ (&rest args)
  `#'(lambda ,@args))

(defmacro define (name &body body)
  (if (listp name)
    `(progn (defun ,(car name) ,(cdr name) ,@body)
            (defvar ,(car name))
            (setq ,(car name) #',(car name)))
    `(progn (defvar ,name)
            (setq ,name ,@body)
            (if (functionp ,name) (setf (symbol-function ',name) ,name))
            ,name)))

#|
(define (! n) (if (< n 2) 1 (* n (! (- n 1)))))
(define !! (^ (n) (if (< n 2) 1 (* n (! (- n 1))))))
(define !!! 24)
|#
