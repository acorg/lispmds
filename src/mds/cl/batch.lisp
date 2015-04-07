(in-package user)

;;;----------------------------------------------------------------------
;;;        run a lisp command in a separate background lisp
;;;                  locally or on gridware
;;;----------------------------------------------------------------------

(defun batch-lisp (expression-to-eval &optional &key 
						gridware
                                                xgrid
						(nice 19)
						(id (progn (seed-random -1) (krandom 467739585)))
						(scratch-directory "/users/robfarrow/mds/src/mds/cl/batch-scratch/")
						(if-exists-action :error)
						)
  (if (not (eql 'progn (car expression-to-eval)))
      ;; somehow we need a progn wrapper, and this only makes sense if outer list is length 1
      (if (not (listp expression-to-eval))
	  (error "unexpected condition for expression ~a" expression-to-eval)
	(setq expression-to-eval
	  `(progn ,expression-to-eval))))
  (let ((expression-scratch-filename (format nil "~a~a-expression-to-eval" scratch-directory id))
	(stdout                      (format nil "~a~a-stdout" scratch-directory id))
	(stderr                      (format nil "~a~a-stderr" scratch-directory id))
	(output-filename             (format nil "~a~a-output" scratch-directory id)))
    (fi expression-to-eval expression-scratch-filename)
    (if gridware
	(gridware-submit 
	 "cl/gw-batch-lisp"
	 expression-scratch-filename
	 output-filename)
      (if xgrid
          (gridware-submit 
           "cl/xgrid-batch-lisp"
           expression-scratch-filename
           output-filename)
        (run-shell-command 
         (print 
          (format 
           nil
           "nice -n ~d alisp -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(fi (eval (fi-in ~s)) ~s ~s)' -batch >> ~s 2>> ~s &" 
           nice
           expression-scratch-filename
           output-filename
           if-exists-action
           stdout
           stderr)))))
    (if gridware
	`(progn
	   (run-shell-command 
	    ,(format nil "scp -C sfi:~a ~a" output-filename output-filename))
           (fi-in ,output-filename))
      (if xgrid
          `(progn
             (run-shell-command 
              ,(format nil "scp -C x2:~a ~a" output-filename output-filename))
             (fi-in ,output-filename))
        `(fi-in ,output-filename)))))
      
#|
(batch-lisp '(progn (+ 3 4)))
(batch-lisp '(progn (+ 3 4)) :gridware t)

(batch-lisp
 '(progn
   (setq bootstrap-sera-seras
     (loop for i from 0 to 4 collect
	   (hi-table-sera
	    (un-asl-hi-table
	     (table-from-save
	      (fi-in (format nil "mds/investigations/merge-hi-tables/seq-t8-bootstrap-runs/seq-t8a-bootstrap-sera-~d.save" i)))))))
   (fi bootstrap-sera-seras "/tmp/foo.lisp")))
|#





(defmacro batches (&body expressions)
  `(list ,@(mapcar (^ (expr) 
                      `(batch-lisp ',expr))
                 expressions)))

;;this can be used on the result of batches
;;to collect the results
(defun eval-expressions (expressions)
  "Evaluate a list if expressions and collect the results."
  (loop for expr in expressions
      collect (eval expr)))

