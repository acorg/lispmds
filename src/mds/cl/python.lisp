(in-package user)

;;; See the definition of *PYTHON-AUTO-FUNCTIONS* at the bottom of this file
;;; for the list of lisp functions that have python equivalents created for
;;; them. It's at the bottom because we also create a python equivalent
;;; for one of the functions in this file, and that needs to be defined
;;; before hitting the setq of *PYTHON-AUTO-FUNCTIONS*.

(defun make-python-scripts (&optional &key dir)
  "Make a bunch of python scripts in dir to call the functions in *PYTHON-AUTO-SCRIPTS* from the shell."
  (let
      ((script-writer #'write-lisp->python-script))
    (if (not dir)
	(let ((mdsroot (sys:getenv *MDS-ROOT-VAR*)))
	  (if (and mdsroot (excl:file-directory-p mdsroot))
	      (setq dir (format nil "~a/bin/auto" mdsroot))
	    (error (format nil "Your ~a environment variable is ~a.~%" *MDS-ROOT-VAR* (if mdsroot "not a directory" "undefined"))))))
    (mapc (lambda (what) 
	    (funcall script-writer (car what) :dir dir :wrap-function (cdr what)))
	  *PYTHON-AUTO-FUNCTIONS*)
    t))


(defun write-lisp->python-script (fn &optional &key wrap-function (py-fn-name nil) (dir ".") (mode #o755))
  (let* ((s (format nil "~a" fn))
	 (fn-name (format nil "~a" (subseq s (1+ (position #\Space s :from-end t)) (dec (len s)))))
	 keywords
	 path)
    (labels ((print-lisp-arg (s arg keyword)
	       (if keyword
		   (format s "i.addOption('--~a', keyword=True)~%" arg)
		 (format s "i.addInitialArg('~a')~%" arg))))
      (if (not py-fn-name)
	  (setq py-fn-name (format nil "~(~a~).py" fn-name)))
      (setq path (format nil "~a/~a" dir py-fn-name))
      (with-open-file (s path :direction :output :if-exists :supersede)
	(multiple-value-bind (args ok)
	    (EXCL::arglist fn)
	  (if (not ok)
	      (warn "Could not get arguments for lisp function ~a~%" fn)
	    (progn
	      (format s "#!/usr/bin/env python~@
                         import sys~@
                         from mds import ClInterface~@
                         i = ClInterface.ClCommand('~a', sys.argv, wrapFunction=~a)~%" fn-name
		      (if wrap-function (format nil "'~a'" wrap-function) "None"))
	      (dolist (arg args)
		(cond
		 ((atom arg) (progn
			       (setq arg (symbol-name arg))
			       (cond ((string-equal arg "&key") (setq keywords t))
				     ((string= (subseq arg 0 1) "&")) ; Ignore other keywords.
				     (t (print-lisp-arg s arg keywords)))))
		 ((listp arg) (print-lisp-arg s (car arg) keywords))
		 (t (format s "# unknown arg: '~a'.~%" arg))))
	      (format s "i.run()~%")))))
      (excl.osi:chmod path mode)
      (format t "Wrote python script ~a for lisp function ~a.~%" path fn-name))))

;;; *PYTHON-AUTO-FUNCTIONS* lists the functions you want to create scripts
;;; for.  Each element of the list gives a function and a wrapping
;;; function. If the wrapper is nil, the function result will not be
;;; wrapped (so if the lisp doesn't write to stdout, you wont see
;;; anything). If the wrapper is "format", the function call will be
;;; wrapped in (format t "~a~%" ... ). In other cases, the function
;;; call is just simple wrapped, e.g., (ppl ...), using the name you
;;; give.

(setq *PYTHON-AUTO-FUNCTIONS*
  `((,#'binomial-series . "ppl")
    (,#'make-python-scripts . nil)
    (,#'read-newick-tree . "format")))
