(in-package user)

(defun lisp->tk (x)
  (string-subst #\) #\}
		(string-subst #\( #\{ 
			      (format nil "~a" x))))

(defun fi-tk-list (x filename &optional (if-exists-action :error))
  (fi (lisp->tk x) filename if-exists-action t))


