(in-package user)

;;;----------------------------------------------------------------------
;;;                          SHOW ASSYMETRIES
;;;----------------------------------------------------------------------

(defun hi-table-assymetries (hi-table)
  (make-hi-table 
   (hi-table-antigens hi-table)
   (hi-table-antisera hi-table)
   (loop for antigen-index below (hi-table-length hi-table) collect
	 (loop for antisera-index below antigen-index collect
	       (- (hi-table-value-by-indices hi-table antigen-index antisera-index)
		  (hi-table-value-by-indices hi-table antisera-index antigen-index))))))


#|
(pp-hi-table (hi-table-assymetries (f-hi-tables (^ (&rest l) (funcall (cfs #'log-titer #'av) l)) 
						sames)))
|#