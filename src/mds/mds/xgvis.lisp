(in-package user)

;;;----------------------------------------------------------------------
;;;                          MDS USING XGVIS
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;                       CALLING XGVIS FROM LISP
;;;----------------------------------------------------------------------

#|
3) According to the manpages (on the web) one can set a "filename.vgroups"
file (associated with "filename" which holds the data). This file supposedly
says which columns are to be treated as one group for scaling and
other purposes, and sounds like just what we want. I created a 
filename.vgroups with the single line
1 1 1
in it for some three dim data and started up xgobi for data in "filename".
Got same graphics as if filename.vgroups did not exist. My last gasp was to 
hit the "I/O" button on the top control bar and read in "filename.vgroups".
xgobi coredumps!
|#

(defun xgvis (hi-table &optional &key 
				 (dims 2)
				 starting-coordss
				 )
  (let ((filename (format nil "xgvis-~d" (krandom 100000))))
    (with-open-file (out (format nil "/tmp/~a.row" filename) :direction :output :if-exists :supersede)
      (format out "~{~a~%~}" (hi-table-antigens hi-table)))
    (with-open-file (out (format nil "/tmp/~a.vgroups" filename) :direction :output :if-exists :supersede)
      (format out "~{~d~%~}" (loop for i below dims collect 1)))
    (with-open-file (out (format nil "/tmp/~a.dist" filename) :direction :output :if-exists :supersede)
      (format out "~{~{~6,4f ~}~%~}" 
	      (f-elements
	       (^ (x) (if (dont-care-p x)
			  'na
			x))
	       (hi-table-values hi-table))))
    (if starting-coordss
	(with-open-file (out (format nil "/tmp/~a.pos" filename) :direction :output :if-exists :supersede)
	  (format out "~{~{~6,4f ~}~%~}" (deconstruct-coordss-plus-more starting-coordss))))
    (run-shell-command (print (format nil "exec xgvis -dims ~d /tmp/~a" dims filename))
		       :wait nil 
		       :input :stream 
		       :output :stream 
		       :error-output :output)))

(defun xgobi (hi-table &optional &key starting-coordss)
  (let ((filename (format nil "xgobi-~d" (krandom 100000))))
    (with-open-file (out (format nil "/tmp/~a.row" filename) :direction :output :if-exists :supersede)
      (format out "~{~a~%~}" (hi-table-antigens hi-table)))
    (with-open-file (out (format nil "/tmp/~a.vgroups" filename) :direction :output :if-exists :supersede)
      (format out "~{~d~%~}" (loop for i below (length (car starting-coordss)) collect 1)))
    (if starting-coordss
	(with-open-file (out (format nil "/tmp/~a.dat" filename) :direction :output :if-exists :supersede)
	  (format out "~{~{~6,4f ~}~%~}" (deconstruct-coordss-plus-more starting-coordss))))
    (run-shell-command (print (format nil "exec xgobi /tmp/~a" filename))
		       :wait nil 
		       :input :stream 
		       :output :stream 
		       :error-output :output)))


;;;----------------------------------------------------------------------
;;;       READING (AND DISPLAYING) THE RESULTS BACK INTO LISP
;;;----------------------------------------------------------------------

(defun show-xgvis-in-2d-tk (filename)
  (let* ((raw-xgvis-coordss (mapcar #'space-delimited-string-to-list 
				    (fi-in-readline (string-append filename ".dat"))))
	 (xgvis-names       (mapcar #'read-from-string 
				    (fi-in-readline (string-append filename ".row")))))
    (show-coordss
     (mapcar (^ (x) (firstn 2 x)) raw-xgvis-coordss)
     filename
     xgvis-names)))


;;(show-xgvis-in-2d-tk "mds/investigations/piecing-hi-tables/by-big-merge/merged-hi-seq-strains-xgvis-2d-1993/foo")



