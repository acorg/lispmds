(in-package user)

(defun raw-diff-lines (new old)
  (run-shell-command (format nil "diff ~a ~a | grep -v '<' | grep -v '>' | grep -v '-' | sed s/'\,'/'\ '/ | sed s/'\,'/'\ '/ | sed s/c/' * '/ | sed s/d/' * '/ | sed s/a/' * '/ > .diff-tmp"
			     new old)))


(defun processed-diff-lines ()
  (with-open-file (diff-lines "~/.diff-tmp")
    (let (diff-line)
      (loop until (eql 'eof (setq diff-line (read-line diff-lines nil 'eof)))
	  collect (let ((line (read-from-string (format nil "(~a)" diff-line))))
		    (list (nth 0 line)
			  (if (eql (nth 1 line) '*)
			      (nth 0 line)
			    (nth 1 line))))))))

(defun add-change-bars-aux (file diff-lines)
  (with-open-file (in file)
    (with-open-file (out (string-append file ".cb") 
		     :direction :output :if-exists :new-version :if-does-not-exist :create)
      (let ((current-line 1))
	(loop for (start-line end-line) in diff-lines do
	      (loop for i from current-line below start-line do
		    (format out "~a~%" (read-line in)))
	      (format out "\\protect\\begin{changebar}~%")
	      (loop for i from start-line to end-line do
		    (format out "~a~%" (read-line in)))
	      (format out "\\protect\\end{changebar}~%")
	      (setq current-line (inc end-line)))
	(let (line)
	  (loop until (eql 'eof (setq line (read-line in nil 'eof)))
	      do (format out "~a~%" line)))))))
			   
;;use this add-change-bars to do on the actual files
;;or the one below to account for reformatting in the files
(defun add-change-bars (old new)
  (raw-diff-lines new old)
  (add-change-bars-aux new (processed-diff-lines)))


#|
(add-change-bars "~/junk/difftest/old.tex" "~/junk/difftest/new.tex")
(add-change-bars "~/papers/im-lazy/drafts/june-22-1997.tex.to-steph" "~/papers/im-lazy/main.tex")
(add-change-bars "~/junk/difftest/june-22-1997.tex.to-steph" "~/junk/difftest/main.tex")
|#


;;;----------------------------------------------------------------------
;;;              SPLIT FILE INTO ONE WORD PER LINE
;;;----------------------------------------------------------------------

;;can't handle ;'s for now, edit them out
(defun one-word-per-line (file)
  (with-open-file (in file)
    (with-open-file (out (string-append file ".ow") 
		     :direction :output :if-exists :new-version :if-does-not-exist :create)
      (let (line
	    (count '(0)))
	(loop until (eql 'eof (setq line (read-line in nil 'eof)))
	    do (setq line (read-from-string 
				     (format nil "(~a)" 
					     (string-subst
					      #\( #\[
					      (string-subst 
					       #\) #\]
					       (string-subst
						#\, #\$
						(string-subst
						 #\: #\$
						 (string-subst
						  #\\ #\$
						  (string-subst
						   #\| #\$
						   line)))))))))
	       (push (+ (length line) (car count)) count)
	       (loop for word in line do
		     (format out "~a~%" word)))
	(reverse count)))))


(defun word-to-line (word-position line-positions)
  (loop for line-position in line-positions
      for i from 0
      when (>= line-position word-position)
      do (return i)
      finally (error "should not have got here")))

(defun merge-lines (line-breaks)
  (cond ((null (cdr line-breaks)) (cadr line-breaks))
	((or (= (cadr (car line-breaks)) (car (cadr line-breaks)))
	     (= (inc (cadr (car line-breaks))) (car (cadr line-breaks))))
	 (merge-lines (cons (list (car (car line-breaks)) (cadr (cadr line-breaks)))
			    (cddr line-breaks))))
	(t (cons (car line-breaks) (merge-lines (cdr line-breaks))))))

(defun process-diff-lines (line-breaks)
  (merge-lines 
   (loop for (start end) in (processed-diff-lines) collect
	 (list (word-to-line start line-breaks) (word-to-line end line-breaks)))))
			   

(defun add-change-bars (old new)
  (one-word-per-line old)
  (let ((line-breaks (one-word-per-line new)))
    (raw-diff-lines (string-append new ".ow") (string-append old ".ow"))
    (add-change-bars-aux new (process-diff-lines line-breaks))))
