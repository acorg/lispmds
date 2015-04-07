(in-package user)

;;;----------------------------------------------------------------------
;;;                  parse the Newick tree format
;;;----------------------------------------------------------------------

#|
http://evolution.genetics.washington.edu/phylip/newicktree.html

(B:6.0,(A:5.0,C:3.0,E:4.0):5.0,D:11.0);
|#


;;;----------------------------------------------------------------------
;;;                      read Newick tree format
;;;----------------------------------------------------------------------

(defun string-colon-to-name-comma-to-name-semicolon-delete (string &optional &key (delete-gt nil))
  (string-subst
   #\:
   #\space
   (string-subst
    #\;
    #\space
    (string-subst 
     #\,
     #\space 
     (if delete-gt
	 (string-subst #\> #\space string)
       string)))))

(defun pair-newick-tree (tree)
  (cond ((null tree) 
	 nil)
	((atom tree)
	 tree)
	(t
	 (cons (list (pair-newick-tree (car tree)) (cadr tree))
	       (pair-newick-tree (cddr tree))))))

(defun read-newick-tree (filename)
  (pair-newick-tree
   (read-from-string
    (apply
     #'string-append
     (fi-in-readline
      filename
      :line-process-f #'string-colon-to-name-comma-to-name-semicolon-delete)))))
   
;; (read-newick-tree "/win/misc/xfer/misc/foo")
;; ((B 6.0) (((A 5.0) (C 3.0) (E 4.0)) 5.0) (D 11.0))


;;;----------------------------------------------------------------------
;;;                      write newick tree format
;;;----------------------------------------------------------------------

;; ((B 6.0) (((A 5.0) (C 3.0) (E 4.0)) 5.0) (D 11.0))
;; (B:6.0,(A:5.0,C:3.0,E:4.0):5.0,D:11.0);
;; (B:6.0,(A:5.0,C:3.0,E:4.0):5.0,D:11.0);


(defun newick-tree-list-to-string (tree)
  (format nil "(~a);" (newick-tree-list-to-string-aux tree)))

(defun newick-tree-list-to-string-aux (tree)
  (glue-up-to-string
   ","
   (loop for element in tree collect
	 (if (atom (car element))
	     (format nil "~%~a:~8,6f" (nth 0 element) (nth 1 element))
	   (format nil "(~a):~8,6f" (newick-tree-list-to-string-aux (nth 0 element)) (nth 1 element))))))
	   
(defun write-newick-tree (tree filename &optional &key (if-exists-action :error))
  (fi (newick-tree-list-to-string tree)
      filename
      if-exists-action
      t))

;;;----------------------------------------------------------------------
;;;                      distances to nodes
;;;----------------------------------------------------------------------

(defun distances-to-nodes (tree &optional (so-far 0))
  (cond ((null tree)
	 nil)
	((atom tree)
	 ;;(error "unexpected 1"))   ;; was never called with distances-from-newick-tree, but is for distances-to-trunk
	 (list (list tree so-far)))  ;; an atom is the unit tree
	((atom (car tree))
	 (error "unexpected 2"))
	((atom (caar tree))
	 (cons (list (nth 0 (car tree)) (+ (nth 1 (car tree)) so-far))
	       (distances-to-nodes (cdr tree) so-far)))
	((listp (caar tree))
	 (append (distances-to-nodes (nth 0 (car tree)) (+ (nth 1 (car tree)) so-far))
		 (distances-to-nodes (cdr tree) so-far)))
	(t 
	 (error "unexpected 3"))))


(defun distances-from-newick-tree (tree)
  (cond ((null tree)
	 nil)
	((atom tree)
	 (error "unexpected 1"))
	((atom (car tree))
	 (error "unexpected 2"))
	((= 1 (length tree))
	 (if (atom (caar tree))
	     nil
	   (distances-from-newick-tree (caar tree))))
	(t 
	 (append (loop for (node distance) in (distances-to-nodes (cdr tree)) append
		       (if (atom (caar tree))
			   (list (list (nth 0 (car tree)) node (+ (nth 1 (car tree)) distance)))
			 (loop for (node2 distance2) in (distances-to-nodes (nth 0 (car tree))) collect
			       (list node2 node (+ (nth 1 (car tree)) distance distance2)))))
		 (distances-from-newick-tree (list (car tree)))
		 (distances-from-newick-tree (cdr tree))))))



#|
(distances-from-newick-tree (read-newick-tree "cl/test-files/newick-tree-format-test-4"))
((B A 16.0) (B C 16.0) (B C2 15.0) (B E 15.0) (B D 17.0)
 (A D 21.0) (C D 21.0) (C2 D 20.0) (E D 20.0)
 (A C 10.0) (A C2 9.0) (A E 9.0) 
 (C E 9.0)  (C2 E 8.0) 
 (C C2 3.0))
|#

(defun node1-node2-distance-list-to-distance-matrix (node1-node2-distance-list)
  (let* ((names (sort-strains
		 (remove-duplicates (append (nths 0 node1-node2-distance-list)
					    (nths 1 node1-node2-distance-list))
				    :test #'equal)))
	 (distance-matrix (make-array (list (length names) (length names)) :initial-element nil)))
    (loop for i below (length names) do
	  (setf (aref distance-matrix i i) 0.0))
    (loop for (node1 node2 distance) in node1-node2-distance-list do
	  (let ((position1 (position node1 names :test #'equal))
		(position2 (position node2 names :test #'equal)))
	    (if (or (aref distance-matrix position1 position2)
		    (aref distance-matrix position2 position1))
		(error "duplicate entry for ~d ~d" (nth position1 names) (nth position2 names))
	      (progn
		(setf (aref distance-matrix position1 position2) distance)
		(setf (aref distance-matrix position2 position1) distance)))))
    (values
     names
     distance-matrix)))

#|
(node1-node2-distance-list-to-distance-matrix (distances-from-newick-tree (read-newick-tree "cl/test-files/newick-tree-format-test-4")))
(B A D C E C2)
#2A((0.0 16.0 17.0 16.0 15.0 15.0)
    (16.0 0.0 21.0 10.0 9.0 9.0)
    (17.0 21.0 0.0 21.0 20.0 20.0)
    (16.0 10.0 21.0 0.0 9.0 3.0)
    (15.0 9.0 20.0 9.0 0.0 8.0)
    (15.0 9.0 20.0 3.0 8.0 0.0))
|#  
  

(defun node1-node2-distance-list-to-table (node1-node2-distance-list)
  (multiple-value-bind (names distance-matrix)
      (node1-node2-distance-list-to-distance-matrix node1-node2-distance-list)
    (make-hi-table
     names
     names
     (list-array distance-matrix))))

#|
(pp-hi-table (node1-node2-distance-list-to-table (distances-from-newick-tree (read-newick-tree "cl/test-files/newick-tree-format-test-4"))))

           B      A      D      C      E     C2 
B        0.0   16.0   17.0   16.0   15.0   15.0 
A       16.0    0.0   21.0   10.0    9.0    9.0 
D       17.0   21.0    0.0   21.0   20.0   20.0 
C       16.0   10.0   21.0    0.0    9.0    3.0 
E       15.0    9.0   20.0    9.0    0.0    8.0 
C2      15.0    9.0   20.0    3.0    8.0    0.0 
|#


#|
treefile.10268347 
treefile.10724917 
treefile.12290219 
treefile.15440956 
treefile.16809830 
treefile.4252725  
treefile.4252774  
treefile.634622   
treefile.9202801

(setq treefile-10268347
  (node1-node2-distance-list-to-table 
   (distances-from-newick-tree 
    (read-newick-tree 
     "mds/investigations/sequence-mds/dnaml/treefile.10268347"))))

(pp-hi-table treefile-10268347 'full 4 nil :num-significant-figures 8)


(setq treefile-10724917
  (node1-node2-distance-list-to-table 
   (distances-from-newick-tree 
    (read-newick-tree 
     "mds/investigations/sequence-mds/dnaml/treefile.10724917"))))

(equal (hi-table-antigens treefile-10268347) (hi-table-antigens treefile-10724917))   T

(transpose (apply-append (hi-table-values treefile-10268347)) (apply-append (hi-table-values treefile-10724917)))

ok, correlation is high, reckon the parsing of distances is correct

|#



;;;----------------------------------------------------------------------
;;;                      distances to trunk
;;;----------------------------------------------------------------------

(defun distances-to-trunk (tree-which-is-list-of-tree-distance-pairs)
  ;; an atom is the unit tree -- a tree-which-is-list-of-tree-distance-pairs
  (if (atom tree-which-is-list-of-tree-distance-pairs)
      (list (list tree-which-is-list-of-tree-distance-pairs 0))
    (loop for ((sub-tree-which-is-list-of-tree-distance-pairs distance) . rest) on tree-which-is-list-of-tree-distance-pairs append
          (if rest
              (distances-to-nodes
               sub-tree-which-is-list-of-tree-distance-pairs
               distance)  
            (distances-to-trunk sub-tree-which-is-list-of-tree-distance-pairs)))))




#|

(fll
 (distances-to-trunk
  '((BB 6.0)
    (((E 4.0)
      (A 5.0)
      (((C2 1.0)
        (CC 2.0))
       3.0))
     5.0)
    (D 11.0))))

BB   6.0
E    9.0
A   10.0
C2   9.0
CC  10.0
D      0

(fll (distances-to-trunk (read-newick-tree "mds/investigations/misc/tree-no-ct-outlier")))

|#


;;;----------------------------------------------------------------------
;;;                      reordering trees
;;;----------------------------------------------------------------------

#|
(B:6.0,(A:5.0,C:3.0,E:4.0):5.0,D:11.0);
(read-newick-tree "/tmp/foot")
((B 6.0) (((A 5.0) (C 3.0) (E 4.0)) 5.0) (D 11.0))
|#



(defun max-tree-distance (tree)
  (apply-max
   (loop for element in tree collect
	 (if (atom (nth 0 element))
	     (nth 1 element)
	   (+ (nth 1 element)
	      (max-tree-distance (nth 0 element)))))))

(defun date-from-tree-strain-name (strain-name)
  (setq strain-name (string strain-name))
  (if (string-member "@" strain-name)
      (read-from-string (substring-after-char #\@ strain-name))
    0))

(defun max-tree-date (tree)
  (apply-max
   (loop for element in tree collect
	 (if (atom (nth 0 element))
	     (date-from-tree-strain-name (nth 0 element))
	   (max-tree-date (nth 0 element))))))

(defun reorder-tree-aux (tree &optional &key test)
  (if (atom tree)
      tree
    (if (= 1 (length tree))
	(progn
          (print "Unexpected condition, please send email to report this message to dsmith@zoo.cam.ac.uk")
          tree)
      (loop for element in (my-sort
			    tree
			    test)
          collect (list (reorder-tree-aux (nth 0 element) :test test)
                        (nth 1 element))))))

(defun remove-zero-length-branches (tree)
  (if (atom tree)
      tree
    (loop for element in tree append
          (if (and (listp (nth 0 element))
                   (zerop (nth 1 element)))
              (remove-zero-length-branches (nth 0 element))
            (list
             (list (remove-zero-length-branches (nth 0 element)) 
                   (nth 1 element)))))))

(defun date-from-strainname-date (x)
  (if (< (length (string x)) 10)
      nil
    (let ((possible-date (reverse (substring (reverse (string x)) 0 9))))
      (if (equal "-<UNKNOWN>" possible-date)
          99999999   ;; as we want below the known dates
        (if (and (equal #\- (aref possible-date 4))
                 (equal #\- (aref possible-date 7)))
            (let ((possible-year  (read-from-string (substring possible-date 0 3)))
                  (possible-month (read-from-string (substring possible-date 5 6)))
                  (possible-day   (read-from-string (substring possible-date 8 9))))
              (if (and (numberp possible-year)
                       (numberp possible-month)
                       (numberp possible-day))
                  (+ (* 10000 possible-year)
                     (*   100 possible-month)
                     (*     1 possible-day))
                nil))
          nil)))))

(defun tree-reorder-test (f)
  (^ (x y)
     (let ((fx (funcall f (list x)))
           (fy (funcall f (list y))))
       (if (= fx fy)
           (if (and (symbolp (car x))
                    (symbolp (car y)))
               (if (and (date-from-strainname-date (car x))
                        (date-from-strainname-date (car y)))
                   (< (date-from-strainname-date (car x))
                      (date-from-strainname-date (car y)))
                 (string< (car x) (car y))))
         (< fx fy)))))
  
(defun reorder-tree (tree)
  ;; reorders tree by max branch length in a clade
  (reorder-tree-aux
   (remove-zero-length-branches
    tree)
   :test (tree-reorder-test #'max-tree-distance)))

(defun reorder-tree-by-date (tree)
  ;; reorders tree by max date in a clade
  (reorder-tree-aux
   (remove-zero-length-branches
    tree)
   :test (tree-reorder-test #'max-tree-date)))



#|
(setq t4 (read-newick-tree "cl/test-files/newick-tree-format-test-4"))
((B 6.0) (((A 5.0) (((C 2.0) (C2 1)) 3) (E 4.0)) 5.0) (D 11.0)) 
(reorder-tree t4)
((B 6.0) (((E 4.0) (((C2 1) (C 2.0)) 3) (A 5.0)) 5.0) (D 11.0)) 


(setq tree
  (read-newick-tree 
   "mds/investigations/sequence-mds/dnaml/treefile.10268347"))
(setq tree-reordered (reorder-tree tree))

(write-newick-tree
 (reorder-tree
  (read-newick-tree 
   "mds/investigations/sequence-mds/dnaml/treefile.10268347"))
 "/tmp/ml-reordered5")


(loop for treefile in '("treefile.10268347"
			"treefile.10724917"
			"treefile.12290219"
			"treefile.15440956"
			"treefile.16809830"
			"treefile.4252725"
			"treefile.4252774"
			"treefile.634622"
			"treefile.9202801") do
      (write-newick-tree
       (reorder-tree
	(read-newick-tree 
	 (format nil "mds/investigations/sequence-mds/dnaml/~a" treefile)))
       (format nil "mds/investigations/sequence-mds/dnaml/~a.reordered" treefile)))

(write-newick-tree
 (reorder-tree
  (read-newick-tree 
   "mds/investigations/sequence-mds/dnaml/treefile-concensus"))
 "mds/investigations/sequence-mds/dnaml/treefile-consensus.reordered")

;; the max likelyhood tree
(reorder-tree
 (read-newick-tree 
  "mds/investigations/sequence-mds/dnaml/treefile.4252725"))



;; ----------------- reorder the 2nd set of Tanmoy trees (the ones for the science submission) ---------------


(loop for treefile in '("treefile.11024666"
			"treefile.1668249"
			"treefile.6310085"
			"treefile.6310053"
			"treefile.11024638"
			"treefile.11024653"
			"treefile.6310072") do
      (write-newick-tree
       (reorder-tree
	(read-newick-tree 
	 (format nil "mds/investigations/sequence-mds/dnaml-for-science/~a" treefile)))
       (format nil "mds/investigations/sequence-mds/dnaml-for-science/~a.reordered" treefile)))


|#






;;;----------------------------------------------------------------------
;;;                 substituting names in a tree
;;;   (supporting ron, getting around the tree labeling restrictions)
;;;----------------------------------------------------------------------

(defun substitute-names-in-tree-from-filename (tree name-pairs)
  (let ((name-pairs (if (stringp name-pairs)
			(fi-in-readline name-pairs :comment-char #\; :line-process-f #'space-delimited-string-to-list)
		      name-pairs)))
    (if (null name-pairs)
	(error "name-pair file should contain some name-pairs if you want me to do anything"))
    (if (not (equal '(2) (remove-duplicates (mapcar #'length name-pairs))))
	(error "Name-pair file must be list of pairs, but some lines do not contain two names"))
    (sublis (mapcar (^ (pair) (cons (nth 0 pair) (nth 1 pair))) name-pairs) tree)))

#||
(ppll (read-newick-tree "cl/test-files/newick-tree-format-test-4"))
((B 6.0) (((A 5.0) (((C 2.0) (C2 1)) 3) (E 4.0)) 5.0) (D 11.0)) 
(ppll (fi-in-s "cl/test-files/newick-tree-format-test-4-name-pairs"))
(B BB C CC) 
(ppll (read-newick-tree "cl/test-files/newick-tree-format-test-4-renamed"))
((BB 6.0) (((A 5.0) (((CC 2.0) (C2 1.0)) 3.0) (E 4.0)) 5.0) (D 11.0)) 
||#




;;;----------------------------------------------------------------------
;;;  color code drawgram postscript names with corresponding save colors
;;;----------------------------------------------------------------------

(defun drawgram-leaf-label-line-p (string)
  (if (>= (length string) 6)
      (equal ") show" (substring string (- (length string) 6)))))

(defun drawgram-leaf-label (string)
  (and (drawgram-leaf-label-line-p string)
       (substring string 1 (- (length string) 7))))

(defun drawgram-leaf-label-before-first-underscore (string)
  (if (substring-before-char #\_ (drawgram-leaf-label string))
      (substring-before-char #\_ (drawgram-leaf-label string))
    (drawgram-leaf-label string)))

(defun color-drawgram-ps-labels (drawgram-ps name-color-s &optional &key additional-colors-to-define output-filename (if-exists :error))
  (if (stringp drawgram-ps)
      (setq drawgram-ps (fi-in-readline drawgram-ps)))
  (if (stringp name-color-s)
      (setq name-color-s (fi-in-readline
			  name-color-s
			  :line-process-f (^ (string)
					     (list
					      (read-from-string (substring-before-char #\space string))
					      (string-trim '(#\space) (substring-after-char #\space string)))))))
  (let* ((unique-colors (remove-duplicates (append (nths 1 name-color-s) additional-colors-to-define) :test #'equal))
	 (unique-color-function-names-alist (loop for unique-color in unique-colors collect
						  (list unique-color (format nil "LT~a" (substring unique-color 1)))))
	 (unique-color-function-definitions (cons
					     "/LTblack { 0 0 0 setrgbcolor } def"
					     (loop for unique-color in unique-colors 
						 for unique-color-function-name in (nths 1 unique-color-function-names-alist) collect
						   (apply #'format nil "/~a { ~5,3f ~5,3f ~5,3f setrgbcolor } def" 
							  unique-color-function-name
							  (rgb256-into-rgb1 (tk-color-to-rgb (string-downcase unique-color)))))))
	 (modified-ps (loop for ps-line in drawgram-ps append
			    (if (equal "%%EndProlog" ps-line)
				(append
				 unique-color-function-definitions
				 (list ps-line))
			      (if (drawgram-leaf-label-line-p ps-line)
				  (list
				   (let ((color-function (assoc-value-1
							  (assoc-value-1 
							   (read-from-string (drawgram-leaf-label-before-first-underscore ps-line))
							   name-color-s)
							  unique-color-function-names-alist
							  :test #'equal
							  )))
				     (if color-function
					 color-function
				       "LTblack"))
				   ps-line)
				(list ps-line))))))
    (if output-filename
	(fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
      modified-ps)))

(defun color-drawgram-ps-labels-from-save (drawgram-ps save &optional &key output-filename (if-exists :error))
  (if (stringp save)
      (setq save (fi-in save)))
  (color-drawgram-ps-labels
   drawgram-ps
   (let* ((names (let ((table-from-save (table-from-save save)))
		   (if (ag-sr-table-p table-from-save)
		       (mapcar #'remove-ag-sr-from-name (hi-table-antigens-short table-from-save))
		     (hi-table-antigens table-from-save))))
	  (colors (firstn 
		   (length names)
		   (loop for color in (coords-name-colors-from-save save) collect
			 (cond ((equal "" color)   "#ffffff") ; set no color to black
			       ((equal "{}" color) "#888888") ; set transparent to light gray
			       (t                  color))))))
     (transpose names colors))
   :output-filename output-filename
   :if-exists       if-exists))


;;;----------------------------------------------------------------------
;;;                rename names in drawgram ps
;;;----------------------------------------------------------------------

(defun substitute-names-in-tree-from-filename (tree name-pairs)
  (let ((name-pairs (if (stringp name-pairs)
			(fi-in-readline name-pairs :comment-char #\; :line-process-f #'space-delimited-string-to-list)
		      name-pairs)))
    (if (null name-pairs)
	(error "name-pair file should contain some name-pairs if you want me to do anything"))
    (if (not (equal '(2) (remove-duplicates (mapcar #'length name-pairs))))
	(error "Name-pair file must be list of pairs, but some lines do not contain two names"))
    (sublis (mapcar (^ (pair) (cons (nth 0 pair) (nth 1 pair))) name-pairs) tree)))



(defun substitute-names-in-drawgram-ps-filename (drawgram-ps-filename name-pairs &optional &key output-filename (if-exists :error))
  (let ((name-pairs (if (stringp name-pairs)
			(fi-in-readline name-pairs :comment-char #\; :line-process-f #'space-delimited-string-to-list-of-strings)
		      name-pairs))
        (drawgram-ps (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename)))
    (if (null name-pairs)
	(error "name-pair file should contain some name-pairs if you want me to do anything"))
    (if (not (equal '(2) (remove-duplicates (mapcar #'length name-pairs))))
	(error "Name-pair file must be list of pairs, but some lines do not contain two names"))

    (let ((modified-ps (loop for ps-line in drawgram-ps collect
                             (let* ((possible-name-after-open-paren (substring-after-char #\( ps-line))
                                    (possible-name (substring-before-char #\) possible-name-after-open-paren))
                                    (rest-of-line  (substring-after-char  #\) possible-name-after-open-paren)))
                               (if possible-name
                                   (if (assoc possible-name name-pairs :test #'equal)
                                       (string-append "(" (assoc-value-1 possible-name name-pairs :test #'equal) ")" rest-of-line)
                                     ps-line)
                                 ps-line)))))
      (if output-filename
          (fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
        modified-ps))))


#|
;; put name info onto name in tree
(fll 
 (let ((name-date-s (fi-in-readline-to-list "~/mds/src/mds/mds/investigations/spatial/name-date-2006-12-05-no-hash.txt")))
   (loop for (name date) in name-date-s collect
         (list 
          name
          (read-from-string 
           (string-append 
            (string name)
            "-"
            (anything->string date)
            (string (implode-list (cons '- (loop for i below (floor (* 12 (nth-value 1 (floor date)))) collect '-))))))))) 
 :filename "/tmp/rename-with-date")
(substitute-names-in-drawgram-ps-filename "/tmp/all-10-reordered-colored.ps" "/tmp/rename-with-date" :output-filename "/tmp/all-10-reordered-colored-renamed.ps" :if-exists :supersede)



;; put name info onto name in tree, blanked name after particular strain's date
;; au/57/04
(substitute-names-in-drawgram-ps-filename
 "/tmp/all-10-reordered-colored.ps"
 (let* ((name-date-s (fi-in-readline-to-list "~/mds/src/mds/mds/investigations/spatial/name-date-2006-12-05-no-hash.txt"))
        (tree-names-without-dates (fi-in-s "/tmp/tree-names-without-dates"))
        (blanking-name 'au/57/04)
        (blanking-date (assoc-value-1 blanking-name name-date-s)))
   (append
    (loop for name-without-date in tree-names-without-dates collect
          (list name-without-date "-"))
    (loop for (name date) in name-date-s collect
          (list 
           name
           (let ((name-with-date-info (read-from-string 
                                       (string-append 
                                        (string name)
                                        "-"
                                        (anything->string date)
                                        (string (implode-list (cons '- (loop for i below (floor (* 12 (nth-value 1 (floor date)))) collect '-))))))))
             (if blanking-date
                 (if (> date blanking-date)
                     "xx"
                   name-with-date-info)
               name-with-date-info))))))
 :output-filename "/tmp/all-10-reordered-colored-renamed-blanked-after-au-57-04.ps"
 :if-exists :supersede)


;; we/1/04
(substitute-names-in-drawgram-ps-filename
 "/tmp/all-10-reordered-colored.ps"
 (let* ((name-date-s (fi-in-readline-to-list "~/mds/src/mds/mds/investigations/spatial/name-date-2006-12-05-no-hash.txt"))
        (tree-names-without-dates (fi-in-s "/tmp/tree-names-without-dates"))
        (blanking-name 'we/1/04)
        (blanking-date (assoc-value-1 blanking-name name-date-s)))
   (append
    (loop for name-without-date in tree-names-without-dates collect
          (list name-without-date "-"))
    (loop for (name date) in name-date-s collect
          (list 
           name
           (let ((name-with-date-info (read-from-string 
                                       (string-append 
                                        (string name)
                                        "-"
                                        (anything->string date)
                                        (string (implode-list (cons '- (loop for i below (floor (* 12 (nth-value 1 (floor date)))) collect '-))))))))
             (if blanking-date
                 (if (> date blanking-date)
                     "xx"
                   name-with-date-info)
               name-with-date-info))))))
 :output-filename "/tmp/all-10-reordered-colored-renamed-blanked-after-we-1-04.ps"
 :if-exists :supersede)

||#



;;;----------------------------------------------------------------------
;;;                     tree timesreies bars
;;;----------------------------------------------------------------------

(defun string-ends-in-yyyy-mm-dd-p (string &optional &key allow-non-numeric-month-and-day)
  (let ((reverse-string (reverse string)))
    (and (>= (length reverse-string) 10)
         (equal "-" (substring reverse-string 2 2))
         (equal "-" (substring reverse-string 5 5))
         (or allow-non-numeric-month-and-day (numberp (read-from-string (reverse (substring reverse-string 0 1)))))
         (or allow-non-numeric-month-and-day (numberp (read-from-string (reverse (substring reverse-string 3 4)))))
         (numberp (read-from-string (reverse (substring reverse-string 6 9)))))))

(defun remove-yyyy-mm-dd-suffix (x)
  (if (stringp x)
      (substring x 0 (dec (- (length x) 11)))
    (let ((x (string x)))
      (read-from-string (substring x 0 (dec (- (length (string x)) 11)))))))

(defun string-suffixed-yyyy-mm-dd-to-date-list (string &optional &key allow-non-numeric-month-and-day)
  (if (not (string-ends-in-yyyy-mm-dd-p string :allow-non-numeric-month-and-day allow-non-numeric-month-and-day))
      (error "String ~a is not suffixed with date in yyyy-mm-dd format" string))
  (let ((reverse-string (reverse string)))
    (list (read-from-string (reverse (substring reverse-string 6 9)))
          (read-from-string (reverse (substring reverse-string 3 4)))
          (read-from-string (reverse (substring reverse-string 0 1)))
          )))

(defun yyyy-mm-dd-list-to-month-ignoring-dd (yyyy-mm-dd-list &optional &key allow-non-numeric-month-and-day)
  (if allow-non-numeric-month-and-day
      (if (numberp (nth 1 yyyy-mm-dd-list))
          (+ (dec (nth 1 yyyy-mm-dd-list)) (* (nth 0 yyyy-mm-dd-list) 12))
        (* (nth 0 yyyy-mm-dd-list) 12))
    (+ (dec (nth 1 yyyy-mm-dd-list)) (* (nth 0 yyyy-mm-dd-list) 12))))

(defun month-to-mmm-yy (date-in-months)
  (multiple-value-bind (year month)
      (floor date-in-months 12)
    (format nil "~a ~2,'0d"
            (nth month '("Jan" 
                         "Feb"
                         "Mar"
                         "Apr"
                         "May"
                         "Jun"
                         "Jul"
                         "Aug"
                         "Sep"
                         "Oct"
                         "Nov"
                         "Dec"))
            (mod year 1000))))

(defun month-to-year (date-in-months)
  (format nil "~4,'0d" (floor date-in-months 12)))


(defun vertical-lines-and-mmm-yy (bars-starting-x
                                  interbar-spacing
                                  bar-length
                                  start-date-in-months
                                  end-date-in-months
                                  timeslot
                                  &optional &key 
                                            bar-time-step-in-months
                                            (vertical-line-full-length nil)
                                            (include-dates-on-timeseries t))
  (let ((by-year (= 12 bar-time-step-in-months)))
    (append
     (list
      ""
      "gsave"
      (format nil "~d 55.000000 translate" 
              (+ bars-starting-x
                 (- (* 0.5 interbar-spacing))
                 (* timeslot
                    (+ bar-length interbar-spacing))))
      "0 -100 moveto"
      "LTtsverticalbarcolor"
      "0.1 setlinewidth"
      (if vertical-line-full-length
          "0 800 lineto stroke"
        "0 655 lineto stroke")
      "grestore")
     (if (and include-dates-on-timeseries
              (not (= timeslot (inc (floor (/ (- end-date-in-months start-date-in-months)
                                              bar-time-step-in-months))))))
         (list
          ""
          "gsave"
          (format nil "/Monaco findfont ~f6 scalefont setfont" (+ bar-length interbar-spacing))
          (format nil "~d 52.00000 translate -90.000000 rotate"
                  (+ bars-starting-x
                     (* timeslot
                        (+ bar-length interbar-spacing))))
          "0 0 moveto"
          "LTblack"
          (format nil "(~a) show" (if by-year 
                                      (month-to-year (+ start-date-in-months (* timeslot bar-time-step-in-months)))
                                    (month-to-mmm-yy (+ start-date-in-months timeslot))))
          "grestore"
          ""
          "gsave"
          (format nil "/Monaco findfont ~f6 scalefont setfont" (+ bar-length interbar-spacing))
          (format nil "~d ~d translate -90.000000 rotate"
                  (+ bars-starting-x
                     (* timeslot
                        (+ bar-length interbar-spacing)))
                  748.00000
                  )
          "0 0 moveto"
          "LTblack"
          (format nil "(~a) show" (if by-year 
                                      (month-to-year (+ start-date-in-months (* timeslot bar-time-step-in-months)))
                                    (month-to-mmm-yy (+ start-date-in-months timeslot))))
          "grestore"
          )))))

(defun add-bar-timeseries-to-drawgram-ps-filename (drawgram-ps-filename 
                                                   &optional &key 
                                                             output-filename 
                                                             (if-exists :error)
                                                             start-date
                                                             end-date
                                                             bars-starting-x
                                                             (bar-width 0.8)
                                                             (bar-length 5.0)
                                                             (interbar-spacing 5.0)
                                                             (bar-time-step-in-months 1)
                                                             (vertical-line-color "grey35")
                                                             (vertical-line-full-length nil)
                                                             (include-dates-on-timeseries t)
                                                             include-lines-connecting-names-and-bars)

  (if (not (or (= bar-time-step-in-months 1)
               (= bar-time-step-in-months 12)))
      (error "Only time intervals of 1 or 12 months are currently implemented"))

  (let* ((by-year (= bar-time-step-in-months 12))
         (drawgram-ps (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename))
         (ps-vertical-line-color-spec (apply #'format nil "/LTtsverticalbarcolor { ~5,3f ~5,3f ~5,3f setrgbcolor } def" 
                                             (rgb256-into-rgb1 (tk-color-to-rgb (string-downcase vertical-line-color)))))
         (drawgram-ps-mod
          (append
           (firstn 10 drawgram-ps)
           (list "/LTblack { 0 0 0 setrgbcolor } def")
           (list ps-vertical-line-color-spec)
           (nthcdr 10 drawgram-ps)))
         (start-date-in-months (yyyy-mm-dd-list-to-month-ignoring-dd (string-suffixed-yyyy-mm-dd-to-date-list start-date)))
         (end-date-in-months   (yyyy-mm-dd-list-to-month-ignoring-dd (string-suffixed-yyyy-mm-dd-to-date-list end-date)))
         (name-date-s-to-mark (loop for ps-line in drawgram-ps-mod append
                                    (let ((possible-name (substring-before-char #\) (substring-after-char #\( ps-line))))
                                      (if possible-name
                                          (if (string-ends-in-yyyy-mm-dd-p possible-name :allow-non-numeric-month-and-day by-year)
                                              (let ((name-date-in-months (yyyy-mm-dd-list-to-month-ignoring-dd
                                                                          (string-suffixed-yyyy-mm-dd-to-date-list possible-name 
                                                                                                                   :allow-non-numeric-month-and-day by-year)
                                                                          :allow-non-numeric-month-and-day by-year)))
                                                (if (and (>= name-date-in-months start-date-in-months)
                                                         (<= name-date-in-months end-date-in-months))
                                                    (list (cons possible-name 
                                                                (cons name-date-in-months 
                                                                      (string-suffixed-yyyy-mm-dd-to-date-list possible-name 
                                                                                                               :allow-non-numeric-month-and-day by-year))))
                                                  nil))
                                            nil)
                                        nil))))
         (name-tsbarxy-s nil)
         (bar-ps-blocks (loop for (name date-in-months) in name-date-s-to-mark append
                              (if (not (position (format nil "(~a) show" name) drawgram-ps-mod :test #'equal))
                                  (progn
                                    (format t "~%Warning: Dated name line in postscript file not in style (<name>) for name ~a" name)
                                    nil)
                                (let* ((name-position (position (format nil "(~a) show" name) drawgram-ps-mod :test #'equal))
                                       (name-block-ps (nth-range (- name-position 5) (+ name-position 1) drawgram-ps-mod))
                                       (this-name-start-x (nth 0 (space-delimited-string-to-list (nth 2 name-block-ps))))
                                       (this-name-start-y (nth 1 (space-delimited-string-to-list (nth 2 name-block-ps))))
                                       (this-bar-start-x (+ bars-starting-x
                                                            (* (floor (/ (- date-in-months start-date-in-months)
                                                                         bar-time-step-in-months))
                                                               (+ bar-length interbar-spacing)))))
                                  (push-end (list name (list (+ this-bar-start-x (/ bar-length 2)) this-name-start-y)) name-tsbarxy-s)  ;; for returning, not used in this function
                                  (append
                                   (list "gsave"
                                         (format nil "%% ~a bar" name)
                                         (format nil "~d ~d translate" 
                                                 this-bar-start-x
                                                 this-name-start-y)
                                         "0 0 moveto"
                                         (nth 4 name-block-ps)
                                         (format nil "~d setlinewidth" bar-width)
                                         (format nil "~d 0 lineto stroke" bar-length)
                                         "grestore")
                                   (if include-lines-connecting-names-and-bars
                                       (list
                                        "%% here"
                                        "gsave"
                                        "1 setlinecap"
                                        "1 setlinejoin"
                                        "0.01 setlinewidth newpath"
                                        "LTblack"
                                        (format nil "~d ~d ~d ~d l"
                                                this-name-start-x
                                                this-name-start-y
                                                this-bar-start-x
                                                this-name-start-y)
                                        "stroke"
                                        "grestore")))))))
         (timeseries-vertical-lines-and-names (cons "%% Vertical lines separating bars, plus timeslot names"
                                                    (loop for timeslot from 0 to (inc (floor (/ (- end-date-in-months start-date-in-months)
                                                                                                bar-time-step-in-months)))
                                                        append
                                                          (vertical-lines-and-mmm-yy 
                                                           bars-starting-x
                                                           interbar-spacing
                                                           bar-length
                                                           start-date-in-months  
                                                           end-date-in-months    
                                                           timeslot
                                                           :bar-time-step-in-months bar-time-step-in-months
                                                           :include-dates-on-timeseries include-dates-on-timeseries
                                                           :vertical-line-full-length vertical-line-full-length
                                                           ))))
                                          
         (modified-ps (append 
                       (butlastn 7 drawgram-ps-mod)
                       bar-ps-blocks
                       timeseries-vertical-lines-and-names
                       (lastn 7 drawgram-ps-mod))))

    (values
     (if output-filename
         (fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
       modified-ps)
     name-tsbarxy-s)))


#||

(progn
  (add-bar-timeseries-to-drawgram-ps-filename
   "/tmp/example-for-D.ps"
   :start-date "2008-03-00"
   :end-date "2008-12-00"
   :bars-starting-x 450
   :output-filename "/tmp/out.ps"
   :if-exists :supersede
   )
  (sleep 0.5)
  (run-shell-command "open /tmp/out.ps"))

(progn
  (add-bar-timeseries-to-drawgram-ps-filename
   "/tmp/example-for-D.ps"
   :start-date "2008-03-00"
   :end-date "2008-12-00"
   :bars-starting-x 450 
   :include-lines-connecting-names-and-bars t   ;; same as above, but with this <<<<<<<<<<<<<<<<<<<<<<<<<<<<
   :output-filename "/tmp/out.ps"
   :if-exists :supersede
   )
  (sleep 0.5)
  (run-shell-command "open /tmp/out.ps"))

(progn
  (add-bar-timeseries-to-drawgram-ps-filename
   "/Users/dsmith/foos/ms2/ron-nuc-closeness-refined-date-fixed.ps"
   :start-date "1997-01-00"
   :end-date "2011-12-31"
   :bars-starting-x 450
   :bar-time-step-in-months 12
   :output-filename  "/Users/dsmith/foos/ms2/ron-nuc-closeness-refined-date-fixed-testout.ps"
   :if-exists :supersede
   )
  (sleep 0.5)
  (run-shell-command "open /Users/dsmith/foos/ms2/ron-nuc-closeness-refined-date-fixed-testout.ps"))

||#



;;;----------------------------------------------------------------------
;;;          convert drawgram slash date format to dash format
;;;              (for retrofitting, not in the usual flow)
;;;----------------------------------------------------------------------

(defun string-ends-in-yyyy-slash-mm-slash-optional-dd-p (string)
  (let ((string-after-last-dash (reverse (substring-before-char #\- (reverse string)))))
    (and (>= (length string-after-last-dash) 8)
         (equal "/" (substring string-after-last-dash 4 4))
         (equal "/" (substring string-after-last-dash 7 7))
         (numberp (read-from-string (reverse (substring string-after-last-dash 0 3)))))))

(defun convert-string-with-yyyy-slash-mm-slash-optional-dd-to-yyyy-mm-dd (string)
  (if (not (string-ends-in-yyyy-slash-mm-slash-optional-dd-p string))
      (error "string should have passed string-ends-in-yyyy-slash-mm-slash-optional-dd-p before this conversion fucntion was called"))
  (let ((string-after-last-dash (reverse (substring-before-char #\- (reverse string)))))
    (let* ((year  (substring string-after-last-dash 0 3))
           (month (substring string-after-last-dash 5 6))
           (day   (if (>= (length string-after-last-dash) 10) 
                      (substring string-after-last-dash 8 9)
                    "XX"))
           (converted-date (format nil "~a-~a-~a" year month day))
           (converted-string (string-append 
                              (reverse (substring-after-char #\- (reverse string)))
                              "-"
                              converted-date)))
      (format t "~%~16a to     ~16a" string-after-last-dash converted-date)
      converted-string)))

(defun convert-slash-dates-to-yyyy-mm-dd-dates-in-drawgram-ps-filename (drawgram-ps-filename 
                                                                        &optional &key 
                                                                                  output-filename 
                                                                                  (if-exists :error))
  (let ((modified-ps 
         (loop for ps-line in (fi-in-readline drawgram-ps-filename) collect
               (let ((possible-name (substring-before-char #\) (substring-after-char #\( ps-line))))
                 (if possible-name
                     (cond ((string-ends-in-yyyy-mm-dd-p possible-name) 
                            ps-line)
                           ((string-ends-in-yyyy-slash-mm-slash-optional-dd-p possible-name)
                            (format nil "(~a) show" (convert-string-with-yyyy-slash-mm-slash-optional-dd-to-yyyy-mm-dd possible-name)))
                           ;;((string-ends-in-yyyy-slash-mm-slash-optional-dd-alt-n-p possible-name)
                           ;; (convert-string-with-yyyy-slash-mm-slash-optional-dd-alt-n-to-yyyy-mm-dd possible-name)))
                           (t ps-line))
                   ps-line)))))
    (format t "~4%       >>>>>>> Best to check that the above conversions look right, in case the expected format was wrong <<<<<<~7%")
    (if output-filename
        (fi 
         modified-ps
         output-filename if-exists t :write-outer-list-elements-individually t)
      modified-ps)))


#||
(convert-slash-dates-to-yyyy-mm-dd-dates-in-drawgram-ps-filename
 "/Users/dsmith/foos/ms2/ron-nuc-closeness-refined.ps"
 :output-filename  "/Users/dsmith/foos/ms2/ron-nuc-closeness-refined-date-fixed.ps"
 :if-exists :supersede
 )
||#


;;;----------------------------------------------------------------------
;;;                      remove elements from tree
;;;----------------------------------------------------------------------

(defun remove-null-elements-from-tree (tree)
  (if (atom tree)
      tree
    (loop for element in tree append
          (if (null (nth 0 element))
              (remove-null-elements-from-tree (nth 0 element))
            (list
             (list (remove-null-elements-from-tree (nth 0 element)) 
                   (nth 1 element)))))))

(defun remove-strains-from-tree-aux (tree strains)
  (if (atom tree)
      tree
    (if (= 1 (length tree))
	(progn
          (print "Unexpected condition, please send email to report this message to dsmith@zoo.cam.ac.uk")
          tree)
      (loop for element in tree
          when (not 
                (and (atom (nth 0 element))
                     (member (nth 0 element) strains)))
          collect (list (remove-strains-from-tree-aux (nth 0 element) strains)
                        (nth 1 element))))))

(defun remove-strains-from-tree (tree strains)
  (remove-null-elements-from-tree  ;; ugly patch up, would be best to have recursed properly in first place
   (remove-strains-from-tree-aux
    (remove-zero-length-branches
     tree)
    strains)))



;;;----------------------------------------------------------------------
;;;                     Extract names from tree
;;;----------------------------------------------------------------------

(defun extract-strain-names-from-drawgram-ps-filename-using-date-suffix-as-identifier-for-strain-name (drawgram-ps-filename)
  (loop for ps-line in (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename) append
        (let ((possible-name (substring-before-char #\) (substring-after-char #\( ps-line))))
          (if possible-name
              (if (string-ends-in-yyyy-mm-dd-p possible-name :allow-non-numeric-month-and-day t)
                  (list possible-name)
                nil)
            nil))))


;;(extract-strain-names-from-drawgram-ps-filename-using-date-suffix-as-identifier-for-strain-name "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps")



;;;----------------------------------------------------------------------
;;;                            Scale Tree
;;;----------------------------------------------------------------------

(defun drawgram-tree-line-line-p (ps-line)
  ;; Test for line of the structure "  455.23    61.38   469.03    61.38 l"
  (if (not (member (substring ps-line 0 0) '("%" "/" "(") :test #'equal))
      (let ((l (space-delimited-string-to-list ps-line)))
        (and (numberp (nth 0 l))
             (numberp (nth 1 l))
             (numberp (nth 2 l))
             (numberp (nth 3 l))
             (eql 'l  (nth 4 l))))))

(defun scale-drawgram-tree-line-line (ps-line x-scale y-scale)
  ;; Scale line structure   "  455.23    61.38   469.03    61.38 l"
  (let ((l (space-delimited-string-to-list ps-line)))
    (format nil "~8,2f ~8,2f ~8,2f ~8,2f l" (* (nth 0 l) x-scale) (* (nth 1 l) y-scale) (* (nth 2 l) x-scale) (* (nth 3 l) y-scale))))


(defun drawgram-name-position-line-p (ps-line)
  ;; Test for line structure   " 138.070336 703.436538 translate -0.000000 rotate"
  (if (not (member (substring ps-line 0 0) '("%" "/" "(") :test #'equal))
      (let ((l (space-delimited-string-to-list ps-line)))
        (and (numberp (nth 0 l))
             (numberp (nth 1 l))
             (eql     (nth 2 l) 'translate)
             (and (numberp (nth 3 l)) (zerop (nth 3 l)))
             (eql     (nth 4 l) 'rotate)))))

(defun scale-drawgram-name-position-line (ps-line x-scale y-scale)
  ;; Scale line structure   " 138.070336 703.436538 translate -0.000000 rotate"
  (let ((l (space-delimited-string-to-list ps-line)))
    (format nil "~f ~f translate -0.000000 rotate" (* (nth 0 l) x-scale) (* (nth 1 l) y-scale))))



(defun drawgram-name-fontsize-line-p (ps-line)
  ;; Test for line structure   "/Times-Roman findfont 0.549036 scalefont setfont"
  (if (not (member (substring ps-line 0 0) '("%" "(") :test #'equal))
      (let ((l (space-delimited-string-to-list ps-line)))
        (and (eql     (nth 0 l) '/Times-Roman)
             (eql     (nth 1 l) 'findfont)
             (numberp (nth 2 l))
             (eql     (nth 3 l) 'scalefont)
             (eql     (nth 4 l) 'setfont)))))

(defun scale-drawgram-name-fontsize-line (ps-line y-scale)
  ;; Scale line structure   "/Times-Roman findfont 0.549036 scalefont setfont"
  ;; Note, to be perfect, we should move the y-position of the font a little to compensate for the fontsize reduction and the text being not center anchored
  (let ((l (space-delimited-string-to-list ps-line)))
    (format nil "/Times-Roman findfont ~f scalefont setfont" (* (nth 2 l) y-scale))))



(defun scale-drawgram-ps-filename (drawgram-ps-filename
                                   &optional &key 
                                             (x-scale 1)
                                             (y-scale 1)
                                             output-filename 
                                             (if-exists :error))
  ;; Do on tree before putting timeseries on the tree, so we don't have to scale the ts, the ts can just fit to the new tree when it is added later."
  (let ((modified-ps 
         (loop for ps-line in (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename) collect
               (if (drawgram-tree-line-line-p ps-line)
                   (scale-drawgram-tree-line-line ps-line x-scale y-scale)
                 (if (drawgram-name-position-line-p ps-line) 
                     (scale-drawgram-name-position-line ps-line x-scale y-scale)
                   (if (and (not (= 1 y-scale)) (drawgram-name-fontsize-line-p ps-line))
                       (scale-drawgram-name-fontsize-line ps-line y-scale)
                     ps-line))))))
    (if output-filename
        (fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
      modified-ps)))


#||
(scale-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps"
 :x-scale 0.5
 :output-filename "/tmp/foo.ps" :if-exists :supersede)

(scale-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps"
 :y-scale 0.5
 :output-filename "/tmp/fooy.ps" :if-exists :supersede)

(scale-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out/WU95-SY97/tree-ordered-renamed-colored-145-ts-labeled.ps"
 :y-scale 0.5
 :output-filename "/tmp/fooy2.ps" :if-exists :supersede)
||#



;;  ------------ what is the x-scale of a tree?  (should be in an output file somewhere, but for now let's extract by an heuristic

(defun branch-horizontal-lengths-from-drawgram-ps-filename (drawgram-ps-filename)
  ;; Line structure is  "  455.23    61.38   469.03    61.38 l"
  (loop for ps-line in (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename) 
      when (drawgram-tree-line-line-p ps-line)
      collect (let ((l (space-delimited-string-to-list ps-line)))
                (- (nth 2 l) (nth 0 l)))))

#||
(fll (sort-hist (branch-horizontal-lengths-from-drawgram-ps-filename
                 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps")))
      0.0  995
 6.889984   21
6.8899994    6
6.8900146   36
 6.899994  193
 6.900009   16
6.9000244   38
13.789978    7
13.789993    1
13.790009   40
13.799988   58
13.800003   14
13.800018   22
17.789993    1
20.689972    5
20.690002   20
20.699982   11
20.699997    3
20.700012   10
27.589996   16
27.590012    1
27.590027    1
27.599976    3
 27.59999    1
27.600006    4
 34.48999   12
34.490005    1
 34.49002    3
     34.5    3
41.390015    1
    48.28    2
48.289993    1
 48.29001    5
55.180008    1
55.190002    1
    62.08    1
62.090027    1


(fll (sort-hist (branch-horizontal-lengths-from-drawgram-ps-filename
                 "mds/investigations/hh-prediction/wobble-trees-out/WU95-SY97/tree-ordered-renamed-colored-145-ts-labeled.ps")))
0.0  3864
1.9599915   292
 1.960022    89
1.9699707     6
1.9700012   467
3.9299927   187
3.9300232    43
 3.939972     1
3.9400024    75
 5.889984    17
5.8900146     6
 5.899994    91
5.9000244    25
7.8599854    13
 7.860016    24
 7.869995    21
7.8700256     5
 9.829987    12
 9.830017     9
11.789978     1
11.790009     2
11.799988     5
11.800018     3
 13.76001    11
 13.77002     1
15.720001     1
 15.72998     1
15.730011     2
17.799995     1
19.660004     2
21.630005     2
23.590027     1
29.490005     1
     29.5     1
 58.98001     1
110.09999     1
163.18001     1


We can for now use the 2nd element in the list as a control to which to scale
||#


;; ------- applying the heuristic found in the exploration above

(defun length-of-one-nuc-in-drawgram-ps-filename (drawgram-ps-filename)
  (nth 0 (nth 1 (sort-hist (branch-horizontal-lengths-from-drawgram-ps-filename drawgram-ps-filename)))))

;; (length-of-one-nuc-in-drawgram-ps-filename "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps")
;; (length-of-one-nuc-in-drawgram-ps-filename "mds/investigations/hh-prediction/wobble-trees-out/WU95-SY97/tree-ordered-renamed-colored-145-ts-labeled.ps")


(defun x-scale-one-nuc-to-length-in-drawgram-ps-filename (drawgram-ps-filename
                                                          one-nuc-length
                                                          &optional &key 
                                                                    output-filename 
                                                                    (if-exists :error))
  (scale-drawgram-ps-filename 
   drawgram-ps-filename
   :x-scale (/ one-nuc-length (length-of-one-nuc-in-drawgram-ps-filename drawgram-ps-filename))
   :output-filename output-filename 
   :if-exists if-exists))

;;(x-scale-one-nuc-to-length-in-drawgram-ps-filename "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps" 1 :output-filename "/tmp/f1.ps")
;;(x-scale-one-nuc-to-length-in-drawgram-ps-filename "mds/investigations/hh-prediction/wobble-trees-out/WU95-SY97/tree-ordered-renamed-colored-145-ts-labeled.ps"   1 :output-filename "/tmp/f2.ps")  
  

;;;----------------------------------------------------------------------
;;;                            Scale Tree
;;;----------------------------------------------------------------------

(defun translate-drawgram-tree-line-line (ps-line x y)
  ;; Scale line structure   "  455.23    61.38   469.03    61.38 l"
  (let ((l (space-delimited-string-to-list ps-line)))
    (format nil "~8,2f ~8,2f ~8,2f ~8,2f l" (+ (nth 0 l) x) (+ (nth 1 l) y) (+ (nth 2 l) x) (+ (nth 3 l) y))))

(defun translate-drawgram-name-position-line (ps-line x y)
  ;; Scale line structure   " 138.070336 703.436538 translate -0.000000 rotate"
  (let ((l (space-delimited-string-to-list ps-line)))
    (format nil "~f ~f translate -0.000000 rotate" (+ (nth 0 l) x) (+ (nth 1 l) y))))

(defun translate-drawgram-ps-filename (drawgram-ps-filename
                                       &optional &key 
                                                 x
                                                 y
                                                 output-filename 
                                                 (if-exists :error))
  ;; Do on tree before putting timeseries on the tree, so we don't have to scale the ts, the ts can just fit to the new tree when it is added later."
  (let ((modified-ps 
         (loop for ps-line in (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename) collect
               (if (drawgram-tree-line-line-p ps-line)
                   (translate-drawgram-tree-line-line ps-line x y)
                 (if (drawgram-name-position-line-p ps-line) 
                     (translate-drawgram-name-position-line ps-line x y)
                   ps-line)))))
    (if output-filename
        (fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
      modified-ps)))


#||
(translate-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps"
 :x 100
 :y 50
 :output-filename "/tmp/foot1.ps" :if-exists :supersede)

;; now move one off the page
(translate-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps"
 :x 200
 :y 200
 :output-filename "/tmp/foot2.ps" :if-exists :supersede)
;; yes, goes to and off the edge of the page (how does it print?)
||#




;;;----------------------------------------------------------------------
;;;                     Draw HZ divider on tree
;;;----------------------------------------------------------------------

(defun add-hz-line-to-drawgram-ps (drawgram-ps
                                   y
                                   &optional &key
                                             (x-start 10) 
                                             (x-end   300)
                                             label
                                             (label-x-offset 0)
                                             (label-y-offset 0)
                                             (label-scalefont 1.0))
  (let ((label-line-and-string-ps-code (cons "%% Horizontal line and annotation"
                                             (append
                                              (list
                                               "gsave"
                                               (format nil "~d ~d translate" x-start y)
                                               "0 0 moveto"
                                               "0 0 0 setrgbcolor" ;;LTblack"
                                               "0.1 setlinewidth"
                                               (format nil "~d ~d lineto stroke" x-end 0)
                                               "grestore")
                                              (list
                                               "gsave"
                                               (format nil "/Monaco findfont ~f scalefont setfont" label-scalefont)
                                               (format nil "~d ~d translate" (+ x-start label-x-offset)  (+ y label-y-offset))
                                               ;;(format nil "-0.000000 rotate")
                                               "0 0 moveto"
                                               "0 0 0 setrgbcolor" ;;LTblack"
                                               (format nil "(~a) show" label)
                                               "grestore"
                                               )))))

    '(loop for ps-line in drawgram-ps append
          (if (equal "stroke showpage " ps-line)   ;; put the code for the label right at the end
              (append
               label-line-and-string-ps-code
               (list ps-line))
            (list ps-line)))
    
    (append 
     (butlastn 7 drawgram-ps)
     label-line-and-string-ps-code
     (lastn 7 drawgram-ps))))


(defun add-hz-lines-to-drawgram-ps-filename (drawgram-ps-filename
                                             ys
                                             &optional &key
                                                       (x-start 10) 
                                                       (x-end   590)
                                                       labels
                                                       (label-x-offsets  0)
                                                       (label-y-offsets  0)
                                                       (label-scalefonts 15)
                                                       output-filename 
                                                       (if-exists :error))

  (if (not labels)                   (setq labels           (loop for i below (length labels) collect "")))
  ;;(setq labels (mapcar #'anything->string labels))

  (if (not (listp label-x-offsets))  (setq label-x-offsets  (loop for i below (length labels) collect label-x-offsets)))
  (if (not (listp label-y-offsets))  (setq label-y-offsets  (loop for i below (length labels) collect label-y-offsets)))
  (if (not (listp label-scalefonts)) (setq label-scalefonts (loop for i below (length labels) collect label-scalefonts)))

  (let ((drawgram-ps (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename)))
    (loop for y in ys
        for label in labels
        for label-x-offset in label-x-offsets
        for label-y-offset in label-y-offsets
        for label-scalefont in label-scalefonts do
          (setq drawgram-ps (add-hz-line-to-drawgram-ps 
                             drawgram-ps
                             y
                             :x-start x-start
                             :x-end   x-end
                             :label label
                             :label-x-offset  label-x-offset 
                             :label-y-offset  label-y-offset 
                             :label-scalefont label-scalefont)))

    (if output-filename
        (fi drawgram-ps output-filename if-exists t :write-outer-list-elements-individually t)
      drawgram-ps)
    ))

#||
(add-hz-lines-to-drawgram-ps-filename 
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps" 
 '(100 200 300)
 ;;(x-start 10) 
 ;;(x-end   300)
 :labels '("100x" "200y" "300z")
 :label-scalefonts 15
 :label-x-offsets 10
 :label-y-offsets -15
 :output-filename "/tmp/fool1.ps" :if-exists :supersede)
||#


;; ------------------ extract y position of strain name for positioning hz lines -------------------

(defun position-of-strain-name-in-drawgram-ps (drawgram-ps strain-name)
  (let ((line-start-string (string-append "(" (string strain-name)))
        (substring-end-position (length (string strain-name))))
    (loop for (line-a line-b line-c line-d) on drawgram-ps 
        when (and ;;(print (list line-a line-b line-c line-d))
                  (< substring-end-position (length line-d))
                  (equal line-start-string (substring line-d 0 substring-end-position)))
        do (let ((line-a-as-list (space-delimited-string-to-list line-a))
                 (line-b-as-list (space-delimited-string-to-list line-b)))
             line-c
             (if (equal 'translate (nth 2 line-a-as-list))
                 (return (firstn 2 line-a-as-list))
               (if (equal 'translate (nth 2 line-b-as-list))
                   (return (firstn 2 line-b-as-list))
                 (error "unexpected condition")))))))
             ;;(return (firstn 2 (space-delimited-string-to-list line-b)))))))  ;; is line-a after ts is added to a tree, line-b before (could test locally, but not now)

#||
(position-of-strain-name-in-drawgram-ps
 (fi-in-readline "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps")
 'A/CONNECTICUT/05/2010-2010-11-24)
(393.30936 279.27692)

(position-of-strain-name-in-drawgram-ps
 (fi-in-readline "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps")
 'A/CONNECTICUT/05/2010)
(393.30936 279.27692)
||#                     


(defun add-hz-lines-above-strain-names-to-drawgram-ps-filename (drawgram-ps-filename
                                                                strain-names
                                                                &optional &key
                                                                          labels
                                                                          (label-x-offsets  0)
                                                                          (label-y-offsets  0)
                                                                          (label-scalefonts 10)
                                                                          (x-start 10) 
                                                                          (x-end   590)
                                                                          output-filename 
                                                                          (if-exists :error))
  (let* ((drawgram-ps (if (stringp drawgram-ps-filename) (fi-in-readline drawgram-ps-filename) drawgram-ps-filename))
         (ys-some-nil-when-strain-not-in-tree (nths 1 (loop for strain-name in strain-names collect (position-of-strain-name-in-drawgram-ps drawgram-ps strain-name))))
         (ys (loop for y in ys-some-nil-when-strain-not-in-tree when y collect y))
         (labels (loop for y in ys-some-nil-when-strain-not-in-tree for label in labels when y collect label)))
    (add-hz-lines-to-drawgram-ps-filename 
     drawgram-ps
     ys
     :labels labels
     :label-x-offsets label-x-offsets
     :label-y-offsets label-y-offsets
     :label-scalefonts label-scalefonts
     :x-start x-start
     :x-end   x-end
     :output-filename output-filename
     :if-exists       if-exists)))
     
     
#||
(add-hz-lines-above-strain-names-to-drawgram-ps-filename
 "mds/investigations/hh-prediction/wobble-trees-out-3/WI05-PE09/tree-ordered-renamed-colored-145-ts-labeled.ps" 
 '(a/japan/af2777/2012 a/new-york/382/2005)
 :labels '(100x 200y)
 :output-filename "/tmp/foos1.ps" :if-exists :supersede)
||#



;;;----------------------------------------------------------------------
;;;                    merge drawgram ps files
;;;----------------------------------------------------------------------

(defun lines-before-and-including-line-drawgram-ps (line lines &key prefixp)
  (let ((prefix-length (length (string line))))
    (firstn (inc (position line lines :test (if prefixp (^ (a b) (and (>= (length b) prefix-length) (equal a (substring b 0 (dec prefix-length))))) #'equal))) lines)))

(defun lines-after-and-including-line-drawgram-ps (line lines &key prefixp)
  (let ((prefix-length (length (string line))))
    (nthcdr (position line lines :test (if prefixp (^ (a b) (and (>= (length b) prefix-length) (equal a (substring b 0 (dec prefix-length))))) #'equal)) lines)))

(defun lines-before-line-drawgram-ps (line lines &key prefixp)
  (let ((prefix-length (length (string line))))
    (firstn (position line lines :test (if prefixp (^ (a b) (and (>= (length b) prefix-length) (equal a (substring b 0 (dec prefix-length))))) #'equal)) lines)))

(defun lines-after-line-drawgram-ps (line lines &key prefixp)
  (let ((prefix-length (length (string line))))
    (nthcdr (inc (position line lines :test (if prefixp (^ (a b) (and (>= (length b) prefix-length) (equal a (substring b 0 (dec prefix-length))))) #'equal))) lines)))

(defun lines-between-and-excluding-drawgram-ps (pre-line post-line lines &key prefixp)
  (lines-before-line-drawgram-ps post-line (lines-after-line-drawgram-ps pre-line lines :prefixp prefixp) :prefixp prefixp))

(defun lines-between-and-including-drawgram-ps (pre-line post-line lines &key prefixp)
  (lines-before-and-including-line-drawgram-ps post-line (lines-after-and-including-line-drawgram-ps pre-line lines :prefixp prefixp) :prefixp prefixp))

;; (lines-between-and-excluding-drawgram-ps "1" "4" '("0" "1" "2" "3" "4" "5"))   ;; ("2" "3")
;; (lines-between-and-including-drawgram-ps "1" "4" '("0" "1" "2" "3" "4" "5"))   ;; ("1" "2" "3" "4")


(defun extract-prolog-from-drawgram-ps (drawgram-ps)
  (lines-before-and-including-line-drawgram-ps
   "%%PaperSize:"
   drawgram-ps
   :prefixp t))

(defun extract-draw-body-from-drawgram-ps (drawgram-ps)
  (lines-between-and-excluding-drawgram-ps
   "%%PaperSize:"
   "stroke showpage "
   drawgram-ps
   :prefixp t))

(defun extract-postlog-from-drawgram-ps (drawgram-ps)
  (lines-after-and-including-line-drawgram-ps
   "stroke showpage "
   drawgram-ps))


;;;----------------------------------------------------------------------
;;;                     replacing lines
;;;----------------------------------------------------------------------

(defun replace-line-in-ps (ps old new)
  (append
   (lines-before-line-drawgram-ps old ps)
   (list new)
   (lines-after-line-drawgram-ps old ps)))
;;(replace-line-in-ps '("0" "1" "2" "3" "4" "5") "1" '"1-replacemnt")   ;; ("0" "1-replacemnt" "2" "3" "4" "5")


;;;----------------------------------------------------------------------
;;;            inserting lines after postscript prolog
;;;----------------------------------------------------------------------

(defun insert-lines-after-line-in-ps (ps line ps-lines-to-insert)
  (append
   (lines-before-and-including-line-drawgram-ps line ps)
   ps-lines-to-insert
   (lines-after-line-drawgram-ps line ps)))
;;(insert-lines-after-line-in-ps '("0" "1" "2" "3" "4" "5") "1" '("1b" "1c"))   ;; ("0" "1" "1b" "1c" "2" "3" "4" "5")

(defun insert-lines-before-line-in-ps (ps line ps-lines-to-insert)
  (append
   (lines-before-line-drawgram-ps line ps)
   ps-lines-to-insert
   (lines-after-and-including-line-drawgram-ps line ps)))




(defun insert-clip-rectangle-in-drawgram-ps-filename (drawgram-ps x1 y1 x2 y2 &optional &key output-filename (if-exists :error))
  (setq drawgram-ps
    (if (stringp drawgram-ps)
        (fi-in-readline drawgram-ps)
      drawgram-ps))
  (let ((modified-ps (insert-lines-after-line-in-ps
                      drawgram-ps
                      "%%PaperSize: Letter"
                      (let ((width  (- x2 x1))
                            (height (- y2 y1)))
                        (list (format nil "~d ~d moveto ~d ~d rlineto ~d ~d rlineto ~d ~d rlineto closepath clip"
                                      x1 y1
                                      0 height
                                      width 0
                                      0  (- height)))))))
    (if output-filename
        (fi modified-ps output-filename if-exists t :write-outer-list-elements-individually t)
      modified-ps)))




#||
%%BoundingBox: 0 0 612 792
%%DocumentPaperSizes: Letter

% Set the page size to A3 
<< /PageSize [842 1191] /Orientation 3 >> setpagedevice 
||#