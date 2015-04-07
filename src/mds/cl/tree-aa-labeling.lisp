(in-package user)

;;;----------------------------------------------------------------------
;;;                       tree aa labeling
;;;----------------------------------------------------------------------

#||


(read-fasta-file "mds/investigations/aa-labeling/all-ha-simple.fasta")


(loop for (name nuc) in (read-fasta-file "mds/investigations/aa-labeling/all-ha-simple.fasta") collect
	(list name 
	      (align-nuc-seqs-to-pro-subseq-and-trim
	       nuc
	       '(T A T L C L G H H)
	       329
	       :aligning-position-of-subseq-starting-at-1 10
	       :num-permissable-missmatches 1
	       :read-frame-shifts '(0 1 2)
	       :no-aa-action :x)))


(fll (setq name-aas-s 
  (loop for (name seq) in (read-fasta-file "mds/investigations/aa-labeling/all-ha-simple.fasta") collect
        (list name (explode-symbol (dna-to-pro (substring (string seq) 0) :no-aa-action :x))))))







(setq tree (read-newick-tree "mds/investigations/aa-labeling/all-ha-simple-retreed-reordered"))

(equal tree (substs-on-branches-in-tree tree name-aas-s))

(substs-on-branches-in-tree tree name-aas-s)





(format nil "~{       ~3,'0d~}" (loop for i from 10 to 400 by 10 collect i))
                   010       020       030       040       050       060       070       080       090       100       110       120       130       140       150       160       170       180       190       200       210       220       230       240       250       260       270       280       290       300       310       320       330       340     
N000000001  QKLPGNDNSTATLCLGHHAVPNGTLVKTITNDQIEVTNATELVQSSSTGRICDSPHRILDGKNCTLIDALLGDPHCDGFQNKEWDLFVERSKAYSNCYPYDVPDYASLRSLVASSGTLEFTNEGFNWTGVAQDGTSYACKRGSVKSFFSRLNWLHKLEYKYPALNVTMPNNDKFDKLYIWGVHHPSTDSDQTSIYVQASGRVTVSTKRSQQTVIPNIGSRPWVRGISSRISIYWTIVKPGDILLINSTGNLIAPRGYFKIRSGKSSIMRSDAPIGNCNSECITPNGSIPNDKPFQNVNRITYGACPRYVKQNTLKLATGMRNVPEKQTR
N000000028  QKLPGNDNSTATLCLGHHAVPNGTLVKTITNDQIEVTNATELVQSSSTGRICDSPHRILDGKNCTLIDALLGDPHCDGFQNKEWDLFVERSKAYSNCYPYDVPDYASLRSLVASSGTLEFTNEGFNWTGVAQDGTSYACKRGSVKSFFSRLNWLHKLEYKYPALNVTMPNNDKFDKLYIWGVHHPSTDSVQTSLYVQASGRVTVSTKRSQQTVIPNIGSRPWVRGISSRISIYWTIVKPGDILLINSTGNLIAPRGYFKIRSGKSSIMRSDAPIGNCNSECITPNGSIPNDKPFQNVNRITYGACPRYVKQNTLKLATGMRNVPEKQTR



---------------------------------------------------------------------------------------------------------------------------------



(setq tree (read-newick-tree "mds/investigations/aa-labeling/science-tree-asia-only-simple-names.newick"))
(setq tree (read-newick-tree "mds/investigations/aa-labeling/science-tree-asia-only-simple-names-with-n0000000020-removed.newick"))


(setq simple-to-real-name-alist (fi-in-readline-to-list "mds/investigations/aa-labeling/science-tree-asia-only-simple-to-real-names.txt"))
(setq real-to-simple-name-alist (mapcar #'reverse simple-to-real-name-alist))

(fll (setq name-aas-s 
  (loop for (name seq) in (read-fasta-file "mds/investigations/aa-labeling/science-tree-asia-only.fasta") collect
        (list (assoc-value-1 name real-to-simple-name-alist) (explode-symbol (dna-to-pro seq :no-aa-action :x))))))



---------------------------------------------------------------------------------------------------------------------------------



(setq tree 
  (substitute-names-in-tree-from-filename
   (read-newick-tree "mds/investigations/aa-labeling/asia-only.fasta-tree-paup-rooted-reordered")
   "mds/investigations/aa-labeling/asia-only.fasta-simple-to-original"))

(setq tree-short 
  (substitute-names-in-tree-from-filename
   (read-newick-tree "mds/investigations/aa-labeling/asia-only.fasta-tree-paup-rooted-reordered-short")
   "mds/investigations/aa-labeling/asia-only.fasta-simple-to-original"))
      
(fll (setq name-aas-s 
  (loop for (name seq) in (read-fasta-file "mds/investigations/aa-labeling/asia-only.fasta") collect
        (list name (explode-symbol seq)))))

(run-shell-command "open mds/investigations/aa-labeling/asia-only.fasta-tree-paup-rooted-reordered-renamed-lines-names-recolored-dated.ps")




---------------------------------------------------------------------------------------------------------------------------------



(setq tree 
  (substitute-names-in-tree-from-filename
   (read-newick-tree "mds/investigations/aa-labeling/new-short-tree-simple-names.newick")
   "mds/investigations/aa-labeling/new-short-tree-simple-to-real-names.txt"))

(fll (setq name-aas-s 
  (loop for (name seq) in (read-fasta-file "mds/investigations/aa-labeling/asia-only.fasta") collect
        (list name (explode-symbol seq)))))

(run-shell-command "open mds/investigations/aa-labeling/new-short-tree.ps")




||#


(defun parse-newick-tree (tree)
  ;; tree is made up of a list of tree-dist pairs--dist is the lenght of the branch before the tree
  (loop for (tree dist) in tree collect
        (if (atom tree)
            (list tree dist)
          (list (parse-newick-tree tree) dist))))









(defun sequence-difference-sequence-sequence (s1 s2)
  (if (not (equal (length s1) (length s2))) (error "Assumption violation"))
  (loop for i from 1
      for e1 in s1
      for e2 in s2
      when (and (not (eql '- e1))
                (not (eql '- e2))
                (not (eql 'x e1))
                (not (eql 'x e2))
                (not (eql e1 e2)))
      collect (read-from-string (format nil "~a~3,'0d~a" e1 i e2))))
;; (all-comparisons (nths 1 name-aas-s) #'sequence-difference-sequence-sequence)



#||
(ppl (loop for name in '(THAILAND/809/2006 TH-TH-701-06 SAITAMA/60/2006 HAMAMATU/183/2006 THAILAND/618/2006 BANGLADESH/1999/2006 THAILAND/715/2006 NP-HC90997-ORIGINAL) collect (sequence-difference-sequence-sequence (assoc-value-1 'JIANGXIDONGHU/1435/2006 name-aas-s) (assoc-value-1 name       name-aas-s))))

THAILAND/809/2006     (                  K082E       D144N L157S K173E) 
TH-TH-701-06          (      N006I S045N             D144N L157S K173E) 
SAITAMA/60/2006       (      N006I             T128A D144N L157S K173E) 
HAMAMATU/183/2006     (      N006I             T128A D144N L157S K173E) 
THAILAND/618/2006     (      N006I             T128A D144N       K173E) 
BANGLADESH/1999/2006  (      N006I             T128A             K173E) 
THAILAND/715/2006     (      N006I             T128A D144N       K173E I192L) 
NP-HC90997-ORIGINAL   (Q001S N006I             T128A D144N       K173E)      

(((THAILAND/809/2006 0.00387 (K082E D144N L157S))
||#




(defun names-in-tree (tree)
  (loop for (tree ignore) in tree append
        (progn
          ignore
          (if (atom tree)
              (list tree)
            (names-in-tree tree)))))

(defun update-sequence-with-substitutions (sequence substitutions)
  (let ((substs (loop for substitution in substitutions collect 
                      (list (read-from-string (substring (string substitution) 0 0))
                            (read-from-string (substring (string substitution) 1 3))
                            (read-from-string (substring (string substitution) 4 4))))))
    (if (not (equal (nths 0 substs) (multiple-nth (mapcar #'dec (nths 1 substs)) sequence))) (error "assumption violation"))
    (replace-multiple-nth (mapcar #'dec (nths 1 substs)) (nths 2 substs) sequence)))
;;(update-sequence-with-substitutions '(a b c d) '(a001u d004x c003w))
;;(U B W X)

(defun sequence-at-end-of-branch (sequence-at-start-of-branch tree name-seq-s branch-substitutions)
  ;; update the sequence at start of branch for recursion, reset to strain on trunk if there is on (as incremental update gets lost by reversions?)
  (loop for (tree dist) in tree
      when (and (atom tree)
                (zerop dist))
      do (progn
           '(if (not 
                (equal 
                 (assoc-value-1 tree name-seq-s)
                 (update-sequence-with-substitutions sequence-at-start-of-branch branch-substitutions)))
               (format t "~%~30a ~a" 
                       tree
                       (sequence-difference-sequence-sequence 
                        (update-sequence-with-substitutions sequence-at-start-of-branch branch-substitutions)
                        (assoc-value-1 tree name-seq-s)))
             (format t "~%match"))
           (return (progn (print 'update) (assoc-value-1 tree name-seq-s))))
      finally (return (progn (print 'incremental) (update-sequence-with-substitutions sequence-at-start-of-branch branch-substitutions)))))

(defun sequence-difference-sequence-tree (sequence tree name-seq-s &optional expected-num-substs)
  (let ((branch-substitutions-raw
         (let* ((differences-to-each-branch-of-tree
                 (loop for name in (names-in-tree tree) collect 
                       (list name (sequence-difference-sequence-sequence sequence (assoc-value-1 name name-seq-s)))))
                (differences-within-proportion-of-length
                 (let ((proportion-of-length (* 0.05 (length sequence))))
                   (collect (^ (l) (< (length (nth 1 l)) proportion-of-length)) differences-to-each-branch-of-tree))))
           
           (if expected-num-substs
               (firstn expected-num-substs (nths 0 (hist-most-common-sort (flatten (nths 1 differences-within-proportion-of-length)))))
             (apply #'nary-intersection (nths 1 differences-within-proportion-of-length))))))
        
    (let ((phase-2 (sequence-difference-sequence-sequence 
                    sequence 
                    (sequence-at-end-of-branch sequence tree name-seq-s branch-substitutions-raw))))
      
      (if (not (equal (sort-alpha branch-substitutions-raw) (sort-alpha phase-2)))
          (print (list expected-num-substs '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> branch-substitutions-raw phase-2)))
      
      phase-2)))


(defun reverse-subst (subst) 
  (setq subst (string subst))
  (read-from-string 
   (string-append (substring subst 4 4) 
                  (substring subst 1 3)
                  (substring subst 0 0))))
   
(defun annotate-reversions (substitutions substs-on-ancestral-branches)
  (let ((flatten-substs-on-ancestral-branches (flatten substs-on-ancestral-branches)))
    (loop for substitution in substitutions collect
          (if (member (reverse-subst substitution) flatten-substs-on-ancestral-branches)
              (progn '(print (list (read-from-string (string-append (string substitution) "-r"))
                                  substs-on-ancestral-branches))
                     (read-from-string (string-append (string substitution) "-r")))
            substitution))))

(defun substs-on-branches-in-tree-aux (sequence-at-start-of-branch tree name-seq-s &optional &key 
                                                                                             approx-dist-per-subst
                                                                                             (substs-on-ancestral-branches '(() () () ())))
  ;; tree is made up of a list of tree-dist pairs--dist is the length of the branch before the tree
  (loop for (tree dist) in tree collect
        (let ((branch-substitutions (sequence-difference-sequence-tree 
                                     sequence-at-start-of-branch
                                     (list (list tree dist))
                                     name-seq-s
                                     (if approx-dist-per-subst (round (/ dist approx-dist-per-subst))))))
          (if (atom tree)
              (list tree 
                    dist
                    (annotate-reversions branch-substitutions substs-on-ancestral-branches))
            (list (substs-on-branches-in-tree-aux
                   (update-sequence-with-substitutions sequence-at-start-of-branch branch-substitutions)
                   tree 
                   name-seq-s
                   :approx-dist-per-subst approx-dist-per-subst
                   :substs-on-ancestral-branches (cons branch-substitutions (butlast substs-on-ancestral-branches)))
                  dist
                  (annotate-reversions branch-substitutions substs-on-ancestral-branches))))))

;;     (substs-on-branches-in-tree (nth 1 (nth 0 name-aas-s)) tree name-aas-s)
;; (fi (substs-on-branches-in-tree (nth 1 (nth 0 name-aas-s)) tree name-aas-s) "/tmp/tree" :supersede)
;; (fi (substs-on-branches-in-tree (assoc-value-1 'JIANGXIDONGHU/1435/2006 name-aas-s) tree-short name-aas-s) "/tmp/tree-short" :supersede)


#||

first time thru (to determine branch length for each subst, and second time through

ULSAN/689/2006                 NIL             ULSAN/689/2006                 NIL           
BEIJINGXICHENG/313/2006        NIL             BEIJINGXICHENG/313/2006        NIL           
JEONBUK/746/2006               NIL             JEONBUK/746/2006               NIL           
GANGWON/741/2006               NIL             GANGWON/741/2006               NIL           
BANGLADESH/1999/2006           (N006I T128A)   BANGLADESH/1999/2006           (T128A N144D) 
THAILAND/618/2006              NIL             THAILAND/618/2006              NIL           
SAITAMA/60/2006                NIL             SAITAMA/60/2006                NIL           
NP-HC91440-ORIGINAL            NIL             NP-HC91440-ORIGINAL            NIL           
TH-TH-701-06                   NIL             TH-TH-701-06                   NIL           
GYEONGNAM/684/2006             NIL             GYEONGNAM/684/2006             NIL           

||#



(defun average-length-per-subsst-in-subst-annotated-tree-aux (tree)
  (loop for (tree dist substs) in tree append
        (if (atom tree)
            (list (list dist substs))
          (cons (list dist substs)
                (average-length-per-subsst-in-subst-annotated-tree-aux tree)))))

(defun average-length-per-subsst-in-subst-annotated-tree (tree)
  (let ((length-substs-s (average-length-per-subsst-in-subst-annotated-tree-aux tree)))
    (float
     (/ (apply-+ (nths 0 length-substs-s))
        (apply-+ (mapcar #'length (nths 1 length-substs-s)))))))

(defun substs-on-branches-in-tree (sequence-at-start-of-branch tree name-seq-s)
  (let ((approx-dist-per-subst (average-length-per-subsst-in-subst-annotated-tree 
                                (substs-on-branches-in-tree-aux sequence-at-start-of-branch tree name-seq-s))))
    (substs-on-branches-in-tree-aux sequence-at-start-of-branch tree name-seq-s :approx-dist-per-subst approx-dist-per-subst)))

;; (fi (substs-on-branches-in-tree     (assoc-value-1 'JIANGXIDONGHU/1435/2006 name-aas-s) tree-short name-aas-s) "/tmp/tree-short5" :supersede)
;; (fi (substs-on-branches-in-tree-aux (assoc-value-1 'JIANGXIDONGHU/1435/2006 name-aas-s) tree name-aas-s) "/tmp/new-short-tree-sux" :supersede)
;; (fi (substs-on-branches-in-tree     (assoc-value-1 'a/hk/434/96 name-aas-s) tree name-aas-s) "/tmp/tree" :supersede)


;;;----------------------------------------------------------------------
;;;                            TODO
;;;----------------------------------------------------------------------

#||

check when reset is not same as update

record last (say) 4 branche updates to check for reversions

do on tree that colin has annotated to see if we get it right

compare new tree-short with old to see how different

mark on drawgram postscript




sylvie, andy

experiments for fitness
  -- miranda and annotate plaques
  -- code the unbiasing
  -- bjorn what experiments (145 and 155 fitness, and compare with fitness change of heavy hitter taken)


mariette

|##
