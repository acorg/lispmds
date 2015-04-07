(in-package user)


;;;----------------------------------------------------------------------
;;;                         genetics
;;;----------------------------------------------------------------------

(defvar *codon-usage*)
(setq *codon-usage*
  '(("---" Gap "-")
    ("TAA" Stop "Z")
    ("TAG" Stop "Z")
    ("TGA" Stop "Z")
    ("TTT" Phe "F")
    ("TTC" Phe "F")
    ("TTA" Leu "L")
    ("TTG" Leu "L")
    ("TCT" Ser "S")
    ("TCC" Ser "S")
    ("TCA" Ser "S")
    ("TCG" Ser "S")
    ("TAT" Tyr "Y")
    ("TAC" Tyr "Y")
    ("TGT" Cys "C")
    ("TGC" Cys "C")
    ("TGG" Trp "W")
    ("CTT" Leu "L")
    ("CTC" Leu "L")
    ("CTA" Leu "L")
    ("CTG" Leu "L")
    ("CCT" Pro "P")
    ("CCC" Pro "P")
    ("CCA" Pro "P")
    ("CCG" Pro "P")
    ("CAT" His "H")
    ("CAC" His "H")
    ("CAA" Gln "Q")
    ("CAG" Gln "Q")
    ("CGT" Arg "R")
    ("CGC" Arg "R")
    ("CGA" Arg "R")
    ("CGG" Arg "R")
    ("ATT" Ile "I")
    ("ATC" Ile "I")
    ("ATA" Ile "I")
    ("ATG" Met "M")
    ("ACT" Thr "T")
    ("ACC" Thr "T")
    ("ACA" Thr "T")
    ("ACG" Thr "T")
    ("AAT" Asn "N")
    ("AAC" Asn "N")
    ("AAA" Lys "K")
    ("AAG" Lys "K")
    ("AGT" Ser "S")
    ("AGC" Ser "S")
    ("AGA" Arg "R")
    ("AGG" Arg "R")
    ("GTT" Val "V")
    ("GTC" Val "V")
    ("GTA" Val "V")
    ("GTG" Val "V")
    ("GCT" Ala "A")
    ("GCC" Ala "A")
    ("GCA" Ala "A")
    ("GCG" Ala "A")
    ("GAT" Asp "D")
    ("GAC" Asp "D")
    ("GAA" Glu "E")
    ("GAG" Glu "E")
    ("GGT" Gly "G")
    ("GGC" Gly "G")
    ("GGA" Gly "G")
    ("GGG" Gly "G")))


(defvar *amino-acids*)
(setq *amino-acids* (sort-alpha (my-remove-duplicates (mapcar #'read-from-string (nthcdr 4 (nths 2 *codon-usage*))))))
;; (A C D E F G H I K L M N P Q R S T V W Y) 


(defvar *hydrophobic-aas*)
(setq *hydrophobic-aas* '(A F I L M P V W))



;;;----------------------------------------------------------------------
;;;                     converting nuc/aa
;;;----------------------------------------------------------------------

(defun dna-to-pro (dna-string-or-symbol &optional &key (no-aa-action :cerror) stop-at-stop-codon)
  ;; if passed string then return string, if passed symbol then return symbol
  (let ((dna-string (string dna-string-or-symbol)))
    (let ((pro-list (loop for i from 0 below (* 3 (floor (/ (length dna-string) 3))) by 3 collect
			  (let* ((codon (substring dna-string i (+ i 2)))
				 (aa (nth 2 (assoc codon *codon-usage* :test #'equal))))
			    (if (null aa)
				(case no-aa-action
				  (:cerror (progn
					     (cerror "continue?" "codon ~a does not correspond to an amino acid" codon)
					     "X"))
				  (:x      "X")
				  (:-      "-")
				  (t (error "unknown action ~a specified for :no-aa-action" no-aa-action)))
			      aa)))))
      (if stop-at-stop-codon
	  (if (member "Z" pro-list :test #'equal)
	      (setq pro-list (firstn (position "Z" pro-list :test #'equal) pro-list))))
      (let ((pro-string (apply #'string-append pro-list)))
	(if (symbolp dna-string-or-symbol)
	    (read-from-string pro-string)
	  pro-string)))))

(defun name-dna-pairs-to-name-pro-pairs (name-dna-string-or-symbol-s &optional &key (no-aa-action :cerror) stop-at-stop-codon)
  (loop for (name dna) in name-dna-string-or-symbol-s collect
	(list name (dna-to-pro dna :no-aa-action no-aa-action :stop-at-stop-codon stop-at-stop-codon))))


;;;----------------------------------------------------------------------
;;;            counting (non)synonomous substitutions
;;;----------------------------------------------------------------------

(defun substitutions-between-codons (nucleotide-3tuple-a nucleotide-3tuple-b &optional &key
                                                                                       (all-synonymous-nonsynonymous 'all)
                                                                                       (positions-considered '(0 1 2)))
  (if (my-set-difference (append nucleotide-3tuple-a nucleotide-3tuple-b) '(a c g t))
      (error "~%Function non-synonomous-substitutions-between-codons has not been coded to handle anything other than ACGT~%"))
  (let ((single-substitution-b-3tuples-s
         (loop for a in nucleotide-3tuple-a
             for b in nucleotide-3tuple-b
             for i from 0
             when (and (not (equal a b))
                       (member i positions-considered))
             collect (replace-nth i b nucleotide-3tuple-a))))
    (loop for single-substitution-b-3tuples in single-substitution-b-3tuples-s 
        when (funcall
              (case all-synonymous-nonsynonymous
                (all           (^ (x y) x y t))
                (   synonymous #'equal)
                (nonsynonymous (^ (x y) (not (equal x y))))
                (t             (error "Fell through case")))
              (assoc-value-2 (string (implode-list nucleotide-3tuple-a))           *codon-usage* :test #'equal)
              (assoc-value-2 (string (implode-list single-substitution-b-3tuples)) *codon-usage* :test #'equal))
        collect (list
                 nucleotide-3tuple-a
                 single-substitution-b-3tuples))))

(defun synonymous-substitutions-between-codons (nucleotide-3tuple-a nucleotide-3tuple-b &optional &key (positions-considered '(0 1 2))) 
  (substitutions-between-codons nucleotide-3tuple-a nucleotide-3tuple-b 
                                :all-synonymous-nonsynonymous 'synonymous
                                :positions-considered         positions-considered))

(defun non-synonymous-substitutions-between-codons (nucleotide-3tuple-a nucleotide-3tuple-b &optional &key (positions-considered '(0 1 2))) 
  (substitutions-between-codons nucleotide-3tuple-a nucleotide-3tuple-b 
                                :all-synonymous-nonsynonymous 'nonsynonymous
                                :positions-considered         positions-considered))

    
;;;----------------------------------------------------------------------
;;;                   alignment to one sequence
;;;----------------------------------------------------------------------

(defun sublist-position (subseq l &optional &key (num-permissable-missmatches 0))
  (let ((length-subseq (length subseq)))
    (loop for i below (inc (- (length l) (length subseq))) 
	when (<=  (hd subseq (firstn length-subseq (nthcdr i l))) num-permissable-missmatches)
	do (return i)
	finally (return nil))))

(defun align-to-subseq-and-trim (seq subseq desired-length &optional &key
								     (aligning-position-of-subseq-starting-at-1 1) ;; starting-at-1
								     (num-permissable-missmatches 0)
								     (left-fill-char '-)
								     (right-fill-char '-))
  (setq seq (explode-symbol seq))
  (let* ((aligning-position-of-subseq aligning-position-of-subseq-starting-at-1)
	 (position-of-subseq (sublist-position subseq seq :num-permissable-missmatches num-permissable-missmatches)))
    (if position-of-subseq
	(implode-list
         (firstn
          desired-length
          (append
           (loop for i below (max 0 (dec (- aligning-position-of-subseq position-of-subseq))) collect
                 left-fill-char)
           (firstn
            (- desired-length (max 0 (dec (- aligning-position-of-subseq position-of-subseq))))
            (nthcdr 
             (max 0 (inc (- position-of-subseq aligning-position-of-subseq)))
             seq))
           (loop for i below (max 0 (- (- desired-length (length seq))
                                       (dec (- aligning-position-of-subseq position-of-subseq)))) collect
                 right-fill-char))))
      nil)))

(defun align-nuc-seqs-to-pro-subseq-and-trim (nuc-seq pro-subseq desired-length &optional &key
											  (aligning-position-of-subseq-starting-at-1 1) ;; starting-at-1
											  (num-permissable-missmatches 0)
											  (left-fill-char '-)
											  (right-fill-char '-)
											  (read-frame-shifts '(0))
											  (no-aa-action :error)
											  stop-at-stop-codon)
  (let (alignment )
    (loop for read-frame-shift in read-frame-shifts 
	when (setq alignment
	       (align-to-subseq-and-trim 
		(dna-to-pro 
                 (read-from-string (substring (string nuc-seq) read-frame-shift))
                 :no-aa-action no-aa-action
                 :stop-at-stop-codon stop-at-stop-codon)
		pro-subseq
		desired-length
		:aligning-position-of-subseq-starting-at-1 aligning-position-of-subseq-starting-at-1
		:num-permissable-missmatches               num-permissable-missmatches 
		:left-fill-char                            left-fill-char
		:right-fill-char                           right-fill-char))
	do (return (implode-list alignment))
	finally (return nil))))


#|
(setq name-nuc
  (let ((accession-nuc-s (read-fas-format "~/papers/coauthor/Guus/122av/raw-data"))
	(full-names (fi-in-s "~/papers/coauthor/Guus/122av/raw-data-names")))   ;; in emacs extracted strain name from name line, replaced city spaces from names, and removed occasional space elsewhere.
    (transpose full-names (nths 1 accession-nuc-s))))
(setq name-pro-raw
  (loop for (name nuc) in name-nuc collect
	(list name 
	      (align-nuc-seqs-to-pro-subseq-and-trim
	       nuc
	       '(T A T L C L G H H)
	       329
	       :aligning-position-of-subseq-starting-at-1 10
	       :num-permissable-missmatches 1
	       :read-frame-shifts '(0 1 2)
	       :no-aa-action :x))))
(setq name-pro (collect (^ (name-pro) (not (null (nth 1 name-pro)))) name-pro-raw))


(length name-pro-raw)
2900
(length name-pro)
2744




(setq name-pro-raw
  (loop for (name nuc) in (read-fas-format "/tmp/foo.fas") collect
	(list name 
	      (align-nuc-seqs-to-pro-subseq-and-trim
	       nuc
	       '(T A T L C L G H H)
	       329
	       :aligning-position-of-subseq-starting-at-1 10
	       :num-permissable-missmatches 1
	       :read-frame-shifts '(0 1 2)
	       :no-aa-action :x))))



||#



;;;----------------------------------------------------------------------
;;;                 postprocessing raw sequences
;;;----------------------------------------------------------------------

(defun align-trim-remove-xs-dash-subseting-name-nucs-s (name-nucs-s-or-fasta &optional &key output-directory)
  (let ((name-nucs-s (if (fasta-file-p name-nucs-s-or-fasta)
                         (read-fasta-file name-nucs-s-or-fasta)
                       name-nucs-s-or-fasta)))
    (let* ((name-nucs-s-aligned-trimmed
            (filter 
             (^ (l) 
                (or (null (nth 1 l))
                    (member 'X (explode-symbol (dna-to-pro (nth 1 l) :no-aa-action :x)))
                    ))
             (loop for (name nucs) in name-nucs-s collect
                   (list name 
                         (align-to-subseq-and-trim
                          (read-from-string (string-subst #\. #\- (string-subst #\~ #\- (string nucs))))
                          (explode-symbol 'ACGGCAACGCTGTGCCTTGGGCACCAT)  ;;'(T A T L C L G H H)
                          (* 329 3)
                          ;;1 2 3 4  5  6  7  8  9  10
                          ;;1 4 7 10 13 16 19 22 25 28
                          :aligning-position-of-subseq-starting-at-1 28 ;; 10 for aas
                          :num-permissable-missmatches (* 1 3))))))
           (name-nucs-s-aligned-trimmed-non-aligners
            (loop for name in (my-set-difference (nths 0 name-nucs-s) (nths 0 name-nucs-s-aligned-trimmed)) collect
                  (list
                   name
                   (assoc-value-1 name name-nucs-s))))
           (name-nucs-s-aligned-trimmed-no-more-than-10-dashes
            (filter (^ (l) (> (length (collect (^ (e) (eql '- e)) (explode-symbol (string (nth 1 l))))) 10)) name-nucs-s-aligned-trimmed))
           (name-nucs-s-aligned-trimmed-no-dashes
            (filter (^ (l) (> (length (collect (^ (e) (eql '- e)) (explode-symbol (string (nth 1 l)))))  0)) name-nucs-s-aligned-trimmed)))
    
      (if (file-or-directory-exists-p output-directory)
          (if (not (directoryp output-directory))
              (error "Directory (~a) specified for output is not a directory, but an existing file." output-directory))
        (mkdir output-directory))

      (loop for nuc-name in (list "name-nucs-s-aligned-trimmed" 
                                  "name-nucs-s-aligned-trimmed-non-aligners"
                                  "name-nucs-s-aligned-trimmed-no-more-than-10-dashes"
                                  "name-nucs-s-aligned-trimmed-no-dashes")
          for name-nucs-s in (list name-nucs-s-aligned-trimmed
                                   name-nucs-s-aligned-trimmed-non-aligners
                                   name-nucs-s-aligned-trimmed-no-more-than-10-dashes
                                   name-nucs-s-aligned-trimmed-no-dashes) 
          do

            (let ((pro-name (string-append "name-pros-" (substring-after-char #\- (substring-after-char #\- nuc-name))))
                  (name-pros-s (name-dna-pairs-to-name-pro-pairs name-nucs-s :no-aa-action :x)))
            
              (fll name-nucs-s :filename    (string-append output-directory "/" nuc-name ".txt"))
              (fll name-pros-s :filename    (string-append output-directory "/" pro-name ".txt"))

              (write-fasta-file name-nucs-s (string-append output-directory "/" nuc-name ".fas"))
              (write-fasta-file name-pros-s (string-append output-directory "/" pro-name ".fas"))))
    
      (with-open-file (out (string-append output-directory "/" "diagnostics.txt") :direction :output)
        (format out "~10d input sequences~%" (length name-nucs-s))
        (format out "~10d duplicate names in input sequences~%" (- (length name-nucs-s) (length (remove-duplicates name-nucs-s))))
        (newline)
        (format out "~10d aligned (~d did not align)~%" (length name-nucs-s-aligned-trimmed) (- (length name-nucs-s) (length name-nucs-s-aligned-trimmed)))
        (format out "~10d aligned <=10 dashes (~d had >10 dashes)~%" (length name-nucs-s-aligned-trimmed-no-more-than-10-dashes) (- (length name-nucs-s-aligned-trimmed) (length name-nucs-s-aligned-trimmed-no-more-than-10-dashes)))
        (format out "~10d aligned no dashes (~d had dashes) ~%" (length name-nucs-s-aligned-trimmed-no-dashes) (- (length name-nucs-s-aligned-trimmed) (length name-nucs-s-aligned-trimmed-no-dashes)))))))
      

#||
(setq sorted-input
  (my-sort 
   (append
    (setq whoshare-public-name-nucs-s
      (multiple-nths 
       '(3 4)
       (csv-fi-in-s "~/mds/src/mds/mds/investigations/strain-selection-meeting/database/combined/investigations/sequence-name-matching/20070912-from-eu/sequences-2007-0911/whoshare-public/hiname-sequence.csv")))

    (setq cdc-becky-name-nucs-s
      (multiple-nths 
       '(3 4)
       (csv-fi-in-s "~/mds/src/mds/mds/investigations/strain-selection-meeting/database/combined/investigations/sequence-name-matching/20070912-from-eu/sequences-2007-0911/cdc-becky/hiname-sequence.csv")))

    (setq melb-naomi-name-nucs-s
      (multiple-nths 
       '(1 2)
       ;; not calling csv-file-in-s because 02SXXX is interpreted by lisp evaluator as something like 2 x 10^XXX  (cf 0S0 and 0D0)
       (fi-in-readline   
        "~/mds/src/mds/mds/investigations/strain-selection-meeting/database/combined/investigations/sequence-name-matching/20070912-from-eu/sequences-2007-0911/melb-naomi/hiname-sequence.csv"
        :line-process-f (^ (string) (csv-to-list (substring-after-char #\, string))))))
    )
   (^ (x y) (equal (list (nth 0 x) (nth 0 y)) 
                   (sort-strains (list (nth 0 x) (nth 0 y)))))))

(align-trim-remove-xs-dash-subseting-name-nucs-s
 sorted-input
 :output-directory "~/mds/src/mds/mds/investigations/strain-selection-meeting/database/combined/investigations/sequence-name-matching/20070912-from-eu/aligned-trimmed-no-xs-dash-subsetting")
||#



#||
h3 alignement motif
(align-to-subseq-and-trim
                          (read-from-string (string-subst #\. #\- (string-subst #\~ #\- (string nucs))))
                          (explode-symbol 'ACGGCAACGCTGTGCCTTGGGCACCAT)  ;;'(T A T L C L G H H)
                          (* 329 3)
                          ;;1 2 3 4  5  6  7  8  9  10
                          ;;1 4 7 10 13 16 19 22 25 28
                          :aligning-position-of-subseq-starting-at-1 28 ;; 10 for aas
                          :num-permissable-missmatches (* 1 3))))))

h1 alignment motif
(align-to-subseq-and-trim
                          (read-from-string (string-subst #\. #\- (string-subst #\~ #\- (string nucs))))
                          (explode-symbol 'atgaaagtaaaactactggtcctatta)  ;;'(T A T L C L G H H)
                          (* 320 3)
                          ;;1 2 3 4  5  6  7  8  9  10
                          ;;1 4 7 10 13 16 19 22 25 28
                          :aligning-position-of-subseq-starting-at-1 1 ;; 10 for aas
                          :num-permissable-missmatches (* 3 9))))))
h1n1pdm alignement motif
(align-to-subseq-and-trim
                          (read-from-string (string-subst #\. #\- (string-subst #\~ #\- (string nucs))))
                          (explode-symbol 'ACGGCAACGCTGTGCCTTGGGCACCAT)  ;;'(T A T L C L G H H)
                          (* 329 3)
                          ;;1 2 3 4  5  6  7  8  9  10
                          ;;1 4 7 10 13 16 19 22 25 28
                          :aligning-position-of-subseq-starting-at-1 28 ;; 10 for aas
                          :num-permissable-missmatches (* 1 3))))))


||#






;;;----------------------------------------------------------------------
;;;                   misc postprocessing of treefiles
;;;----------------------------------------------------------------------

(defun resize-bars-in-colin-tree (input-tree-ps-filename
                                  output-tree-ps-filename
                                  strains-to-resize 
                                  &optional &key 
                                            (grow-amount 4)
                                            (if-exists-action :error))
  
  (let ((raw-data (fi-in-readline input-tree-ps-filename)))
    (loop for strain-to-resize in strains-to-resize do
          (let* ((match      (string-append "%%(" (string strain-to-resize)))
                 (match-size (length match))
                 (commented-out-strain-position
                  (position match raw-data :test (^ (a b) (equal a (if (>= (length b) match-size) (substring b 0 (dec match-size))))))))
            (if commented-out-strain-position
                (let* ((lower-x-line-position (- commented-out-strain-position 3))
                       (upper-x-line-position (+ commented-out-strain-position 2))
                       (lower-x-line (nth lower-x-line-position raw-data))
                       (upper-x-line (nth upper-x-line-position raw-data))
                       (lower-x-line-as-list (space-delimited-string-to-list lower-x-line))
                       (upper-x-line-as-list (space-delimited-string-to-list upper-x-line))
                       (new-lower-x (- (car lower-x-line-as-list) (/ grow-amount 2.0)))
                       (new-upper-x (+ (car upper-x-line-as-list)    grow-amount))
                       (new-lower-x-line (format nil "~a ~a" new-lower-x (substring-after-char #\space lower-x-line)))
                       (new-upper-x-line (format nil "~a ~a" new-upper-x (substring-after-char #\space upper-x-line))))
                  (setf (nth lower-x-line-position raw-data) new-lower-x-line)
                  (setf (nth upper-x-line-position raw-data) new-upper-x-line))
              (print (list strain-to-resize 'not-found)))))
    (fi raw-data output-tree-ps-filename if-exists-action t :write-outer-list-elements-individually t)))
              
;; (resize-bars-in-colin-tree "/tmp/tree.ps" "/tmp/tree-updated.ps" '(ULAANBAATAR/1600/2008) :if-exists-action :supersede)

(defun resize-bars-in-colin-tree-from-fasta-difference (input-tree-ps-filename
                                                        output-tree-ps-filename
                                                        old-fasta-file
                                                        new-fasta-file
                                                        &optional &key 
                                                                  (grow-amount 4)
                                                                  (if-exists-action :error))

  (let ((strains-to-resize (set-difference (nths 0 (read-fasta-file new-fasta-file))
                                           (nths 0 (read-fasta-file old-fasta-file)))))
    (resize-bars-in-colin-tree 
     input-tree-ps-filename
     output-tree-ps-filename
     strains-to-resize 
     :grow-amount grow-amount
     :if-exists-action if-exists-action)))

#||
(resize-bars-in-colin-tree-from-fasta-difference
 "/home/dsmith/Desktop/colin/NH09-TC1-tree-with-subs.ps"
 "/home/dsmith/Desktop/colin/NH09-TC1-tree-with-subs-updated.ps"
 "/home/dsmith/Desktop/colin/SH08.fasta"
 "/home/dsmith/Desktop/colin/NH09.fasta"
 :grow-amount 3
 :if-exists-action :supersede)
||#


;;;----------------------------------------------------------------------
;;;                  Annotate fasta with dates for beast
;;;----------------------------------------------------------------------

(defun annotate-fasta-names-with-days-for-beast (name-day-s-filename 
                                                 fasta-input-filename
                                                 fasta-output-filename
                                                 &optional &key (if-exists-action :error))
  (let* ((fasta-lines (fi-in-readline fasta-input-filename))
         (name-day-s (mapcar (^ (string) (explode-string #\space string)) (fi-in-readline name-day-s-filename))))
    (fi
     (loop for fasta-line in fasta-lines collect
           (if (equal ">" (substring fasta-line 0 0))
               (loop for (name day) in name-day-s do
                     (if (string-member name fasta-line)
                         (return (string-subst-string name (format nil "~a_~d" name day) fasta-line)))
                     finally (error "~%No matching name in name-day file for fasta line ~a~%" fasta-line))
             fasta-line))
     fasta-output-filename
     if-exists-action
     t
     :write-outer-list-elements-individually t)))

#||
(annotate-fasta-names-with-days-for-beast "/tmp/name-date" "/tmp/fasta-example" "/tmp/fasta-example-output" :if-exists-action :supersede)

||#


;;;----------------------------------------------------------------------
;;;                        List number ACGTs
;;;----------------------------------------------------------------------

(defun filename-count-ACGTs (fasta-filename output-filename &optional &key (if-exists :error))
  (fll
   (count-ACGTs (read-fasta-file fasta-filename))
   :filename  output-filename
   :if-exists if-exists))

(defun count-ACGTs (name-seq-s)
  (cons '("Name" "" "Length" "" "#As" "#Cs" "#Gs" "#Ts" "#non-ACGT")
        (loop for (name seq) in name-seq-s collect
              (let ((seq-exploded (explode-symbol seq)))
                (list name 
                      ""
                      (length seq-exploded)
                      ""
                      (length (collect (^ (x) (eql 'a x)) seq-exploded))
                      (length (collect (^ (x) (eql 'c x)) seq-exploded))
                      (length (collect (^ (x) (eql 'g x)) seq-exploded))
                      (length (collect (^ (x) (eql 't x)) seq-exploded))
                      (let ((num-non-acgt (length (collect (^ (x) (not (or (eql 'a x)
                                                                           (eql 'c x)
                                                                           (eql 'g x)
                                                                           (eql 't x)))) seq-exploded))))
                        (if (zerop num-non-acgt)
                            ""
                          num-non-acgt)))))))
                
                
                
#|
From: "R.A.M. Fouchier" <r.fouchier@erasmusmc.nl>
Subject: Hacking for ron?
Date: Tue, 7 Jul 2009 13:40:19 +0200
To: "Derek Smith" <dsmith@zoo.cam.ac.uk>
Cc: linster@erasmusmc.nl

Derek,
How much work would it be for you to add some "counting" tool to the MDS
package? You can already read fasta file. What I would like from the fasta
file is a list of strain names, followed by sequence length, followed by
#As, #Cs, #Gs, #Ts (and if easy # non-ACTG) of that sequence.
This is for a task I asked Martin to do (looking at nucleotide bias of
pandemic viruses). There does not seem to be a program for this. Martin
can not do it by hand (thousands of seqs).
I think it is trivial for you, but maybe I'm wrong.
Ron
|#              



;;;----------------------------------------------------------------------
;;;                     precition utilities
;;;                       neighboring aas
;;;----------------------------------------------------------------------


#||
64 possible codons

000   100   200   300
001   101   201   301
002   102   202   302
003   103   203   303
                     
010   110   210   310
011   111   211   311
012   112   212   312
013   113   213   313
                     
020   120   220   320
021   121   221   321
022   122   222   322
023   123   223   323
                     
030   130   230   330
031   131   231   331
032   132   232   332
033   133   233   333




which are 1-neighbors

000

001
002
003

010
020
030

100
200
300



aaa

aac
aag
aat

aca
aga
ata

caa
gaa
taa
||#


(defun codon-to-aa (codon)
  (let ((aa (read-from-string 
             (assoc-value-2 
              (cond ((stringp codon) (string-upcase codon))
                    ((listp   codon) (list-to-string codon))
                    ((symbolp codon) (string codon))
                    (t (error "Type of codon unexpected, expected string, symbol, or list, got ~a" codon)))
              *codon-usage*
              :test #'equal))))
    (if (stringp codon)
        (string aa)
      aa)))

(defun one-nuc-neighboring-codons (codon)
  (let ((1st-nuc-news (my-set-difference '(a c g t) (list (nth 0 codon))))
        (2nd-nuc-news (my-set-difference '(a c g t) (list (nth 1 codon))))
        (3rd-nuc-news (my-set-difference '(a c g t) (list (nth 2 codon)))))
    (append (loop for i below 3 collect (list (nth 0 codon)        (nth 1 codon)        (nth i 3rd-nuc-news)))
            (loop for i below 3 collect (list (nth 0 codon)        (nth i 2nd-nuc-news) (nth 2 codon) ))
            (loop for i below 3 collect (list (nth i 1st-nuc-news) (nth 1 codon)        (nth 2 codon))))))
          
#||
(fll (one-nuc-subst-neighboring-aas '(a a a)))

A  A  C
A  A  G
A  A  T

A  C  A
A  G  A
A  T  A

C  A  A
G  A  A
T  A  A
||#


(defun one-nuc-neighboring-aas (codon) 
  (mapcar #'codon-to-aa (one-nuc-neighboring-codons codon)))

;;(one-nuc-neighboring-aas '(a a a))
;;(N K N T R I Q E Z)

(defun unique-nonstop-one-nuc-neighboring-aas (codon) 
  (sort-alpha (remove 'z (my-remove-duplicates (remove (codon-to-aa codon) (one-nuc-neighboring-aas codon))))))

;;(unique-nonstop-one-nuc-neighboring-aas '(a a a))
;;(N T R I Q E)



(defun unique-nonhydrophobic-nonstop-one-nuc-neighboring-aas (codon) 
  (sort-alpha (my-set-difference (unique-nonstop-one-nuc-neighboring-aas codon) *hydrophobic-aas*)))



;;;----------------------------------------------------------------------
;;;                     polymorphism at position
;;;----------------------------------------------------------------------

(defun polymorphism-at-position (name-seq-string-or-exploded-s-or-fasta-filename position &key as-proportion)
  (let ((name-explodedseq-s 
         (loop for (name seq) in (if (stringp name-seq-string-or-exploded-s-or-fasta-filename)
                                     (read-fasta-file name-seq-string-or-exploded-s-or-fasta-filename)
                                   name-seq-string-or-exploded-s-or-fasta-filename)
             collect (list name (if (listp seq) seq (explode-symbol seq))))))
    (hist-most-common-sort (nths (dec position) (nths 1 name-explodedseq-s)) :as-proportion as-proportion)))

(defun print-polymorphism-at-position (name-seq-s-or-fasta-filename position &key as-proportion)
  (fll (polymorphism-at-position name-seq-s-or-fasta-filename position :as-proportion as-proportion)))

#||
(print-polymorphism-at-position "/Users/dsmith/mds/src/mds/mds/data/all-seq/2004-02/dutch-only-pro.fas" 145)
(print-polymorphism-at-position "/Users/dsmith/mds/src/mds/mds/data/all-seq/2004-02/dutch-only-pro.fas" 145 :as-proportion t)
||#



;;;----------------------------------------------------------------------
;;;                 sequences neighbors at position
;;;----------------------------------------------------------------------

(defun nearest-neighbors-in-sequences-in-addition-particular-subst (name-seq-s position from-aa to-aa)
  (let ((name-seq-with-from-aa (loop for (name seq) in name-seq-s when (equal from-aa (nth (dec position) seq)) collect (list name seq)))
        (name-seq-with-to-aa   (loop for (name seq) in name-seq-s when (equal to-aa   (nth (dec position) seq)) collect (list name seq))))
    (sort-nth 2
              (loop for (from-name from-seq) in name-seq-with-from-aa append
                    (loop for (to-name to-seq) in name-seq-with-to-aa collect
                          (list from-name
                                to-name 
                                (dec (h-dist from-seq to-seq))
                                (if (< (h-dist from-seq to-seq) 5)
                                    (loop for i from 1
                                        for from-residue in from-seq
                                        for to-residue in to-seq
                                        when (and (not (equal from-residue to-residue))
                                                  (not (= i position)))
                                        collect (read-from-string (format nil "~a~2,'0d~a" from-residue i to-residue))))))))))


;;;----------------------------------------------------------------------
;;;                     num nucs between aas
;;;----------------------------------------------------------------------

(defun codons-for-aa (aa)
  (let ((aa (string aa)))
    (loop for (codon name codon-aa) in *codon-usage* when (equal aa codon-aa) collect (progn name codon))))
  

(defun num-nuc-mutations-between-aas (aa1 aa2)
  (let* ((aa1-codons-exploded (mapcar #'explode-symbol (codons-for-aa aa1)))
         (aa2-codons-exploded (mapcar #'explode-symbol (codons-for-aa aa2))))
    (loop for aa1-codon in aa1-codons-exploded collect
          (loop for aa2-codon in aa2-codons-exploded collect
                (hd aa1-codon aa2-codon)))))
    
(defun min-num-nuc-mutations-between-aas (aa1 aa2)
  (apply-min (apply-append (num-nuc-mutations-between-aas aa1 aa2))))

#||
(num-nuc-mutations-between-aas 'g 'd)
(min-num-nuc-mutations-between-aas 'g 'd)
||#


;;;----------------------------------------------------------------------
;;;                     Miyata distance
;;;----------------------------------------------------------------------

(defvar *aa-polarity-volume*)
(setq *aa-polarity-volume*
  '((S 9.2 32)
    (R 10.5 124)
    (L 4.9 111)
    (P 8.0 32.5)
    (T 8.6 61)
    (A 8.1 31)
    (V 5.9 84)
    (G 9.0 3)
    (I 5.2 111)
    (F 5.2 132)
    (Y 6.2 136)
    (C 5.5 55)
    (H 10.4 96)
    (E 12.3 83)  ;;10.5 85)
    (D 13.0 54)  ;;11.6 56)
    (K 11.3 119)
    (N 11.6 56)  ;;13.0 54)
    (Q 10.5 85)  ;;12.3 83)
    (M 5.7 105)
    (W 5.4 170)))

#||
    S   9.2    32
    R  10.5   124
    L   4.9   111
    P   8.0  32.5
    T   8.6    61
    A   8.1    31
    V   5.9    84
    G   9.0     3
    I   5.2   111
    F   5.2   132
    Y   6.2   136
    C   5.5    55
    H  10.4    96
    E  12.3    83
    D  13.0    54
    K  11.3   119
    N  11.6    56
    Q  10.5    85
    M   5.7   105
    W   5.4   170
||#

(defun aa-polarity (aa) (assoc-value-1 aa *aa-polarity-volume*))
(defun aa-volume   (aa) (assoc-value-2 aa *aa-polarity-volume*))

(defun miyata-distance-x (aa1 aa2)
  (sqrt (+ (square (/ (- (aa-polarity aa1) 
                         (aa-polarity aa2))
                      (sd (nths 1 *aa-polarity-volume*) :unbias t)))
           (square (/ (- (aa-volume aa1) 
                         (aa-volume aa2))
                      (sd (nths 2 *aa-polarity-volume*) :unbias t))))))


#|
(all-comparisons-full *amino-acids* #'miyata-distance)

(gnuplot-correlation
 (transpose 
  (all-comparisons *amino-acids* #'miyata-distance)
  (all-comparisons *amino-acids* #'miyata-distance-x)))


The Miyata distance matrix, kindly typed in by David.from their 1979 Dij = sqrt( sqr(deltaVolume/StdDevVolume) + sqr(deltaPolarity/StdDevPolarity))

          A      C      D      E      F      G      H      I      K      L      M      N      P      Q      R      S      T      V      W      Y 
A      0.00   1.39   2.37   2.46   3.23   0.91   2.17   2.69   2.96   2.76   2.42   1.78   0.06   1.92   2.92   0.51   0.90   1.85   4.23   3.18 
C      1.39   0.00   3.48   3.26   2.24   2.22   2.56   1.63   3.27   1.65   1.46   2.83   1.33   2.48   3.06   1.84   1.45   0.86   3.34   2.38 
D      2.37   3.48   0.00   0.90   4.27   2.37   1.72   3.98   2.05   4.10   3.69   0.65   2.40   1.47   2.34   1.87   2.05   3.40   4.88   3.95 
E      2.46   3.26   0.90   0.00   3.59   2.78   0.96   3.39   1.14   3.53   3.13   0.85   2.48   0.84   1.45   2.06   1.83   2.97   4.08   3.22 
F      3.23   2.24   4.27   3.59   0.00   4.14   2.63   0.61   2.85   0.63   0.82   3.70   3.17   2.81   2.47   3.45   2.60   1.43   1.11   0.48 
G      0.91   2.22   2.37   2.78   4.14   0.00   2.78   3.60   3.54   3.67   3.34   1.96   0.97   2.48   3.58   0.85   1.70   2.76   5.13   4.08 
H      2.17   2.56   1.72   0.96   2.63   2.78   0.00   2.45   0.79   2.59   2.19   1.29   2.15   0.32   0.82   1.94   1.32   2.11   3.16   2.27 
I      2.69   1.63   3.98   3.39   0.61   3.60   2.45   0.00   2.84   0.14   0.29   3.37   2.62   2.57   2.49   2.95   2.14   0.85   1.72   0.86 
K      2.96   3.27   2.05   1.14   2.85   3.54   0.79   2.84   0.00   2.98   2.63   1.84   2.94   1.06   0.40   2.71   2.10   2.70   3.11   2.42 
L      2.76   1.65   4.10   3.53   0.63   3.67   2.59   0.14   2.98   0.00   0.41   3.49   2.70   2.70   2.62   3.04   2.25   0.91   1.73   0.94 
M      2.42   1.46   3.69   3.13   0.82   3.34   2.19   0.29   2.63   0.41   0.00   3.08   2.36   2.30   2.29   2.67   1.86   0.62   1.89   0.93 
N      1.78   2.83   0.65   0.85   3.70   1.96   1.29   3.37   1.84   3.49   3.08   0.00   1.80   0.99   2.04   1.31   1.40   2.76   4.39   3.42 
P      0.06   1.33   2.40   2.48   3.17   0.97   2.15   2.62   2.94   2.70   2.36   1.80   0.00   1.92   2.90   0.56   0.87   1.79   4.17   3.12 
Q      1.92   2.48   1.47   0.84   2.81   2.48   0.32   2.57   1.06   2.70   2.30   0.99   1.92   0.00   1.13   1.65   1.12   2.13   3.42   2.48 
R      2.92   3.06   2.34   1.45   2.47   3.58   0.82   2.49   0.40   2.62   2.29   2.04   2.90   1.13   0.00   2.74   2.03   2.43   2.72   2.02 
S      0.51   1.84   1.87   2.06   3.45   0.85   1.94   2.95   2.71   3.04   2.67   1.31   0.56   1.65   2.74   0.00   0.89   2.15   4.38   3.33 
T      0.90   1.45   2.05   1.83   2.60   1.70   1.32   2.14   2.10   2.25   1.86   1.40   0.87   1.12   2.03   0.89   0.00   1.42   3.50   2.45 
V      1.85   0.86   3.40   2.97   1.43   2.76   2.11   0.85   2.70   0.91   0.62   2.76   1.79   2.13   2.43   2.15   1.42   0.00   2.51   1.52 
W      4.23   3.34   4.88   4.08   1.11   5.13   3.16   1.72   3.11   1.73   1.89   4.39   4.17   3.42   2.72   4.38   3.50   2.51   0.00   1.06 
Y      3.18   2.38   3.95   3.22   0.48   4.08   2.27   0.86   2.42   0.94   0.93   3.42   3.12   2.48   2.02   3.33   2.45   1.52   1.06   0.00 
|#
           

