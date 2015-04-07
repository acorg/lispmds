(in-package user)

;;;----------------------------------------------------------------------
;;;                      SETUP GLOBALS
;;;----------------------------------------------------------------------

;; THE FILES USED IN setup-globals WERE CREATED BY THE CODE BELOW

(defun setup-globals ()

  (setq all-jan-tables
    (loop for i from 1 to 48 collect
	  (let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	    (set hi-table-name
		 (read-hi-table (format nil "mds/data/all-HI/~a.txt" hi-table-name))))))

  (setq nl-seq-hi (fi-in-hi-table "~/mds/data/all-HI/merged/nl-seq-list-multiples-hi-3-values-modified.lisp"))
  (setq nl-seq-hi-no-5s (fi-in-hi-table "~/mds/data/all-HI/merged/nl-seq-list-multiples-no-5s-hi-3-values-modified.lisp"))
  
  (setq nl-dna (fi-in-hi-table "~/mds/data/all-seq/latest-version/nl-dna.lisp"))
  (setq nl-pro (fi-in-hi-table "~/mds/data/all-seq/latest-version/nl-pro.lisp"))
  (setq nl-ags (fi-in-hi-table "~/mds/data/all-seq/latest-version/nl-ags.lisp"))

  (setq nl-dna-hi-subset (fi-in-hi-table "~/mds/data/all-HI/merged/nl-dna-hi-subset.lisp"))
  (setq nl-pro-hi-subset (fi-in-hi-table "~/mds/data/all-HI/merged/nl-pro-hi-subset.lisp"))
  (setq nl-ags-hi-subset (fi-in-hi-table "~/mds/data/all-HI/merged/nl-ags-hi-subset.lisp"))

  (setq nl-seq-names (fi-in-s "~/mds/data/all-seq/latest-version/nl-seq-names"))
  (setq nl-seq-hi-names (hi-table-antigens nl-seq-hi))

  (setq nl-pro-coords2d (mapcar #'cadr (fi-in-s "~/mds/data/all-seq/latest-version/nl-seq-names-and-coords2d-pro-1264"))))


#|
;;;----------------------------------------------------------------------
;;;                  MAKING THE MERGED HI TABLES
;;;----------------------------------------------------------------------

(setq all-jan-tables
  (loop for i from 1 to 48 collect
	(let ((hi-table-name (read-from-string (format nil "TAB~d" i))))
	  (set hi-table-name
	       (read-hi-table (format nil "mds/data/all-HI/~a.txt" hi-table-name))))))

(setq all-jan-tables-merged (merge-hi-tables all-jan-tables))
(setq all-jan-tables-merged-no-5s (merge-hi-tables all-jan-tables (list-multiples-ignore-less-than-equal-tos 5)))

(setq merged-hi-names (hi-table-antigens all-jan-tables-merged))
(setq nl-seq-names (fi-in-s "~/mds/data/all-seq/latest-version/nl-seq-names"))

(setq nl-seq-hi (extract-hi-table all-jan-tables-merged (reverse (intersection nl-seq-names merged-hi-names))))
(setq nl-seq-hi-no-5s (extract-hi-table all-jan-tables-merged-no-5s (reverse (intersection nl-seq-names merged-hi-names))))

;;make some anomolous values dont-cares
(progn 
  (setq nl-seq-hi (set-hi-table-value nl-seq-hi 'bi/5930/74 'F379 'dont-care))
  (setq nl-seq-hi (set-hi-table-value nl-seq-hi 'sp/22/90 'F478/9 'dont-care))
  (setq nl-seq-hi (set-hi-table-value nl-seq-hi 'nl/33/94 'f524/5 'dont-care)))
(progn 
  (setq nl-seq-hi-no-5s (set-hi-table-value nl-seq-hi-no-5s 'bi/5930/74 'F379 'dont-care))
  (setq nl-seq-hi-no-5s (set-hi-table-value nl-seq-hi-no-5s 'sp/22/90 'F478/9 'dont-care))
  (setq nl-seq-hi-no-5s (set-hi-table-value nl-seq-hi-no-5s 'nl/33/94 'f524/5 'dont-care)))

(fi-hi-table nl-seq-hi "~/mds/data/all-HI/merged/nl-seq-list-multiples-hi-3-values-modified.lisp")
(fi-hi-table nl-seq-hi-no-5s "~/mds/data/all-HI/merged/nl-seq-list-multiples-no-5s-hi-3-values-modified.lisp")


;;------some subsets (because these table about 25mins each to extract on berlin (why so long?) --------
(progn
  (time (setq nl-dna-hi-subset (extract-hi-table nl-dna nl-seq-hi-names nl-seq-hi-names)))
  (time (setq nl-pro-hi-subset (extract-hi-table nl-pro nl-seq-hi-names nl-seq-hi-names)))
  (time (setq nl-ags-hi-subset (extract-hi-table nl-ags nl-seq-hi-names nl-seq-hi-names))))

(progn
  (fi-hi-table nl-dna-hi-subset "~/mds/data/all-HI/merged/nl-dna-hi-subset.lisp")
  (fi-hi-table nl-pro-hi-subset "~/mds/data/all-HI/merged/nl-pro-hi-subset.lisp")
  (fi-hi-table nl-ags-hi-subset "~/mds/data/all-HI/merged/nl-ags-hi-subset.lisp"))


;;;----------------------------------------------------------------------
;;;                MAKING SEQUENCE DISTANCE MATRICES
;;;----------------------------------------------------------------------

;; make sequence names be taking the names from ~/mds/data/all-seq/latest-version/ALLDNA.sequence
(setq all-sequence-names (fi-in-s "~/mds/data/all-seq/latest-version/ALL.sequence-no-V.names"))

;; make the nl-seq-names by taking the previous all-nl-antigens 
;;  (set in setup-globals in ~/mds/investigations/piecing-hi-tables/main2.lisp)
;;  (it is the antigens in the previous distance matrix (previous to the ones in latest versions))
;; and remove strains with ?'s in them, and also the strains that are duplicates
;;(setq nl-seq-names
;;   (reverse (set-difference (remove-duplicates all-nl-antigens) 
;;			    (append '(MA/O351?/95 GE/AI9509/95 MA/G125?/93 MA/G118?/93)
;;				    '(HK/25/90 HK/23/92 NL/172/96)))))
;; for the next time around, just take the strains in the sequence's, remove duplicates, look for strange names, and intersect with merged-hi-names
(setq nl-seq-names (fi-in-s "~/mds/data/all-seq/latest-version/nl-seq-names"))

(setq all-dna-raw (read-hi-table-and-convert "~/mds/data/all-seq/latest-version/ALLDNA.similarity-matrix" 1))
(setq all-dna
  (make-hi-table
   all-sequence-names
   all-sequence-names
   (hi-table-values all-dna-raw)))
(setq all-dna-full (hi-table-values-upper-triangle-to-lower-triangle all-dna))
(setq nl-dna (extract-hi-table all-dna-full nl-seq-names nl-seq-names))
(fi-hi-table nl-dna "~/mds/data/all-seq/latest-version/nl-dna.lisp")

(setq all-pro-raw (read-hi-table-and-convert "~/mds/data/all-seq/latest-version/ALLPRO.similarity-matrix" 1))
(setq all-pro
  (make-hi-table
   all-sequence-names
   all-sequence-names
   (hi-table-values all-pro-raw)))
(setq all-pro-full (hi-table-values-upper-triangle-to-lower-triangle all-pro))
(setq nl-pro (extract-hi-table all-pro-full nl-seq-names nl-seq-names))
(fi-hi-table nl-pro "~/mds/data/all-seq/latest-version/nl-pro.lisp")

(setq all-ags-raw (read-hi-table-and-convert "~/mds/data/all-seq/latest-version/ALLAGS.similarity-matrix" 1))
(setq all-ags
  (make-hi-table
   all-sequence-names
   all-sequence-names
   (hi-table-values all-ags-raw)))
(setq all-ags-full (hi-table-values-upper-triangle-to-lower-triangle all-ags))
(setq nl-ags (extract-hi-table all-ags-full nl-seq-names nl-seq-names))
(fi-hi-table nl-ags "~/mds/data/all-seq/latest-version/nl-ags.lisp")



;;;----------------------------------------------------------------------
;;;                      changed i've made 
;;;----------------------------------------------------------------------

;;not in the above and not sent to ron
;; tab 24, GE/5366/9l -> GE/5366/91  (ell to one) as table 17 has it as a 1
;; tab 2   BI/3996/95 -> BI/3996/75  (this strain appears nowhere else)

|#