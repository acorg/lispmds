(in-package user)

;;********************************************************************************
;;    LINK: if this file is edited, also edit utils.system and defsystem.lisp
;;********************************************************************************

(eval 
 `(defsystem :utils
      (:default-pathname ,(uw-sfnr "cl/" :assertIsDir t))
    :members
    ("macros"
     "applies-for-lispworks"
     "map"
     ;;"c-knuth-random"
     "knuth-random-single"
     "knuth-random-multiple"
     "knuth-random"
     "misc"
     "batch"
     "intersection"
     "new-intersection"
     "tk-misc"
     "gaussian-noise"
     "tk-interface"
     ;;"process-interface"  not used (i don't think)
     "g-plot"
     "gnuplot"
     "scatter"
     "statistics"
     "chi-squared"
     "bootstrap"
     "hillclimb"
     "conjugant-gradient-numerical-recipies-macros"
     "conjugant-gradient-numerical-recipies"
     "conjugant-gradient"
     "matrix-defpackage"
     "matrix"
     "pca"
     "genetics"
     "newick-tree-format")
    :rules ((:in-order-to :compile :all
			  (:requires (:load :previous))))))

;;   :rules ((:in-order-to :compile 
;;            (:requires (:load :previous))
;;            (:caused-by (:compile :all)))))

