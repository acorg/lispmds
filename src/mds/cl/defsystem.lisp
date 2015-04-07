(in-package excl)

;;****************************************************************************************
;;    LINK: if this file is edited, also edit utils.system and defsystem-lispworks.lisp
;;****************************************************************************************

(eval
 `(defsystem :utils
      (:default-pathname ,(user::uw-sfnr "cl/" :assertIsDir t))
    (:serial   
     "macros"
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
     "tk-socket"
     "tk-interface"
     "process-interface"
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
     "newick-tree-format"
     "geographic"
     "python")))
