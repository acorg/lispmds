(in-package excl)

;;***************************************************************************************
;;    LINK: if this file is edited, also edit mds.system and defsystem-lispworks.lisp
;;***************************************************************************************

(eval 
 `(defsystem :mds
      ;;(:default-pathname "mds/")  for the windows compatibility  (along with no ~ in .clinit.cl and cl/systems names of files to load in cl/systems
      (:default-pathname ,(user::uw-sfnr "mds/" :assertIsDir t))
    (:serial 
     "misc"
     "hi-table"
     "color"
     "tk-interface"
     "input-ui"
     "input-ui-merge"
     "mds"
     "metric-mds"
     "metric-mds-conjugant-gradient"
     "ordinal-mds"
     "ordinal-full-mds"
     "ordinal-partial-mds"
     "run-mds"
     "mds-visualization"
     "save"
     "error"
     "prediction"
     "prediction-automated"
     "procrustes"
     "procrustes-kmeans"
     "constant-force-locii"
     "titer"
     "projection"
     "ag-sr-points"
     "xgvis"
     "matlab"
     "pymol"
     "landscapes"
     "batch"
     "remote"
     "setup-eur-globals"
     "shepherd-plots"
     "panel-map-diagnostics"
     "error-array"
     "dim-anneal"                    ;; lisp version
     "alan-dim-anneal/dim-anneal"    ;; gridware lisp version
     "strain-annotations"
     "codon-information"
     "start-gui-on-windows"
     "multiple-optima-check-by-single-point-randomize"
     "table-analysis-script"
     "tables-analysis-script"
     "timeseries"
     "genetic-misc"
     "regression-analysis-script-setup"
     "regression-analysis-script"
     "strain-like"
     "clusters"
     "spatial"
     "image-maps"
     )))

;;(make-and-visualize-random-coordss 10)
;;(make-strain-selection-window hi90d)
;;(make-master-mds-window <hi-table>
;;(batch-mds <hi-table>

#|
things to fix
|#
