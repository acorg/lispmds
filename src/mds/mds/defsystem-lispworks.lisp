(in-package user)

;;******************************************************************************
;;    LINK: if this file is edited, also edit mds.system and defsystem.lisp
;;******************************************************************************

(eval 
 `(defsystem :mds
     (:default-pathname ,(uw-sfnr "mds/" :assertIsDir t))
   :members
   ("misc"
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
    "procrustes"
    "procrustes-kmeans"
    "constant-force-locii"
    "titer"
    "projection"
    "ag-sr-points"
    "xgvis"
    "matlab"
    "pymol"
    "batch"
    "remote"
    "setup-eur-globals"
    "shepherd-plots"
    "error-array"
    "dim-anneal"                    ;; lisp version
    "alan-dim-anneal/dim-anneal"    ;; c version
    "strain-annotations"
    "codon-information"
    ;;"tmp-patch"   
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
    )
   :rules ((:in-order-to :compile :all
			 (:requires (:load :previous))))))

;;(make-and-visualize-random-coordss 10)
;;(make-strain-selection-window hi90d)
;;(make-master-mds-window <hi-table>
;;(batch-mds <hi-table>

#|
things to fix
|#