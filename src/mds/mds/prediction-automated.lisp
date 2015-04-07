(in-package user)


(defun map-resolution-determination-by-computational-titer-prediction (save-or-save-filename
                                                                       directory
                                                                       &optional &key
                                                                                 (num-runs-per-optimization 10)
                                                                                 (relax-original-save-after-setting-dont-care-titers t)
                                                                                 (num-random-replicates 25)
                                                                                 (dimensions-to-test '(1 2 3 4 5))
                                                                                 (proportions-to-dont-care '(0.1 0.2 0.3 0.4 0.5))
                                                                                 (random-seed 467739585)
                                                                                 (generate-saves-only nil)
                                                                                 (analyze-saves-only nil)
                                                                                 (hardcopy-fontsize 22)
                                                                                 (plot-size 1.5)
                                                                                 (experiment-title "")
                                                                                 (pop-up-web-page t))
  
  (seed-random random-seed)
  
  (if (file-or-directory-exists-p directory)
      (if (not (directoryp directory))
	  (error "Directory (~a) specified for output is not a directory, but an existing file." directory))
    (mkdir directory))

  (let ((run-all-sections (not (or generate-saves-only
                                   analyze-saves-only))))

    ;; we could collect the saves as a return value from the below, but as these runs might take a long time,
    ;; and as we might have to restart, instead file the saves as they are generated, and read back in below
    (if (or run-all-sections generate-saves-only)
        (let* ((save (if (stringp save-or-save-filename) (fi-in save-or-save-filename) save-or-save-filename))
               (coordss-from-original-save (starting-coordss-from-save save))
               (table (table-from-save save)))

          (write-save-form
           save
           (format nil "~a/original-save.save" directory))

          (loop for proportion-to-dont-care in proportions-to-dont-care collect
                (let ((tables-dont-cared (loop for i below num-random-replicates collect
                                               (make-proportion-of-table-values-dont-cares table proportion-to-dont-care))))
                  (loop for mds-dimension in dimensions-to-test collect
                        (loop for table-dont-cared in tables-dont-cared for table-number from 0 do
                              (write-save-form
                               (set-starting-coordss-and-batch-runs-from-batch-runs-in-save
                                (make-save-form
                                 :hi-table table-dont-cared)
                                (sort-batch-runs
                                 (append
                                  (progn
                                    (print (list (time-and-date) proportion-to-dont-care mds-dimension table-number 'relaxation-of-original-save))
                                    (if relax-original-save-after-setting-dont-care-titers
                                        (list
                                         (batch-mds-keyword-args
                                          table-dont-cared
                                          :starting-coordss 
                                          (reduce-coordss-to-dimension
                                           mds-dimension
                                           (add-zeros-to-increase-coordss-to-dimension 
                                            mds-dimension
                                            coordss-from-original-save))))))
                                  (loop for i below num-runs-per-optimization collect
                                        (progn
                                          (print (list (time-and-date) proportion-to-dont-care mds-dimension table-number 'iteration i))
                                          (batch-mds-keyword-args
                                           table-dont-cared
                                           :starting-coordss 5
                                           :dim-anneal-f #'lisp-dim-anneal-two-phase
                                           :dim-anneal-starting-dimension 5
                                           :dim-anneal-ending-dimension mds-dimension)))))
                                :not-found-action :add)
                               (format nil "~a/dim-~2,'0d-propdc-~d-iteration-~3,'0d.save"
                                       directory
                                       mds-dimension
                                       proportion-to-dont-care
                                       table-number))))))))
    
    (if (or run-all-sections analyze-saves-only)
        (let* ((save (if (stringp save-or-save-filename) (fi-in save-or-save-filename) save-or-save-filename))
               (master-table (table-from-save save))
               (predictions  (loop for proportion-to-dont-care in proportions-to-dont-care collect
                                   (loop for mds-dimension in dimensions-to-test collect
                                         (loop for replicate below num-random-replicates collect
                                               (multiple-value-bind (prediction-errors prediction-error-details av-abs-error sd-error correlation)
                                                   (predictions-of-master-table-from-prediction-save
                                                    master-table
                                                    (fi-in
                                                     (format nil "~a/dim-~2,'0d-propdc-~d-iteration-~3,'0d.save"
                                                             directory
                                                             mds-dimension
                                                             proportion-to-dont-care
                                                             replicate)))
                                                 prediction-errors         ;; to avoid comiler warnings
                                                 prediction-error-details  ;; to avoid comiler warnings
                                                 (dps-tree (list av-abs-error sd-error correlation) 8))))))
               (predictions-summary (loop for proportion-to-dont-care-predictions in predictions collect
                                          (loop for mds-dimension-predictions in proportion-to-dont-care-predictions collect
                                                (mapcar (^ (l) (multiple-value-list (av-sd l))) (apply-transpose mds-dimension-predictions)))))
               (plot-y-max (ceiling (apply-max (nths 0 (apply-append (apply-append predictions-summary)))))))
          
          ;; generate prediction plot for set of proportions, and for each dimension
          
          ;; first generate plots of prederror, sdprederror, and correlation, for each dimension in same plot
          ;;    - and a plot for each proportion
          (loop for proportion-to-dont-care-predictions-summary in predictions-summary 
              for proportion-to-dont-care in proportions-to-dont-care do
                (progn
                  (let ((ps-filename (format nil "~a/propdc-~d-pred-summary.ps" directory proportion-to-dont-care)))
                    (gnuplot 
                     (loop for dim in dimensions-to-test 
                         for av in (nths 0 (nths 0 proportion-to-dont-care-predictions-summary)) 
                         collect (list dim av))
                     :title (format nil "~d proportion of titers set to dont-care" proportion-to-dont-care)
                     :element-name "Av abs pred error"
                     :element-pointsize 2
                     :element-symbol t
                     :x-title "Dimensions"
                     :y-min 0
                     :y-max plot-y-max
                     ;;:x-min 0
                     ;;:x-max 5.5
                     :ratio 1
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size)
                    (sleep 2.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test 
                         for sd in (nths 0 (nths 1 proportion-to-dont-care-predictions-summary)) 
                         collect (list dim sd))
                     :element-name "Av SD pred error"
                     :element-pointsize 2
                     :element-symbol t
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for r in (nths 0 (nths 2 proportion-to-dont-care-predictions-summary))
                         collect (list dim r))
                     :element-name "Av correlation"
                     :element-pointsize 2
                     :element-symbol t
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 1)
                    (ps-to-png ps-filename "")
                    (sleep 1)
                    (gnuplot-exit)
                    (sleep 1))

                  (let* ((table-filename (format nil "~a/propdc-~d-pred-summary-with-se.txt" directory proportion-to-dont-care))
                         (ps-filename    (format nil "~a/propdc-~d-pred-summary-with-se.ps"  directory proportion-to-dont-care)))

                    (fll
                     (loop for dim in dimensions-to-test 
                         for av-ape in (nths 0 (nths 0 proportion-to-dont-care-predictions-summary)) 
                         for sd-ape in (nths 1 (nths 0 proportion-to-dont-care-predictions-summary))
                         for av-se  in (nths 0 (nths 1 proportion-to-dont-care-predictions-summary)) 
                         for sd-se  in (nths 1 (nths 1 proportion-to-dont-care-predictions-summary))
                         for av-cor in (nths 0 (nths 2 proportion-to-dont-care-predictions-summary)) 
                         for sd-cor in (nths 1 (nths 2 proportion-to-dont-care-predictions-summary))
                         collect (list dim 
                                       av-ape sd-ape
                                       av-se  sd-se
                                       av-cor sd-cor))
                     :preamble "Dim av-prediciton-error &SE     av-SE &SE           av-correlation &SE"
                     :filename table-filename)

                    (gnuplot 
                     (loop for dim in dimensions-to-test 
                         for av in (nths 0 (nths 0 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 0 proportion-to-dont-care-predictions-summary))
                         collect (list dim av sd))
                     :title (format nil "~d proportion of titers set to dont-care" proportion-to-dont-care)
                     :element-name "Av abs pred error"
                     :element-linetype 1
                     :element-pointsize 2
                     ;;:element-symbol t
                     :element-style 'yerr
                     :element-pointtype 2
                     :bar 'small
                     :x-title "Dimensions"
                     :y-min 0
                     :y-max plot-y-max
                     ;;:x-min 0.5
                     ;;:x-max 5.5
                     :ratio 1
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for av in (nths 0 (nths 0 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 0 proportion-to-dont-care-predictions-summary))
                         collect (list dim av sd))
                     :element-linetype 1
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for av in (nths 0 (nths 1 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 1 proportion-to-dont-care-predictions-summary))
                         collect (list dim av sd))
                     :element-name "Av SD pred error"
                     :element-linetype 2
                     :element-style 'yerr
                     :element-pointtype 2
                     :bar 'small
                     :element-pointsize 2
                     :element-symbol t
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for av in (nths 0 (nths 1 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 1 proportion-to-dont-care-predictions-summary))
                         collect (list dim av sd))
                     :element-linetype 2
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for av in (nths 0 (nths 2 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 2 proportion-to-dont-care-predictions-summary))
                         collect (list dim av sd))
                     :element-name "Av correlation"
                     :element-linetype 3
                     :element-style 'yerr
                     :element-pointtype 2
                     :bar 'small
                     :element-pointsize 2
                     :element-symbol t
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 0.5)
                    (gnuplot
                     (loop for dim in dimensions-to-test
                         for av in (nths 0 (nths 2 proportion-to-dont-care-predictions-summary)) 
                         for sd in (nths 1 (nths 2 proportion-to-dont-care-predictions-summary))
                         collect (list dim av))
                     :element-linetype 3
                     :hardcopy-only     t 
                     :ps-filename       ps-filename
                     :hardcopy-fontsize hardcopy-fontsize
                     :size              plot-size
                     :refresh nil)
                    (sleep 2)
                    (ps-to-png ps-filename "")
                    (sleep 1)
                    (gnuplot-exit)
                    (sleep 1))))
          
          ;; write web page
          (write-map-resolution-determination-by-computational-titer-prediction-web-page
           save-or-save-filename
           directory
           :experiment-title experiment-title
           :num-runs-per-optimization num-runs-per-optimization 
           :num-random-replicates     num-random-replicates
           :dimensions-to-test        dimensions-to-test
           :proportions-to-dont-care  proportions-to-dont-care
           :random-seed               random-seed)
          
          (let ((open-command (format nil "open ~a/index.html" directory)))
            (if pop-up-web-page (run-shell-command open-command))
            `(run-shell-command ,open-command))
          ))))
          

(defun write-map-resolution-determination-by-computational-titer-prediction-web-page (save-or-save-filename
                                                                                      directory
                                                                                      &optional &key
                                                                                                experiment-title
                                                                                                num-runs-per-optimization 
                                                                                                num-random-replicates
                                                                                                dimensions-to-test
                                                                                                proportions-to-dont-care
                                                                                                random-seed
                                                                                                (if-exists-action :error))
  (with-open-file (out (format nil "~a/index.html" directory) :direction :output :if-exists if-exists-action)
    (format out "<H1><CENTER>~a<br><FONT SIZE=-2>(Map resolution testing by titer prediction, automated diagnostics version 0.0)</FONT></CENTER></H1>" experiment-title)
    (newline out)
    (format out "<PRE>")
    (newline out)
    (format out "
<hr>
<h3>Input parameters</h3>
   Original save ~a, (and copied <A href=\"~a\">here</A>)

   num-runs-per-optimization ~a
   num-random-replicates     ~a
   dimensions-to-test        ~a
   proportions-to-dont-care  ~a
   random-seed               ~a
  
<hr>
<h3>Results</h3>
~a




~a
~a




  First row prediction, SD, and correlation plotted against dimension, for different proportions of titer predicted
  Second row, same as first row, but with error bars indicating SE (with tabular data that was used to make the plots linked below the plot)



<hr>

Directory for all individual saves and predictions: <A href=\".\">here</A> 


<hr>
"
            (if (stringp save-or-save-filename) save-or-save-filename "passed as save, not as save-filename")
            (format nil "original-save.save")
            num-runs-per-optimization
            num-random-replicates    
            dimensions-to-test       
            proportions-to-dont-care 
            random-seed              
            (format nil "~{~a~}"
                    (loop for proportion-to-dont-care in proportions-to-dont-care collect
                          (let ((png-filename (format nil "propdc-~d-pred-summary.png" proportion-to-dont-care)))
                            (format nil "<IMG src=~a alt=~a>" png-filename png-filename))))
            (format nil "~{~a~}"
                    (loop for proportion-to-dont-care in proportions-to-dont-care collect
                          (let ((png-filename (format nil "propdc-~d-pred-summary-with-se.png" proportion-to-dont-care)))
                            (format nil "<IMG src=~a alt=~a>" png-filename png-filename))))
            (format nil "~{~a~}"
                    (loop for proportion-to-dont-care in proportions-to-dont-care collect
                          (let ((tabular-results-filename (format nil "propdc-~d-pred-summary-with-se.txt" proportion-to-dont-care)))
                            (format nil "              Proportion ~d tabular results <A href=\"~a\">here</A>                                              " proportion-to-dont-care tabular-results-filename))))
            )
    (newline out)
    (format out "</PRE>")
    (newline out))
  )


#||

TODO:
  test we are getting the orignal save relaxed, as well as the batch runs
  gui


(map-resolution-determination-by-computational-titer-prediction
 "mds/investigations/rabies/prediction/fmno131.160707.save"
 "mds/investigations/rabies/prediction/auto-prediction-test-1"
 :num-runs-per-optimization 2
 :relax-original-save-after-setting-dont-care-titers t
 :num-random-replicates 2
 :dimensions-to-test '(2 5)
 :proportions-to-dont-care '(0.1 0.5)
 :experiment-title "Test")



(map-resolution-determination-by-computational-titer-prediction
 "mds/investigations/rabies/prediction/fmno131.160707.save"
 "mds/investigations/rabies/prediction/auto-prediction-test-2"
 :num-runs-per-optimization 25
 :relax-original-save-after-setting-dont-care-titers t
 :num-random-replicates 2
 :dimensions-to-test '(1 2 3 4 5)
 :proportions-to-dont-care '(0.1 0.2 0.3 0.4 0.5)
 :experiment-title "Test")

(map-resolution-determination-by-computational-titer-prediction
 "mds/investigations/rabies/prediction/fmno131.160707.save"
 "mds/investigations/rabies/prediction/auto-prediction-test-2a"
 :num-runs-per-optimization 25
 :relax-original-save-after-setting-dont-care-titers t
 :num-random-replicates 2
 :dimensions-to-test '(1 2 3 4 5)
 :proportions-to-dont-care '(0.1 0.2 0.3 0.4 0.5)
 :experiment-title "Test"
 :analyze-saves-only t)

(write-map-resolution-determination-by-computational-titer-prediction-web-page
 "mds/investigations/rabies/prediction/fmno131.160707.save"
 "mds/investigations/rabies/prediction/auto-prediction-test-2a"
 :experiment-title "Test"
 :num-random-replicates 2
 :dimensions-to-test '(1 2 3 4 5)
 :proportions-to-dont-care '(0.1 0.2 0.3 0.4 0.5)
 :if-exists-action :supersede)



||#


#||
(fll
 (dps-tree
  (setq predictions-summary
    (loop for per-dim-predictions in 1-to-5-dims-predictions collect
	  (mapcar (^ (l) (multiple-value-list (av-sd l))) (apply-transpose per-dim-predictions))))
  3))

(1.74 0.293)   (2.218 0.455)  (0.5 0.15)
(1.26 0.228)   (1.586 0.28)   (0.697 0.117)
(1.193 0.241)  (1.47 0.258)   (0.738 0.084)
(1.258 0.276)  (1.542 0.293)  (0.705 0.113)
(1.265 0.28)   (1.539 0.286)  (0.708 0.109)
||#



