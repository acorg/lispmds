(in-package user)

(defun geographic-timeseries-from-acmacs-b1 (name-date-x-y-color-s image directory
                                             &optional &key
                                                       (image-scale 1)
                                                       (data-scale 1)
                                                       (date-prefix "")  
                                                       years month-starts month-ends
                                                       (dot-size 1)
                                                       (label-color "black")
                                                       (label-font-size 24))

  (run-shell-command (format nil "mkdir -p ~a" directory) :wait t)

  (loop for year in years
      for month-start in month-starts
      for month-end   in month-ends do
        (loop for month from month-start to month-end do
              (let ((filename (format nil "~a/~d-~2,'0d" directory year month))
                    (this-months-strains (loop for (name date x y color) in name-date-x-y-color-s
                                             when (and (eql (string-isolation-date-year  date) year)
                                                       (eql (string-isolation-date-month date) month))
                                             collect (list name date x y color))))

                (print (list year month))

                ;; file names and isolation dates
                (fll 
                 (sort-nth
                  3
                  (sort-nth 
                   0
                   (loop for (name date) in this-months-strains collect
                         (list name
                               (string-isolation-date-year  date)
                               (string-isolation-date-month date)
                               (string-isolation-date-day   date)))
                   #'strain-name-<))
                 :filename (format nil "~a.names" filename))

                (let* ((plot-spec (loop for (name date x y color) in this-months-strains collect
                                        (progn
                                          date ;; to stop compiler warning
                                          (list name
                                                :cs (list x y)
                                                :co color
                                                :oc color
                                                )
                                          )))
                       (tk (make-geographic-map-image
                            :background-image image
                            :plot-spec plot-spec
                            :label (format nil "~a ~a ~a" 
                                           date-prefix 
                                           year
                                           (month-number-to-name month))
                            :label-color label-color
                            :label-font-size label-font-size
                            :dot-size dot-size
                            :image-scale image-scale
                            :data-scale data-scale
                            :filename-without-suffix-for-ps-pdf-png filename)))
                  (tk-close tk))

                )))

    (run-shell-command (format nil "mkdir ~a/png"   directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/names" directory) :wait t)
    (run-shell-command (format nil "mkdir ~a/ps"    directory) :wait t)

    (run-shell-command (format nil "mv ~a/*.png   ~a/png/"   directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.names ~a/names/" directory directory) :wait t)
    (run-shell-command (format nil "mv ~a/*.ps    ~a/ps/"    directory directory) :wait t)

    (run-shell-command (format nil "pushd ~a; zip -r png   png;   popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r names names; popd" directory) :wait t)
    (run-shell-command (format nil "pushd ~a; zip -r ps    ps;    popd" directory) :wait t)
    )




(defun remove-dashes-from-string (string) (apply #'string-append (explode-string #\- string)))
  

#||
(setq name-date-x-y-color-s
  (loop for (no name date color longitude latitude x y) in
        (append
         (cdr (csv-fi-in-s "~/mds/docs/whocc/influenza/20090919/pngs/B-Victoria-CDC/antigens-cdc.csv" :leave-elements-as-strings t))
         (cdr (csv-fi-in-s "~/mds/docs/whocc/influenza/20090919/pngs/B-Victoria-CNIC/antigens-cdc.csv" :leave-elements-as-strings t))  ;; [sic]
         (cdr (csv-fi-in-s "~/mds/docs/whocc/influenza/20090919/pngs/B-Victoria-MELB/antigens-melb.csv" :leave-elements-as-strings t))
         (cdr (csv-fi-in-s "~/mds/docs/whocc/influenza/20090919/pngs/B-Victoria-NIID/antigens-niid.csv" :leave-elements-as-strings t))
         (cdr (csv-fi-in-s "~/mds/docs/whocc/influenza/20090919/pngs/B-Victoria-NIMR/antigens-nimr.csv" :leave-elements-as-strings t)))
      when (if (equal "" date)
               (progn
                 (format t "Because of no isolation date, excluding ~a~%" name)
                 nil)
             t)
      collect
        (list 
         name 
         (remove-dashes-from-string date)
         (- (read-from-string x) 246)
         (- (read-from-string y) 500)
         color)))

(geographic-timeseries-from-acmacs-b1
 name-date-x-y-color-s
 "/Users/dsmith/ts/world-map-gif-cropped.gif"  
 ;;"/Users/dsmith/ts/world-map-tmp.gif"  
 ;;"/Users/dsmith/ts/world-map-cropped.gif"
 "~/ts/ts/B-Victoria-geographic"        ;; image 2538x1317, coords in 1269x1000
 :image-scale 3  ;;(float (/ 2538 360))
 :data-scale  0.1  ;;(* 0.5 (float (/ 360 1269)))
 :date-prefix ""
 :years        '(2009)
 :month-starts '(01)
 :month-ends   '(01)  
 :dot-size 10
 :label-color "black"
 :label-font-size 24)

;; acorg:~eu/x2/ac/cc-data/trunk/data/spatial/world-map.pdf



pdf is 1701x1141 cropped at [246 90 1515 1090] (I think it's [left top right bottom]), i.e. the real size is probably 1269x1000.


use my original figure, with original scaleing, but also need to know how to scale eu's coords


(geographic-timeseries-from-acmacs-b1
 name-date-x-y-color-s
 "/Users/dsmith/ts/world-map-gif-cropped.gif"  
 ;;"/Users/dsmith/ts/world-map-tmp.gif"  
 ;;"/Users/dsmith/ts/world-map-cropped.gif"
 "~/ts/ts/B-Victoria-geographic"        ;; image 2538x1317, coords in 1269x1000
 :image-scale 3  ;;(float (/ 2538 360))
 :data-scale  0.3  ;;(* 0.5 (float (/ 360 1269)))
 :date-prefix ""
 :years        '(2009)
 :month-starts '(01)
 :month-ends   '(01)  
 :dot-size 10
 :label-color "black"
 :label-font-size 24)

||#