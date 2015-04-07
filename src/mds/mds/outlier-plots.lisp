(in-package :user)

#||
(outlier-plot '(((0.1 0.2) (0.2 0.2) (0.3 0.2) (0.4 0.2) (0.5 0.3)  (0.6 0.3)  (0.7 0.3)  (0.8 0.3))
                ((0.15 0.2) (0.25 0.2) (0.35 0.2) (0.45 0.2) (2.9 0.5) (3.0 0.4) (3.1 0.8) (-0.3 0.7) (-4.0 0.2))
                ((0.11 0.2) (0.12 0.2) (0.13 0.2) (0.14 0.2)))
              :lines '(("one" 0.25)) :y-range 1.0 :x-length 10 
              :filename "plotOne" :directory "/Users/robfarrow" :resize 0.5 :fontsize 8)


(outlier-plot '(((0.1 0.2) (0.2 0.2) (0.3 0.2) (0.4 0.2) (0.5 0.3)  (0.6 0.3)  (0.7 0.3)  (0.8 0.3))
                ((0.15 0.2) (0.25 0.2) (0.35 0.2) (0.45 0.2) (2.9 0.5) (3.0 0.4) (3.1 0.8) (-0.3 0.7) (-4.0 0.2))
                ((0.11 0.2) (0.12 0.2) (0.13 0.2) (0.14 0.2)))
              :lines '(("one" 0.25)) :y-range 1.0 :x-length 10)
||#

(defun outlier-plot (input-lists 
                     &optional &key 
                               (line-colors '(2 3 4 5 6 7 8))
                               title 
                               lines 
                               (directory "") 
                               filename        ;;no filetype
                               y-range         ;;defaults to largest or smallest value (then no outliers!)
                               x-length       ;;defaults to longest input-list
                               (fontsize 10)
                               (resize 1.0)
                               (pointtype 2))
  "Plot multiple lists of input points.

If filename (without filetype) then a PNG is saved.
fontsize, resize: are for PNG"
  (assert (not (null line-colors))) 
  (let* ((color-len (length line-colors))
         (at-color 0) 
         (max-x (+ 0.5 (or  x-length (apply #'max (mapcar #'length input-lists)))))
         (range-y (or y-range 
                      (max (loop for pairs in input-lists
                               maximize (loop for (value) in pairs
                                            maximize value))
                           (abs (loop for pairs in input-lists
                                    minimize (loop for (value) in pairs
                                                 minimize value))))))
         (margin (if y-range (+ (/ range-y 3) range-y) range-y))
         (margin-offset (- margin (/ margin 10)))
         (y-offset (/ range-y 9))
         hi-liers lo-liers)
    (flet ((next-color () 
             (prog1 
                 (nth (mod at-color color-len) line-colors)
               (incf at-color))) )
      (gnuplot 
       `((0.5 0) (,(+ max-x 0.5) 0))
       :y-min (- margin)
       :y-max margin
       :x-min 0.5
       :x-max max-x
       :element-linetype 0
       :element-linewidth 0.5
       :label (if title (list (string title) 0.6 (/ range-y 20)))
       )

      (loop for pairs in input-lists
          for x-offset from 0.0 by 0.1
          do (let ((color (next-color))
                   (points (loop for (value err) in pairs
                               for i from 1
                               collect (cond ((> value range-y) 
                                              (push (list i value) hi-liers)
                                              (list (+ i x-offset) (+ range-y y-offset) err))
                                             ((< value (- range-y)) 
                                              (push (list i value) lo-liers)
                                              (list (+ i x-offset) (- (+ range-y y-offset)) err))
                                             (t (list (+ i x-offset) value err))
                                             ))) )
               (gnuplot points
                        :element-pointtype pointtype
                        :element-style 'yerr
                        :bar 'small 
                        :element-linetype color
                        :refresh nil)
               (gnuplot points
                        :element-linetype color
                        :refresh nil)
               ))
      
      (if lines 
          (loop for (name value) in lines
              do (if (and value (<= value range-y) (>= value (- range-y)))
                     (gnuplot 
                      `((0.5 ,value) (,max-x ,value))
                      :element-linetype (next-color)
                      :element-linewidth 2
                      :label (list name 0.6 value)
                      :refresh nil)
                   (let ((pos (- range-y 0.3)) )
                     (if (< value range-y) 
                         (gnuplot '((0 0)) :label (list (format nil "[~d]" value) 0.6 pos) :refresh nil)
                       (gnuplot '((0 0)) :label (list (format nil "[~d]" value) 0.6 (- pos)  :refresh nil))
                       ))
                   )))
      
      (when y-range
        (let ((label (format nil "HI: ~{(~d)~}" (nths 1 (sort-car hi-liers))))
              (color (next-color)) )
          ;;HI Labels
          (gnuplot 
           `((0.5 ,range-y) (,max-x ,range-y))
           :element-linetype color
           :label (list label 0.6 margin-offset)
           :refresh nil)
          
          ;;LO labels
          (setq label (format nil "LO: ~{(~d)~}" (nths 1 (sort-car lo-liers))))
          (gnuplot 
           `((0.5 ,(- range-y)) (,max-x ,(- range-y)))
           :element-linetype color
           :label (list label 0.6 (- margin-offset))
           :refresh nil)))
      
      (when filename
        (unless (file-or-directory-exists-p directory)
          (make-directory directory))
        (sleep 0.5)
        (gnuplot-png fontsize (format nil "~a/~a.ps" directory filename) resize)
        (sleep 0.5)
        (gnuplot-exit)
        (format nil "<img src=\"~a/~a.png\">" directory filename))
      )))