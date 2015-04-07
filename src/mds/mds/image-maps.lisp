(in-package user)

(defun abbrev-date-longname-date-location (name)
  (format nil "  ~a  ~a    ~50a ~a" 
          name
          (if (and (isolation-date-year  (remove-ag-sr-from-name name))
                   (isolation-date-month (remove-ag-sr-from-name name))
                   (isolation-date-day   (remove-ag-sr-from-name name)))
              (format nil "~4d/~2,'0d/~2,'0d"
                      (isolation-date-year  (remove-ag-sr-from-name name))
                      (isolation-date-month (remove-ag-sr-from-name name))
                      (isolation-date-day   (remove-ag-sr-from-name name)))
            "          ")
          (if (string-longname-from-hiabbrev name)
              (string-longname-from-hiabbrev name)
            "")
          (let ((location-info (location-info-from-strain-name name)))
            (if location-info
                (pretty-location-info-from-location-info location-info)
              ""))))

(defun image-map-of-mds-window-from-save (save &optional &key (x-offset 0) (y-offset 0) (scale 0.96) (map-name "map") filename (if-exists :error))
  (let ((html (let* ((coordss (starting-mds-coordss-coordss-from-save save))
                     (names   (antigens-from-save save))
                     (radii   (coords-dot-sizes-from-save save)))
                (append
                 (list (format nil "<MAP name=\"~a\">" map-name))
                 (loop for name in names
                     for coords in coordss
                     for radius in radii collect
                       (let ((x (+ x-offset (* scale (nth 0 coords))))
                             (y (+ y-offset (* scale (nth 1 coords)))))
                         (format nil "  <AREA ~a onmouseover=\"return overlib('~a');\" onmouseout=\"return nd();\"/>"
                                 (if (serum-name-p name)
                                     (format nil "shape=\"rect\" coords=\"~d,~d,~d,~d\""
                                             (round (- x radius))
                                             (round (- y radius))
                                             (round (+ x radius))
                                             (round (+ y radius)))
                                   (format nil "shape=\"circle\" coords=\"~d,~d,~d\""
                                           (round x)
                                           (round y)
                                           (round (inc radius))))
                                 name
                                 ;;(abbrev-date-longname-date-location name)  ;; this to get long name into imagemap (and also put in the geographic the same way but backed off because of NO-DATE, and wondering why i'd not come across that before
                                 )))
                 (list "</MAP>")))))
    (if filename
        (fi html
            filename
            if-exists
            t
            :write-outer-list-elements-individually t))
    html))



#|
(image-map-of-mds-window-from-save save :filename "/tmp/test.html" :if-exists :supersede :x-offset -10 :y-offset -4)  ;; (for tab1)

<MAP name="map1">
  <AREA shape="rect" coords="589,379,596,385" onmouseover="return overlib('Another popup');" onmouseout="return nd();"/>
  <AREA shape="circle" coords="592,293,100"   onmouseover="return overlib('VI/1203/2004');"  onmouseout="return nd();"/>
</MAP>
|#