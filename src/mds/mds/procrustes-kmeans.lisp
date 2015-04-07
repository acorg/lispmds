(in-package user)

;;;----------------------------------------------------------------------
;;;                      procrustes kmeans
;;;----------------------------------------------------------------------

(defun procrustes-kmeans-saves (master-save slave-save num-clusters &optional &key scalep seed (iterations 1))
  "Returns the master's names in each cluster, plus the strains not in any cluster because they were not in the slave save"
  (let* ((master-table (table-from-save master-save))
	 (master-coordss (coordss (starting-coordss-or-best-batch-from-save master-save)))
	 (slave-table (table-from-save slave-save))
	 (slave-coordss (coordss (starting-coordss-or-best-batch-from-save slave-save))))
    (procrustes-kmeans-tables-and-coordss master-table master-coordss slave-table slave-coordss num-clusters 
					  :scalep scalep :seed seed :iterations iterations)))

(defun procrustes-kmeans-tables-and-coordss (master-table master-coordss slave-table slave-coordss num-clusters 
					     &optional &key 
						       scalep 
						       seed
						       (iterations 1))
  "Returns the master's names in each cluster, plus the strains not in any cluster because they were not in the slave save"
  ;; this is the main entry points, coming in from saves or from the mds window, everything comes through here
  ;; so this is also where we do the iterations
  (let* ((master-identifier (format nil "~a-~d" (hi-table-name master-table) (krandom 100000)))
	 (master-names (hi-table-antigens master-table))
	 (slave-identifier (format nil "~a-~d" (hi-table-name slave-table) (krandom 100000)))
	 (slave-names (hi-table-antigens slave-table))
	 (common-names (reverse (intersection master-names slave-names)))
	 (master-names-not-in-slave (reverse (set-difference master-names common-names)))
	 (master-indices-not-in-slave (mapcar (^ (name) (position name master-names)) master-names-not-in-slave))
	 (common-master-coordss (multiple-nth (antigen-indices-in-hi-table master-table common-names) master-coordss))
	 (common-slave-coordss  (multiple-nth (antigen-indices-in-hi-table slave-table  common-names) slave-coordss )))
    (let ((multiple-runs
	   (loop for i below iterations collect
		 (multiple-value-list
		  (procrustes-kmeans-foreign-function-call
		   master-identifier
		   slave-identifier
		   common-names
		   common-master-coordss
		   common-slave-coordss
		   num-clusters
		   :scalep scalep
		   :seed (if seed seed (krandom 467739585)))))))
      (multiple-value-bind (clusters lms-error)
	  (apply #'values
		 (car (sort-nth 1 multiple-runs)))
	(setq clusters
	  (mapcar (^ (cluster) (reindex-cluster-to-master-indices cluster common-names master-names)) clusters))
	(values
	 clusters
	 master-indices-not-in-slave
	 lms-error
	 multiple-runs)))))

(defun reindex-cluster-to-master-indices (cluster-indices-into-common-names common-names master-names)
  (loop for cluster-index-into-common-names in cluster-indices-into-common-names collect
	(position (nth cluster-index-into-common-names common-names) master-names)))

(defun procrustes-kmeans-decode-output (output)
  (values 
   ;; get into a canonical form.
   ;; sort the strain indices within a cluster
   ;; and sort the clusters by size
   (my-sort
    (mapcar #'my-sort (mapcar #'cdr (butlast output)))
    (^ (cluster-x cluster-y) (> (length cluster-x) (length cluster-y))))
   (cadr (car (last output)))))

(defun procrustes-kmeans-foreign-function-call (master-identifier slave-identifier common-names common-master-coordss common-slave-coordss
						num-clusters
						&optional &key 
							  (directory (if (running-on-windows-p)
									 (uw-sfnr "mds/procrustes/tmp-files/" :assertIsDir t)
								       "/tmp/"))
							  scalep
							  seed)
  (let* ((master-filename (format nil "~a~a" directory master-identifier))
	 (slave-filename  (format nil "~a~a" directory slave-identifier))
	 (output-filename (format nil "~a~a-~a.output" directory master-identifier slave-identifier))
	 (master-num-dimensions (length (car common-master-coordss)))
	 (slave-num-dimensions  (length (car common-slave-coordss)))
	 (num-dimensions (if (= master-num-dimensions slave-num-dimensions)
			     master-num-dimensions
			   (error "for now num dimensions in master and slave must be the same"))))
    (output-names-and-coordss
     master-filename
     common-names
     common-master-coordss)
    (output-names-and-coordss
     slave-filename
     common-names
     common-slave-coordss)
    (procrustes-kmeans-by-filename
     master-filename
     slave-filename
     output-filename 
     num-dimensions
     num-clusters
     :scalep scalep
     :seed seed)
    (sleep 1)
    (procrustes-kmeans-decode-output
     (fi-in-s output-filename))))

(defun procrustes-kmeans-by-filename (master-filename slave-filename output-filename num-dimensions num-clusters 
				      &optional &key 
						scalep
						seed)
  ;; gcc -O -o Procrustes-Kmeans-for-lisp Procrustes-Kmeans-for-lisp.c
  ;; gcc -O -o Procrustes-Kmeans-for-lisp-mac-ppc Procrustes-Kmeans-for-lisp.c
  ;; gcc -O -o Procrustes-Kmeans-for-lisp-mac-intel Procrustes-Kmeans-for-lisp.c
  (if (not seed)
      (error "Need a random number seed, the design of this code is such that it should have been set in the function procrustes-kmeans-tables-and-coordss"))
  (run-shell-command
   (format nil "~a \"~a\" \"~a\" ~d ~d ~d ~d \"~a\" ~d"
	   (if (running-on-windows-p)
	       (uw-sfnr "mds/procrustes-kmeans/Debug/Procrustes-Kmeans-for-lisp.exe" :assertIsFile t)
	     (let ((processor (car (run-shell-command-wait-and-collect-output "uname -m")))
                   (os        (car (run-shell-command-wait-and-collect-output "uname -s"))))
               (cond ((equal processor "Power Macintosh")
                      (uw-sfnr "mds/procrustes-kmeans/Procrustes-Kmeans-for-lisp-mac-ppc" :assertIsFile t))
                     ((and (or (equal processor "i386") (equal processor "x86_64")) (equal os "Darwin"))
                      (uw-sfnr "mds/procrustes-kmeans/Procrustes-Kmeans-for-lisp-i386-Darwin" :assertIsFile t))
                     ((and (or (equal processor "i686") (equal processor "x86_64")) (equal os "Linux"))
                      (uw-sfnr "mds/procrustes-kmeans/Procrustes-Kmeans-for-lisp-i686-Linux" :assertIsFile t))
                     (t (error "Procrustes-Kmeans-for-lisp.c needs to be compiled for your OS/processor combination")))))
	   (if (running-on-windows-p) 
	       (make-windows-lisp-filename-passable-as-argument master-filename)
	     master-filename)
	   (if (running-on-windows-p)
	       (make-windows-lisp-filename-passable-as-argument slave-filename)
	     slave-filename)
	   num-dimensions
	   num-clusters
	   0.1
	   seed
	   (if (running-on-windows-p)
	       (make-windows-lisp-filename-passable-as-argument output-filename)
	     output-filename)
	   (bool->bit scalep)
	   )))


#|

(setq seq-t4-save (fi-in "mds/investigations/merge-hi-tables/sequenced-strains-take-4-max-in-col-oriented.save"))
(setq seq-t5-save (fi-in "mds/investigations/merge-hi-tables/seq-t5-oriented.save"))

(procrustes-kmeans-saves seq-t5-save seq-t4-save 2)
 
((0 1 2 3 4 5 6 7 8 10 11 46 48 49 50 52 53 57 59 62 63 64 65 66 68 70 72 73 74 76 78 79 80 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101
  104 105 108 110 111 125 127 129 131 141 160 161 164 167 168 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193
  194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231
  232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269
  270 271 272 273 274 275 276 277 285 289 291 293 299 302 303 305 306 307 308 309 310 311 312 313 314 315 316 318 319 320 321 322 323 324 325 326 327 328
  329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351)
 (9 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 47 51 54 55 56 58 60 61 67 69 71 75 77 81 102
  103 106 107 109 112 113 114 115 116 117 118 119 120 121 122 123 124 126 128 130 132 133 134 135 136 137 138 139 140 142 143 144 145 146 147 148 149 150
  151 152 153 154 155 156 157 158 159 162 163 165 166 169 170 278 279 280 281 282 283 284 286 287 288 290 292 294 295 296 297 298 300 301 304 317)) 
TotalLMS= 1261.049561

((3 66 67 68 70 72 73 74 76 78 80 82 83 84 85 86 87 88 89 90 91 92 93 94 95 97 98 100 101 102 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
  120 121 122 123 124 125 126 127 128 129 131 132 133 134 135 136 137 138 139 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
  160 161 162 163 164 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
  199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236
  237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274
  275 306 307 309 310 311 312 313 314 315 316 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344
  345 346 347 348 349 350 351)
 (0 1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53
  54 55 56 57 58 59 60 61 62 63 64 65 69 71 75 77 79 81 96 99 103 104 130 140 165 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293
  294 295 296 297 298 299 300 301 302 303 304 305 308 317)) 
TotalLMS= 1196.456909

i've run this about 6 times now, and 5 times out of 6 we get the 2nd lower LMS clusters

on about the 10th run we got:
  TotalLMS= 890.847107
|#


;;;----------------------------------------------------------------------
;;;                    calling from the GUI
;;;----------------------------------------------------------------------

(defun procrustes-kmeans-from-mds-window (mds-window num-clusters save-or-file-name-of-slave-save 
					  &optional &key 
						    scalep
						    seed
						    (iterations 1))
  ;; save-or-file-name-of-slave-save dispatches based on the type of the arg, string indicates filename
  (let* ((master-table (get-hi-table (get-table-window-for-mds-window mds-window)))
	 (master-coordss (coordss (get-mds-coordss mds-window)))
	 (slave-save (if (stringp save-or-file-name-of-slave-save) 
			 (fi-in-any-save-format save-or-file-name-of-slave-save)
		       save-or-file-name-of-slave-save))
	 (slave-table (extract-table-from-save slave-save))
	 (slave-coordss (coordss (starting-coordss-or-best-batch-from-save slave-save))))
    (procrustes-kmeans-tables-and-coordss 
     master-table
     master-coordss
     slave-table
     slave-coordss
     num-clusters
     :scalep scalep
     :seed seed
     :iterations iterations)))
      

;;;----------------------------------------------------------------------
;;;            doing things in the GUI with the results
;;;----------------------------------------------------------------------

(defun mds-window-color-code-based-on-clusters (mds-window clusters cluster-colors &optional &key title (color-if-not-in-any-cluster "{}"))
  (visualize-mds-coordss 
   mds-window
   'update
   (get-mds-coordss mds-window)
   (get-coords-names-working-copy (get-table-window-for-mds-window mds-window))
   (loop for i below (hi-table-length (get-hi-table (get-table-window-for-mds-window mds-window))) collect
	 (let ((cluster-number (position i clusters :test #'member)))
	   (if cluster-number
	       (nth
		cluster-number
		cluster-colors)
	     color-if-not-in-any-cluster)))
   nil  ;; leave stress it as it is
   :title title))

(defun procrustes-kmeans-from-mds-window-color-code-clades (mds-window 
							    &optional &key 
								      num-clusters
								      save-or-file-name-of-slave-save 
								      scalep
								      seed
								      (iterations 1)
								      kmeans-data)  ;; if we supply this, then no need for other args
  (if (and kmeans-data
	   (or num-clusters save-or-file-name-of-slave-save))
      (error "Supply either the kmeans-data or the num-clusters and save-or-file-name-of-save, but not both"))
  (multiple-value-bind (clusters master-indices-not-in-slave lms-error)
      ;; check here if procrustes-kmeans has run already for this window, if it has, then use it
      ;; no -- don't want to use it accidently, ok for drawing the arrows below, but not here also
      (if kmeans-data
	  (apply #'values kmeans-data)
	(procrustes-kmeans-from-mds-window mds-window num-clusters save-or-file-name-of-slave-save :scalep scalep :seed seed :iterations iterations))
    ;; color code the clades (do blue green red, then onto random colors)
    (mds-window-color-code-based-on-clusters
     mds-window
     clusters
     (primary-then-random-tk-colors (length clusters))    ;; note we derive the num clusters here in case it is not passed in
     :title (format nil "KmeansLMS: ~d" (2dp lms-error)))
    (set-procrustes-kmeans mds-window (list clusters master-indices-not-in-slave lms-error))
    (tk-put mds-window "set kmeansHasRunAlready 1")
    (values
     clusters
     master-indices-not-in-slave
     lms-error)))

(defun procrustes-kmeans-from-mds-window-procrustes-clades (mds-window save-or-file-name-of-slave-save  
							    &optional &key 
								      num-clusters 
								      scalep
								      seed
								      (iterations 1)
								      kmeans-data)
  ;; note, we have to supply the slave-save so we can do the cluster procrustes with it
  (if (and kmeans-data
	   num-clusters)
      (error "Supply either the kmeans-data or num-clusters but not both"))
  (let ((stored-kmeans-data (get-procrustes-kmeans mds-window)))
    (if (and (not kmeans-data) 
	     stored-kmeans-data)
	(setq kmeans-data stored-kmeans-data))
    (if (and num-clusters
	     (not (= num-clusters (length (nth 0 kmeans-data)))))
	(setq kmeans-data nil)))

  (let* ((slave-save (if (stringp save-or-file-name-of-slave-save) 
			 (fi-in-any-save-format save-or-file-name-of-slave-save)
		       save-or-file-name-of-slave-save))
	 (master-antigens (hi-table-antigens (get-hi-table (get-table-window-for-mds-window mds-window)))))
    (multiple-value-bind (clusters master-indices-not-in-slave lms-error)
	(if kmeans-data
	    ;; we already ran the grouping, use it
	    ;; (if we want another, then run again and we will get the new, or go to a new window)
	    ;; this is so we can put the procrustes arrows on a map that has already been clade color coded
	    ;; we might have the kmeans data because we passed it in, 
	    ;; or bacause it was set in the window by doing the color coding above (set as the default if we do not pass it in)
	    (apply #'values kmeans-data)
	  (procrustes-kmeans-from-mds-window mds-window num-clusters slave-save :scalep scalep :seed seed :iterations iterations))
      (loop for cluster in clusters 
	  for cluster-color in (primary-then-random-tk-colors (length clusters)) do  ;; note we derive the num clusters here in case it is not passed in
	    (if cluster  ;; testing for empty cluster, might be a more fundamental place to test for this in the procrustes.lisp
		(map-save-on-to-mds-window
		 mds-window
		 (subset-save-form
		  slave-save
		  (multiple-nth cluster master-antigens))
		 :scalep scalep   ;; because the clustering was done with scaling (as we don't have non-scaling clustering in kmeans yet)
		 :arrow-color cluster-color)))
      (values
       clusters
       master-indices-not-in-slave
       lms-error))))



;;;----------------------------------------------------------------------
;;;              orient slave to master with procrustes
;;;----------------------------------------------------------------------

(defun master-and-map-windows-from-save (save)
  (let* ((master-window (eval (blank-save save)))
	 (map-window (inc master-window)))
    (print "0.5s delay in master-and-map-windows-from-save to see if it prevents the hangup")
    (sleep 0.5)  ;; to test to see if stops the hangup
    (mds-hi-table master-window 
		  'metric-mds-global-norm-conjugant-gradient
		  'square
		  nil
		  :existing-mds-window nil
		  :num-trials 0
		  :num-climbs 0)
    (sleep 1)
    (values 
     master-window
     map-window)))

(defun write-and-close-map-window (map-window image-filename &optional &key master-window-for-closing-when-done)
  (png-from-canvas 
   map-window
   image-filename)  ;; .png is appended
  (sleep 1)
  (tk-close master-window-for-closing-when-done)
  (tk-close map-window)
  (sleep 1))

(defun write-ps-pdf-and-gif-and-close-map-window (map-window image-filename-without-suffix &optional &key master-window-for-closing-when-done)
  (ps-pdf-and-gif-from-canvas 
   map-window
   (format nil "~a.ps" image-filename-without-suffix)
   (format nil "~a.pdf" image-filename-without-suffix)
   (format nil "~a.gif" image-filename-without-suffix))
  (sleep 1)
  (tk-close master-window-for-closing-when-done)
  (tk-close map-window)
  (sleep 1))

(defun write-ps-pdf-and-png-and-close-map-window (map-window image-filename-without-suffix &optional &key master-window-for-closing-when-done)
  (ps-pdf-and-png-from-canvas 
   map-window
   (format nil "~a.ps" image-filename-without-suffix)
   (format nil "~a.pdf" image-filename-without-suffix)
   (format nil "~a.png" image-filename-without-suffix))
  (sleep 1)
  (tk-close master-window-for-closing-when-done)
  (tk-close map-window)
  (sleep 1))



(defun orient-slave-save-onto-master-save (slave-save master-save &optional &key slave-indices scalep name-subset)
  (if (null (starting-coordss-from-save master-save))
      (progn
        (format nil "~%Warning: master-save had no coords, returning slave-save un-oriented~%")
        slave-save)
    (let* ((slave-reoriented-to-master
            (transform-slave-save-best-coords-by-procrustes-to-master-save-best-coordss-set-as-starting-coordss-in-new-save
             slave-save
             master-save
             :scalep scalep
             :slave-indices slave-indices
             :name-subset name-subset)))
      (if (equal slave-reoriented-to-master slave-save) ;; no points in common between slave and master
          slave-save
        ;;reoriented slave to master by including plot spec
        (set-save-keyword-entry-from-other-save
         master-save
         slave-reoriented-to-master
         :canvas-coord-transformations
         :not-found-in-source-action :return-nil
         :not-found-in-destination-action :add)))))

(defun reorient-mds-window-to-save (mds-window master-save &optional &key scalep slave-indices name-subset)
  ;; recenter after rotate (so no need to translate, and for now no scale?)
  ;; print num matches, rms, av, sd (and do on a normal window too)
  (let* ((table-window (get-table-window-for-mds-window mds-window))
	 (reoriented-mds-window-save (orient-slave-save-onto-master-save 
				      (make-save-form
				       :hi-table (get-hi-table table-window)
				       :starting-coordss (get-mds-coordss mds-window))
				      (if (stringp master-save)
					  (fi-in master-save)
					master-save)
				      :slave-indices slave-indices
				      :scalep scalep
				      :name-subset name-subset)))
    (set-canvas-basis-vectors-from-canvas-coord-transforamtions
     mds-window
     (canvas-coord-transformations-from-save
      reoriented-mds-window-save))
    (enable-auto-scale-and-translate-but-not-rotate mds-window)
    (set-mds-coordss mds-window (starting-coords-from-save reoriented-mds-window-save))
    (visualize-mds-coordss 
     mds-window 
     'update 
     (get-mds-coordss mds-window) 
     (get-coords-names-working-copy table-window)
     (get-coords-colors table-window)
     nil)))

(defun orient-slave-save-onto-master-save-and-write-image (slave-save master-save image-filename 
							   &optional &key
								     map-window-hook
								     slave-indices
								     scalep
								     reoriented-save-filename)
  (let ((reoriented-slave-save (orient-slave-save-onto-master-save slave-save master-save :slave-indices slave-indices :scalep scalep)))
    (if reoriented-save-filename
	(write-save-form reoriented-slave-save reoriented-save-filename :if-exists :supersede))
    (multiple-value-bind (master-window map-window)
	(master-and-map-windows-from-save reoriented-slave-save)
      (if map-window-hook  ;; for doing other things to the window
	  (funcall map-window-hook map-window))
      (write-and-close-map-window map-window image-filename :master-window-for-closing-when-done master-window))))

(defun orient-slave-save-onto-master-save-and-write-ps-pdf-and-gif (slave-save master-save image-filename-without-suffix
								    &optional &key
									      map-window-hook
									      slave-indices
									      scalep
									      reoriented-save-filename
									      name-subset)
  (if *ugly-hack-to-write-to-file-instead-of-wish*
      (setq *unix-wish-location*
	(open (format nil "~a.tcl" image-filename-without-suffix) :direction :output)))
  (if *ugly-hack-to-convert-tcl-to-ps*
      (let ((tcl-filename (format nil "~a.tcl" image-filename-without-suffix))
	    (ps-filename  (format nil "~a.ps"  image-filename-without-suffix))
	    (pdf-filename (format nil "~a.pdf" image-filename-without-suffix))
	    (gif-filename (format nil "~a.gif" image-filename-without-suffix)))
	(run-shell-command (format nil "mds/investigations/strain-selection-meeting/database/scripts/wish-expect/wish-expect.pl ~a" tcl-filename) :wait t)
    	(tk-image-convert-ps-to-pdf ps-filename pdf-filename)
	(tk-image-convert-ps-to-gif ps-filename gif-filename))
    (let ((reoriented-slave-save (orient-slave-save-onto-master-save slave-save master-save
								     :slave-indices slave-indices
								     :scalep scalep
								     :name-subset name-subset)))
      (if reoriented-save-filename
	  (write-save-form reoriented-slave-save reoriented-save-filename :if-exists :supersede))
      (multiple-value-bind (master-window map-window)
	  (master-and-map-windows-from-save reoriented-slave-save)
	(if map-window-hook  ;; for doing other things to the window
	    (funcall map-window-hook map-window))
	(write-ps-pdf-and-gif-and-close-map-window
	 map-window
	 image-filename-without-suffix
	 :master-window-for-closing-when-done master-window)))))

(defun run-hook-in-map-window-and-write-ps-pdf-and-gif (save image-filename-without-suffix
							&optional &key
								  map-window-hook)
  (if *ugly-hack-to-write-to-file-instead-of-wish*
      (setq *unix-wish-location*
	(open (format nil "~a.tcl" image-filename-without-suffix) :direction :output)))
  (if *ugly-hack-to-convert-tcl-to-ps*
      (let ((tcl-filename (format nil "~a.tcl" image-filename-without-suffix))
	    (ps-filename  (format nil "~a.ps"  image-filename-without-suffix))
	    (pdf-filename (format nil "~a.pdf" image-filename-without-suffix))
	    (gif-filename (format nil "~a.gif" image-filename-without-suffix)))
	(run-shell-command (format nil "mds/investigations/strain-selection-meeting/database/scripts/wish-expect/wish-expect.pl ~a" tcl-filename) :wait t)
    	(tk-image-convert-ps-to-pdf ps-filename pdf-filename)
	(tk-image-convert-ps-to-gif ps-filename gif-filename))
    (multiple-value-bind (table-window map-window)
	(master-and-map-windows-from-save save)
      (if map-window-hook  ;; for doing other things to the window
	  (funcall map-window-hook map-window))
      (write-ps-pdf-and-gif-and-close-map-window
       map-window
       image-filename-without-suffix
       :master-window-for-closing-when-done table-window))))

(defun save-pc-save-and-write-ps-pdf-and-gif (save save-to-pc image-filename-without-suffix
					      &optional &key
							scalep
							name-subset
							(arrow-color "#bbbbbb"))
  (run-hook-in-map-window-and-write-ps-pdf-and-gif
   save
   image-filename-without-suffix
   :map-window-hook (^ (mds-window) 
		       (map-save-on-to-mds-window 
			mds-window
			save-to-pc 
			:scalep scalep
			:name-subset name-subset
			:arrow-color arrow-color))))

(defun save-pc-saves-and-write-ps-pdf-and-gif (save saves-to-pc image-filename-without-suffix
					       &optional &key
							 scalep
							 name-subset
							 (arrow-colors (loop for save in saves-to-pc collect (progn save "#bbbbbb"))))
  (run-hook-in-map-window-and-write-ps-pdf-and-gif
   save
   image-filename-without-suffix
   :map-window-hook (^ (mds-window)
		       (loop for save-to-pc in saves-to-pc
			   for arrow-color in arrow-colors do
			     (map-save-on-to-mds-window 
			      mds-window
			      save-to-pc 
			      :scalep scalep
			      :name-subset name-subset
			      :arrow-color arrow-color)))))

(defun orient-slave-save-onto-master-save-using-primary-cluster-and-write-image (slave-save master-save image-filename num-clusters
										 &optional &key
											   scalep
											   (iterations 1)
											   (color-code-clades t)
											   (procrustes-arrows-clades t)
											   reoriented-save-filename
											   kmeans-save-filename)
  (multiple-value-bind (clusters slave-indices-not-in-slave lms-error)
      (procrustes-kmeans-saves slave-save master-save num-clusters :scalep scalep :iterations iterations)
    (if kmeans-save-filename
	(fi (list clusters slave-indices-not-in-slave lms-error) kmeans-save-filename))
    (orient-slave-save-onto-master-save-and-write-image
     slave-save
     master-save
     image-filename
     :reoriented-save-filename reoriented-save-filename
     :scalep scalep
     :map-window-hook
     ;; color code and pc arrows the clusters
     (^ (mds-window)
	(if color-code-clades
	    (procrustes-kmeans-from-mds-window-color-code-clades mds-window :kmeans-data (list clusters slave-indices-not-in-slave lms-error)))
	(if procrustes-arrows-clades
	    (procrustes-kmeans-from-mds-window-procrustes-clades mds-window master-save :kmeans-data (list clusters slave-indices-not-in-slave lms-error))))
     :slave-indices (nth 0 clusters))))  ;; orient using the primary cluster
    


#|
(setq seq-t4-save (fi-in "mds/investigations/merge-hi-tables/seq-t4.save"))
(setq seq-t5-save (fi-in "mds/investigations/merge-hi-tables/seq-t5.save"))
(setq seq-t5-as-master-save (fi-in "mds/investigations/merge-hi-tables/seq-t5-as-master-2.save"))

(orient-slave-save-onto-master-save-and-write-image                       seq-t4-save seq-t5-save "/tmp/t45-1-clade")
(orient-slave-save-onto-master-save-using-primary-cluster-and-write-image seq-t4-save seq-t5-save "/tmp/t45-3-clades" 3)
|#


#|
i've had problems with orienting to seq-t6.  here we investigate, and see that there is something wrong with seq-t6!
so now i've made seq-t6-for-orienting.save, which is just seq-t6, and i moved it around and resaved it.

/tmp/70s-hi-master.save

(setq 70s-hi-master-save (fi-in "/tmp/70s-hi-master.save"))

(make-master-mds-window (hi-table-titer-bootstrap (table-from-save 70s-hi-master-save)))

/tmp/70s-hi-boostrap.save

(setq 70s-hi-bootstrap-save (fi-in "/tmp/70s-hi-boostrap.save"))

(eval (orient-slave-save-onto-master-save 70s-hi-bootstrap-save 70s-hi-master-save))

yes, it worked!

----------------------------------------------------------


(setq seq-t6-save (fi-in "mds/investigations/merge-hi-tables/seq-t6.save"))

(make-master-mds-window (hi-table-titer-bootstrap (table-from-save seq-t6-save)) :show-hi-table nil)

(setq seq-t6-bootstrap-save (fi-in "/tmp/seq-t6-bootstrap.save"))

(eval (blank-save (orient-slave-save-onto-master-save seq-t6-bootstrap-save seq-t6-save)))

;; not ok!


(eval (blank-save (orient-slave-save-onto-master-save seq-t6-bootstrap-save (fi-in "mds/investigations/merge-hi-tables/t7-early.save"))))
ok

(eval (blank-save (orient-slave-save-onto-master-save seq-t6-bootstrap-save (fi-in "/tmp/seq-t6-alt.save"))))
ok!


then something is strange about seq-t6!


;; ---------- test t6-alt on another map -----------------

(setq t6-bootstrap-iteration-0 (fi-in (format nil "mds/investigations/shape/bootstrap-titers-t6-~d.save" 0)))

(eval (blank-save (orient-slave-save-onto-master-save t6-bootstrap-iteration-0 seq-t6-save)))  ;; not ok
(eval (blank-save (orient-slave-save-onto-master-save t6-bootstrap-iteration-0 (fi-in "/tmp/seq-t6-alt.save"))))  ;; yes works

ok, then there is something with seq-t6!

mv /tmp/seq-t6-alt.save mds/investigations/merge-hi-tables/seq-t6-for-orienting.save

|#


(defun procrustes-kmeans-saves (master-save slave-save num-clusters &optional &key scalep seed (iterations 1))
  "Returns the master's names in each cluster, plus the strains not in any cluster because they were not in the slave save"
  (let* ((master-table (table-from-save master-save))
	 (master-coordss (coordss (starting-coordss-or-best-batch-from-save master-save)))
	 (slave-table (table-from-save slave-save))
	 (slave-coordss (coordss (starting-coordss-or-best-batch-from-save slave-save))))
    (procrustes-kmeans-tables-and-coordss master-table master-coordss slave-table slave-coordss num-clusters 
					  :scalep scalep :seed seed :iterations iterations)))

(defun find-clusters-and-procrustes-from-saves (master-save slave-save num-clusters &optional &key scalep seed (iterations 1))
  (let ((master-antigens (hi-table-antigens (table-from-save master-save))))
    (multiple-value-bind (clusters master-indices-not-in-slave lms-error)
	(procrustes-kmeans-saves master-save slave-save num-clusters :scalep scalep :seed seed :iterations iterations)
      (let ((clusters-procrustes-output
	     (loop for cluster in clusters collect
		   (if cluster  ;; testing for empty cluster, might be a more fundamental place to test for this in the procrustes.lisp
		       (multiple-value-list
			(procrustes-slave-onto-master-saves 
			 (subset-save-form
			  slave-save
			  (multiple-nth cluster master-antigens))
			 master-save
			 :scalep scalep))
		     nil))))
	(values
	 (nths 7 clusters-procrustes-output)
	 clusters-procrustes-output
	 lms-error
	 master-indices-not-in-slave
	 )))))

(defun cluster-procrustes-among-runs (save num-clusters &optional &key (num-runs (length (batch-runs-from-save save)))
						     scalep seed (iterations 1))
  (loop for i below num-runs collect
	(loop for j from (inc i) below num-runs collect
	      (2dps (find-clusters-and-procrustes-from-saves
		     (set-nth-best-batch-as-starting-coordss-in-save j save)
		     (set-nth-best-batch-as-starting-coordss-in-save i save)
		     num-clusters
		     :scalep scalep
		     :seed seed
		     :iterations iterations)))))
