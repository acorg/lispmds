(in-package user)


;;;----------------------------------------------------------------------
;;;                      submit/retrieve mds
;;;----------------------------------------------------------------------

#|
(run-external-mds        "tab1-local" :table last-hi-table :num-runs 2)
(save-external-mds       "tab1-local")
(view-saved-external-mds "tab1-local")

(run-external-mds        "tab1-external" :table last-hi-table :num-runs 2 :external-system "unm")
(retrieve-external-mds   "tab1-external" :external-system "unm")
(save-external-mds       "tab1-external")
(view-saved-external-mds "tab1-external")
|#

(defun run-external-mds (run-name
			 &key (table (eval (read-from-string run-name)))
			      (num-runs 12)
			      (starting-coordss-or-num-dimensions 2)
			      (external-system "localhost")
			      (run-directory "external-runs/mds/")
			      ;;table-file-already-saved
			      post-make-system-files-to-load
			      (post-files-expression-to-eval "(seed-random -1)"))  ;; use \ to quote any " we want in here
  
  (let* ((table-filename      (string-append run-directory run-name "-table"))
	 (output-filename     (string-append run-directory run-name "-output"))
	 (lisplog-filename    (string-append run-directory run-name "-lisplog"))
	 (lisperrlog-filename (string-append run-directory run-name "-lisperrlog"))
	 (lisp-binary-pathname (cond ((or (equal "unm" external-system)
					  (equal "cs.unm.edu" (substring-after-char #\. external-system)))
				      "/nfs/guest/dsmith/acl4.3/bin/pb_cl")
				     ((equal external-system "localhost")
				      "/usr/local/acl4.3/bin/pb_cl")
				     (t "home/dsmith/acl4.3/bin/pb_cl")))
	 (lisp-command-string
	  (format nil "nice -n 19 ~a -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' ~{-e '(load ~s)' ~} -e '~a' -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> ~a 2>> ~a &" 
		  lisp-binary-pathname
		  post-make-system-files-to-load
		  post-files-expression-to-eval
		  table-filename
		  num-runs
		  starting-coordss-or-num-dimensions
		  1000 ;; num-climbs
		  nil  ;; num-trials-per-climb
		  output-filename
		  lisplog-filename
		  lisperrlog-filename)))
    
    (fi-hi-table table table-filename)

    (if (not (equal "localhost" external-system))
	(run-shell-command (print (format nil "rsync -e ssh -avz ~a ~a:~a"
					  table-filename
					  external-system
					  run-directory))))
    
    (run-shell-command 
     (print 
      (if (equal "localhost" external-system)
	  lisp-command-string
	(format nil "ssh ~a ~s" external-system lisp-command-string))))))

(defun retrieve-external-mds (run-name
			      &key (external-system "localhost")
				   (run-directory "external-runs/mds/"))

  (let* ((run-name (string run-name))
	 (locator (string-append run-directory run-name)))

    ;; if extenal system is not localhost, then first retrieve the files to localhost
    (if (not (equal external-system "localhost"))
	(run-shell-command (print (format nil "rsync -e ssh -avz ~a:~a ~a" 
					  external-system
					  (string-append locator "-'{lisplog,lisperrlog,table,output}'")
					  run-directory))))))

(defun merge-external-mds-runs (individual-run-names
				composed-run-name
				&key (run-directory "external-runs/mds/"))
  (let ((individual-run-names-expanded (mapcar (^ (x) (string-append run-directory x)) individual-run-names))
	(composed-run-name-expanded (string-append run-directory composed-run-name)))
    (loop for suffix in '("-lisplog" "-lisperrlog" "-table" "-output") collect
	  (run-shell-command (print (format nil "cat ~{~{~a~} ~} > ~a~a" 
					    (mapcar (^ (x) (list x suffix)) individual-run-names-expanded)
					    composed-run-name-expanded suffix))))))

(defun save-external-mds (run-name
			  &key (run-directory "external-runs/mds/")
			       (save-directory "mds/data/external-saves/"))
  (let ((save-form (make-save-form 
		    :hi-table (fi-in-hi-table (string-append run-directory run-name "-table"))
		    :batch-runs (sort-nth 
				 1 
				 (fi-in-s 
				  (string-append run-directory run-name "-output"))))))
    (fi 
     save-form
     (string-append (string-append save-directory run-name "-save")))
    save-form))

(defun view-saved-external-mds (run-name
				&key (save-directory "mds/data/external-saves/"))
  (eval (fi-in (string-append save-directory run-name "-save"))))




;;;----------------------------------------------------------------------
;;;      ************ remainder is depreciated *************             
;;;----------------------------------------------------------------------




;;;----------------------------------------------------------------------
;;;               run in batch mode on a remote machine
;;;----------------------------------------------------------------------

#|
;; this works with hardcoded tmp-filename
(defun remote-mds (remote-system table num-runs dimensions num-climbs num-trials-per-climb output-filename)
  (let ((hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" "foo")))

    (fi-hi-table table hi-filename :supersede)
    (run-shell-command (print (format nil "scp ~a ~a:~a" hi-filename remote-system hi-filename)))

    (run-shell-command (print (format nil "ssh ~a ~s" remote-system (format nil "nice -n 19 /home/dsmith/acl4.3/bin/pb_cl -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' -e '(setq foo (fi-in-hi-table \"/tmp/tmp-hi-table-foo.lisp\"))' -e '(loop for i below ~d do (fi (batch-mds foo ~d ~d ~d) ~s :append))' -batch >> /tmp/lisplog 2>> /tmp/lisperrorlog &" num-runs dimensions num-climbs num-trials-per-climb output-filename))))))
|#

;; this seems to work in general
(defun remote-mds (remote-system table num-runs starting-coordss-or-num-dimensions num-climbs num-trials-per-climb output-filename)
  (let ((hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" (krandom 100000))))

    (fi-hi-table table hi-filename :supersede)
    (run-shell-command (print (format nil "scp ~a ~a:~a" hi-filename remote-system hi-filename)))

    (run-shell-command (print (format nil "ssh ~a ~s" remote-system (format nil "nice -n 19 /home/dsmith/acl4.3/bin/pb_cl -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> /tmp/lisplog 2>> /tmp/lisperrorlog &" (format nil "~a" hi-filename) num-runs starting-coordss-or-num-dimensions num-climbs num-trials-per-climb output-filename))))
    ))

;; (remote-mds "pp" foo 10 2 1000000 200 "/tmp/real")



;; post defsystem should be post make-system
;; and it would be more general if we did expressions, not just files to load
;; (then we need to go back and change the code (or copy this to those files
;; and make new code here, yeah, we want to make this other than a -alt anyhow

;; 2002-01-09: we should have a post processor, that converts the output into a save file

(defun remote-mds-alt (remote-system table num-runs output-filename
		       &key (starting-coordss-or-num-dimensions 2)
			    (num-climbs 1000)
			    num-trials-per-climb
			    post-defsystem-files-to-load)
  (let ((hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" (krandom 100000))))

    (fi-hi-table table hi-filename :supersede)
    (run-shell-command (print (format nil "scp -C ~a ~a:~a" hi-filename remote-system hi-filename)))

    (run-shell-command (print (format nil "ssh ~a ~s" remote-system (format nil "nice -n 19 ~a -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' ~{-e '(load ~s)' ~} -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> /tmp/lisplog 2>> /tmp/lisperrorlog &" 
									    (cond ((equal remote-system "unm" )
										   "/nfs/guest/dsmith/acl4.3/bin/pb_cl")
										  (t "/home/dsmith/acl4.3/bin/pb_cl"))
									    post-defsystem-files-to-load
									    (format nil "~a" hi-filename)
									    num-runs
									    starting-coordss-or-num-dimensions
									    num-climbs
									    num-trials-per-climb
									    output-filename))))))

(defun remote-mds-alt (remote-system table num-runs output-filename
		       &key (starting-coordss-or-num-dimensions 2)
			    (num-climbs 1000)
			    num-trials-per-climb
			    post-defsystem-files-to-load)
  (let ((hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" (krandom 100000))))

    (fi-hi-table table hi-filename :supersede)
    (run-shell-command (print (format nil "scp -C ~a ~a:~a" hi-filename remote-system hi-filename)))

    (run-shell-command (print (format nil "ssh ~a ~s" remote-system (format nil "nice -n 19 ~a -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' ~{-e '(load ~s)' ~} -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> /tmp/lisplog 2>> /tmp/lisperrorlog &" 
									    (cond ((equal remote-system "unm" )
										   "/nfs/guest/dsmith/acl4.3/bin/pb_cl")
										  (t "/home/dsmith/acl4.3/bin/pb_cl"))
									    post-defsystem-files-to-load
									    (format nil "~a" hi-filename)
									    num-runs
									    starting-coordss-or-num-dimensions
									    num-climbs
									    num-trials-per-climb
									    output-filename))))))

;; scp ~/mds/* rpp:mds/
;; (compile-file "mds/investigations/hi-metric/code-mods.html")
;; scp ~/mds/investigations/hi-metric/code-mods.fasl rpp:mds/code-mods-for-hi-metric.fasl
;; scp ~/mds/investigations/hi-metric/code-mods.fasl unm:mds/code-mods-for-hi-metric.fasl
;; (compile-file "mds/investigations/less-than-ten/code-mods.lisp")
;; scp ~/mds/investigations/less-than-ten/code-mods.fasl rpp:mds/code-mods-for-less-than-ten.fasl
;; scp ~/mds/investigations/less-than-ten/code-mods.fasl unm:mds/code-mods-for-less-than-ten.fasl
;; scp ~/mds/investigations/less-than-ten/code-mods-with-adjusts.fasl unm:mds/code-mods-with-adjusts-for-less-than-ten.fasl

;; (remote-mds-alt "rpp" foo 10 2 "/tmp/foo-test")



;;;----------------------------------------------------------------------
;;;           READING (AND DISPLAYING) THE RESULTS BACK
;;;----------------------------------------------------------------------

(defun show-remote-runs-in-2d-tk (filename 
				  &optional &key 
					    names
					    colors
					    preprocess-f)  ;;usually sorting
  (let* ((raw-data (fi-in-s filename)))
    (if preprocess-f
	(setq raw-data (funcall preprocess-f raw-data)))
    (loop for (coordss stress) in raw-data collect
	  (progn
	    (sleep 0.1)
	    (show-coordss
	     coordss
	     stress
	     names
	     colors)))))


;; (show-remote-runs-in-2d-tk "/tmp/real" (mapcar #'smart-long-strain-abbreviation (hi-table-antigens foo)))



(defun lowest-stress-coordss-from-batch-runs (filename)
  (nth 0
       (nth 0 
	    (my-sort 
	     (fi-in-s filename)		
	     (^ (a b)
		(< (nth 1 a) (nth 1 b)))))))



#|

the below was started, but not finished i think (found in a recover file)

;;;----------------------------------------------------------------------
;;;                 mroe general remote running
;;;----------------------------------------------------------------------

;; post defsystem should be post make-system
;; and it would be more general if we did expressions, not just files to load
;; (then we need to go back and change the code (or copy this to those files
;; and make new code here, yeah, we want to make this other than a -alt anyhow
(defun remote (remote-system table num-runs output-filename
		       &key (starting-coordss-or-num-dimensions 2)
			    (num-climbs 1000)
			    num-trials-per-climb
			    post-defsystem-files-to-load)
  (let ((hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" (krandom 100000))))

    (fi-hi-table table hi-filename :supersede)
    (run-shell-command (print (format nil "scp ~a ~a:~a" hi-filename remote-system hi-filename)))

    (run-shell-command (print (format nil "ssh ~a ~s" remote-system (format nil "nice -n 19 ~a -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' ~{-e '(load ~s)' ~} -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> /tmp/lisplog 2>> /tmp/lisperrorlog &" 
									    (cond ((equal remote-system "unm" )
										   "/nfs/research/dsmith/acl4.3/bin/pb_cl")
										  (t "/home/dsmith/acl4.3/bin/pb_cl"))
									    post-defsystem-files-to-load
									    (format nil "~a" hi-filename)
									    num-runs
									    starting-coordss-or-num-dimensions
									    num-climbs
									    num-trials-per-climb
									    output-filename))))))

;; scp ~/mds/* rpp:mds/
;; (compile-file "mds/investigations/hi-metric/code-mods.html")
;; scp ~/mds/investigations/hi-metric/code-mods.fasl rpp:mds/code-mods-for-hi-metric.fasl
;; scp ~/mds/investigations/hi-metric/code-mods.fasl unm:mds/code-mods-for-hi-metric.fasl
;; (compile-file "mds/investigations/less-than-ten/code-mods.lisp")
;; scp ~/mds/investigations/less-than-ten/code-mods.fasl rpp:mds/code-mods-for-less-than-ten.fasl
;; scp ~/mds/investigations/less-than-ten/code-mods.fasl unm:mds/code-mods-for-less-than-ten.fasl
;; scp ~/mds/investigations/less-than-ten/code-mods-with-adjusts.fasl unm:mds/code-mods-with-adjusts-for-less-than-ten.fasl

;; (remote-mds-alt "rpp" foo 10 2 "/tmp/foo-test")

|#



(defun local-remote-mds-alt (remote-system table num-runs output-filename
			     &key (starting-coordss-or-num-dimensions 2)
				  (num-climbs 1000)
				  num-trials-per-climb
				  post-defsystem-files-to-load
				  (hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" (krandom 100000)))
				  hi-file-already-saved
				  remote-system-is-local)
  (if (not (hi-file-alread-saved))
      (fi-hi-table table hi-filename :supersede))
  )


;;hacks for local (is there a betterway?)
;; make the logs unique
(defun remote-mds-alt-alt (remote-system table num-runs output-filename
		       &key (starting-coordss-or-num-dimensions 2)
			    (num-climbs 1000)
			    num-trials-per-climb
			    post-defsystem-files-to-load
			    (random-number (krandom 100000))
			    (hi-filename (format nil "/tmp/tmp-hi-table-~d.lisp" random-number))
			    hi-file-already-saved
			    remote-system-is-local)
  (if (not hi-file-already-saved)
      (fi-hi-table table hi-filename :supersede))
  (if (not remote-system-is-local)
      (run-shell-command (print (format nil "scp -C ~a ~a:~a" hi-filename remote-system hi-filename))))

  (run-shell-command (print (format nil "~a~s" (if remote-system-is-local
						    ""
						  (format nil "ssh ~a " remote-system))
				    (format nil "nice -n 19 ~a -backtrace-on-error -qq -e '(load \"~~/.clinit.cl\")' -e '(make-system :mds)' ~{-e '(load ~s)' ~} -e '(setq foo (fi-in-hi-table ~s))' -e '(loop for i below ~d do (fi (time (batch-mds foo ~d ~d ~d)) ~s :append))' -batch >> ~a 2>> ~a &" 
									  (cond ((equal remote-system "unm" )
										 "/nfs/guest/dsmith/acl4.3/bin/pb_cl")
										((equal remote-system "berlin")
										 "/usr/local/acl4.3/bin/pb_cl")
										(t "home/dsmith/acl4.3/bin/pb_cl"))
									  post-defsystem-files-to-load
									  (format nil "~a" hi-filename)
									  num-runs
									  starting-coordss-or-num-dimensions
									  num-climbs
									  num-trials-per-climb
									  output-filename
									  (format nil "/tmp/lisplog-~a" random-number)
									  (format nil "/tmp/lisperrorlog-~a" random-number))))))

;;(remote-mds-alt-alt "localhost" 'none 2 "/tmp/testrun" :hi-filename "/tmp/hi-table" :hi-file-already-saved t :remote-system-is-local t)

#|
(remote-mds-alt-alt 
 "localhost"
 last-hi-table
 2
 "/tmp/last-hi-table-runs"
 :remote-system-is-local t)

(remote-mds-alt-alt 
 "localhost"
 'none
 2
 "/tmp/testrun"
 :hi-filename "/tmp/hi-table"
 :hi-file-already-saved t
 :remote-system-is-local t)

(remote-mds-alt-alt 
 "unm" 
 last-hi-table   ;; an ag-sr-hi-table
 2 
 "tmp/tab1-test")
|#