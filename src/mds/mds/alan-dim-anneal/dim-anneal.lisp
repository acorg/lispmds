(in-package user)

#|
gcc -O -lm -o dim-anneal dim-anneal.c

note there are numerical recipies library files pointed to in dim-anneal.c


# usagae dim-anneal inputfile, outputfile, input-num-dims output-num-dims seed

/home/dsmith/mds/alan-dim-annealing/dim-anneal $1 $1.$5.intermediate $4 $3 0.00000001 $5 1000000 10 7654321 2>> /tmp/dim-anneal.log
/home/dsmith/mds/alan-dim-annealing/dim-anneal $1.$5.intermediate $2 $4 $4 0.00000001 $5 1000000 10 7654321 2>> /tmp/dim-anneal.log
|#

;;;----------------------------------------------------------------------
;;;                      random numbers
;;;----------------------------------------------------------------------

(defvar *dim-anneal-random-number-generator*)
(setq *dim-anneal-random-number-generator* 9)

(seed-random -1 *dim-anneal-random-number-generator*)  ;; if we dont do this, then we keep on reusing the same filenames
                                                       ;; which can cause errors if we start two different lisps

;;;----------------------------------------------------------------------
;;;                      unix and window wrappers
;;;----------------------------------------------------------------------

(defun  unix-windows-universal-copy-file (from to)
  (if (running-on-windows-p)
      (system::copy-file 
	   (make-windows-lisp-filename-passable-as-argument from)
	   (make-windows-lisp-filename-passable-as-argument to))
    (run-shell-command (format nil "cp ~a ~a" from to))))

(defun dim-anneal-wrapper (input-filename output-filename start-num-dims end-num-dims)
  (run-shell-command
   (format nil "~a ~a ~a ~d ~d ~d ~d ~d ~d ~d"
	   (if (running-on-windows-p)
	       (uw-sfnr "mds/alan-dim-anneal/Debug/dim-anneal.exe")
	     (uw-sfnr "mds/alan-dim-anneal/dim-anneal"))
	   (if (running-on-windows-p) 
	       (make-windows-lisp-filename-passable-as-argument input-filename)
	     input-filename)
	   (if (running-on-windows-p)
	       (make-windows-lisp-filename-passable-as-argument output-filename)
	     output-filename)
	   end-num-dims
	   start-num-dims
	   0.00000001   ;; not used
	   0            ;; seed         ;; not used if i write starting coordss in the input-file as i am
	   1000000      ;; max iterations, just set high so we never hit it
	   10           ;; less than threshold
	   7654321      ;; not used
	   )))


;;;----------------------------------------------------------------------
;;;                running locally by lisp scripts
;;;----------------------------------------------------------------------

(defun dim-anneal-two-phase (input-filename output-filename start-dims end-dims) 
  (dim-anneal-wrapper input-filename                                (format nil "~a.intermediate" input-filename) start-dims end-dims)
  (dim-anneal-wrapper (format nil "~a.intermediate" input-filename) output-filename                               end-dims   end-dims))


(defun dim-anneal-short-steps (input-filename output-filename start-dims end-dims) 
  (unix-windows-universal-copy-file input-filename (string-append input-filename (format nil ".~d" start-dims)))
  (loop for step from start-dims downto (inc end-dims) do
	(dim-anneal-wrapper (string-append input-filename (format nil ".~d" step))
			    (string-append input-filename (format nil ".~d" (dec step)))
			    step
			    (dec step)))
  (dim-anneal-wrapper (string-append input-filename (format nil ".~d" end-dims))
		      output-filename
		      end-dims
		      end-dims))

(defun dim-anneal-long-steps (input-filename output-filename start-dims end-dims) 
  (unix-windows-universal-copy-file input-filename (string-append input-filename (format nil ".~d" start-dims)))
  (loop for step from start-dims downto (inc end-dims) do
	(dim-anneal-wrapper (string-append input-filename (format nil ".~d" step))
			    (string-append input-filename (format nil ".~d.pluspenaltydim" (dec step)))
			    step
			    (dec step))
	(dim-anneal-wrapper (string-append input-filename (format nil ".~d.pluspenaltydim" (dec step)))
			    (string-append input-filename (format nil ".~d" (dec step)))
			    (dec step)
			    (dec step)))
  (unix-windows-universal-copy-file (string-append input-filename (format nil ".~d" end-dims)) output-filename))



;;;----------------------------------------------------------------------
;;;    running locally with scripts (will become obselete from lisp)
;;;----------------------------------------------------------------------

(defun dim-anneal-script (script-filename input-filename output-filename start-dims end-dims) 
  (run-shell-command 
   (format nil "~a ~a ~a ~d ~d"
	   (if (running-on-windows-p)
	       (format t "Running via a script, this is Unix only right now.")
	     (uw-sfnr script-filename))
	   input-filename
	   output-filename
	   start-dims
	   end-dims)))

(defun dim-anneal-two-phase-script (input-filename output-filename start-dims end-dims) 
  (dim-anneal-script "mds/alan-dim-anneal/dim-anneal-two-phase" input-filename output-filename start-dims end-dims))
(defun dim-anneal-short-steps-script (input-filename output-filename start-dims end-dims) 
  (dim-anneal-script "mds/alan-dim-anneal/dim-anneal-short-steps" input-filename output-filename start-dims end-dims))
(defun dim-anneal-long-steps-script (input-filename output-filename start-dims end-dims) 
  (dim-anneal-script "mds/alan-dim-anneal/dim-anneal-long-steps" input-filename output-filename start-dims end-dims))


;;;----------------------------------------------------------------------
;;;                 running on gridware at sfi
;;;----------------------------------------------------------------------

(defun gridware-submit (script-filename input-filename output-filename &rest args)
  (run-shell-command 
   (format nil "scp -C ~a x2:~a" input-filename input-filename))
  (run-shell-command 
   (format nil "ssh x2 'chmod a+r ~a'" input-filename))
  (run-shell-command 
   (print (format nil 
	   ;; "ssh sfi qsub -p -20 -o ~a -e ~a  ~a ~a ~a ~{~a ~}"
	   ;;"ssh sfi ssh vulture qsub -p -20 -o ~a -e ~a  ~a ~a ~a ~{~a ~}"   ;; go thru sfi as no remote ssh into vulture now
           "ssh x2 'xgrid -h 192.168.1.2 -job submit -so ~a -se ~a  ~a ~a ~a ~{~a ~}'"  ;; xgrid instead of gridware
	   (string-append output-filename "-stdout")
	   (string-append output-filename "-stderr")
	   (if (running-on-windows-p)
	       (format t "Running via a script, this is Unix only right now.")
	     (uw-sfnr script-filename :users-to-home t)  ;; users-to-home because will run at sfi, not locally
	     )
	   input-filename
	   output-filename
	   args))))


;;;----------------------------------------------------------------------
;;;                      xgrid utilities
;;;----------------------------------------------------------------------

(defun xgrid-job-ids ()
  (let* ((raw-job-list (car (run-shell-command-wait-and-collect-output "ssh x2 xgrid -h 192.168.1.2 -job list")))
         (job-ids (read-from-string (string-subst #\, #\space (substring-before-char #\; (substring-after-char #\= raw-job-list))))))
    job-ids))

(defun xgrid-job-status (jobid)
  (run-shell-command (format nil "ssh x2 xgrid -h 192.168.1.2 -job attributes -id ~a" jobid)))

(defun xgrid-jobs-status (&optional &key jobs)
  (mapcar #'xgrid-job-status (or jobs (xgrid-job-ids))))
      

#|
;; this was for when i could not ssh into vulture, i had to write the submission scipt to a file then go execute the file
;; later i figured out i could ssh sfi ssh vulture <command>, i could do this before, but i was not forwarding my ssh-agent
;; so i had to type in my password.  now i am forwarding the agent so can freely do ssh sfi ssh vulture <command>
(defun gridware-submit-manual (script-filename input-filename output-filename &rest args)
  (run-shell-command (format nil "gzip ~a" input-filename))
  (run-shell-command 
   (print (format nil "scp ~a.gz sfi:~a.gz" input-filename input-filename)))
  (newline)
  (run-shell-command 
   (print (format nil "ssh sfi 'gunzip ~a.gz'" input-filename)))
  (newline)
  (run-shell-command 
   (print (format nil "ssh sfi 'echo qsub -p -20 -o ~a -e ~a  ~a ~a ~a ~{~a ~} >> junk/gridware-qsub-tmp'"
	   (string-append output-filename "-stdout")
	   (string-append output-filename "-stderr")
	   (if (running-on-windows-p)
	       (format t "Running via a script, this is Unix only right now.")
	     (uw-sfnr script-filename))
	   input-filename
	   output-filename
	   args))))
|#

;; ------------------------------ alan c code -----------------------------
(defun dim-anneal-two-phase-gridware (input-filename output-filename start-dims end-dims) 
  (gridware-submit "mds/alan-dim-anneal/dim-anneal-two-phase" input-filename output-filename start-dims end-dims))
(defun dim-anneal-short-steps-gridware (input-filename output-filename start-dims end-dims) 
  (gridware-submit "mds/alan-dim-anneal/dim-anneal-short-steps" input-filename output-filename start-dims end-dims))
(defun dim-anneal-long-steps-gridware (input-filename output-filename start-dims end-dims) 
  (gridware-submit "mds/alan-dim-anneal/dim-anneal-long-steps" input-filename output-filename start-dims end-dims))

;; --------------------------- cmucl lisp code ----------------------------
(defun cmucl-dim-anneal-two-phase-gridware (input-filename output-filename start-dims end-dims &optional num-runs) 
  ;; hack going acl instead of cmucl, and xgrid instead of gridware
  (if num-runs
      (format t " (~d runs bundled together)~%" num-runs))
  (if num-runs
      ;;(gridware-submit "mds/alan-dim-anneal/cmucl-dim-anneal-two-phase" input-filename output-filename start-dims end-dims num-runs)
      (gridware-submit "mds/alan-dim-anneal/acl-dim-anneal-two-phase" input-filename output-filename start-dims end-dims num-runs)
    ;;(gridware-submit "mds/alan-dim-anneal/cmucl-dim-anneal-two-phase" input-filename output-filename start-dims end-dims)
    (gridware-submit "mds/alan-dim-anneal/acl-dim-anneal-two-phase" input-filename output-filename start-dims end-dims)))

(defun cmucl-batch-mds-from-save-gridware (input-filename output-filename) 
  (gridware-submit "mds/alan-dim-anneal/cmucl-batch-mds-from-save" input-filename output-filename))  ;; script filename is at sfi

;;qsub -o /home/dsmith/junk/gridware/gw-stdout -e /home/dsmith/junk/gridware/gw-stderr /home/dsmith/mds/mds/alan-dim-anneal/dim-anneal-long-steps /home/dsmith/junk/gridware/gwlapt1-5d /home/dsmith/junk/gridware/gwlapt1-5d-out 5 2

;; if nfs
;; xgrid -job submit -so /tmp/so1 -se /tmp/se1 /Users/dsmith/mds/src/mds/mds/alan-dim-anneal/cmucl-dim-anneal-two-phase /tmp/tab1-no-runs.save /tmp/tab2-no-runs-xgrid-output 5 2 

;; using xgrid to pass dir
;; xgrid -job submit -so /tmp/so1 -se /tmp/se1 /Users/dsmith/mds/src/mds/mds/alan-dim-anneal/cmucl-dim-anneal-two-phase /tmp/tab1-no-runs.save /tmp/tab2-no-runs-xgrid-output 5 2 

;;--------------------- recovering gridware runs -----------------------

(defun recover-gridware-run (out-filename unique-filename)
  (let ((file-exists
	 (run-shell-command
	  ;;(print (format nil "scp -C vulture:~a ~a" out-filename out-filename))
	  ;;(print (format nil "scp sfi:~a ~a" out-filename out-filename))
          (print (format nil "scp -C x2:~a ~a" out-filename out-filename)))))   ;; switch from gridware to xgrid
    (if (not (bit->bool file-exists))  ;; not because a return of 0 indicates success in unix
	(prog1
	    (if (lapedes-format-file-p out-filename)
		(multiple-value-bind (coordss stress)
		    (lapedes-file-to-coordss-and-stress out-filename)
		  (list coordss stress 'no-termination-reason-lapedes-dim-anneal-gridware-recovered 'no-fitness-history))
	      ;; assume from running lisp and we just return the return-form
	      (fi-in out-filename))
	  (if (running-on-windows-p)
	      (print "how do we remove files under windows?")
	    (run-shell-command (format nil "rm ~a" out-filename)))
	  ;; maybe later also do deleteing at sfi, and for that we will use unique-filename, or more carefully 
	  ;; and yes, much better, delete the specific files (then i'm not using rm <something>* in a program
	  ;; with a danger of <something> being nothing
	  ))))

(defun batch-runs-ui-window-recover-gridware-runs (batch-runs-ui-window)
  (let ((table-window (get-table-window-for-batch-runs-ui-window batch-runs-ui-window)))
    (initialize-batch-runs-ui
     batch-runs-ui-window
     (recover-gridware-runs (get-batch-runs-data table-window) :batch-runs-ui-window batch-runs-ui-window)
     :comment "Done")))

(defun hack-clean-grouped-batch-runs-from-gridware (l)
  (loop for e in l append
	(if (and (listp e)
		 (listp (car e))
		 (listp (caar e))
		 (listp (caaar e)))
	    e
	  (list e))))

(defun recover-gridware-runs (batch-runs-sorted-data &optional &key batch-runs-ui-window)
  (let ((num-runs-to-recover (length (collect (^ (x) (eql 'recover-gridware-run (nth 0 (nth 0 x)))) batch-runs-sorted-data))) ;; only used for ui if passed
	(recover-number 0))                                                                                           ;; only used for ui if passed
    (sort-batch-runs
     (hack-clean-grouped-batch-runs-from-gridware
      (loop for batch-run in batch-runs-sorted-data collect
	    (let ((coordss (nth 0 batch-run)))
	      (if (eql 'recover-gridware-run (nth 0 coordss))
		  (progn
		    (if batch-runs-ui-window
			(tk-put batch-runs-ui-window ".counter conf -text {Recovering ~d of ~d}" 
				(setq recover-number (inc recover-number))
				num-runs-to-recover))
		    (let ((gridware-results (eval coordss)))
		      (if gridware-results
			  gridware-results
			batch-run)))
		batch-run)))))))

	 
;;;----------------------------------------------------------------------
;;;                  orchestrating the runs
;;;----------------------------------------------------------------------

(defun lapedes-dim-anneals (num-runs
			    hi-table
			    &key (starting-dimension 5)
				 (final-dimension 2)
				 (starting-coordss starting-dimension)
				 (dim-anneal-f #'dim-anneal-two-phase))
  (loop for i below num-runs collect
	(lapedes-dim-anneal hi-table 
			    :starting-dimension starting-dimension
			    :final-dimension final-dimension
			    :starting-coordss starting-coordss
			    :dim-anneal-f dim-anneal-f)))
			    

(defun lapedes-dim-anneal (hi-table
			   &key (starting-dimension 5)
			        (final-dimension 2)
				(starting-coordss starting-dimension)
				(dim-anneal-f #'dim-anneal-two-phase))
  (let* ((unique-filename (uw-sfnr
			   (string-append 
			    "mds/alan-dim-anneal/tmp-files/"
			    (string (hi-table-name hi-table))
			    "-"
			    (anything->string (krandom 9999999 *dim-anneal-random-number-generator*)))))
	 (in-filename  (string-append unique-filename "-infile"))
	 (out-filename (string-append unique-filename "-outfile")))
    (fi-lapedes-format
     (hi-table-to-lapedes 
      hi-table 
      :starting-coordss 
      (cond ((or (functionp starting-coordss)
		 (and (listp starting-coordss) (eq 'lambda (car starting-coordss))))
	     (funcall starting-coordss))
	    ((listp starting-coordss)   ;; later do the same checking that is done in tk-interface
	     starting-coordss)
	    ((integerp starting-coordss)
	     (make-random-coordss 
	      (hi-table-length hi-table)
	      starting-coordss
	      (hi-table-max-value hi-table)
	      (or (ag-sr-table-p hi-table)
		  (similarity-table-p hi-table))
	      hi-table))
	    (t (error "unexpected starting-coords, expected function, coordss, or num-dimensions, but got ~a"
		      starting-coordss))))
     in-filename)
    (funcall dim-anneal-f in-filename out-filename starting-dimension final-dimension)
    (multiple-value-bind (coordss stress)
	(if (or (eql dim-anneal-f #'dim-anneal-two-phase-gridware)
		(eql dim-anneal-f #'dim-anneal-short-steps-gridware)
		(eql dim-anneal-f #'dim-anneal-long-steps-gridware))
	    (values (list 'recover-gridware-run out-filename unique-filename) "Queued")
	  (lapedes-file-to-coordss-and-stress out-filename))
      (if (running-on-windows-p)
	  (print "not removing intermediate files under windows yet")
	(run-shell-command (format nil "rm ~a*" unique-filename))
	)
      (list coordss stress 'no-termination-reason-lapedes-dim-anneal 'no-fitness-history))))
    

;;;----------------------------------------------------------------------
;;;                      todo
;;;----------------------------------------------------------------------

#|
link:

and send to r n a
the latest runs of the merge
the dim anneal release
the dim anneal experiments
alan and running at sfi the scripts, the while, the gridware


at some time later:
  if we double click on queued, then go try to get that entry, or update the info on that entry
    right now the find-closes in mds-visualization-from-batch-runs-ui gives us the first entry in the list
  we need a way to abort trying to recover the runs if sfi is down, or i change my mind
  way to cancel queued runs (just go thru the gridware interface)

delete files at sfi
which was to run called in a menu  

do my experiments to figure out best dim anneal schedule before releasing on windows

then on windows
then make release
then make sequence table
(check it will work with dim anneal (ie do we get the unlogging and ag-sr-ing general enuf))
and do rons runs (use 3d so we can look, and look for the hammerhead a bit)
  (note the hammerhead might not all be at the end of a clade but might be in it too, but later strains)
|#       