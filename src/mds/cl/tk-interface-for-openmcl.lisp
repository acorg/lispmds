(in-package user)

(defmacro display-error-and-continue (form)
  `(handler-case
       ,form
     (error (condition) 
       (let ((error-string (apply #'format t (condition-format-control condition) (condition-format-arguments condition))))
	 (print error-string)
	 ;;(let ((tk (tk-open)))
	 ;;  (tk-put tk "dialog {foo} {bar} {} -1 OK"))
	 condition))))


(defvar *tk-streams*)
(setq *tk-streams* nil)

(defvar *last-background-process*)
(setq *last-background-process* nil)
;;I need to keep the last process on tk-streams, that way it is process dependent, 
;;and I can do the removal after windows are arrested, and get the gc
;;right now, if a process is arrested, and another process sets up last-background-process,
;;then I do not get a chance to kill the old process for GC, and the new one, if
;;in background may be killed.
;;also if one is suspended and I do a run in another, then the old one is run

;;error-output was *terminal-io*
;;to run wish on say fiveup, and lisp on mendel i can do "rsh fiveup ~dsmith/bin/sd-wish &
;;i could put blt_wish in my bin and call terry's for running at unm, it could even have the
;;rsh in there

(defun lisp-system-independent-process-run-function (name f &optional &key args)
  #+:allegro
  (apply #'mp:process-run-function name f args)
  #+:lispworks
  (apply #'mp:process-run-function name nil f args)
  #+:openmcl
  (apply #'process-run-function name f args)
  )

(defun lisp-system-independent-process-add-arrest-reason (process reason)
  #+:allegro
  (apply #'mp:process-add-arrest-reason process reason)
  #+:lispworks
  (setf (mp:process-arrest-reasons process) (cons reason (mp:process-arrest-reasons process)))
  #+:openmcl
  (setf (process-arrest-reasons process) (cons reason (process-arrest-reasons process)))
  )

(defun lisp-system-independent-process-revoke-arrest-reason (process reason)
  #+:allegro
  (apply #'mp:process-revoke-arrest-reason process reason)
  #+:lispworks
  (setf (mp:process-arrest-reasons process) (remove reason (mp:process-arrest-reasons process)))
  #+:openmcl
  (setf (process-arrest-reasons process) (remove reason (process-arrest-reasons process)))
  )

(defun tk-open (&optional (interpreter (uw-wish-location))
			  (send-stream-number t))
  (let* ((stream 
	  #+:allegro
	  (run-shell-command (if (running-on-windows-p)
				 interpreter
			       (string-append "exec " interpreter))
			     :wait nil 
			     :input :stream 
			     :output :stream 
			     :error-output :output)
          #+:cmu
	  (error "tk-open is not implemented yet")
	  #+:lispworks
	  (if (running-on-windows-p)
	      (win32::open-pipe :command-line interpreter)
	    (sys:open-pipe (string-append "exec " interpreter) :direction :io))
	  #+:openmcl
	  (run-program interpreter nil :wait nil :output :stream :input :stream :error :output)
	  )
	 (stream-number (inc (length *tk-streams*)))
	 (stream-listener '(lisp-system-independent-process-run-function 
			   "background-tk-listener"
			   #'tk-interpret
			   :args (list stream '(run))))
	  )
    (push (list stream-number 
		stream 
		'possible-object-id 
		stream-listener
		nil) ;;stream-alist
	  *tk-streams*)
    (if send-stream-number (tk-put stream "set lisp_tk_stream_number ~d" stream-number))
    ;;(tk-put stream "source ~dsmith/cl/toLisp.tk")
    (tk-put stream "source ~s" (uw-sfnr "cl/toLisp.tk" :assertIsFile t))
    stream-number))

(defun tk-put (stream command &rest command-string-args)
  (if (numberp stream)
      (setq stream (assoc-value-1 stream *tk-streams*)))
  #+:openmcl
  (if (typep stream 'external-process) 
      (setq stream (external-process-input-stream stream)))
  (setq command-string-args
    (f-elements (^ (x) 
		   (if (typep x 'long-float)
		       (coerce x 'short-float)
		     x))
		command-string-args))
  (if command-string-args
      (setq command (apply #'format nil command command-string-args)))
  
  '(with-open-file (out (format nil "~~/dribble.~a" (position stream (reverse (nths 1 *tk-streams*))))
		   :direction :output 
		   :if-exists :append
		   :if-does-not-exist :create)
    (write-line command out))
  (write-line command stream)
  (force-output stream)
  command)

(defun tk-get (stream)
  (if (numberp stream)
      (setq stream (assoc-value-1 stream *tk-streams*)))
  #+:openmcl
  (if (typep stream 'external-process) 
      (setq stream (external-process-output-stream stream)))
  (read-line stream))

(defun tk-close (stream)
  (if (or (null stream) (not (or (numberp stream) (streamp stream))))
      (error "please pass a stream number or stream to tk-close, you passed ~d" stream))
  (if (streamp stream)
      (setq stream 
	(nth 0 (find stream *tk-streams* :test (^ (stream list) (equal stream (nth 1 list)))))))
  (if (numberp stream)
      (let* ((stream-info-list (assoc stream *tk-streams*))
	     (tk-interpreter (nth 3 stream-info-list)))
	(setq stream (assoc-value-atom stream *tk-streams*))
	(setf (nth 1 stream-info-list) 'stream-closed)
	(if (nth 2 stream-info-list)
	    (setf (nth 2 stream-info-list) 'Object-gone))
	(setf (nth 3 stream-info-list) 'Interpreter-killed)
	(tk-put stream "after 1 exit")
	#+:allegro
	(system:os-wait)
	(if *last-background-process*
	    (progn
	      (lisp-system-independent-process-revoke-arrest-reason 
	       *last-background-process*
	       (car (#-:openmcl
		     mp:process-arrest-reasons
		     #+:openmcl
		     process-arrest-reasons
		     *last-background-process*)))
	      (#-:openmcl
	       mp:process-kill
	       #+:openmcl
	       process-kill
	       *last-background-process*)
	      (setq *last-background-process* nil)))
	(close stream)
	(#-:openmcl
	 mp:process-kill
	 #+:openmcl
	 process-kill
	 tk-interpreter))))

;; shorter version for early lispworks port
(defun tk-interpret (stream &optional background-commands)
  (let (command)
    (loop for i below 7777777777 do
	  (setq command (tk-get
			 (print 
			 #-:openmcl
			 stream
			 #+:openmcl
			 (external-process-output-stream stream)
			 )
			 ))
	  (if (or (equal #\( (aref command 0))
		  (equal #\' (aref command 0))
		  (equal #\" (aref command 0)))
	      (let ((command (read-from-string command)))
		(cond ((or (equal "'eof" command) 
			   (equal ''eof command)
			   (equal '(display-error-and-continue 'eof) command))
		       (return (tk-close stream)))
		      (t (eval command))))
	    (if (not (string-equal command "stty: TCGETS: Operation not supported on socket"))
		(print command))))))

#|
(defun tk-interpret (stream &optional background-commands)
  (let (command)
    (loop for i below 7777777777 do
	  (setq command (tk-get stream))
	  (if (or (equal #\( (aref command 0))
		  (equal #\' (aref command 0))
		  (equal #\" (aref command 0)))
	      (let ((command (read-from-string command)))
		(cond ((or (equal "'eof" command) 
			   (equal ''eof command)
			   (equal '(display-error-and-continue 'eof) command))
		       (return (tk-close stream)))
		      ((equal ''arrest command)
		       (if *last-background-process*
			   (lisp-system-independent-process-add-arrest-reason 
			    *last-background-process* 
			    'circle-ui-arrest)
			 (print "No process to stop")))
		      ((and (listp command) (member (car command) background-commands))
		       ;;there is a bg process of this name, 
		       ;;do not start another (2 runs interfere for instance)
		       ;;and if it is arrested then unarrest
		       (if (and *last-background-process*
				(#-:openmcl
				 mp:process-arrest-reasons
				 #+:openmcl
				 process-arrest-reasons
				 *last-background-process*))
			   (lisp-system-independent-process-revoke-arrest-reason 
			    *last-background-process*
			    (car (#-:openmcl
				  mp:process-arrest-reasons
				  #+:openmcl
				  process-arrest-reasons
				  *last-background-process*)))
			 (setq *last-background-process*
			   (lisp-system-independent-process-run-function
			    (format nil "Background-tk-interpret-~a" (gensym))
			    (eval `(^ () ,command))))))
		      (t (eval command))))
	    (if (not (string-equal command "stty: TCGETS: Operation not supported on socket"))
		(print command))))))
|#

#|
(defun start-lisp-blt-wish ()
  (let ((stream (tk-open "/nfs/u8/dsmith/bin/lisp_blt_wish")))
    (tk-put stream "wm geometry . +20+200")
    (tk-put stream "wm iconify .")
    stream))

(mp:process-run-function "background-tk-listener" 
			 #'tk-interpret (start-lisp-blt-wish) '(run))
|#