(in-package user)

;; this is a derivative of tk-open, that does not do some tk-specific things
;; i should really take all the tk-specific stuff out of here, and build tk-open on top of it
;; and also make process-put and process-get

(defun process-open (&optional (interpreter (if (running-on-windows-p) 
					   "c:/tcl/bin/wish82"
					 "wish"))
			       (send-stream-number t)
			       (stream-listener-f #'tk-interpret))
  (let* ((stream (run-shell-command (if (running-on-windows-p)
					interpreter
				      (string-append "exec " interpreter))
				   :wait nil 
				   :input :stream 
				   :output :stream 
				   :error-output :output))
	 (stream-number (inc (length *tk-streams*)))
	 (stream-listener (if stream-listener-f
			      (mp:process-run-function "background-tk-listener" stream-listener-f stream))))
    (push (list stream-number 
		stream 
		'possible-object-id 
		stream-listener  ;;nil if non (so we do our own explict tk-get)
		nil) ;;stream-alist
	  *tk-streams*)
    (if send-stream-number 
	(progn
	  (tk-put stream "set lisp_tk_stream_number ~d" stream-number)
	  ;;(tk-put stream "source ~dsmith/cl/toLisp.tk")
	  (tk-put stream "source cl/toLisp.tk")))
    stream-number))