(in-package user)

(defconstant *wish-quit-command*  "quit" "The command to send to a wish slave process to tell it to quit.")

(defconstant *wish-exception-string*  "...EXC..." "The string the wish slave process should return if the command raises an exception.")
(defconstant *wish-exception-string-len*  (length *wish-exception-string*) "The length of the string the wish slave process should return if the command raises an exception.")

(defconstant *wish-command* (uw-wish-location) "Location of the wish executable.") 
(defconstant *wish-comms-program* (uw-sfnr "cl/tk-socket.tk" :assertIsFile t) "The location of the wish socket communication script.")


(defun wish-read (stream)
  (string-right-trim '(#\Return) (read-line stream)))

(defun open-wish-socket (&optional (start-slave t))
  
  "Pass nil for start-slave if you don't want this code to run the slave process.
  In that case, you are expected to run the slave manually from outside lisp.
  This is a good way to take the port number, hello string and quit command
  (all reported below) and invoke the slave on the command line yourself.
  
  This command can be called many times, each time returning a new stream to
  talk to a new instance of the slave process, or nil if a stream could not
  be opened successfully.

  Use (tell-wish stream line) to say something to the slave.
  Use (tell-wish-quit stream) when you want the slave to exit."

  (let*
      (reply
       (hello-string "HELLO")
       (sock (acl-socket::make-socket :connect :passive))
       (port (acl-socket::local-port sock))
       (command (format nil "~s ~s -- ~s ~s ~s ~s" *wish-command* *wish-comms-program* port hello-string *wish-exception-string* *wish-quit-command*))
       stream)
    
    (if start-slave
	;; (format t "Starting slave. Command = '~a'~%" command)
	(run-shell-command command :wait nil)
      (format t "Run your listener from the command line now. Suggested command:~%~a~%" command))
    
    ;; (format t "Lisp listening for slave on port ~a~%" port)
    (setq stream (acl-socket::accept-connection sock))
  
    ;; Read and confirm the hello line from the client.
    ;; (format t "Lisp waiting for hello~%")
    (setq reply (wish-read stream))
  
    (if (not (string= reply hello-string))
	(progn
	  (format t "Client replied with '~a' instead of expected '~a'" reply hello-string)
	  (close stream)
	  nil)
      stream)))
      
(defun tell-wish (stream line)
  (format stream "~a~%" line)
  (force-output stream)
  (if (string= *wish-quit-command* line)
      (close stream)))

(defun close-wish-socket (stream)
  (tell-wish stream *wish-quit-command*))
      


#|
(defun test-wish-simple (&optional (start-slave t))
  (let*
      ((wish-command1 "set a 55")
       (stream1 (open-wish-socket start-slave))
       (reply1 (tell-wish stream1 wish-command1)))
    
    (format t "Told wish1 '~a'.~%Got reply '~a'.~%" wish-command1 reply1)
    (close-wish-socket stream1)
    ))

(defun test-wish-double (&optional (start-slave t))
  (let*
      ((wish-command1 "set a 55")
       (wish-command2 "set b 56")
       (stream1 (open-wish-socket start-slave))
       (stream2 (open-wish-socket start-slave))
       (reply1 (tell-wish stream1 wish-command1))
       (reply2 (tell-wish stream2 wish-command2)))
    
    (format t "Told wish1 '~a'.~%Got reply '~a'.~%" wish-command1 reply1)
    (format t "Told wish2 '~a'.~%Got reply '~a'.~%" wish-command2 reply2)
    (close-wish-socket stream1)
    (close-wish-socket stream2)
    ))

(defun test-wish-exception (&optional (start-slave t))
  (let*
      ((wish-command1 "klkklkjlkjlkj")
       (stream1 (open-wish-socket start-slave))
       (reply1 (tell-wish stream1 wish-command1)))
    
    (assert (equal reply1 nil))
    (format t "Told wish1 '~a'.~%Got expected exception.~%" wish-command1)
    (close-wish-socket stream1)
    ))
|#