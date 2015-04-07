(in-package user)

(defun fork-and-pipe (command)
  (let* ((stream (run-shell-command (string-append "exec " command)
				    :wait nil 
				    :input :stream 
				    :output :stream 
				    :error-output :output))
	 (stream-number (inc (length *tk-streams*))))
    (push (list stream-number stream 'possible-object-id)
	  *tk-streams*)
    stream-number))

(defun x-fi (lines &optional (if-exists-action :append))
  (loop for line in (if (listp lines) lines (list lines)) do
	(fi line "~/.lisp-xmgr.tmp" if-exists-action t)))


(defun ll-to-x-plot (ll)
  (format nil "~{~{~a ~}~%~}" ll))

(defun x-plot (coords &key (style 'line) 
			   ;;(refresh t)
			   ;;(element-name (gensym))
			   ;;(element-color (random-x-color))
			   ;;element-symbol
			   ;;element-dashes
			   ;;element-linewidth
			   x-min x-max y-min y-max
			   x-title y-title
			   ;;latex
			   ;;ps-filename
			   ;;(legend-corner 'off-right)
			   ;;(legend-mapped t)
			   ;;big-font
			   ;;tag
			       )
  (x-fi "# ACE/gr parameter file" :new-version)
  (x-fi "#")
  
  (x-fi "@with g0")
  
  (x-fi (case style
	  (line "type XY")))
  
  (if x-title (x-fi (format nil "@    xaxis  label \"~a\"" x-title)))
  (if y-title (x-fi (format nil "@    yaxis  label \"~a\"" y-title)))
  
  (if x-min (x-fi (format nil "@    world xmin ~a" x-min)))
  (if x-max (x-fi (format nil "@    world xmax ~a" x-max)))
  (if y-min (x-fi (format nil "@    world ymin ~a" y-min)))
  (if y-max (x-fi (format nil "@    world ymax ~a" y-max)))
    
  (x-fi (ll-to-x-plot coords))
  (fork-and-pipe "xmgr ~/.lisp-xmgr.tmp"))


(defun x-fi (lines &optional (if-exists-action :append))
  (loop for line in (if (listp lines) lines (list lines)) do
	(fi line "~/.lisp-xmgr.tmp" if-exists-action t)))

(defun x-plot (coords &key ;;(style 'line) 
			   ;;(refresh t)
			   ;;(element-name (gensym))
			   ;;(element-color (random-x-color))
			   ;;element-symbol
			   ;;element-dashes
			   ;;element-linewidth
			   x-min x-max y-min y-max
			   x-title y-title
			   ;;latex
			   ;;ps-filename
			   ;;(legend-corner 'off-right)
			   ;;(legend-mapped t)
			   ;;big-font
			   ;;tag
			       )

  (fork-and-pipe "xmgr -source stdin")
  
  (x-fi "@with g0")
  
  (if x-title (x-fi (format nil "@    xaxis  label \"~a\"" x-title)))
  (if y-title (x-fi (format nil "@    yaxis  label \"~a\"" y-title)))
  
  (if x-min (x-fi (format nil "@    world xmin ~a" x-min)))
  (if x-max (x-fi (format nil "@    world xmax ~a" x-max)))
  (if y-min (x-fi (format nil "@    world ymin ~a" y-min)))
  (if y-max (x-fi (format nil "@    world ymax ~a" y-max)))
    
  (x-fi (ll-to-x-plot coords))
  (fork-and-pipe "xmgr -source stdin"))

#|
(x-plot '((0 0) (1 2) (2 2) (3 1)))
(x-plot '((0 0) (1 2) (2 2) (3 1)) 
	:x-title "test X title"
	:y-title "test Y title"
	:x-min -2 :x-max 5
	:y-min -1 :y-max 6)
|#

