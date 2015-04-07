(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "pack .b"))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {toLisp \"(print 'bar)\"}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c"))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {send lisp_blt_wish to_lisp \"\"(print 'bar)\"\"}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c"))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {send lisp_blt_wish to_lisp {\"(print 'bar)\"}}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c"))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {to_lisp {\"(print 'bar)\"}}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {to_lisp \"(print 'bar)\"}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {to_lisp \"(print 'bar)\";to_lisp \"(print 'bar-again)\"}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "lisp_blt_wish"))

  (tk-put foo "button .b -text foo -command {toLisp \"(print 'foo)\"}")
  (tk-put foo "button .c -text bar -command {puts \"(print 'bar)\";flush stdout}")
  (tk-put foo "pack .b")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "wish"))
  (tk-put foo "button .c -text bar -command {puts \"(print 'bar)\";flush stdout}")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "wish"))
  (tk-put foo "button .c -text bar -command {puts \"(print 'bar)\";flush stdout}")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "wish"))
  (tk-put foo "source cl/toLisp.tk")
  (tk-put foo "button .c -text bar -command {toLisp \"(print 'bar)\"}")
  (tk-put foo "pack .c")
  (mp:process-run-function "background-tk-listener" 
			 #'tk-interpret foo '(run)))

(progn
  (setq foo (tk-open "wish"))
  (tk-put foo "button .c -text bar -command {toLisp \"(print 'bar)\"}")
  (tk-put foo "pack .c"))



