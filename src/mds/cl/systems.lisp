(in-package user)

;;;-------------------------------------------------------------------
;;;                       DEFSYSTEMS
;;;-------------------------------------------------------------------

(load (uw-sfnr "cl/defsystem" :assertIsFile t :suffix ".lisp"))
(load (uw-sfnr "mds/defsystem" :assertIsFile t :suffix ".lisp"))

;;(load (uw-sfnr "mds/investigations/plaque-assay-bias/defsystem" :assertIsFile t :suffix ".lisp"))

;;(if (equal (user-name) "dsmith")
;;    (load (uw-sfnr "mds/investigations/bernadette/defsystem" :assertIsFile t :suffix ".lisp")))


;;(if (not (running-on-windows-p))
;;    (load (uw-sfnr "im/defsystem" :assertIsFile t)))

;;(load "~dsmith/im/version-1.4/defsystem")
;;(load "~dsmith/im/version-1.5/defsystem")

;;(load "~dsmith/bb/derek/src/defsystem")

;; OLD FOR WINDOWS ACL 5.0, which did not let compiled files be saved
;;(defun make-system (system)
;;  (if (running-on-windows-p)
;;      (let ((default-pathname (defsys:default-pathname (find-system system))))
;;	(map-system system
;;		    #'(lambda (module) 
;;			(compile-file 
;;			 (concatenate 'string
;;			   default-pathname
;;			   (defsys:module-file module))
;;			 :load-after-compile t))))
;;    (load-system system :compile t)))

;; now in .clinit.cl
;;(defun make-system (system)
;;  (load-system system :compile t))


