(in-package excl)

;;***************************************************************************************
;;    LINK: if this file is edited, also edit mds.system and defsystem-lispworks.lisp
;;***************************************************************************************

(eval 
 `(defsystem :test
      ;;(:default-pathname "mds/")  for the windows compatibility  (along with no ~ in .clinit.cl and cl/systems names of files to load in cl/systems
      (:default-pathname ,(user::uw-sfnr "test/" :assertIsDir t))
    (:serial
     "lisp-unit"
     )))
