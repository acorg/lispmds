(in-package user)

;;; Note that *MDS-ROOT-VAR* is the _name_ of the variable, not its value. To get
;;; the value use (sys:getenv *MDS-ROOT-VAR*).
(defvar *MDS-ROOT-VAR* "MDS_ROOT" "The environment variable giving your MDS root directory.")

;;;----------------------------------------------------------------------
;;;                      cmucl initializations
;;;----------------------------------------------------------------------

;;#+:cmu
;;(progn
;;  (require :defsystem)
;;  (mk:add-registry-location "/home/dsmith/cmucl/Registry/"))

;; hmm, these commands cannot be in a progn above, otherwise cmucl barfs that the package mk is not defined, split as below

#+:cmu
(require :defsystem)

#+:cmu
(progn
  (mk:add-registry-location "/home/dsmith/mds/src/mds/cl/")
  (mk:add-registry-location "/home/dsmith/mds/src/mds/mds/"))

#+:cmu
(set-floating-point-modes :traps '(:invalid :divide-by-zero))


;;;----------------------------------------------------------------------
;;;                    openmcl initalizations
;;;----------------------------------------------------------------------

#+:openmcl
(defun defsystem-as-file (system)
  ;; assume file is in ~dsmith/<system>/defsystem-as-file.lisp
  (load (format nil "/Users/dsmith/~a/defsystem-as-file.lisp" (string-downcase (string system)))))

;;(load "/usr/local/defsystem-3.3i.1/defsystem.lisp")

#+:openmcl
(defun exit ()
  (quit))

(defun statOrNil (file)
  (let (stat)
    (handler-case
	(setq stat (excl.osi:stat file))
      (file-error () nil)
      (:no-error (c) stat))))

(defun isXXX (file mode)
  (let ((stat (statOrNil file)))
    (and (not (null stat))
	 (eq (logand mode (logior 0 (excl.osi:stat-mode stat))) mode))))

(defun isFile (file)
  (isXXX file excl.osi:*s-ifreg*))

(defun isDir (file)
  (isXXX file excl.osi:*s-ifdir*))

(defun isExistent (file)
  (not (isNonExistent file)))

(defun isNonExistent (file)
  (null (statOrNil file)))
  

#|
(defun testFileFuncs ()
  (let* ((home (sys:getenv "HOME"))
	 (existingDir home)
	 (existingFile (concatenate 'string home "/.bashrc"))
	 (nonExistent (concatenate 'string home "lkjlllljljljlkjljlkj")))
	
    (assert (isExistent existingDir))
    (assert (not (isNonExistent existingDir)))
    (assert (isExistent existingFile))
    (assert (not (isNonExistent existingFile)))
    (assert (not (isExistent nonExistent)))
    (assert (isNonExistent nonExistent))
    (assert (isDir existingDir))
    (assert (not (isDir existingFile)))
    (assert (not (isDir nonExistent)))
    (assert (isFile existingFile))
    (assert (not (isFile existingDir)))
    (assert (not (isFile nonExistent)))
    ))
|#


(defun checkPath (path &optional &key assertIsFile assertIsDir assertExistent assertNonExistent (suffix ""))
    ;; Only try the existence tests if we have loaded the funcs in os.lisp. We can't just require them
  ;; here because we use this function to load files in an OS-independent way.
  (if (> (length suffix) 0)
      (setq path (concatenate 'string path suffix)))
  
  (if assertIsFile
      (if (fboundp 'isFile)
	  (assert (isFile path))
	(if *load-verbose*
	    (warn "Cannot assert '~a' is a file, as isFile is not yet bound." path))))
  (if assertIsDir
      (if (fboundp 'isDir)
	  (assert (isDir path))
	(if *load-verbose*
	    (warn "Cannot assert '~a' is a directory, as isDir is not yet bound." path))))
  (if assertExistent
      (if (fboundp 'isExistent)
	  (assert (isExistent path))
	(if *load-verbose*
	    (warn "Cannot assert '~a' exists, as isExistent is not yet bound." path))))
  (if assertNonExistent
      (if (fboundp 'isNonExistent)
	  (assert (isNonExistent path))
	(if *load-verbose*
	    (warn "Cannot assert that '~a' does not exist, as isNonExistent is not yet bound." path))))
  path)


;;;-------------------------------------------------------------------
;;;                    WINDOWS/UNIX UTILS
;;;-------------------------------------------------------------------


(defun substring (string start &optional (end (- (length string) 1)))
  (if (> start (length string))
    (error "start requested is beyond end of string"))
  (let ((ans (make-string (+ 1 (- end start)))))
    (loop for source from start to end
          for destination from 0 do
          (setf (aref ans destination) (aref string source)))
    ans))

; D - why is this a function, not a constant?
(defun running-on-windows-p ()
  #+lispworks
  t
  #+:cmu
  nil
  #+:allegro
  ;;(let ((home (sys:getenv "HOME")))
  ;;  (or (null home) ;; when starting from the franz desktop icon this is not set
  ;;	(eql #\: (aref home 1))))   ;; eg c:/ or f:/  (but from emacs on windows it is)
  (not (or (string-equal "Apple" (substring (software-type) 0 4))
           (string-equal "Linux" (substring (software-type) 0 4))))
       ;; need to generalize to all unix, or find what all windows report
  )

;;;-------------------------------------------------------------------
;;;                    WINDOWS FILENAMES
;;;-------------------------------------------------------------------

(defun forward-slash-to-back-slash-in-string (string)
  (let ((new-string (make-string (length string))))
    (loop for i below (length string) do
       (setf (aref new-string i) 
          (if (eql #\/ (aref string i))
            #\\
            (aref string i))))
    new-string))

(defun find-file-in-dir-list (files dirs)
  (let* (found
         (windows (running-on-windows-p))
         (n 0)
         nfiles
         (ndirs (length dirs))
         dir
         (sep (if windows "\\" "/"))
         fullpath)
    
    (if windows
        (setq dirs (mapcar 'forward-slash-to-back-slash-in-string dirs)))
    
    (if (stringp files)
        (setq files (list files)))
    
    (setq nfiles (length files))

    (while (and (< n ndirs) (not found))
      (let
          ((f 0))
        (while (and (< f nfiles) (not found))
          (setq fullpath (concatenate 'string (nth n dirs) sep (nth f files)))
	  ;; We don't use isFile (from os.lisp) here as it's not loaded, and to use it
	  ;; we'd need uw-sfnr, but that needs *windows-source-filename-root*, which
	  ;; is not yet defined (in fact we are called to define it).
          (handler-case
              (excl.osi:stat fullpath)
            (file-error ())
            (:no-error (c) (setq found t)))
          (incf f))
        (incf n)))
    
      (if found
          fullpath
        (error (format nil "Could not locate file(s) ~s in any of ~s~%" files dirs)))
    ))


(defun add-to-all (str l &optional (appendp t) (sep (if (running-on-windows-p) "\\" "/")))
  (mapcar `(lambda (s) 
	     (if (zerop (length ,str))
		 s
	       (if ,appendp
		   (concatenate 'string ,str ,sep s)
		 (concatenate 'string s ,sep ,str))))
	     l))

(defun flatten (ll)
  (if (null ll)
      nil
    (if (atom ll)
	(list ll)
      (append (flatten (car ll))
	      (flatten (cdr ll))))))

(defun possible-paths (alternatives &optional (base-dir ""))
  (let
      ((n 0)
       (s (list base-dir))
       (nalts (length alternatives))
       appendp
       these
       this
       news)
    (while (< n nalts)
      (setq this (nth n alternatives))
      (if (stringp this)
	  (setq appendp t
		these (list this))
	(setq appendp (car this)
	      these (cadr this)))
      (setq
	  s (flatten (mapcar `(lambda (x) (add-to-all x (quote ,these) ,appendp)) s))
	  n (+ 1 n)))
    s))

(defun make-windows-lisp-filename-passable-as-argument (string)
  (list-to-string
   (loop for i below (length string) append
	 (if (eql #\/ (aref string i))
	     (list #\\)
	   (list (aref string i))))))

(defun uw-sfnr (filename &optional &key users-to-home assertIsFile assertIsDir assertExistent assertNonExistent (suffix ""))
  (let ((result
	 (if (running-on-windows-p)
	     (progn
	       #+:allegro
	       (concatenate 'string *windows-source-filename-root* "\\" filename)
	       #+:lispworks
	       (forward-slash-to-back-slash-in-string (concatenate 'string *windows-source-filename-root* "\\" filename))
	       )

	   (let ((unix-full-name (namestring (pathname (concatenate 'string *unix-source-filename-root* "/" filename)))))
	     (if users-to-home
		 (if (equal "/Users/" (subseq unix-full-name 0 7))
		     (concatenate 'string "/home/" (subseq unix-full-name 7))
		   unix-full-name)
	       unix-full-name)))))
    (checkPath result :assertIsFile assertIsFile :assertIsDir assertIsDir :assertExistent assertExistent :assertNonExistent assertNonExistent :suffix suffix)))

(if (running-on-windows-p)
  (let ((*windows-volumes*            '("c:" "d:" "e:" "f:" "y:"))
	(*windows-program-files-dirs* '("Program Files" "Progra~1" "Archivos de Programa" "Archiv~1" "mds"))
	(*windows-tcl-dirs*           '("tcl" "ActiveTcl" "Evolane"))
	(*windows-pymol-dirs*         '("Delano Scientific" "Delano~1")))

  (defvar *windows-source-filename-root* 
      (find-file-in-dir-list 
       "mds" 
       (possible-paths `((t ,*windows-volumes*)
			 (t ,*windows-program-files-dirs*)))))

  (defvar *windows-wish-location*
      (find-file-in-dir-list
       '("wish" "wish.exe" "wish84.exe" "wish83.exe")
       (possible-paths `((t ,*windows-volumes*)
			 (t ,*windows-program-files-dirs*)
			 (t ,*windows-tcl-dirs*)
			 "bin"
			 ))))

  (defvar *pymol-executable-filename*
      (find-file-in-dir-list
       "pymol.exe"
       (possible-paths `((t ,*windows-volumes*)
			 (t ,*windows-program-files-dirs*)
			 (t ,*windows-pymol-dirs*)
			 "pymol"
			 ))))))


;;;------------------------------------------------------------------- UNIX
;;;                    FILENAMES
;;;-------------------------------------------------------------------

(defvar *unix-source-filename-root*)
;;(setq *unix-source-filename-root* 
;;  (if ;;(and (equal "dsmith" (sys:getenv "USER"))
;;      ;;     (equal "berlin" (sys:getenv "LOCATION")))   ;; does not work...
;;      (equal "dsmith" (sys:getenv "USER"))
;;      "~/"
;;    "~/mds/"))

(setq *unix-source-filename-root* 
  (if (sys:getenv "MDS_ROOT")
      (concatenate 'string (sys:getenv "MDS_ROOT") "/src/mds")
    "~/mds/src/mds"))


;; cmucl needs full path (it seems), leave other with ~ as that is more general for systems that are not /home/dsmith - based
#+:cmu
(setq *unix-source-filename-root* "/home/dsmith/mds/src/mds")   

#+:openmcl
(setq *unix-source-filename-root* "/home/dsmith/mds/src/mds")   


(when (not (boundp '*unix-wish-location*))
  (defvar *unix-wish-location*)
  (setq *unix-wish-location* "wish"))


;;;-------------------------------------------------------------------
;;;                    UNIX/WINDOWS FILENAMES
;;;-------------------------------------------------------------------

(defun uw-wish-location ()
  (if (running-on-windows-p) 
      *windows-wish-location*
    *unix-wish-location*))

;;;-------------------------------------------------------------------
;;;                         USERID
;;;-------------------------------------------------------------------

(defun user-name ()
  (if (running-on-windows-p)
      (sys::getenv "USERNAME")
    (sys:getenv "USER")))


;;;-------------------------------------------------------------------
;;;                    Column basis assumptions
;;;-------------------------------------------------------------------

(defvar *set-col-basis-to-this-value*)
(setq *set-col-basis-to-this-value* nil)

(defvar *set-col-bases-from-max-in-table*)
(setq *set-col-bases-from-max-in-table* t)
(setq *set-col-bases-from-max-in-table* nil)

(defvar *set-col-basis-from-max-titer-in-col*)
(setq *set-col-basis-from-max-titer-in-col* t)
(setq *set-col-basis-from-max-titer-in-col* nil)

(defvar *set-col-basis-from-max-of-max-titer-and-this-titer*)
(setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil)
(setq *set-col-basis-from-max-of-max-titer-and-this-titer* 1280)

;; Ugly hack to not set 1280 for Nicola 
;; (it is taking more of my time to answer her questions on this and keep it in my mind
;;  than it is to just fix it and have this hack)
;; Something like this should really be in a config file, which this file is trying to be, but it changes too often.
(if (member (user-name) '("nsl25" "Nicola Lewis" "mjm88") :test #'equal)
    (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil))


(defvar *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2*)
(setq *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2* t)
(setq *set-col-basis-from-max-of-max-titer-and-1280-and-max-titer-in-all-strains-take-2* nil)

(defvar *set-col-basis-to-homologous-if-exists-otherwise-to-max-of-max-titer-and-1280*)
(setq *set-col-basis-to-homologous-if-exists-otherwise-to-max-of-max-titer-and-1280* t)
(setq *set-col-basis-to-homologous-if-exists-otherwise-to-max-of-max-titer-and-1280* nil)

(defvar *set-col-basis-from-max-of-max-titer-and-1280-plus-this-variable*)
(setq *set-col-basis-from-max-of-max-titer-and-1280-plus-this-variable* 1)
(setq *set-col-basis-from-max-of-max-titer-and-1280-plus-this-variable* nil)


(defun 1280-hack-off ()
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-bases-from-max-in-table* nil)
  (setq *set-col-basis-from-max-titer-in-col* t)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil))
 
(defun 1280-hack-on ()
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-bases-from-max-in-table* nil)
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* 1280))

(defun set-min-col-bases-to-this-HI-titer (hi-titer)
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-bases-from-max-in-table* nil)
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* hi-titer))

(defun column-basis-hack-off ()
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-bases-from-max-in-table* nil)
  (setq *set-col-basis-from-max-titer-in-col* t)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil))
 
(defun column-basis-hack-on (titer)
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-bases-from-max-in-table* nil)
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* titer))

(defun set-col-bases-from-max-in-table ()
  (setq *set-col-basis-to-this-value* nil)
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil)
  (setq *set-col-bases-from-max-in-table* t))

(defun set-col-bases-to-this-logged-value (x)
  (setq *set-col-basis-to-this-value* x)
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil)
  (setq *set-col-bases-from-max-in-table* nil))

(defun set-col-bases-to-this-HI-titer (x)
  (setq *set-col-basis-to-this-value* (std-log-titer x))
  (setq *set-col-basis-from-max-titer-in-col* nil)
  (setq *set-col-basis-from-max-of-max-titer-and-this-titer* nil)
  (setq *set-col-bases-from-max-in-table* nil))


;;;----------------------------------------------------------------------
;;;                 hack change 5 or less to <10
;;;----------------------------------------------------------------------

(defvar *convert-5-or-less-to-lt10*)
(setq *convert-5-or-less-to-lt10* t)

(defun avian-on ()
  (setq *convert-5-or-less-to-lt10* nil))

(defun avian-off ()
  (setq *convert-5-or-less-to-lt10* t))


;;;----------------------------------------------------------------------
;;;                             PYMOL
;;;----------------------------------------------------------------------

;; assume that if not windows, then mac. will need another case for linux
(when (and (not (running-on-windows-p))
	   (not (boundp '*pymol-executable-filename*)))
  (defvar *pymol-executable-filename*
      (cond ((probe-file "/Applications/MacPyMOL.app/Contents/MacOS/PyMOL")    "/Applications/MacPyMOL.app/Contents/MacOS/PyMOL")
	    ((probe-file "/Applications/MacPyMOL.app/Contents/MacOS/MacPyMOL") "/Applications/MacPyMOL.app/Contents/MacOS/MacPyMOL")
	    (t (progn (format t "~2%    >>>>>  Warning: Pymol executable not in an expected place, either install pymol, or edit clinit.cl point to where it is already installed.~2%")
		      "Pymol executable not in an expected place, either install pymol, or edit clinit.cl point to where it is already installed.")))))
    

(defvar *pymol-map-viewer-plugin-filename* (uw-sfnr "pymol/map-viewer-plugin/map-viewer.py"))


;;;-------------------------------------------------------------------
;;;                       WINDOWS/UNIX
;;;-------------------------------------------------------------------

#+:allegro
(load (uw-sfnr "cl/systems.lisp"))
#+:lispworks
(load (uw-sfnr "cl/systems-lispworks.lisp"))
;; on cmucl, the systems are put in the "Registry" directory (we define its pathname above)

(defun make-system (system)
  #+:allegro
  (load-system system :compile t :silent (not *load-verbose*) :no-warn (not *load-verbose*))
  #+:cmu
  (mk:load-system system :compile-during-load t)
  #+:lispworks
  (compile-system system :load t)
  #+:openmcl
  (defsystem-as-file system)
  )

;; this can go when we do a real defsystem for openmcl
#-:openmcl
(make-system :utils)
#+:openmcl
(make-system :cl)

(make-system :mds)

;;(make-system :plaques)
;;(if (equal (user-name) "dsmith")
;;    (make-system :hmpv-dips))

;;#+:lispworks
;;(compile-system :mds :load t)     ;; for some reason the above (make-system :mds) does not work in lispworks


;;(load "~/im/2014/start.lisp")