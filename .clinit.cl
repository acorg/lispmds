(let* ((var "MDS_ROOT")
       (dir (or (sys:getenv var) "/usr/local/lispmds")))
  (if (and dir (excl:file-directory-p dir))
      (load (format nil "~a/src/mds/mds/clinit.cl" dir))
    (warn (format nil "Your ~a environment variable is ~a.~%" 
		  var
		  (if dir "not a directory" "undefined")))))
