(in-package user)

(defun bootstrap (data statistic 
		  &optional &key
                            (num-repeats 200)
		            (num-samples-each-repeat (length data)))
  (let ((repeats 
	 (loop for i below num-repeats collect
	       (funcall statistic  (sample-with-replacement num-samples-each-repeat data)))))
    (values
     (sd repeats)
     (av repeats)
     repeats)))

(defun sample-with-replacement (num-samples data)
  (loop for i below num-samples collect
	(random-element data)))

;;note, there is also "random-samples-with-replacement" in cl/misc.lisp