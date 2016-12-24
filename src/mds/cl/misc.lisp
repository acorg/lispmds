(in-package user)

;;;----------------------------------------------------------------------
;;;                      CONSTANTS
;;;----------------------------------------------------------------------

(defvar *large-double*)
(setq *large-double* 1.0d100)

(defun <epsilon (x &optional (epsilon 0.000001))
  (< x epsilon))


;;;-------------------------------------------------------------------
;;;                       NEWLINE
;;;-------------------------------------------------------------------

(defun newline (&optional (stream t) &key (times 1)) (loop for i below times do (format stream "~%")))


;;;-------------------------------------------------------------------
;;;                       RANDOMNESS
;;;-------------------------------------------------------------------

;;(define (coin &optional (sides 2)) (krandom sides))

(defun coin (&optional (sides 2) generator)
  (krandom sides generator))

(defun random-element (l &optional generator)
  (nth (krandom (length l) generator) l))

(defun random-unique-elements (num-uniques l &optional generator)
  (if (> num-uniques (length l))
      (error "cannot sample ~d unique elements from a list of ~d elements" num-uniques (length l)))
  (collect-uniques num-uniques (^ () (random-element l generator))))

(defun unique-randoms (num-uniques n)
  (collect-uniques num-uniques (^ () (krandom n))))

(defun random-samples-without-replacement (n l &optional generator)
  (random-unique-elements n l generator))

(defun random-samples-with-replacement (n l)
  (loop for i below n collect (random-element l)))

(defun random-in-range (low high)
  (+ (* (knuth-random) (- high low))
     low))

(defun random-on-circumference (r &optional (start 0) (end (* 2 pi)))
  (polar-to-rectangular (list r (random-in-range start end))))
  
(defun remove-random-element (l)
  (butnth (krandom (length l)) l))


;;;-------------------------------------------------------------------
;;;                      PERTURBS
;;;-------------------------------------------------------------------

(defun uniform-perturb (e noise)
  (random-in-range (- e noise) (+ e noise)))

(defun uniform-perturbs (l noise)
  (mapcar (^ (e) (uniform-perturb e noise)) l))

(defun uniform-perturb-proportion (e proportion)
  (uniform-perturb e (* e proportion)))

(defun uniform-perturbs-proportion (l proportion)
  (mapcar (^ (e) (uniform-perturb-proportion e proportion)) l))

(defun one-uniform-perturb (l noise)
  (let ((perturb (krandom (length l))))
    (loop for e in l for i from 0 collect
          (if (= i perturb)
            (uniform-perturb e noise)
            e))))

(defun n-uniform-perturbs (n l noise)
  ;;for ease, don't worry about non-replacement
  (let ((perturbs (loop for i below n collect (krandom (length l)))))
    (loop for e in l for i from 0 collect
          (if (member i perturbs)
            (uniform-perturb e noise)
            e))))


;;;-------------------------------------------------------------------
;;;                      CONVERSIONS
;;;-------------------------------------------------------------------

(define (bool->bit bool) (if bool 1 0))
(define (bit->bool bit) (= 1 bit))


;;;-------------------------------------------------------------------
;;;                       ARITHMETIC
;;;-------------------------------------------------------------------

(defun square (x) (* x x))
(defun half (x) (/ x 2))
(defun double (x) (* 2 x))

(defun sign (x) (if (plusp x) 1 (if (minusp x) -1 0)))

(defun round-to-nearest (r n)
  (* r (round n r)))

(defun quadratic-roots (a b c)
  (let ((root (sqrt (- (* b b) (* 4 a c)))))
    (values 
     (/ (+ (- b) (+ root)) (* 2 a))
     (/ (+ (- b) (- root)) (* 2 a)))))

(defun %age (numerator denominator)
  (* (/ numerator denominator)
     100.0))

(defun add-percentage (percent x)
  (+ (/ (* x percent) 100.0) x))


;;;-------------------------------------------------------------------
;;;                       STATISTICS
;;;-------------------------------------------------------------------

(defun av (l) 
  (let* ((n (length l))
	 (av (float (/ (loop for e in l sum e) n))))
    av))
(defun av-sd (l &optional &key unbias)
  (let* ((n (length l))
	 (av (float (/ (loop for e in l sum e) n)))
         (sd (if (<= n 1)
		 0 ; 'not-defined
	       (sqrt (float (/ (loop for e in l sum (square (- av e))) (if unbias (dec n) n)))))))   ;; corrected to (dec n) 2003-05-31
    (values                                                                                          ;; and back to n 2004-01-13
     av
     sd)))
;; divide by n if population, n-1 if sample (as an unbiasing)
;; for the unbias story http://cuwu.editthispage.com/stories/storyReader$13
;; and http://www.pitt.edu/~wpilib/statfaq/95varqn.html  (n for probability, n-1 for statistics)

(defun av-sd-sem (l &optional &key unbias) 
  (let* ((n (length l))
	 (av (float (/ (loop for e in l sum e) n)))
         (sd (if (<= n 1)
		 'not-defined
	       (sqrt (float (/ (loop for e in l sum (square (- av e))) (if unbias (dec n) n))))))   ;; note we do the n-1 here for the SD
	 (sem (if (<= n 1)
		  'not-defined
		(/ sd (sqrt n)))))
    (values
     av
     sd
     sem)))
(defun sd (l &optional &key unbias)
  (multiple-value-bind (av sd)
      (av-sd l :unbias unbias)
    (values sd av)))
(defun sem (l &optional &key unbias)
  ;; the standard error of the mean
  (/ (sd l :unbais unbias) (sqrt (length l))))
    
(defun average (&rest args)
  (av args))


(defun weighted-av (weighted-list)
  (float
   (/ (apply #'+ (map-apply #'* weighted-list))
      (apply #'+ (nths 1 weighted-list)))))


(defun md (args)
  (nth (dec (ceiling (/ (length args) 2)))
       (my-sort args)))

(defun median (&rest args)
  (nth (dec (ceiling (/ (length args) 2)))
       (my-sort args)))

(defun median-of-list (args)
  (nth (dec (ceiling (/ (length args) 2)))
       (my-sort args)))

(defun sd-of-a-proportion (proportion n)
  (/ (sqrt (* proportion (- 1 proportion)))
     (sqrt n)))

(defun num-sds-from-average (x av sd)
  ;;same function (other than the 7777777 in statistics.lisp
  ;;  name there is standard-deviation-difference
  ;;  name here is better
  (if (zerop sd)
      7777777  ;;no variation in the data
    (/ (- x av)
       sd)))

(defun av-nths (data)
  (mapcar #'av (apply-transpose data)))

(defun sd-nths (data)
  (mapcar #'sd (apply-transpose data)))

(defun av-hist (hist)
  (let ((sum 0)
        (occurences 0))
    (loop for (value num-with-value) in hist do
          (setq sum (+ sum (* value num-with-value)))
          (setq occurences (+ occurences num-with-value)))
    (float (/ sum occurences))))

(defun av-sd-hist (hist)
  (let* ((av (av-hist hist))
         (sum 0)
         (occurences 0)
         var
         sd)
    (loop for (value num-with-value) in hist do
          (setq sum (+ sum (* num-with-value (square (- av value)))))
          (setq occurences (+ occurences num-with-value)))
    (setq var (float (/ sum occurences)))
    (setq sd (sqrt var))
    (values av sd)))

(defun hists-above (hist-list &optional (n 1))
  (collect (^ (hist-element) (> (nth 1 hist-element) n)) hist-list))


(defun geometric-av (l) 
  (exp (/ (loop for e in l sum (log e))
	  (length l))))


(defun rms (l)
  (sqrt (loop for e in l sum (square e))))


;;;-------------------------------------------------------------------
;;;                       STRINGS
;;;-------------------------------------------------------------------

#-:lispworks
(define (string-append &rest strings)
    (apply #'concatenate 'string strings))

(defun string-subst (from to string)
  ;;(string-subst #\( #\{ (format nil "~a" '(1 2 (3) (4))))
  (let ((new-string (make-string (length string))))
    (loop for i below (length string) do
	  (setf (aref new-string i)
	    (if (eql (aref string i) from)
		to
	      (aref string i))))
    new-string))

(defun substring (string start &optional (end (dec (length string))))
  (if (> start (length string))
    (error "start requested is beyond end of string"))
  (let ((ans (make-string (inc (- end start)))))
    (loop for source from start to end
          for destination from 0 do
          (setf (aref ans destination) (aref string source)))
    ans))

(defun substring-after-char (char string &optional (final nil))
  (let ((p (position char string :from-end final)))
    (if p 
	(substring string (inc p))
      nil)))


(defun substring-before-char (char string)
  (let ((p (position char string)))
    (if p 
	(substring string 0 (dec p))
      nil)))

(defun number->string (x)
  (format nil "~d" x))

(defun anything->string (x) ;; &optional &key use~a)
  ;; this listp test is so we get everything on one line, not split over many when long list
  (if (listp x)
      (format nil "(~{~a~#[~:; ~]~})" (mapcar #'anything->string x))
    (if (or (keywordp x) (stringp x))
	(format nil "~s" x)   ;; to get "'s escaped
      (format nil "~a" x))))

(defun string->number (string)
  (read-from-string string))

(defun string-member (substring string)
  (let ((substring-length (length substring)))
    (loop for i below (- (length string) (dec substring-length)) do
	  (if (string= substring (substring string i (+ i (dec substring-length))))
	      (return i)))))

(defun char-member (char string)
  (loop for i below (length string) do
	(if (eql char (aref string i))
	    (return i))))



(defun substring-after-string (part full)
  (let ((p (string-member part full)))
    (if p 
	(substring full (+ p (length part)))
      nil)))

(defun substring-before-string (part full)
  (let ((p (string-member part full)))
    (if p 
	(substring full 0 (inc (- p (length part))))
      nil)))



(defun string-subst-string (from to string &optional &key all-occurrences)
  ;;this works on from and to being strings, string-subst works on from and to being characters
  (let ((string-member (string-member from string)))
    (if string-member
	(string-append (substring string 0 (dec string-member)) 
		       to
		       (if all-occurrences
			   (string-subst-string
			    from
			    to
			    (substring string (+ string-member (length from)))
			    :all-occurrences t)
			 (substring string (+ string-member (length from)))))
      string)))

(defun string-subst-string-in-tree (from to tree)
  (cond ((null tree) nil)
	((atom tree) (if (stringp tree)
			 (string-subst-string from to tree)
		       (read-from-string (string-subst-string from to (format nil "~a" tree)))))
	(t (cons (string-subst-string-in-tree from to (car tree))
		 (string-subst-string-in-tree from to (cdr tree))))))
	
(defun underscore-to-char (string)
  (string-subst #\- #\_ string))


(defun remove-free-periods-from-string (string)
  (if (= 0 (length string))
      string
    (chars-to-string
     (cons
      (aref string 0)
      (loop for (previous this next) on (explode-string nil string) 
	  when (not (or (null this)                  ;; end of list
			(and (eql #\space previous)  ;; string has " . " in it 
			     (eql #\. this)          
			     (eql #\space next))
			(and (eql #\` previous)  ;; string has " `.' " in it 
			     (eql #\. this)          
			     (eql #\' next))
			(and (eql #\. this)       ;; string has two dots together   
			     (eql #\. next))
			(and (eql #\. this)          ;;string has a dot before a paren
			     (eql #\) next))
                        (and (eql #\' previous)  ;; string has " '.' " in it 
			     (eql #\. this)          
			     (eql #\' next))
			(and (eql #\space previous)   ;; string ends in .
			     (eql #\. this)
			     (null next))))   
	  collect this)))))

(defun space-delimited-string-to-list-replacing-colon-with-asterix-and-comma-with-space-semicolon-with-space-and-removing-free-periods-unparen-intercept (string)
  (mapcar 
   (^ (e)
      (if (or (equal '(INTERCEPT) e)
	      (equal "(Intercept)" e))
	  'intercept
	e))
   (string-to-atoms 
    (remove-free-periods-from-string (string-subst #\; #\space (string-subst #\, #\space (string-subst #\: #\* string))))
    :add-matching-opening-and-closing-parens-if-necessary t)))

(defun space-delimited-string-to-list-replacing-colon-with-dash-and-comma-with-space-semicolon-with-space-and-removing-free-periods (string)
  (string-to-atoms (remove-free-periods-from-string (string-subst #\; #\space (string-subst #\, #\space (string-subst #\: #\- string))))))
  
(defun space-delimited-string-to-list-replacing-colon-with-dash-and-comma-with-space (string)
  (space-delimited-string-to-list-replacing-colon-with-dash (string-subst #\, #\space string)))

(defun space-delimited-string-to-list-replacing-colon-with-dash (string)
  (space-delimited-string-to-list (string-subst #\: #\- string)))

(defun space-delimited-string-to-list-replacing-comma-with-space (string)
  (space-delimited-string-to-list (string-subst #\, #\space string)))

(defun space-delimited-string-to-list-replacing-comma-with-underscore (string)
  (space-delimited-string-to-list (string-subst #\, #\_ string)))

(defun space-delimited-string-to-list (string &optional &key leave-elements-as-strings)
  (if leave-elements-as-strings
      (space-delimited-string-to-list-of-strings string)
    (string-to-atoms string)))

(defun space-delimited-string-to-list-of-strings (string)
  (let ((position-of-first-space (char-member #\space string)))
    (if position-of-first-space
        (cons 
         (substring string 0 (dec position-of-first-space))
         (space-delimited-string-to-list-of-strings (substring string (inc position-of-first-space))))
      (list string))))

(defun char-substs-in-string (string char-substs)
  (if (null char-substs)
      string
    (char-substs-in-string 
     (string-subst (nth 0 (car char-substs)) (nth 1 (car char-substs)) string) 
     (cdr char-substs))))

(defun string-to-symbol-then-trailing-string (string)
  (let ((substring-before-first-space (substring-before-char #\space string))
	(substring-after-first-space  (substring-after-char  #\space string)))
    (list (read-from-string substring-before-first-space)
	  (string-left-trim '(#\space) substring-after-first-space))))

(defun add-matching-opening-and-closing-parens-if-necessary-to-string (string)
  (let ((num-opening-parens (loop for i below (length string)
				when (eql #\( (aref string i))
				sum 1))
	(num-closing-parens (loop for i below (length string)
				when (eql #\) (aref string i))
				sum 1)))
    (if (> num-opening-parens num-closing-parens)
	(string-append string (format nil "~{~a~}" (loop for i below (- num-opening-parens num-closing-parens) collect ")")))
      (if (> num-closing-parens num-opening-parens)
	  (string-append (format nil "~{~a~}" (loop for i below (- num-closing-parens num-opening-parens) collect "(")) string)
	string))))

(defun string-to-atoms (string &optional &key add-matching-opening-and-closing-parens-if-necessary)
  (read-from-string 
   (format nil "(~a)" 
	   (if add-matching-opening-and-closing-parens-if-necessary
	       (add-matching-opening-and-closing-parens-if-necessary-to-string string)
	     string))))

#|
;; replaces the above, but not needed, so leave as above
(defun wrap-string-with-verticals-if-contains-parens (string)
  (if (or (string-member "(" string)
          (string-member ")" string))
      (format nil "|~a|" string)
    string))

(defun string-to-atoms (string)
  (setq string (string-trim '(#\space) (string-subst #\tab #\space string)))
  (if (substring-after-char #\space string)
      (cons (read-from-string (wrap-string-with-verticals-if-contains-parens (substring-before-char #\space string)))
            (string-to-atoms (substring-after-char #\space string)))
    (list (read-from-string (wrap-string-with-verticals-if-contains-parens string)))))
|#

(defun list-to-string (l &optional &key (spacer ""))
  ;; should really not have the last one
  ;;(format nil "~{~a~a~}" (loop for e in l collect (list e spacer)))  ;; breaks going from acl6.2 to acl7.0
  (format nil "~{~a~}" (loop for e in l append (list e spacer))))


(defun char-to-symbol (char)
  (read-from-string (format nil "~c" char)))

(defun chars-to-symbol (chars)
  (read-from-string (format nil "~{~c~}" chars)))

(defun chars-to-string (chars)
  (format nil "~{~c~}" chars))


;;;-------------------------------------------------------------------
;;;                     DESTRUCTIVE CONSING
;;;-------------------------------------------------------------------

#-:lispworks
(defmacro push-end (item list)
  `(progn
     (if (null ,list)
       (push ,item ,list)
       (setf (cdr (last ,list)) (list ,item)))
     ,list))

;;;-------------------------------------------------------------------
;;;                     HISTOGRAMMING
;;;-------------------------------------------------------------------

(defun num-occurences (e l &key (test #'eql))
  (loop for ee in l when (funcall test e ee) sum 1))

;;(defun hist (l &key (test #'eql))
;;  (if (not (null l))
;;    (cons (list (car l) (num-occurences (car l) l :test test))
;;          (hist (remove (car l) l :test test) :test test))))

;;this takes about the same amount of cpu time as the one above, but
;;does very little consing, versus a chunk above, so it is about 2x quicker
;;on a 100k list.
(defun hist (l &key (test #'eql) as-proportion)
  (let (car-hist hist)
    (loop for e in l do
	  (let ((position (position e car-hist :test test)))
	    (if position
		(setf (cadr (nth position hist)) (inc (cadr (nth position hist))))
	      (progn (push e car-hist) 
		     (push (list e 1) hist)))))
    (if as-proportion
        (let ((length (length l))) (loop for (e count) in hist collect (list e (float (/ count length)))))
      hist)))

(defun sort-hist (l &optional (sort-f (^ (a b) (< (car a) (car b)))) &key (test #'eql) lower upper fill as-proportion)
  ;;problem here is that if i want to use the &keys, then I need to give the sort-f
  ;;maybe I should make the sort-f an &key too
  (let ((sort-hist (sort (hist l :test test :as-proportion as-proportion) sort-f)))
    (if (or lower upper fill)
	(let ((lower (or lower (apply-min (nths 0 sort-hist))))
	      (upper (or upper (apply-max (nths 0 sort-hist)))))
	  (loop for i from lower to upper collect
		(list i (if (assoc i sort-hist)
			    (assoc-value-1 i sort-hist)
			  0))))
      sort-hist)))

(defun hist-most-common-sort (l &key (test #'eql) as-proportion)
  (reverse (sort-nth 1 (hist l :test test :as-proportion as-proportion))))

(defun hist-length (hist)
  (loop for e in hist sum (second e)))

(defun hist-min (hist)
  (loop for e in hist minimize (first e)))

(defun hist-max (hist)
  (loop for e in hist maximize (first e)))

(defun hist-to-proportion-hist (hist &key dps is-cumulative-list)
  (let ((total-occurrences (if is-cumulative-list (nth 1 (car (last hist))) (apply-+ (nths 1 hist)))))
    (loop for (entry num-occurrences) in hist collect
	  (list entry (if dps
			  (dps (float (/ num-occurrences total-occurrences)) dps)
			(float (/ num-occurrences total-occurrences)))))))

(defun most-common-element (l)
  (let ((element-count (car (last (sort-nth 1 (hist l))))))
    (values
     (nth 0 element-count)
     (if (numberp (nth 1 element-count))  ;; if l is empty, still return 0 here
	 (nth 1 element-count)
       0))))


;;;-------------------------------------------------------------------
;;;                      SMOOTHING
;;;-------------------------------------------------------------------

#|
;; superseded by the below to be more efficient on long lists with long windows
(defun smooth-list (l window-half-size &optional (composition-f (^ (&rest args)
								   (float (/ (apply #'+ args)
									     (length args))))))  ;;also works nicely with #'+ and #'append
  (loop for center from window-half-size below (- (length l) window-half-size) collect
	(apply composition-f
	       (loop for i from (- center window-half-size) to (+ center window-half-size) collect
		     (nth i l)))))
|#

;;#|
;; this is a reimplementation of the above to be faster on long lists with long windows
;; now superseded by the variable index code
(defun smooth-list-old (l window-half-size &optional (composition-f (^ (&rest args)
								       (float (/ (apply-+ args)
										 (length args))))))  ;;also works nicely with #'+ and #'append
  (let ((sliding-window (cons 'dummy-start (firstn (* 2 window-half-size) l))))
    (loop for next in (nthcdr (* 2 window-half-size) l) collect
	  (apply composition-f 
		 (setq sliding-window 
		   (push-end next (cdr sliding-window)))))))
;;|#

;; allow the window size to increase (for log plots)
;; there is an error here with the window one too small (compared to the above)
(defun smooth-list-variable-size-window (l window-half-size &key (composition-f (^ (&rest args)
										   (float (/ (apply-+ args)
											     (length args)))))  ;;also works nicely with #'+ and #'list
								 increment-window-size-every)  ;; either nil or a number
  (let ((sliding-window (firstn (* 2 window-half-size) l)))
    (loop for next in (nthcdr (* 2 window-half-size) l) 
	for i from 0 collect
	  (apply composition-f 
		 (setq sliding-window 
		   (let ((ugh (setq sliding-window
				(if (and increment-window-size-every
					 (zerop (mod i increment-window-size-every)))
				    sliding-window
				  (cdr sliding-window)))))
		     (push-end 
		      next
		      ugh)))))))

#|
;; this is just used below, i can remove it
(defun consecutivep (l)
  (if (not (null (filter #'numberp l)))
      (error "expected all numbers in ~a, but got some non-numbers" l))
  (loop for (previous next) on l
      until (null next)
      do (if (not (= next (inc previous)))
	     (return nil))
      finally (return t)))
|#
;; this is just use a little, i can depreciate it
(defun smooth-indexed-list (l window-half-size &optional (composition-f (^ (&rest args)
									   (float (/ (apply-+ args)
										     (length args))))))  ;;also works nicely with #'+ and #'append
  (error "depreciated function, use smooth-list"))
;;  "Like smooth-list but smooth the y-coord, checking the x-coords are consecutive"
;;  (if (not (consecutivep (nths 0 l)))
;;      (error "x-coords are not consective: ~a" (nths 0 l)))
;;  (transpose
;;   (nthcdr window-half-size
;;	   (butlastn window-half-size
;;		     (nths 0 l)))
;;   (smooth-list (nths 1 l) window-half-size composition-f)))
	   

;; this can just be smooth-list, and can take x into account if we are passed pairs
(defun smooth-list (l window-half-size 
		    &key (composition-f (^ (&rest args)
					   (float (/ (apply-+ args)
						     (length args)))))  ;;also works nicely with #'+ and #'list
			 increment-window-size-every)
  (if increment-window-size-every
      (error "incrementing window size is depereciated, use smooth-list-variable-size-window if not using variable x-values"))
  (let ((full-window-size (* 2 window-half-size))
	(x-values (if (listp (car l)) (nths 0 l) (series 0 (dec (length l)))))
	(y-values (if (listp (car l)) (nths 1 l) l))
	x-values-in-window
	y-values-in-window
	reversed-smoothed-xy-values)
    (loop for x-value in x-values
	  for y-value in y-values do
	  (progn
	    (push-end x-value x-values-in-window)
	    (push-end y-value y-values-in-window)
	    ;;(print (list x-values-in-window y-values-in-window))
	    (loop for i below (length x-values-in-window) 
		until (> full-window-size (- x-value (car x-values-in-window)))
		      ;; window size >= threshold
		      ;; do smoothing and remove entries until the window size is below threshold
		do (progn
		     (push (list (av x-values-in-window)
				 (apply composition-f y-values-in-window))
			   reversed-smoothed-xy-values)
		     (setq x-values-in-window (cdr x-values-in-window))
		     (setq y-values-in-window (cdr y-values-in-window))))))
    (if (listp (car l))
	(reverse reversed-smoothed-xy-values)
      (nths 1 (reverse reversed-smoothed-xy-values)))))
	  
#|
(smooth-list-old '(0 1 2 3 4 5 6 7) 0)
(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0)
(smooth-list '(0 1 2 3 4 5 6 7) 0)
(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0)

(smooth-list '(0 1 2 3 4 5 6 7) 1)
(1.0 2.0 3.0 4.0 5.0 6.0)
(smooth-list-old '(0 1 2 3 4 5 6 7) 1)
(1.0 2.0 3.0 4.0 5.0 6.0)

(smooth-list '((1 3) (2 4) (3 5) (5 7)) 0.6)
((2.0 4.0) (3.3333333 5.3333335) (4.0 6.0))

(smooth-list '((1 3) (2 4) (3 5) (4 6) (5 7)) 0.6)
((2.0 4.0) (3.0 5.0) (4.0 6.0))

(smooth-list '((1 3) (2 4) (3 5) (4 6) (5 7)) 0.2)
((1.5 3.5) (2.5 4.5) (3.5 5.5) (4.5 6.5))

(smooth-list (firstn 500 s00) 3)
(2772.5715 2808.2856 3154.0 3214.8572 2775.5715 3203.5715 2936.2856 2955.0 3194.7144 2927.0 ...)
USER(93): (smooth-list-old (firstn 500 s00) 3)
(2772.5715 2808.2856 3154.0 3214.8572 2775.5715 3203.5715 2936.2856 2955.0 3194.7144 2927.0 ...)
USER(94): (equal * **)
T
|#  


(defun average-indexed-lists-to-be-smoothed (ll)
  (my-sort (apply-append ll) (^ (a b) (< (nth 0 a) (nth 0 b)))))


(defun average-gridded-lists (ll)
  (let ((xy-hash (make-hash-table)))
    (loop for l in ll do
	  (loop for (x y) in l do
		(let ((gethash (gethash x xy-hash)))
		  (if gethash
		      (setf (gethash x xy-hash) (cons y gethash))
		    (setf (gethash x xy-hash) (list y))))))
    (sort-car
     (mapcar (^ (x-ys) (list (car x-ys) (av (cadr x-ys)))) (listhash xy-hash)))))



;;;-------------------------------------------------------------------
;;;                     FIND CLOSEST
;;;-------------------------------------------------------------------

(defun find-closest (e l &key (comparison-f (^ (a b) (abs (- a b)))))
  (let* ((closest-element-so-far (car l))
	 (closest-distance-so-far (funcall comparison-f e (car l)))
         (closest-element-position-so-far 0))
    (loop for el in (cdr l) for p from 1 do
          (if (< (funcall comparison-f e el) closest-distance-so-far)
            (progn (setq closest-distance-so-far (funcall comparison-f e el))
                   (setq closest-element-so-far el)
		   (setq closest-element-position-so-far p))))
    (values
     closest-element-so-far
     closest-distance-so-far
     closest-element-position-so-far)))


;;;-------------------------------------------------------------------
;;;                       HASH TABLES
;;;-------------------------------------------------------------------

(defun showhash (hash-table)
  (maphash (^ (key value) (print (list key value))) hash-table))

(defun listhash (hashtable)
  (let (list)
    (maphash (^ (&rest l) (setq list (cons l list))) hashtable)
    (reverse list)))

;;;-------------------------------------------------------------------
;;;                           PROABAILITY
;;;-------------------------------------------------------------------

(defun ! (n)
  (if (= (/ -1 2) n)
      (sqrt pi)
    (if (minusp n)
	(error "negative factorial")
      (if (integerp n)
	  (let ((product 1))
	    (loop for i from 1 to n do (setq product (* i product)))
	    product)
	(if (= (/ 1 2) (mod n 1))
	    (let ((product (sqrt pi)))
	      (loop for i from (/ 1 2) to n do (setq product (* i product)))
	      product)
	  (error "only the fraction n+1/2 are supported"))))))

(defun s! (n)
  (* (expt n (+ n 0.5)) (exp (- n)) (sqrt (* 2 pi))))

;;(defun ! (n)       ;maybe do some sterlings here
;;  (if (minusp n)
;;    (error "negative factorial")
;;    (let ((product 1))
;;      (loop for i from 1 to n do (setq product (* i product)))
;;      product)))
       
(define (nCr n r)
  ;;(print "choose is better than nCr")
  (if (minusp r)
    0   ;;correct?
    (if (> r n)
      0  ;;correct?
      (/ (! n) (* (! (- n r)) (! r))))))

(define (nM n &rest ns)
  (if (not (= n (apply #'+ ns)))
    (error "Multinomial n's must sum to N"))
  (/ (! n) (apply #'* (mapcar #'! ns))))

(define (nM-1 n &rest ns)
    (apply #'nM n (- n (apply #'+ ns)) ns))

(defun expt-falling (m k)
  (let ((product 1))
    (loop for i below k do
	  (setq product (* product (- m i))))
    product))

(defun choose (r k)
  (if (not (integerp k))
      (error "choose requires integer k but called with ~a" k)
    (if (< k 0)
	0
      (/ (expt-falling r k) (! k)))))

(defun choose-n-1 (n &rest ns)
  (apply #'* 
	 (loop for i from 0 below (length ns) collect
	       (choose (apply #'- n (or (firstn i ns) (list 0))) (nth i ns)))))

(defun choose-n (n &rest ns)
  (if (not (= n (apply #'+ ns)))
    (error "Multinomial n's must sum to N"))
  (apply #'choose-n-1 n (cdr ns)))

;;(defun choose (r k)
;;  (/ (falling-factorial r k)
;;     (! k)))

(define (perms l)
  (case (length l)
    ((0 1) (list l))
    (t (map-union 
        (^ (e)
           (mapcar 
            (^ (pl)
               (cons e pl))
            (perms (remove e l :count 1))))
        l))))

#|
(perms '())
(perms '(0))
(perms '(0 1))
(perms '(0 1 2))
(perms '(0 1 2 3))
(perms '(a a b))
|#

(define (combs k S)
  (let ((size (length S)))
    (if (= k 1)
      (mapcar #'list S)
      (if (= k size)
        (list S)
        (loop for i from 0 to (- size k) append
              (loop for l in (combs (dec k) (nthcdr (inc i) S)) collect
                    (cons (nth i S) l)))))))

#|
(combs 1 '(0 1 2))
(combs 2 '(0 1 2))
(combs 2 '(0 1 2 3 4))
(combs 3 '(0 1 2 3 4))
(combs 3 '(0 1 2 3 4 5))
|#

(defun power-set (set)
  (if (null set)
      (list set)
    (let ((p (power-set (cdr set))))
      (append p
	      (mapcar (^ (x) (cons (car set) x)) p)))))

;;;-------------------------------------------------------------------
;;;                          RANDOM VARIABLES
;;;-------------------------------------------------------------------

(defun binomial-term (n p i)
  (* (choose n i) (expt (coerce p 'long-float) i) (expt (- 1 (coerce p 'long-float)) (- n i))))

(defun binomial-pd (n p)
  (loop for i from 0 to n collect (binomial-term n p i)))

(defun binomial-series (n p)
  (mapcar (^ (e) (round (* (expt 2 n) e))) (binomial-pd n p)))

(defun poisson-term (mu i)
  (/ (* (exp (- mu)) (expt (coerce mu 'long-float) i))
     (coerce (! i) 'long-float)))

(defun poisson-term-long (mu i)
  (* (exp (- (coerce mu 'long-float)))
     (apply #'* (loop for ii from 1 to i collect (/ mu ii)))))

(defun geometric-term (p i)
  (setq p (coerce p 'long-float))
  (* (expt (- 1 p) i) p))



;;;-------------------------------------------------------------------
;;;                 CUMULATIVE DISTRIBUTION FUNCTIONS
;;;-------------------------------------------------------------------

#|
;;;no longer needed 
(defun poisson-cdf (mu i)
  ;;there is an incremental version that is more complex, but I could use
  (loop for j from 0 to i sum
	(poisson-term mu j)))

(defun geometric-cdf (p i)
  (loop for j from 0 to i sum
	(geometric-term p j)))
|#

(defun make-cdf-inverse (distribution-term-f parameter max)
  (let ((cdf-inverse (list (funcall distribution-term-f parameter 0))))
    (loop for i from 1 do
	  (push (+ (car cdf-inverse) 
		   (funcall distribution-term-f parameter i)) cdf-inverse)
	  (if (> (car cdf-inverse) max)
	      (return (reverse cdf-inverse))))))

(defun make-poisson-cdf-inverse (mu max)
  (make-cdf-inverse #'poisson-term mu max))

(defun make-poisson-cdf-inverse-long (mu max)
  (make-cdf-inverse #'poisson-term-long mu max))

(defun make-geometric-cdf-inverse (p max)
  (make-cdf-inverse #'geometric-term p max))

(defun make-binomial-cdf-inverse (n p max)
  (make-cdf-inverse (^ (n-p-list i) (binomial-term (nth 0 n-p-list) (nth 1 n-p-list) i))
		    (list n p)
		    max))

(defun random-from-inverse-cdf (inverse-cdf &optional generator)
  (let ((random-uniform (knuth-random generator)))
    (loop for x in inverse-cdf
	for i from 0 do
	  (if (< random-uniform x)
	      (return i))
	finally (progn 
		  (print "exceeded precalculated cdf")
		  (return (length inverse-cdf))))))


(defun accumulate (l)
  (let ((so-far 0))
    (loop for e in l collect (setq so-far (+ so-far e)))))

(defun accumulate-ys (xys)
  (let ((so-far 0))
    (loop for (x y) in xys collect (list x (setq so-far (+ so-far y))))))


;;;-------------------------------------------------------------------
;;;                           PRETTY PRINTING
;;;-------------------------------------------------------------------

(defun pp (item)
  (cond ((arrayp item)
         (case (array-rank item)
           (1 (loop for i below (array-dimension item 0) do 
                    (format t "~5a" (aref item i))))
           (2 (loop for i below (array-dimension item 0) do
                    (newline)
                    (loop for j below (array-dimension item 1) do
                          (format t "~5a" (aref item i j)))))
           (t "do not know how to handle arrays other than ranks 1 or 2")))
        ((and (listp item) (floatp (car item)))
         (mapcar (^ (f) (read-from-string (format nil "~5,2f" f))) item))
        ((floatp item)
         (let ((pp (read-from-string (format nil "~6,3f" item))))
           (if (not (equal 0.0 pp))
             pp
             "too small for pp")))
	((and (listp item) (listp (car item)))
         (format t "~{~a~%~}" (mapcar #'pp item)))
        ((listp item)
         (format t "~{~a~%~}" item))
        (t "Don't know how to pp this type")))

(defun ppl (l &optional stream)
  (loop for e in l do 
	(if stream
	    (format stream "~a~%" e)
	  (print e)))
  l)

(defun ppll (l)
  (ppl (list l))
  l)

(defun fll (ll &optional &key (stream t) use~s filename (if-exists :error) always-right-justify preamble postamble separate-with-single-space-p)
  (if filename
      (setq stream (open filename :direction :output :if-exists if-exists)))
  (if preamble (format stream (format nil "~~~a~%" (if use~s "s" "a")) preamble))
  (if (null ll)
      nil
    (let* ((tll (apply-transpose ll))
	   (lengths (loop for tl in tll collect
			  (let ((max-length (apply-max (mapcar (^ (e) (length (anything->string e))) tl))))
			    (if (and use~s (stringp (car tl)))
				(+ 2 max-length)
			      max-length))))
	   (format-strings (loop for length in lengths for tl in tll collect
                                 (if separate-with-single-space-p
                                     (format nil "~~~a" (if use~s "s" "a"))
                                   (let ((right-justify (or always-right-justify
                                                            (and tl     ;; right justify if there are a majority of numbers
                                                                 (> (/ (length (collect #'numberp tl))
                                                                       (length tl))
                                                                    0.5)))))
                                     (format nil "~~~d~a~a" length (if right-justify "@" "") (if use~s "s" "a")))))))
      (loop for l in ll do
	    (let ((line (format nil (if separate-with-single-space-p "~{~a ~}" "~{~a  ~}")
				(loop for e in l 
				    for format-string in format-strings collect
				      (format nil format-string (if (listp e) (anything->string e) e))))))
	      ;; trimming the trailing space is ascthetic for when large differneces in line length
	      (format stream "~a~%" (string-right-trim '(#\space) line))))))  
  (if postamble (format stream (format nil "~~~a~%" (if use~s "s" "a")) postamble))
  (if filename
      (close stream))
  ll)

(defun csvll (ll &optional &key (stream t) use~s filename (if-exists :error))
  ;; use "" to make a blank field
  (if filename
      (setq stream (open filename :direction :output :if-exists if-exists)))
  (loop for l in ll do
	(format stream (format nil "~~{~~~a~~#[~~:;,~~]~~}~~%" (if use~s "s" "a")) l))
  (if filename
      (close stream))
  ll)
  
;; does not work, no commas
;;(defun write-ll-as-csv (ll filename &optional &key (if-exists :error))
;;  (fi (csvll ll)
;;      filename
;;      if-exists
;;      nil
;;      :write-outer-list-elements-individually t
;;      :write-inner-list-elements-individually nil
;;      :include-newline t))

(defun read-csv-file-into-ll (filename &optional &key simple-csv leave-elements-as-strings)
  (fi-in-readline filename 
                  :line-process-f (^ (line) (csv-to-list line :simple-csv simple-csv :leave-elements-as-strings leave-elements-as-strings))))

(defun csv-fi-in-s (filename &optional &key simple-csv leave-elements-as-strings) 
  (read-csv-file-into-ll filename :simple-csv simple-csv :leave-elements-as-strings leave-elements-as-strings))
  
(defun csv-file-p (filename)
  (let ((read (fi-in filename)))
    (and (not (listp read))
	 (substring-after-char #\, (string read)))))
  


(defun dps (x dps)
  (let ((rounder (expt 10.0 dps)))
    (/ (round (* x rounder)) rounder)))

(defun dps-tree (tree dps)
  (cond ((null tree) nil)
	((atom tree) (dps tree dps))
	((listp tree) (cons (dps-tree (car tree) dps) (dps-tree (cdr tree) dps)))
	(t (error "unexpected datatype"))))
	 

(defun 2dp (x)
  (float (/ (round (* x 100))
	    100)))

(defun 2dps (l) (mapcar #'2dp l))
(defun 2dps-when-numeric (l) (mapcar (^ (x) (if (numberp x) (2dp x) x)) l))

(defun 1dp (x)
  (dps x 1))


(define (inc x) (+ 1 x))
(define (dec x) (- x 1))

(define (equal-n &rest args)
  (loop for n below (dec (length args))
        when (not (equal (nth n args) (nth (inc n) args)))
        do (return nil)
        finally (return t)))

        
#|
;;i need to think this out some more
(define (make-nary f zero)  ;;how do i do a unit?
  (let ((self nil))
    (setf self 
          (^ (&rest args)
             (cond ((= 0 (length args)) zero)
                   ((= 1 (length args)) (car args))    ;;really should handle unit here
                   ((= 2 (length args)) (apply f args))
                   (t (funcall self (car args) (funcall self (cdr args)))))))))
||#

(defun month-number-to-name (n)
  (case n
    ( 1 "January")
    ( 2 "February")
    ( 3 "March")
    ( 4 "April")
    ( 5 "May")
    ( 6 "June")
    ( 7 "July")
    ( 8 "August")
    ( 9 "September")
    (10 "October")
    (11 "November")
    (12 "December")
    (t (error "expected month number in range 1 to 12 but got ~a~%" n))))


;;;----------------------------------------------------------------------
;;;                           LISTS
;;;----------------------------------------------------------------------

(defun firstn (n l)
  (loop for e in l for i below n collect e))

(defun firstn-proportion (proportion l &optional (rounding-function #'round))
  (firstn (funcall rounding-function (* proportion (length l))) l))

(defun lastn (n l)
  (reverse (firstn n (reverse l))))

(defun butlastn (n l)
  (reverse (nthcdr n (reverse l))))

(defun butnth (n l)
  (append (firstn n l) (nthcdr (inc n) l)))

(defun elements-in-range (low high l)
  (firstn (inc (- high low)) (nthcdr low l)))

(defun interleave (l1 l2)
  (loop for e1 in l1 for e2 in l2 append (list e1 e2)))

(defun nary-interleave (&rest ls)
  (apply-append (apply-transpose ls)))

(defun n-of (n e)
  (loop for i below n collect e))

(defun multiple-nth (ns l)
  (loop for n in ns collect (nth n l)))

(defun multiple-butnth (ns l)
  (loop for i below (length l) 
      for e in l 
      when (not (member i ns))
      collect e))
	
(defun nth-range (m n l)
  (firstn (inc (- n m)) (nthcdr m l)))

(defun nths (n ll)
  (loop for l in ll collect (nth n l)))

(defun butnths (n ll)
  (loop for l in ll collect (butnth n l)))

(defun multiple-nths (ns l)
  (mapcar (^ (l) (multiple-nth ns l)) l))

(defun firsts (l) (nths 0 l))

(defun lastns (n ll)
  (loop for l in ll collect (lastn n l)))

(defun first-half (l)
  (if (not (evenp (length l)))
      (error "List must be even"))
  (firstn (/ (length l) 2) l))

(defun second-half (l)
  (if (not (evenp (length l)))
      (error "List must be even"))
  (nthcdr (/ (length l) 2) l))

(defun nthcdrs (n ll)
  (loop for l in ll collect (nthcdr n l)))

(defun enths (n l &optional (start 0))
  ;;collect every nth element of a list starting with the startingth element
  (loop for i below (length l)
      for e in (nthcdr start l)
      when (zerop (mod i n))
      collect e))

(defun positions (e l &key (test #'eql))
  ;; like position, but returns all positions
  (loop for i below (length l)
      for el in l
      when (funcall test e el)
      collect i))

(defun all-equal-p (l &optional &key (test #'equal))
  (let ((head (car l)))
    (loop for e in (cdr l)
	when (not (funcall test head e))
	do (return nil)
	finally (return t))))

(defun transpose (&rest lists)
  ;;this is v. slow on a 3 lists of length 6000 (~30seconds)
  (if (not (all-equal-p (mapcar #'length lists) :test #'equal))
      (error "All lists are not equal length"))
  ;;(loop for i below (length (car lists)) collect (nths i lists)))
  ;;this mapcar is much faster than the loop above (because of the nths above)
  ;;on 2 lists of 6000 50ms vs 2000ms (on vaio) (i guess the timing above was old!
  (if (null lists)
      nil
    (apply #'mapcar (^ (&rest args) (apply #'list args)) lists)))

(defun flatten (ll)
  (if (null ll)
      nil
    (if (atom ll)
	(list ll)
      (append (flatten (car ll))
	      (flatten (cdr ll))))))

(defun into-matrix (l m &optional (n (/ (length l) m)))
  (loop for i below n collect (firstn m (nthcdr (* m i) l))))

(defun plus (&rest args)
  (if (null (car args))
      nil
    (if (atom (car args))
	(apply #'+ args)
      (cons (apply #'plus (nths 0 args))
	    (apply #'plus (nthcdrs 1 args))))))

(defun coordinate (l &optional (start 0))
  (loop for e in l for i from start collect (list i e)))

(defun index-list-with-base-1 (l)
  (coordinate l 1))

(defun groups-of-n (n l)
  (loop for i below (length l) by n collect
	(firstn n (nthcdr i l))))

(defun group-by-firsts-in-list (ll)
  (let ((unique-firsts (remove-duplicates (mapcar #'car ll) :test #'equal)))
    (loop for first in unique-firsts collect
	  (list first
		(loop for (f . rest) in ll
		    when (equal f first)
		    append rest)))))

(defun split-list (split-determination-f l &key (split-element-fate 'with-none) ;; fate can be with-none, with-last, with-next, with-self
						pass-so-far-to-split-determination-f
						pass-to-come-to-split-determination-f
						pass-so-far-and-to-come-to-split-determination-f)
  (if (not (member split-element-fate '(with-none with-last with-self with-next)))
      (error "Invalid argument ~a to split-list, expected one of (with-none with-last with-self with-next)" split-element-fate))
  (let (piece
	pieces)
    (loop for e in l for i from 1 do
	  (if (if pass-so-far-to-split-determination-f
		  (funcall split-determination-f e piece)
		(if pass-to-come-to-split-determination-f
		    (funcall split-determination-f e (nthcdr i l))
		  (if pass-so-far-and-to-come-to-split-determination-f
		      (funcall split-determination-f e piece (nthcdr i l))
		    (funcall split-determination-f e))))
	      (progn
		(if (eql 'with-last split-element-fate)
		    (push-end e piece))
		(if piece (push-end piece pieces))  ;; to avoid the first piece being '() caused by a split at the start
		(if (eql 'with-self split-element-fate)
		    (push-end (list e) pieces))
		(setq piece (if (eql 'with-next split-element-fate)
				(list e)
			      nil)))
	    (push-end e piece)))
    (if piece (push-end piece pieces))  ;; to avoid the last piece being '() caused by a split at the end
    pieces))
;;(split-list #'numberp '(-1 a b 0 c d 3 f 4) :split-element-fate 'with-self)
;;((-1) (A B) (0) (C D) (3) (F) (4))
;;(split-list #'numberp '(-1 a b 0 c d 3 f 4) :split-element-fate 'with-last)
;;((-1) (A B 0) (C D 3) (F 4))
;;(split-list #'numberp '(-1 a b 0 c d 3 f 4) :split-element-fate 'with-next)
;;((-1 A B) (0 C D) (3 F) (4))
;;(split-list #'numberp '(-1 a b 0 c d 3 f 4) :split-element-fate 'with-none)
;;((A B) (C D) (F))

;;(split-list (^ (e so-far) (and (numberp e)
;;			         (member 'c so-far)))
;;	      '(-1 a b 0 c d 3 f 4) :split-element-fate 'with-self :pass-so-far-to-split-determination-f t)
;;((-1 A B 0 C D) (3) (F 4))


(defun replace-nth (nth new-item l)
  (append (firstn nth l)
	  (list new-item)
	  (nthcdr (inc nth) l)))

(defun replace-multiple-nth (nths new-items l)
  ;; note, does not require nths to be sorted
  (loop for e in l for i from 0 collect
	(if (member i nths)
	    (nth (position i nths) new-items)
	  e)))


(defun rotate-list (l)
  (append (cdr l) (list (car l))))

(defun rotate-list-n-times (n l)
  (if (zerop n)
      l
    (if (= 1 n)
	(rotate-list l)
      (rotate-list-n-times (dec n) (rotate-list l)))))


(defun my-intersection (a b &optional &key (test #'eql))
  (reverse (intersection a b :test test)))

(defun my-set-difference (a b &optional &key (test #'eql))
  (reverse (set-difference a b :test test)))


(defun my-remove-duplicates (l &optional &key (test #'eql))
  "Same as system's remove-duplicates, but keep the order in the original list"
  (let (uniques)
    (loop for e in l
        when (not (member e uniques :test test))
        do (setq uniques (cons e uniques)))
    (reverse uniques)))
                 


(defun list-between-and-including-elements (list start-element &optional end-element &key (not-found-action :error) (test #'eql))
  (let ((start-position (position start-element list :test test))
        (end-position   (position end-element   list :test test)))
    (if (and start-position end-position (< end-position start-position))
        (error "end position less than start position"))
    (if (and (eql not-found-action :error)
             (or (null start-position)
                 (null end-position)))
        (error "The list subset start-element ~a was~a found and the end-element ~a was~a found" 
               start-element (if start-position "" " not")
               end-element   (if end-position   "" " not")))
    (cond ((and (null start-position) (null end-position)) nil)
          ((null start-position) (firstn (inc end-position) list))
          ((null end-position)   (nthcdr start-position list))
          (t (nthcdr start-position (firstn (inc end-position) list))))))
#||
(list-between-and-including-elements '(0 1 2 3 4 5) 1 4 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 1 1 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 4 1 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 0 5 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 1 9 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 9 4 :not-found-action :ignore)
(list-between-and-including-elements '(0 1 2 3 4 5) 8 9 :not-found-action :ignore)
||#


;;;---------------------------------------------------------------------
;;;                      ASSOCIATION LISTS
;;;---------------------------------------------------------------------

(defun assoc-value-atom (key alist &key (test #'eql))
  (second (assoc key alist :test test)))

(defun assoc-value (key alist &key (test #'eql))
  (cdr (assoc key alist :test test)))

(defun assoc-value-1 (key alist &key (test #'eql) (not-found-action :return-nil))
  (let ((value (second (assoc key alist :test test))))
    (if value
        value
      (case not-found-action
	(:error (error "no such keyword as ~a in alist" key))
	(:return-nil nil)
        (:return-zero 0)
	(t (error "unexpected not-found-action ~a, expected one of :error :add or :ignore" not-found-action))))))


(defun assoc-value-2 (key alist &key (test #'eql))
  (third (assoc key alist :test test)))

(defun assoc-value-3 (key alist &key (test #'eql))
  (fourth (assoc key alist :test test)))

(defun assoc-value-4 (key alist &key (test #'eql))
  (fifth (assoc key alist :test test)))

(defun assoc-value-5 (key alist &key (test #'eql))
  (sixth (assoc key alist :test test)))

(defun assoc-position (key l &optional &key (test #'eql))
  (position key (nths 0 l) :test test))
  

;;;----------------------------------------------------------------------
;;;                         ARRAYS
;;;----------------------------------------------------------------------

(define (list-array a) 
    (case (length (array-dimensions a))
      (1 (loop for i below (array-dimension a 0) collect (aref a i)))
      (2 (loop for i below (array-dimension a 0) collect 
	       (loop for j below (array-dimension a 1) collect
		     (aref a i j))))
      (t (error "List Array only handles 1d and 2d arrays"))))

(defun array-list-dimensions (l)
  ;; determine the array
  (if (and (not (null l)) (listp (car l)))
      (cons (length l) 
	    (array-list-dimensions (car l)))
    (list (length l))))

(defun array-list (l)
  (make-array
   (array-list-dimensions l)
   :initial-contents l))

(defun copy-array (a)
  (make-array (array-dimensions a) :initial-contents (list-array a)))


;;;---------------------------------------------------------------------
;;;                         FILING DATA
;;;---------------------------------------------------------------------

(defun fi (x &optional out (if-exists-action :error) use~a &key write-outer-list-elements-individually 
								write-inner-list-elements-individually  
								(include-newline t))
  ;;if-exists-actions are :error :new-version :overwirte :append :supersede nil
  ;;CAREFUL :overwrite IS JUST OVERWRITE WHAT IS THERE, AND LEAVE THE REST
  ;;use~a.  we use ~s in the format for being able to read exactly what we write out
  ;;        we use ~a for the special case of strings, to not put out the double quotes (for tk esp)
  ;;NOTE: set *print-pretty* to nil to get no carriage returns
  ;;NOTE: if we do write-outer-list-elements-individually then fi-in-s is the reverse of fi
  ;; would be good to have all arguments as keywords, but i use fi too much everywhere to refactor 
  ;;   it is ugly now, cause it looks like the use~a is the parameter to the keyword used for the if-exists-action

  (cond ((or (null out)
	     (equal "" out))
	 (fi x 
	     *standard-input* 
             if-exists-action
	     use~a 
	     :write-outer-list-elements-individually write-outer-list-elements-individually 
	     :write-inner-list-elements-individually write-inner-list-elements-individually  
	     :include-newline                        include-newline))
	((not (streamp out))
	 (with-open-file (output-stream out :direction :output :if-exists if-exists-action)
	   (fi x 
	       output-stream
	       if-exists-action
	       use~a 
	       :write-outer-list-elements-individually write-outer-list-elements-individually 
	       :write-inner-list-elements-individually write-inner-list-elements-individually  
	       :include-newline                        include-newline)))
	(t (let ((format-string
		  (format nil "~a~a~a~a~a~a"
			  (if write-outer-list-elements-individually "~{" "")
			  (if write-inner-list-elements-individually "~{" "")
			  (if use~a "~a" "~s")
			  (if write-inner-list-elements-individually " ~}" "")
			  (if include-newline "~%" "")
			  (if write-outer-list-elements-individually "~}" ""))))
	     (format out format-string x))))
  x)

(defun fi-in (filename)
  (with-open-file (in filename)
    (read in)))

(defun fi-in-s (filename)
  (with-open-file (in filename)
    (let (read)
      (loop for i from 0
	  until (eql 'eof (setq read (read in nil 'eof)))
	  collect read))))

(defun clean-mac-string (line)
  (if (equal "" line)
      line
    (if (eql #\Return (aref line (dec (length line))))
        (progn
          (format t "Warning: last character of line is #\Return, this might be a Windows file being read on Mac, removing this last char~%")
          (substring line 0 (- (length line) 2)))
      line)))



(defun fi-in-readline (in &optional &key comment-char line-process-f first-line-only)
  (let ((ans (cond ((or (null in)
                        (equal "" in))
                    (fi-in-readline *standard-input* :comment-char comment-char :line-process-f line-process-f))
                   ((not (streamp in))
                    (with-open-file (input-stream in)
                      (fi-in-readline input-stream :comment-char comment-char :line-process-f line-process-f)))
                   (t (fi-in-readline-aux
                       (let (line)
                         (loop for i from 0
                             until (or (and first-line-only (= i 1))
                                       (eql 'eof (setq line (read-line in nil 'eof))))
                             collect line))
                       :comment-char comment-char
                       :line-process-f line-process-f)))))
    (if first-line-only
        (car ans)
      ans)))

(defun fi-in-readline-aux (lines &optional &key comment-char line-process-f)

  ;; likely win-mac incompatibility
  (if (char-member #\return (car lines))
      (if (equal 1 (length lines))
          (setq lines (explode-string #\return (car lines)))
        (setq lines (mapcar #'clean-mac-string lines))))
  
  (loop for line in lines
      unless (or (= 0 (length line))
                 (and comment-char (eql (aref line 0) comment-char)))
      collect (progn
                (if (and comment-char (substring-before-char comment-char line)) ;; this gets around if no comment char substring-before-char returning nil
                    (setq line (substring-before-char comment-char line)))
                (if line-process-f
                    (funcall line-process-f line)
                  line)
                )))


;; to read list of space and line delimited coords (eg a gnuplot file) to a list of coords
;; (fi-in-readline <filename> :comment-char #\# :line-process-f #'space-delimited-string-to-list)

(defun fi-in-readline-to-list (filename &optional &key (comment-char #\;) first-line-only)
  (fi-in-readline filename :comment-char comment-char :line-process-f #'space-delimited-string-to-list :first-line-only first-line-only))


(defun print-file (filename)
  (let ((lines (fi-in-readline filename)))
    (loop for line in lines do (format t "~%~a" line))
    nil))	

#||
(defun csv-to-list (line &optional &key empty-place-holder)
  ;; commas separated values, usually from excel
  (let* ((list (read-from-string
		(string-append
		 "(\""
		 (string-subst-string "," "\" \"" line :all-occurrences t)
		 "\")")))
	 (last-string (car (last list))))
    (if (and (not (equal "" last-string))  ;; special case, where we don't want to dec the length and use a -1 for aref
	     (eql #\return (aref last-string (dec (length last-string)))))
	(setq list
	  (append
	   (butlast list)
	   (list (substring last-string 0 (dec (dec (length last-string))))))))
    (mapcar (^ (string)
	       (if (or (equal "" string)
		       (equal " " string))
		   empty-place-holder
		 (if (char-member #\space string)
		     string
		   (read-from-string string))))
	    list)))
||#

(defun simple-csv-to-list (line &optional &key leave-elements-as-strings) 
  (csv-to-list line :simple-csv t :leave-elements-as-strings leave-elements-as-strings))

(defun csv-to-list (line &optional &key simple-csv leave-elements-as-strings)
  ;; some strings to deal with
  ;;   CDC ID #,(Sub)type,Date Coll't,Patient Location,Country ,Resistant/Sensitive?
  ;;   2005705008,H3,9/17/04,"PROVO, UT",Utah,S
  (if simple-csv  
      ;; just replace commas with spaces and readline
      (space-delimited-string-to-list (string-subst-string "," " " line :all-occurrences t) :leave-elements-as-strings leave-elements-as-strings)
    (let (list
          symbol-chars
          in-string)
      (loop for this-char in (explode-string nil line) do
            (cond ((equal #\" this-char)
                   (progn
                     (setq in-string (not in-string))
                     (push-end this-char symbol-chars)))
                  ((equal #\, this-char)
                   (if in-string
                       (push-end this-char symbol-chars)
                     (progn
                       (push-end symbol-chars list)
                       (setq symbol-chars nil))))
                  (t (push-end this-char symbol-chars))))
      ;; the last symbol
      (push-end symbol-chars list)

      ;; make each of the symbol-char-lists in list, or leave as string
      (loop for char-list in list collect
            (if (or (null char-list)
                    (equal '(#\Space) (remove-duplicates char-list))) ;; all spaces
                ""
              (let* ((as-string (chars-to-string char-list))
                     (as-symbol (if (not leave-elements-as-strings) (read-from-string as-string))))
                (if (= (length as-string) (length (format nil "~a" as-symbol)))
                    (if leave-elements-as-strings as-string as-symbol)
                  as-string)))))))


;;;---------------------------------------------------------------------
;;;                           GRAPHICS
;;;---------------------------------------------------------------------

(defun random-x-color ()
  (format nil "#~2,'0x~2,'0x~2,'0x" 
	  (random 256) (random 256) (random 256)))

;;;---------------------------------------------------------------------
;;;                           DEFSYSTEM
;;;---------------------------------------------------------------------

;;12/99  make-system is now defined in ./systems.lisp

;; the defsys package is not the package that defsystem is defined in for cmucl, so comment this out as i don't use
#+allegro
(defun save-system (system save-directory)
  (run-shell-command (format nil "mkdir ~a" save-directory))
  (map-system system 
	      (^ (module) 
		 (run-shell-command
		  (format nil "cp ~a~a.* ~a"
			  (directory-namestring (defsys::source-pathname module))
			  (defsys::pretty-name module)
			  save-directory))))
  (run-shell-command (format nil "cp ~adefsystem.lisp ~a" 
			     (defsystem::get-pathname (find-system system))
			     save-directory)))


;;;---------------------------------------------------------------------
;;;                            REPEAT
;;;---------------------------------------------------------------------

(defmacro repeat (expression times)
  `(loop for i below ,times do ,expression))


;;;---------------------------------------------------------------------
;;;                      COLOR MODEL CONVERSIONS
;;;---------------------------------------------------------------------

(defun r-rgb-to-r-hsv (r g b)
  (if (= 1.0 r) (setq r 0.9999999))
  (if (= 1.0 g) (setq g 0.9999999))
  (if (= 1.0 b) (setq b 0.9999999))
  (let* ((min (min r g b))
	 (max (max r g b))
	 (delta (- max min)))
    (if (zerop delta)
	(values
	 0.0
	 0.0
	 max)
      (let* ((s (float (/ delta max)))
	     (v (float max))
	     (h-intermediate
	      (* 60 
		 (cond ((= r max) (+ 0 (/ (- g b) delta)))
		       ((= g max) (+ 2 (/ (- b r) delta)))
		       (t         (+ 4 (/ (- r g) delta))))))
	     (h (/ (if (minusp h-intermediate)
		       (+ h-intermediate 360)
		     h-intermediate)
		   360.0)))
	(values
	 h
	 s 
	 v)))))

(defun r-hsv-to-r-rgb (h s v)
  ;;hsv are reals in [0,1, answer is in reals [0,1
  ;;from Foley & van Dam ed 2 p593 (with e for t)
  (if (= 1.0 h) (setq h 0.9999999))
  (if (= 1.0 s) (setq s 0.9999999))
  (if (= 1.0 v) (setq v 0.9999999))
  (if (zerop s)
      (values v v v)
    (let* ((h (* h 6))
	   (i (floor h))
	   (f (- h i))
	   (p (* v (- 1 s)))
	   (q (* v (- 1 (* s f))))
	   (e (* v (- 1 (* s (- 1 f))))))
      (case i
	(0 (values v e p))
	(1 (values q v p))
	(2 (values p v e))
	(3 (values p q v))
	(4 (values e p v))
	(5 (values v p q))
	(t (error "Expected values for i were integers 0 to 5, but caluclated i=~a" i))))))

(defun r-hsv-to-rgb (h s v)
  (multiple-value-bind (r g b)
      (r-hsv-to-r-rgb h s v)
    (values (floor (* r 255))
	    (floor (* g 255))
	    (floor (* b 255)))))



(defun rgb-to-r-hsv (r g b)
  (r-rgb-to-r-hsv (/ r 255.0) (/ g 255.0) (/ b 255.0)))

(defun rgb-to-r-rgb (r g b)
  (list (/ r 255.0) (/ g 255.0) (/ b 255.0)))

#|
;; perfect match (many failures)
(progn
  (setq non-match nil)
  (loop for r below 256 do
	(loop for g below 256 do
	      (loop for b below 256 do
		    (let ((original (list r g b) )
			  (in-and-out (multiple-value-list 
				       (apply 
					#'r-hsv-to-rgb 
					(multiple-value-list 
					 (rgb-to-r-hsv r g b))))))
		      (if (not (equal original in-and-out))
			  (setq non-match 
			    (cons (print (list original in-and-out))
				  non-match))))))))

;; allow difference of 1  (no failures)
(progn
  (setq non-match nil)
  (loop for r below 256 do
	(loop for g below 256 do
	      (loop for b below 256 do
		    (let ((original (list r g b) )
			  (in-and-out (multiple-value-list 
				       (apply 
					#'r-hsv-to-rgb 
					(multiple-value-list 
					 (rgb-to-r-hsv r g b))))))
		      (if (> (apply-max (mapcar #'abs (mapcar #'- original in-and-out))) 1)
			  (setq non-match 
			    (cons (print (list original in-and-out))
				  non-match))))))))
|#


;;;----------------------------------------------------------------------
;;;                        PRINTING TIME AND DATE
;;;----------------------------------------------------------------------

;;;I cannot belive that this is not done somewhere in the system
;;;it sure was on the lispm

(defun time-and-date ()
  (let ((time (multiple-value-list (get-decoded-time))))
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~2,'0d/~2,'0d/~4,'0d"
	      (nth 2 time)
	      (nth 1 time)
	      (nth 0 time)
	      (nth 4 time)
	      (nth 3 time)
	      (nth 5 time))))

(defun numeric-sortable-date ()
  (let ((time (multiple-value-list (get-decoded-time))))
    (format nil "~2,'0d~2,'0d~2,'0d"
	    (nth 5 time)
	    (nth 4 time)
	    (nth 3 time))))


;;;---------------------------------------------------------------------
;;;                           TRIGONOMETRY
;;;---------------------------------------------------------------------

(defun sind (x)
  (sin (* x (/ (float pi (if (integerp x) (float x) x)) 180))))


;;;---------------------------------------------------------------------
;;;                          MAKING NAMES
;;;---------------------------------------------------------------------

(defun symbol-append (&rest symbols)
  ;; will return the symbol
  ;; read-from-string produces an error if symbols is nil
  (read-from-string (apply #'concatenate 'string (mapcar #'string symbols))))


(defun punctuate (list symbol)
  (butlast 
   (loop for e in list append 
	 (list e symbol))))


;;;---------------------------------------------------------------------
;;;                            SORTING
;;;---------------------------------------------------------------------

(defun my-sort (l &optional (f #'<))
  (sort (copy-list l) f))

(defun sort-car (l &optional (f #'<))
  (sort (copy-list l) (^ (x y) (funcall f (car x) (car y)))))

(defun sort-nth (n l &optional (f #'<))
  (sort (copy-list l) (^ (x y) (funcall f (nth n x) (nth n y)))))

(defun sort-strings (l)
  (my-sort l #'string<))

(defun sort-alpha (l)
  (mapcar #'read-from-string (my-sort (mapcar #'anything->string l) #'string<)))

(defun sort-alpha-nth (n l)
  (sort-nth n l (^ (a b) (string< (string a) (string b)))))

(defun sort-alpha-substring (l start end)
  (mapcar #'read-from-string (my-sort (mapcar #'anything->string l) 
                                      (^ (a b) (string< (substring (string a) start end) (substring (string b) start end))))))

(defun sort-alpha-substring-nth (n l start end)
  (sort-nth n l (^ (a b) 
                   (let ((string-a (string a))
                         (string-b (string b)))
                     (if (and (> (length string-a) end)
                              (> (length string-b) end))
                         (string< (substring (string a) start end) (substring (string b) start end))
                       nil)))))

(defun asc-sort (l)
  ;;ascending sort
  (my-sort l))


;;;----------------------------------------------------------------------
;;;           use sort to get a reordering vector
;;;----------------------------------------------------------------------

;; not sure how to do this, see a way around it, so not pushing it now.


;;;----------------------------------------------------------------------
;;;                      reordering lists
;;;----------------------------------------------------------------------

(defun group (items &key (extractor #'identity) (test #'equal))
  (let ((uniques (remove-duplicates (mapcar extractor items) :test test)))
    (loop for unique in uniques collect
	  (collect (^ (x) (funcall test unique (funcall extractor x))) items))))

(defun reorder-elements (l new-order &optional &key length-difference-ok)
  (if (not length-difference-ok)
      (if (not (= (length l) (length new-order)))
	  (error "the new ordering needs to be the same length as the list")))
  (loop for i in new-order collect
	(nth i l)))

;;here is alternate from mac that is more general than reorder-element (above)
(defun reorder (order source &key (destructor #'identity))
  (append
   (loop for next in order
         when (position next (mapcar destructor source))
         collect (nth (position next (mapcar destructor source)) source))
   (loop for s in source
         when (not (position (funcall destructor s) order))
       collect s)))

(defun select (item source test)
  ;; select item from source, using test to find item in source element
  (nth (position item source :test test) source))

(defun selects (items source test)
  (loop for item in items collect
	(select item source test)))


;;;---------------------------------------------------------------------
;;;                       STIRLING NUMBERS
;;;---------------------------------------------------------------------

(defun s2 (n k)
  ;;p259 Concrete Math  Stirling numbers of the 2nd kind
  (if (= n 0)
      (if (= k 0)
	  1
	0)
    (if (= k 0)
	0
      (+ (s2 (dec n) (dec k))
	 (* k (s2 (dec n) k))))))


(defun s2-groups (objects num-subsets)
  ;;p259 Concrete Math  Stirling numbers of the 2nd kind
  (if (= (length objects) 0)
      (if (= num-subsets 0)
	  '(())
	'())
    (if (= num-subsets 0)
	'()
      (append
       (mapcar (^ (grouping)
		  (cons (list (car objects))
			grouping))
	       (s2-groups (cdr objects) (dec num-subsets)))
       (apply #'append
	      (mapcar (^ (grouping)
			 (loop for i below num-subsets collect
			       (loop for subset in grouping
				   for subset-number from 0 collect
				     (if (= i subset-number)
					 (cons (car objects) subset)
				       subset))))
		      (s2-groups (cdr objects) num-subsets)))))))

(defun s2-groups-new (objects num-subsets)
  ;;thought this would be auto-canonical, but it is not unfortunately
  ;;p259 Concrete Math  Stirling numbers of the 2nd kind
  (if (= (length objects) 0)
      (if (= num-subsets 0)
	  '(())
	'())
    (if (= num-subsets 0)
	'()
      (append
       (mapcar (^ (grouping)
		  (cons (list (car objects))
			grouping))
	       (s2-groups (cdr objects) (dec num-subsets)))
       (apply #'append
	      (mapcar (^ (grouping)
			 (loop for i below num-subsets collect
			       `(,(cons (car objects) (nth i grouping))
				 ,@(firstn i grouping)
				 ,@(nthcdr (inc i) grouping))))
		      (s2-groups (cdr objects) num-subsets)))))))

(defun s2-groups-canonical (objects num-subsets)
  (mapcar (^ (group)
	     (sort group (^ (a b) (string< (string (car a)) (string (car b))))))
	  (s2-groups objects num-subsets)))

(defun all-s2-groups (objects)
  (loop for i from 1 to (length objects) append
	(s2-groups-canonical objects i)))
			
;;;------------------------------------------------------------------------
;;;                         K UNIQUE ELEMENTS
;;;------------------------------------------------------------------------

(defun k-unique-positions (k n &optional rn-generator)
  (let (uniques)
    (if (> k n)
	(error "k-unique was asked for more uniques than there are elements"))
    (loop until (= (length uniques) k) do
	  (let ((trial (krandom n rn-generator)))
	    (if (not (member trial uniques))
		(push trial uniques))))
    uniques))

(defun k-unique (k l &optional rn-generator)
  (mapcar (^ (p) (nth p l)) (k-unique-positions k (length l) rn-generator)))

(defun scramble (l &optional rn-generator)
  ;;(k-unique (length l) l rn-generator)  this works ok until the list gets > say 100
  (shuffle l rn-generator))  ;; this might not be much better...

(defun shuffle (l &optional generator)
  (let* ((reals (loop for i below (length l) collect (knuth-random generator)))
	 (sorted-reals (my-sort reals))
	 (shuffle-order (loop for sorted-real in sorted-reals collect
			      (position sorted-real reals :test #'equal))))
    (reorder-elements l shuffle-order)))
	 

;;;------------------------------------------------------------------------
;;;                          GLUEING SYMBOLS
;;;------------------------------------------------------------------------

(defun glue-symbol (subsets)
  (glue-up (loop for subset in (remove nil subsets) collect (glue-up subset "")) '-))

(defun glue-up (symbols &optional (connector '-))
  (read-from-string (glue-up-to-string connector symbols)))

(defun glue-up-to-string (connector symbols)
   (apply #'string-append
	  (mapcar (^ (x) (format nil "~a" x))
		  (butlast (interleave symbols (n-of (length symbols) connector))))))

(defun glue-with-hyphens (xs)
  (let ((glued (glue-up-to-string "-" xs)))
    (if (stringp (car xs))
        glued
      (read-from-string glued))))

(defun unglue-symbol (symbol)
  (loop for group in (read-from-string 
		      (format 
		       nil "(~a)" (string-subst #\- #\space (string symbol)))) collect
	(let ((string-group (string group)))
	  (loop for i below (length string-group) collect
		(read-from-string (make-string 1 :initial-element (aref string-group i)))))))

(defun explode-symbol (symbol)
  (let ((string-symbol (string symbol)))
    (loop for i below (length string-symbol) collect
	  (read-from-string (make-string 1 :initial-element (aref string-symbol i))))))

(defun implode-list (list)
  (if (null list)
      nil
    (read-from-string (format nil "~{~a~}" list))))

(defun unglue-hyphenated-symbol (symbol)
  (read-from-string 
   (format 
    nil "(~a)" (string-subst #\- #\space (string symbol)))))

;; maybe this is the same as above?
(defun explode-hypenated-symbol (date)
  (space-delimited-string-to-list (string-subst #\- #\space (string date))))


(defun digit-list-to-integer (cardinality digit-list)
  (let ((num-bits (ceiling (log cardinality 2)))
	(i 0))
    (loop for digit in digit-list
	for d from 0 do
	  (setq i (dpb digit (byte num-bits (* d num-bits)) i)))
    i))

(defun integer-to-digit-list (cardinality length integer)
  (let ((num-bits (ceiling (log cardinality 2))))
    (loop for d below length collect
	  (ldb (byte num-bits (* d num-bits)) integer))))


(defun digit-list-to-decimal (digit-list)
  (loop for digit in digit-list for power from (dec (length digit-list)) downto 0 sum
	(* digit (expt 10 power))))

(defun explode-string (explosion-char string)
  (if (null explosion-char)
      (loop for i below (length string) collect (aref string i))
    (progn
      (setq string (string-trim (make-string 1 :initial-element explosion-char) string))
      (if (not (char-member explosion-char string))
	  (list string)
	(cons (substring-before-char explosion-char string)
	      (explode-string explosion-char (substring-after-char explosion-char string)))))))


;;;----------------------------------------------------------------------
;;;                   CONSTRUCTING UNIX WILDCARDS
;;;----------------------------------------------------------------------

(defun delimit-string (delimiter string)
  (string-append delimiter string delimiter))

(defun curly-brace-wildcard (xs &key star-delimit)
  (let* ((glue (glue-up-to-string "," xs))
         (maybe-braced (if (> (length xs) 1) 
                           (string-append "{" glue "}")
                         glue)))
    (if star-delimit
        (delimit-string "*" maybe-braced)
      maybe-braced)))


;;;---------------------------------------------------------------------------------
;;;             UTILITIES FOR GENERATING LISTS (POINTS) (RECEPTORS)
;;;---------------------------------------------------------------------------------

(defun random-base (&optional (n *ab-receptor-length*) (k *receptor-cardinality*)
			      random-generator)
  (loop for i below n collect (krandom k random-generator)))

(defun zero-base (&optional (n *ab-receptor-length*))
  (loop for i below n collect 0))


;;;-----------------------------------------------------------------------------
;;;                   BINARY HAMMING DISTANCE ON INTEGERS
;;;-----------------------------------------------------------------------------

(defun hd-i-b (x y)
  (logcount (logxor x y)))

(defun hd (aa bb)
  (loop for a in aa for b in bb unless (eql a b) sum 1))

(defun hd-dash-does-not-count (aa bb)
  (loop for a in aa for b in bb unless (or (eql a b) (eql '- a) (eql '- b)) sum 1))

(defun hds (l)
  (all-comparisons-full l #'hd))

(defun hs (aa bb)
  "Hamming similarity"
  (loop for a in aa for b in bb when (eql a b) sum 1))

(defun codon-difference-base-hd-function (aa bb difference-function)
  ;; assume aa and bb are nucleotide sequences
  (loop for i below (ceiling (/ (length aa) 3)) sum
        (length
         (funcall difference-function
          (nth-range (* i 3) (+ 2 (* i 3)) aa)
          (nth-range (* i 3) (+ 2 (* i 3)) bb)))))

(defun non-synonymous-hd (aa bb &optional &key (positions-considered '(0 1 2))) 
  (codon-difference-base-hd-function 
   aa bb 
   (^ (aa bb)
      (non-synonymous-substitutions-between-codons 
       aa bb
       :positions-considered positions-considered))))

(defun synonymous-hd (aa bb &optional &key (positions-considered '(0 1 2))) 
  (codon-difference-base-hd-function 
   aa bb 
   (^ (aa bb)
      (synonymous-substitutions-between-codons 
       aa bb
       :positions-considered positions-considered))))


;;;-----------------------------------------------------------------------------
;;;                      DISTANCE METRICS ON LISTS
;;;-----------------------------------------------------------------------------

(defun e-dist (l1 l2)
  "Euclidean distance on lists."
  (if (not (= (length l1) (length l2)))
      (error "Points must have same number of dimensions to take the e-dist"))
  (sqrt (loop for e1 in l1
	    for e2 in l2
	    sum (square (- e1 e2)))))

(defun e-dist-with-dont-cares (l1 l2)
  "Euclidean distance on lists, allowing for dont cares."
  (if (or (eql 'dont-care l1) (eql 'dont-care l2))
      'dont-care
    (sqrt (loop for e1 in l1
	      for e2 in l2
	      sum (square (- e1 e2))))))

(defun m-dist (l1 l2)
  "Manhatten distance on lists."
  (loop for e1 in l1
      for e2 in l2
      sum (abs (- e1 e2))))

(defun h-dist (l1 l2)
  "Hamming distance on lists."
  (loop for e1 in l1
      for e2 in l2
      sum (bool->bit (not (equal e1 e2)))))


(defun l2-norm (&rest l)
  (sqrt (apply #'+ (mapcar #'square l))))



(defun num-unique-elements (l &optional &key (test #'eql))
  (length (remove-duplicates l :test test)))



;;;-----------------------------------------------------------------------------
;;;                      FOR FUNCTIONAL COMPOSITION
;;;-----------------------------------------------------------------------------

;;(defun id (x)
;;  x)


;;;------------------------------------------------------------------------------
;;;                           NORMALIZATIONS
;;;------------------------------------------------------------------------------

(defun put-in-range-0-to-1 (xs)
  (let* ((min (apply-min xs))
	 (zeroed-xs (loop for x in xs collect (- x min)))
	 (max (apply-max zeroed-xs)))
    (loop for x in zeroed-xs collect (float (/ x max)))))

(defun scale-zero-based-data-so-max-is-1 (xs &optional &key if-max-less-than-this-value-return-original-list)
  (let* ((max (apply-max xs)))
    (if (and if-max-less-than-this-value-return-original-list
	     (< max if-max-less-than-this-value-return-original-list))
	xs
      (loop for x in xs collect (float (/ x max))))))

(defun put-in-range (xs &optional (target-min -1) (target-max +1))
  (let* ((data-min (apply-min (flatten xs)))
	 (data-max (apply-max (flatten xs)))
	 (data-range (- data-max data-min))
	 (data-midpoint (+ data-min (/ data-range 2)))
	 (target-range (- target-max target-min))
	 (target-midpoint (+ target-min (/ target-range 2)))
	 (range-scale (/ target-range data-range)))
    (f-lists
     (^ (x)
	(float
	 (+ (* (- x data-midpoint)
	       range-scale)
	    target-midpoint)))
     xs)))

(defun leave-zero-make-max-absolute-value (xs &optional (target-max-abs-value 1))
  (let* ((data-max-abs-value (apply-max (mapcar #'abs (flatten xs))))
	 (scale (/ target-max-abs-value data-max-abs-value)))
    (f-lists
     (^ (x)
	(float
	 (* x scale)))
     xs)))

(defun normal-to-percent-abscissa (xys)
  (loop for xy in xys collect (list (* 100 (car xy)) (cadr xy))))

(defun normalize-sum (l &optional (normal 1.0))
  ;; same as convert-to-frequency
  (let ((multiplicand (/ normal (apply-+ l))))
    (mapcar (^ (x) (* x multiplicand)) l)))  

(defun convert-to-frequency (l) (normalize-sum l))

(defun add-0-to-1-range-ordinate (ys)
  (loop for x in (put-in-range-0-to-1 (series 1 (length ys)))
      for y in ys collect
        (list x y)))

;;;----------------------------------------------------------------------
;;;                         BINARY->NARY
;;;----------------------------------------------------------------------

(defun binary->nary (binary-f right-associative unit)
  (^ (&rest args)
     (labels ((self (args)
		(COND ((NULL args) unit) ;0 args   return the unit
		      ((NULL (CDR args)) (CAR args)) ;1 arg    eg (+ 2) => 2
		      ((NULL (CDDR args)) (funcall binary-f (CAR args) (CADR args)))
		      (t (IF right-associative
			     (funcall binary-f (CAR args) (self (CDR args)))
			     (funcall binary-f (self (BUTLAST args 1)) (CAR (last args 1))))))))
	     (self args))))

(defun zero-unit (binary-f zero unit)
  (^ (x y)
   (COND ((OR (EQUAL x zero) (EQUAL y unit)) x)
	 ((OR (EQUAL y zero) (EQUAL x unit)) y)
	 (t (funcall binary-f x y)))))

(defun generalize (binary-f zero unit) 
  (binary->nary (zero-unit binary-f zero unit) nil unit))

;;ideally i'd like to be able to just set the function slot, but i do this for now
(defmacro generalize! (binary-f zero unit)
  `(defun ,(glue-up (list 'nary binary-f)) (&rest args)
     (apply ,(generalize binary-f zero unit) args)))


;;;----------------------------------------------------------------------
;;;                         nARY OPERATIONS
;;;----------------------------------------------------------------------

;;(generalize! equal t nil)

;;after all that i don't know how to do equal (what the zero and unit are)
;;hack it for now

;;what should this do if no args are supplied?
(defun nary-equal (&rest args)
  (= 1 (length (remove-duplicates args :test #'equal))))

;;what is going on here with union? i have to do the below, ugh, to get the 
;;elements to come out in the order i put them in
#|
(generalize! union nil nil)

(defun nary-union (&rest args)
  ;;horrid hack for now
  (let ((so-far (car args)))
    (loop for arg in (cdr args) do
	  (setq so-far (union so-far arg :test #'equal)))
    so-far))
|#

(defun my-union (l1 l2)
  (let (new-elements)
    (loop for e in l2
	when (not (member e l1 :test #'equal))
	do (setq new-elements (cons e new-elements)))
    (append l1 (reverse new-elements))))

(defun nary-union (&rest args)
  (cond ((null args) nil)
	(t (my-union (car args) (apply #'nary-union (cdr args))))))

(defun nary-intersection (&rest args)
  ;;we make this so an element must be in all lists to be in the intersection
  ;;ie it is the full intersection, the center of the venn diagram
  (cond ((null args) nil)
	((= 1 (length args)) (car args))
	(t (my-intersection (car args) (apply #'nary-intersection (cdr args))))))

;;;------------------------------------------------------------------------------
;;;                        MATRIX OPERATIONS
;;;------------------------------------------------------------------------------

(defun cross (l1 l2 &optional (cross-f #'list))
  (loop for e in l1 collect
	(loop for f in l2 collect 
	      (funcall cross-f e f))))

(defun f-list (f &rest lists)
  ;;apply f to corresponding elements in the list
  (if (not (apply #'nary-equal (mapcar #'length lists)))
      (error "lists must be same length"))
  (loop for i below (length (car lists)) collect
	(apply f (nths i lists))))

(defun f-lists (f ll)
  ;;applies f to every element in ll
  ;;CAREFUL, this is very different from f-list (above) which applies a function
  ;;to corresponding elements in mulitple lists
  ;; there is a problem if there is a null in the list.  should there be this problem?
  (cond ((null ll) nil)
	((atom (car ll)) (cons (funcall f (car ll))
			       (f-lists f (cdr ll))))
	((listp (car ll)) (cons (f-lists f (car ll))
				(f-lists f (cdr ll))))
	(t (error "should not be here in f-lists"))))

#|
;; attempt to unify f-lists and f-list.  not sure if there is a unification.
(defun f-lists-new (f &rest lls)
  ;; careful, does not work when () is an entry in the list (neither does the f-lists above)
  ;;applies f to every element in ll
  ;;CAREFUL, this is very different from f-list (above) which applies a function
  ;;to corresponding elements in mulitple lists
  ;; assumes each of the ll in lls have equal structure (i should test for that all up front, or everywhere as we recurse)
  (let ((ll (car lls)))
    (cond ((null ll) nil)
	  ((atom (car ll)) (cons (apply f (mapcar #'car lls))
				 (apply #'f-lists-new f (mapcar #'cdr lls))))  ;; compiles OK?
	  ((listp (car ll)) (cons (apply #'f-lists-new f (mapcar #'car lls))
				  (apply #'f-lists-new f (mapcar #'cdr lls))))
	  (t (error "should not be here in f-lists")))))
;; (f-lists-new #'+ '((1 (((2)))) 3) '((10 (((20)))) 30) '((100 (((200)))) 300))  =>  ((111 (((222)))) 333)
;; (f-lists-new #'+ '((0 (((1)))) () 2) '((10 (((20)))) () 30) '((100 (((200)))) () 300))  => error, need to fix
|#


;; these shoule be generalized (as of march 2001 only used in some investigation code)

#|
(defun compose-unequal-length-lists (ll composition-f)
  (loop for i below (apply #'max (mapcar #'length ll)) collect
	(funcall composition-f (nths i ll))))
|#

;; this replaces the above and is orders of magnitude faster
(defun compose-unequal-length-lists (ll composition-f)
  (let* ((max-length (apply-max (mapcar #'length ll)))
	 (nil-filled-max-length-list (loop for i below max-length collect nil))
	 (equal-length-ll (mapcar (^ (l) (append l (nthcdr (length l) nil-filled-max-length-list))) ll)))
    (mapcar composition-f (apply-transpose equal-length-ll))))

(defun average-unequal-length-lists (ll)
  (compose-unequal-length-lists
   ll
   (^ (l) (av (filter #'null l)))))
;; (average-unequal-length-lists '((0 1) (0 11 12 13 14) (0) (0 31 32 33 34 35) (0 41 42))) -> (0.0 21.0 28.666666 23.0 24.0 35.0)


;;;------------------------------------------------------------------------------
;;;                          COEFFICIENTS
;;;------------------------------------------------------------------------------

(defun multinomial-coefficients (n num-groups)
  (if (not (= 4 num-groups))
      (error "only coded right now for 4 groups"))
  (let (coefficients)
    (loop for a to n do
	  (loop for b to n do
		(loop for c to n do
		      (loop for d to n do
			    (if (= n (+ a b c d))
				(push (list a b c d) coefficients))))))
    (reverse coefficients)))

;;;----------------------------------------------------------------------
;;;                        DESTRUCTURING
;;;----------------------------------------------------------------------

(defmacro let-list ((binding-list list-expression) . body)
  `(let* ((internal-value-let-list ,list-expression)
	  ,@(loop for variable in binding-list for i from 0 collect
		  `(,variable (nth ,i internal-value-let-list))))
     ,@body))


;;;----------------------------------------------------------------------
;;;                      LIST MANIPULATION                      
;;;----------------------------------------------------------------------

(defun infix-operator (operator operands)
  (butlast (interleave operands (n-of (length operands) operator))))

(defun f-elements (f l)
  (cond ((null l) nil)
	((atom l) (funcall f l))
	((listp l) (cons (f-elements f (car l))
			 (f-elements f (cdr l))))))

;;;----------------------------------------------------------------------
;;;                      ROULETTE WHEEL SELECTION
;;;----------------------------------------------------------------------

(defun roulette (hist)
  (let* ((slots (apply #'+ (nths 1 hist)))
	 (slot (krandom slots))
	 (so-far 0))
    (loop for (group size) in hist do
	  (setq so-far (+ so-far size))
	  (if (< slot so-far)
	      (return group))
	finally (error "fell out of the roulette wheel"))))

(defun random-choice (l)
  (nth (krandom (length l)) l))

;;from mac
;;(defun roulette-select (i pairs)
;;  (let ((current 0))
;;    (loop for (number value) in pairs do
;;          (if (< i (setq current (+ number current)))
;;            (return 
;;             (values 
;;              value
;;              (- i (- current number))))))))

;;;------------------------------------------------------------------------
;;;                       LINEAR INTERPOLATION
;;;------------------------------------------------------------------------

(defun linear-interpolation-m-c (x1 y1 x2 y2)
  (let* ((m (/ (- y2 y1)
               (if (zerop (- x2 x1))
		   0.00000000001      ;;do i really want to do this?
		 (- x2 x1))))
         (c (- y2 (* m x2))))
   (values m c)))

(defun linear-interpolation (x x1 y1 x2 y2)
  (multiple-value-bind (m c)
                       (linear-interpolation-m-c x1 y1 x2 y2)
    (+ (* m x) c)))

(defun linear-interpolation-from-series (x xys &optional &key extrapolate-beyond-series)
  (loop for ((low-x low-y) (high-x high-y)) on xys 
      when (and high-x
                (>= x low-x)
                (<= x high-x))
      do (return (linear-interpolation x low-x low-y high-x high-y))
      finally (if extrapolate-beyond-series
                  (if (< x (nth 0 (nth 0 xys)))
                      (return (apply #'linear-interpolation x (apply-append (firstn 2 xys))))
                    (if (> x (nth 0 (nth 0 (last xys))))
                        (return (apply #'linear-interpolation x (apply-append (lastn 2 xys))))
                      (error "Unexpected case")))
                (error "Out of interpolation range"))))

(defun inverse-linear-interpolation (y x1 y1 x2 y2)
  (multiple-value-bind (m c)
                       (linear-interpolation-m-c x1 y1 x2 y2)
    (/ (- y c) m)))

(defun coords-from-m-c (m c x1 x2)
  (list
   (list x1 (+ (* m x1) c))
   (list x2 (+ (* m x2) c))))

(defun extend-line (x1y1 x2y2 proportion)
  (let ((x1 (nth 0 x1y1))
	(y1 (nth 1 x1y1))
	(x2 (nth 0 x2y2))
	(y2 (nth 1 x2y2)))
    (multiple-value-bind (m c)
	(linear-interpolation-m-c x1 y1 x2 y2)
      (coords-from-m-c m c x1 (+ x2 (* proportion (- x2 x1)))))))


;;;----------------------------------------------------------------------
;;;                   POLAR and RECTANGULAR COORDINATES
;;;----------------------------------------------------------------------

(defun polar-to-rectangular (r theta)
  ;;only for 2d for now
  ;;theta is in radians
  (list (* r (cos theta)) (* r (sin theta))))

(defun polar-to-rectangular-l (r-theta)
  (apply #'polar-to-rectangular r-theta))

(defun rectangular-to-polar (x y)
  ;;ugh, the atan part is much uglier than i thought it would be
  (list (hypotenuse-length-from-lengths x y)
        (if (zerop x)
          (if (zerop y) 0 (if (plusp y) (/ pi 2) (* 3 (/ pi 2))))
          (let ((atan (atan (/ y x))))
            (if (minusp x)
              (+ atan pi)
              (if (minusp y)
                (+ atan pi pi)
                atan))))))

(defun rectangular-to-polar-l (xy)
  (apply #'rectangular-to-polar xy))

;;(loop for i from 0 to (+ 1 (* pi 2)) by 0.25 collect 
;;      (rectangular-to-polar-l (polar-to-rectangular 1 i)))
;;(loop for i from 0 to (* pi 2) by (/ pi 8) collect 
;;      (rectangular-to-polar-l (polar-to-rectangular 1 i)))


;; ---------------- misc using polar coords ---------------------

(defun coordss-center-average (coordss)
  (mapcar #'av (apply-transpose coordss)))

(defun order-coordss-radially (coordss)
  (if (not (= 2 (length (car coordss))))
      (error "only works for 2d right now"))
  (map-apply #'polar-to-rectangular (sort-nth 1 (map-apply #'rectangular-to-polar coordss))))
    
(defun order-coordss-radially-about-center (center coordss)
  (translate-coordss
   (order-coordss-radially
    (translate-coordss coordss (mapcar #'- center)))
   center))

(defun order-coordss-radially-about-center-av (coordss)
  (order-coordss-radially-about-center
   (coordss-center-average coordss)
   coordss))


;; ----------------- angle-between strains -------------------

(defun angle-from-coords-to-coords (coords-a coords-b)
  (radians-to-degrees
   (nth 
    1
    (rectangular-to-polar 
     (- (nth 0 coords-b) (nth 0 coords-a))
     (- (nth 1 coords-b) (nth 1 coords-a))))))


;;;----------------------------------------------------------------------
;;;                               angles
;;;----------------------------------------------------------------------

(defun degrees-to-radians (degrees)
  ;; pi is double-float, as we don't need to have a double-float answer if degrees is not double-float
  (if (typep degrees 'double-float)
      (* degrees (/ pi 180))
    (coerce (* degrees (/ pi 180)) 'single-float)))

(defun radians-to-degrees (radians)
  ;; pi is double-float, as we don't need to have a double-float answer if radians is not double-float
  (if (typep radians 'double-float)
      (* 360 (/ radians (* 2 pi)))
    (coerce (* 360 (/ radians (* 2 pi))) 'single-float)))


(defun degrees-about-origin (origin-coords point-coords)
  (let ((x (- (nth 0 point-coords) (nth 0 origin-coords)))
	(y (- (nth 1 point-coords) (nth 1 origin-coords))))
    (if (zerop x)
	(if (minusp y) 270 90)
      (if (zerop y)
	  (if (minusp x) 180 0)
	(let ((raw-degrees (radians-to-degrees (atan (/ y x)))))
	  (cond ((and (plusp  x) (plusp  y)) raw-degrees)          ;; 1st quadrant
		((and (minusp x) (plusp  y)) (+ 180 raw-degrees))  ;; 2nd quadrant
		((and (minusp x) (minusp y)) (+ 180 raw-degrees))  ;; 3rd quadrant
		((and (plusp  x) (minusp y)) (+ 360 raw-degrees))  ;; 4th quadrant
		))))))

#|
;; this should come out monotonically increasing
(gnuplot
 (setq foo 
  (loop for point-coords in (append 
			     (loop for x from 1 downto 0 by 0.1 
				 for y from 0 to 1 by 0.1 collect
				   (list x y))
			     (loop for x from 0 downto -1 by 0.1 
				 for y from 1 downto 0 by 0.1 collect
				   (list x y))
			     (loop for x from -1 to 0 by 0.1 
				 for y from 0 downto -1 by 0.1 collect
				   (list x y))
			     (loop for x from 0 to 1 by 0.1 
				 for y from -1 to 0 by 0.1 collect
				   (list x y))) collect
	(angle-about-origin '(0 0) point-coords))))

;; and coverting the above angles back to coords
(gnuplot (mapcar (^ (angle) (polar-to-rectangular 1 (degrees-to-radians angle))) foo))
|#


;;;----------------------------------------------------------------------
;;;                            VALUES
;;;----------------------------------------------------------------------

;;this already exists, maybe in the system
;;(defmacro nth-value (n sexp)
;;  `(nth ,n (multiple-value-list ,sexp)))
  
(defmacro nth-values (ns sexp)
  `(multiple-nth ,ns (multiple-value-list ,sexp)))


;;;----------------------------------------------------------------------
;;;                            COLORS
;;;----------------------------------------------------------------------

(defun hsv-tk-color (h s v)
  (multiple-value-bind (r g b)
      (r-hsv-to-rgb h s v)    
    (format nil "#~2,'0x~2,'0x~2,'0x" r g b)))

(defun rgb-tk-color (r g b)
  (format nil "#~2,'0x~2,'0x~2,'0x" r g b))

(defun rgb-xxxxxx-color (r g b)
  (format nil "~2,'0x~2,'0x~2,'0x" r g b))

(defun rgb256-into-rgb1 (rgb)
  (mapcar (^ (x) (/ x 255.0)) rgb))

(defun tk-color-to-rgb (tkcolor)
  ;; some special cases first for named colors
  ;; file xcolors has list of more, and showrgb on unix usually gives a list

  (if (and (= 7 (length tkcolor))
	   (equal "#" (substring tkcolor 0 0)))
      (let ((r (read-from-string (format nil "#x~a" (substring tkcolor 1 2))))
	    (g (read-from-string (format nil "#x~a" (substring tkcolor 3 4))))
	    (b (read-from-string (format nil "#x~a" (substring tkcolor 5 6)))))
	(list r g b))
    (progn
      (setq tkcolor (string-downcase tkcolor))
      (cond ((equal "snow" tkcolor) (list 255 250 250))
	    ((equal "ghost white" tkcolor) (list 248 248 255))
	    ((equal "ghostwhite" tkcolor) (list 248 248 255))
	    ((equal "white smoke" tkcolor) (list 245 245 245))
	    ((equal "whitesmoke" tkcolor) (list 245 245 245))
	    ((equal "gainsboro" tkcolor) (list 220 220 220))
	    ((equal "floral white" tkcolor) (list 255 250 240))
	    ((equal "floralwhite" tkcolor) (list 255 250 240))
	    ((equal "old lace" tkcolor) (list 253 245 230))
	    ((equal "oldlace" tkcolor) (list 253 245 230))
	    ((equal "linen" tkcolor) (list 250 240 230))
	    ((equal "antique white" tkcolor) (list 250 235 215))
	    ((equal "antiquewhite" tkcolor) (list 250 235 215))
	    ((equal "papaya whip" tkcolor) (list 255 239 213))
	    ((equal "papayawhip" tkcolor) (list 255 239 213))
	    ((equal "blanched almond" tkcolor) (list 255 235 205))
	    ((equal "blanchedalmond" tkcolor) (list 255 235 205))
	    ((equal "bisque" tkcolor) (list 255 228 196))
	    ((equal "peach puff" tkcolor) (list 255 218 185))
	    ((equal "peachpuff" tkcolor) (list 255 218 185))
	    ((equal "navajo white" tkcolor) (list 255 222 173))
	    ((equal "navajowhite" tkcolor) (list 255 222 173))
	    ((equal "moccasin" tkcolor) (list 255 228 181))
	    ((equal "cornsilk" tkcolor) (list 255 248 220))
	    ((equal "ivory" tkcolor) (list 255 255 240))
	    ((equal "lemon chiffon" tkcolor) (list 255 250 205))
	    ((equal "lemonchiffon" tkcolor) (list 255 250 205))
	    ((equal "seashell" tkcolor) (list 255 245 238))
	    ((equal "honeydew" tkcolor) (list 240 255 240))
	    ((equal "mint cream" tkcolor) (list 245 255 250))
	    ((equal "mintcream" tkcolor) (list 245 255 250))
	    ((equal "azure" tkcolor) (list 240 255 255))
	    ((equal "alice blue" tkcolor) (list 240 248 255))
	    ((equal "aliceblue" tkcolor) (list 240 248 255))
	    ((equal "lavender" tkcolor) (list 230 230 250))
	    ((equal "lavender blush" tkcolor) (list 255 240 245))
	    ((equal "lavenderblush" tkcolor) (list 255 240 245))
	    ((equal "misty rose" tkcolor) (list 255 228 225))
	    ((equal "mistyrose" tkcolor) (list 255 228 225))
	    ((equal "white" tkcolor) (list 255 255 255))
	    ((equal "black" tkcolor) (list   0   0   0))
	    ((equal "dark slate gray" tkcolor) (list  47  79  79))
	    ((equal "darkslategray" tkcolor) (list  47  79  79))
	    ((equal "dark slate grey" tkcolor) (list  47  79  79))
	    ((equal "darkslategrey" tkcolor) (list  47  79  79))
	    ((equal "dim gray" tkcolor) (list 105 105 105))
	    ((equal "dimgray" tkcolor) (list 105 105 105))
	    ((equal "dim grey" tkcolor) (list 105 105 105))
	    ((equal "dimgrey" tkcolor) (list 105 105 105))
	    ((equal "slate gray" tkcolor) (list 112 128 144))
	    ((equal "slategray" tkcolor) (list 112 128 144))
	    ((equal "slate grey" tkcolor) (list 112 128 144))
	    ((equal "slategrey" tkcolor) (list 112 128 144))
	    ((equal "light slate gray" tkcolor) (list 119 136 153))
	    ((equal "lightslategray" tkcolor) (list 119 136 153))
	    ((equal "light slate grey" tkcolor) (list 119 136 153))
	    ((equal "lightslategrey" tkcolor) (list 119 136 153))
	    ((equal "gray" tkcolor) (list 190 190 190))
	    ((equal "grey" tkcolor) (list 190 190 190))
	    ((equal "light grey" tkcolor) (list 211 211 211))
	    ((equal "lightgrey" tkcolor) (list 211 211 211))
	    ((equal "light gray" tkcolor) (list 211 211 211))
	    ((equal "lightgray" tkcolor) (list 211 211 211))
	    ((equal "midnight blue" tkcolor) (list  25  25 112))
	    ((equal "midnightblue" tkcolor) (list  25  25 112))
	    ((equal "navy" tkcolor) (list   0   0 128))
	    ((equal "navy blue" tkcolor) (list   0   0 128))
	    ((equal "navyblue" tkcolor) (list   0   0 128))
	    ((equal "cornflower blue" tkcolor) (list 100 149 237))
	    ((equal "cornflowerblue" tkcolor) (list 100 149 237))
	    ((equal "dark slate blue" tkcolor) (list  72  61 139))
	    ((equal "darkslateblue" tkcolor) (list  72  61 139))
	    ((equal "slate blue" tkcolor) (list 106  90 205))
	    ((equal "slateblue" tkcolor) (list 106  90 205))
	    ((equal "medium slate blue" tkcolor) (list 123 104 238))
	    ((equal "mediumslateblue" tkcolor) (list 123 104 238))
	    ((equal "light slate blue" tkcolor) (list 132 112 255))
	    ((equal "lightslateblue" tkcolor) (list 132 112 255))
	    ((equal "medium blue" tkcolor) (list   0   0 205))
	    ((equal "mediumblue" tkcolor) (list   0   0 205))
	    ((equal "royal blue" tkcolor) (list  65 105 225))
	    ((equal "royalblue" tkcolor) (list  65 105 225))
	    ((equal "blue" tkcolor) (list   0   0 255))
	    ((equal "dodger blue" tkcolor) (list  30 144 255))
	    ((equal "dodgerblue" tkcolor) (list  30 144 255))
	    ((equal "deep sky blue" tkcolor) (list   0 191 255))
	    ((equal "deepskyblue" tkcolor) (list   0 191 255))
	    ((equal "sky blue" tkcolor) (list 135 206 235))
	    ((equal "skyblue" tkcolor) (list 135 206 235))
	    ((equal "light sky blue" tkcolor) (list 135 206 250))
	    ((equal "lightskyblue" tkcolor) (list 135 206 250))
	    ((equal "steel blue" tkcolor) (list  70 130 180))
	    ((equal "steelblue" tkcolor) (list  70 130 180))
	    ((equal "light steel blue" tkcolor) (list 176 196 222))
	    ((equal "lightsteelblue" tkcolor) (list 176 196 222))
	    ((equal "light blue" tkcolor) (list 173 216 230))
	    ((equal "lightblue" tkcolor) (list 173 216 230))
	    ((equal "powder blue" tkcolor) (list 176 224 230))
	    ((equal "powderblue" tkcolor) (list 176 224 230))
	    ((equal "pale turquoise" tkcolor) (list 175 238 238))
	    ((equal "paleturquoise" tkcolor) (list 175 238 238))
	    ((equal "dark turquoise" tkcolor) (list   0 206 209))
	    ((equal "darkturquoise" tkcolor) (list   0 206 209))
	    ((equal "medium turquoise" tkcolor) (list  72 209 204))
	    ((equal "mediumturquoise" tkcolor) (list  72 209 204))
	    ((equal "turquoise" tkcolor) (list  64 224 208))
	    ((equal "cyan" tkcolor) (list   0 255 255))
	    ((equal "light cyan" tkcolor) (list 224 255 255))
	    ((equal "lightcyan" tkcolor) (list 224 255 255))
	    ((equal "cadet blue" tkcolor) (list  95 158 160))
	    ((equal "cadetblue" tkcolor) (list  95 158 160))
	    ((equal "medium aquamarine" tkcolor) (list 102 205 170))
	    ((equal "mediumaquamarine" tkcolor) (list 102 205 170))
	    ((equal "aquamarine" tkcolor) (list 127 255 212))
	    ((equal "dark green" tkcolor) (list   0 100   0))
	    ((equal "darkgreen" tkcolor) (list   0 100   0))
	    ((equal "dark olive green" tkcolor) (list  85 107  47))
	    ((equal "darkolivegreen" tkcolor) (list  85 107  47))
	    ((equal "dark sea green" tkcolor) (list 143 188 143))
	    ((equal "darkseagreen" tkcolor) (list 143 188 143))
	    ((equal "sea green" tkcolor) (list  46 139  87))
	    ((equal "seagreen" tkcolor) (list  46 139  87))
	    ((equal "medium sea green" tkcolor) (list  60 179 113))
	    ((equal "mediumseagreen" tkcolor) (list  60 179 113))
	    ((equal "light sea green" tkcolor) (list  32 178 170))
	    ((equal "lightseagreen" tkcolor) (list  32 178 170))
	    ((equal "pale green" tkcolor) (list 152 251 152))
	    ((equal "palegreen" tkcolor) (list 152 251 152))
	    ((equal "spring green" tkcolor) (list   0 255 127))
	    ((equal "springgreen" tkcolor) (list   0 255 127))
	    ((equal "lawn green" tkcolor) (list 124 252   0))
	    ((equal "lawngreen" tkcolor) (list 124 252   0))
	    ((equal "green" tkcolor) (list   0 255   0))
	    ((equal "chartreuse" tkcolor) (list 127 255   0))
	    ((equal "medium spring green" tkcolor) (list   0 250 154))
	    ((equal "mediumspringgreen" tkcolor) (list   0 250 154))
	    ((equal "green yellow" tkcolor) (list 173 255  47))
	    ((equal "greenyellow" tkcolor) (list 173 255  47))
	    ((equal "lime green" tkcolor) (list  50 205  50))
	    ((equal "limegreen" tkcolor) (list  50 205  50))
	    ((equal "yellow green" tkcolor) (list 154 205  50))
	    ((equal "yellowgreen" tkcolor) (list 154 205  50))
	    ((equal "forest green" tkcolor) (list  34 139  34))
	    ((equal "forestgreen" tkcolor) (list  34 139  34))
	    ((equal "olive drab" tkcolor) (list 107 142  35))
	    ((equal "olivedrab" tkcolor) (list 107 142  35))
	    ((equal "dark khaki" tkcolor) (list 189 183 107))
	    ((equal "darkkhaki" tkcolor) (list 189 183 107))
	    ((equal "khaki" tkcolor) (list 240 230 140))
	    ((equal "pale goldenrod" tkcolor) (list 238 232 170))
	    ((equal "palegoldenrod" tkcolor) (list 238 232 170))
	    ((equal "light goldenrod yellow" tkcolor) (list 250 250 210))
	    ((equal "lightgoldenrodyellow" tkcolor) (list 250 250 210))
	    ((equal "light yellow" tkcolor) (list 255 255 224))
	    ((equal "lightyellow" tkcolor) (list 255 255 224))
	    ((equal "yellow" tkcolor) (list 255 255   0))
	    ((equal "gold" tkcolor) (list 255 215   0))
	    ((equal "light goldenrod" tkcolor) (list 238 221 130))
	    ((equal "lightgoldenrod" tkcolor) (list 238 221 130))
	    ((equal "goldenrod" tkcolor) (list 218 165  32))
	    ((equal "dark goldenrod" tkcolor) (list 184 134  11))
	    ((equal "darkgoldenrod" tkcolor) (list 184 134  11))
	    ((equal "rosy brown" tkcolor) (list 188 143 143))
	    ((equal "rosybrown" tkcolor) (list 188 143 143))
	    ((equal "indian red" tkcolor) (list 205  92  92))
	    ((equal "indianred" tkcolor) (list 205  92  92))
	    ((equal "saddle brown" tkcolor) (list 139  69  19))
	    ((equal "saddlebrown" tkcolor) (list 139  69  19))
	    ((equal "sienna" tkcolor) (list 160  82  45))
	    ((equal "peru" tkcolor) (list 205 133  63))
	    ((equal "burlywood" tkcolor) (list 222 184 135))
	    ((equal "beige" tkcolor) (list 245 245 220))
	    ((equal "wheat" tkcolor) (list 245 222 179))
	    ((equal "sandy brown" tkcolor) (list 244 164  96))
	    ((equal "sandybrown" tkcolor) (list 244 164  96))
	    ((equal "tan" tkcolor) (list 210 180 140))
	    ((equal "chocolate" tkcolor) (list 210 105  30))
	    ((equal "firebrick" tkcolor) (list 178  34  34))
	    ((equal "brown" tkcolor) (list 165  42  42))
	    ((equal "dark salmon" tkcolor) (list 233 150 122))
	    ((equal "darksalmon" tkcolor) (list 233 150 122))
	    ((equal "salmon" tkcolor) (list 250 128 114))
	    ((equal "light salmon" tkcolor) (list 255 160 122))
	    ((equal "lightsalmon" tkcolor) (list 255 160 122))
	    ((equal "orange" tkcolor) (list 255 165   0))
	    ((equal "dark orange" tkcolor) (list 255 140   0))
	    ((equal "darkorange" tkcolor) (list 255 140   0))
	    ((equal "coral" tkcolor) (list 255 127  80))
	    ((equal "light coral" tkcolor) (list 240 128 128))
	    ((equal "lightcoral" tkcolor) (list 240 128 128))
	    ((equal "tomato" tkcolor) (list 255  99  71))
	    ((equal "orange red" tkcolor) (list 255  69   0))
	    ((equal "orangered" tkcolor) (list 255  69   0))
	    ((equal "red" tkcolor) (list 255   0   0))
	    ((equal "hot pink" tkcolor) (list 255 105 180))
	    ((equal "hotpink" tkcolor) (list 255 105 180))
	    ((equal "deep pink" tkcolor) (list 255  20 147))
	    ((equal "deeppink" tkcolor) (list 255  20 147))
	    ((equal "pink" tkcolor) (list 255 192 203))
	    ((equal "light pink" tkcolor) (list 255 182 193))
	    ((equal "lightpink" tkcolor) (list 255 182 193))
	    ((equal "pale violet red" tkcolor) (list 219 112 147))
	    ((equal "palevioletred" tkcolor) (list 219 112 147))
	    ((equal "maroon" tkcolor) (list 176  48  96))
	    ((equal "medium violet red" tkcolor) (list 199  21 133))
	    ((equal "mediumvioletred" tkcolor) (list 199  21 133))
	    ((equal "violet red" tkcolor) (list 208  32 144))
	    ((equal "violetred" tkcolor) (list 208  32 144))
	    ((equal "magenta" tkcolor) (list 255   0 255))
	    ((equal "violet" tkcolor) (list 238 130 238))
	    ((equal "plum" tkcolor) (list 221 160 221))
	    ((equal "orchid" tkcolor) (list 218 112 214))
	    ((equal "medium orchid" tkcolor) (list 186  85 211))
	    ((equal "mediumorchid" tkcolor) (list 186  85 211))
	    ((equal "dark orchid" tkcolor) (list 153  50 204))
	    ((equal "darkorchid" tkcolor) (list 153  50 204))
	    ((equal "dark violet" tkcolor) (list 148   0 211))
	    ((equal "darkviolet" tkcolor) (list 148   0 211))
	    ((equal "blue violet" tkcolor) (list 138  43 226))
	    ((equal "blueviolet" tkcolor) (list 138  43 226))
	    ((equal "purple" tkcolor) (list 160  32 240))
	    ((equal "medium purple" tkcolor) (list 147 112 219))
	    ((equal "mediumpurple" tkcolor) (list 147 112 219))
	    ((equal "thistle" tkcolor) (list 216 191 216))
	    ((equal "snow1" tkcolor) (list 255 250 250))
	    ((equal "snow2" tkcolor) (list 238 233 233))
	    ((equal "snow3" tkcolor) (list 205 201 201))
	    ((equal "snow4" tkcolor) (list 139 137 137))
	    ((equal "seashell1" tkcolor) (list 255 245 238))
	    ((equal "seashell2" tkcolor) (list 238 229 222))
	    ((equal "seashell3" tkcolor) (list 205 197 191))
	    ((equal "seashell4" tkcolor) (list 139 134 130))
	    ((equal "antiquewhite1" tkcolor) (list 255 239 219))
	    ((equal "antiquewhite2" tkcolor) (list 238 223 204))
	    ((equal "antiquewhite3" tkcolor) (list 205 192 176))
	    ((equal "antiquewhite4" tkcolor) (list 139 131 120))
	    ((equal "bisque1" tkcolor) (list 255 228 196))
	    ((equal "bisque2" tkcolor) (list 238 213 183))
	    ((equal "bisque3" tkcolor) (list 205 183 158))
	    ((equal "bisque4" tkcolor) (list 139 125 107))
	    ((equal "peachpuff1" tkcolor) (list 255 218 185))
	    ((equal "peachpuff2" tkcolor) (list 238 203 173))
	    ((equal "peachpuff3" tkcolor) (list 205 175 149))
	    ((equal "peachpuff4" tkcolor) (list 139 119 101))
	    ((equal "navajowhite1" tkcolor) (list 255 222 173))
	    ((equal "navajowhite2" tkcolor) (list 238 207 161))
	    ((equal "navajowhite3" tkcolor) (list 205 179 139))
	    ((equal "navajowhite4" tkcolor) (list 139 121	 94))
	    ((equal "lemonchiffon1" tkcolor) (list 255 250 205))
	    ((equal "lemonchiffon2" tkcolor) (list 238 233 191))
	    ((equal "lemonchiffon3" tkcolor) (list 205 201 165))
	    ((equal "lemonchiffon4" tkcolor) (list 139 137 112))
	    ((equal "cornsilk1" tkcolor) (list 255 248 220))
	    ((equal "cornsilk2" tkcolor) (list 238 232 205))
	    ((equal "cornsilk3" tkcolor) (list 205 200 177))
	    ((equal "cornsilk4" tkcolor) (list 139 136 120))
	    ((equal "ivory1" tkcolor) (list 255 255 240))
	    ((equal "ivory2" tkcolor) (list 238 238 224))
	    ((equal "ivory3" tkcolor) (list 205 205 193))
	    ((equal "ivory4" tkcolor) (list 139 139 131))
	    ((equal "honeydew1" tkcolor) (list 240 255 240))
	    ((equal "honeydew2" tkcolor) (list 224 238 224))
	    ((equal "honeydew3" tkcolor) (list 193 205 193))
	    ((equal "honeydew4" tkcolor) (list 131 139 131))
	    ((equal "lavenderblush1" tkcolor) (list 255 240 245))
	    ((equal "lavenderblush2" tkcolor) (list 238 224 229))
	    ((equal "lavenderblush3" tkcolor) (list 205 193 197))
	    ((equal "lavenderblush4" tkcolor) (list 139 131 134))
	    ((equal "mistyrose1" tkcolor) (list 255 228 225))
	    ((equal "mistyrose2" tkcolor) (list 238 213 210))
	    ((equal "mistyrose3" tkcolor) (list 205 183 181))
	    ((equal "mistyrose4" tkcolor) (list 139 125 123))
	    ((equal "azure1" tkcolor) (list 240 255 255))
	    ((equal "azure2" tkcolor) (list 224 238 238))
	    ((equal "azure3" tkcolor) (list 193 205 205))
	    ((equal "azure4" tkcolor) (list 131 139 139))
	    ((equal "slateblue1" tkcolor) (list 131 111 255))
	    ((equal "slateblue2" tkcolor) (list 122 103 238))
	    ((equal "slateblue3" tkcolor) (list 105  89 205))
	    ((equal "slateblue4" tkcolor) (list  71  60 139))
	    ((equal "royalblue1" tkcolor) (list  72 118 255))
	    ((equal "royalblue2" tkcolor) (list  67 110 238))
	    ((equal "royalblue3" tkcolor) (list  58  95 205))
	    ((equal "royalblue4" tkcolor) (list  39  64 139))
	    ((equal "blue1" tkcolor) (list   0   0 255))
	    ((equal "blue2" tkcolor) (list   0   0 238))
	    ((equal "blue3" tkcolor) (list   0   0 205))
	    ((equal "blue4" tkcolor) (list   0   0 139))
	    ((equal "dodgerblue1" tkcolor) (list  30 144 255))
	    ((equal "dodgerblue2" tkcolor) (list  28 134 238))
	    ((equal "dodgerblue3" tkcolor) (list  24 116 205))
	    ((equal "dodgerblue4" tkcolor) (list  16  78 139))
	    ((equal "steelblue1" tkcolor) (list  99 184 255))
	    ((equal "steelblue2" tkcolor) (list  92 172 238))
	    ((equal "steelblue3" tkcolor) (list  79 148 205))
	    ((equal "steelblue4" tkcolor) (list  54 100 139))
	    ((equal "deepskyblue1" tkcolor) (list   0 191 255))
	    ((equal "deepskyblue2" tkcolor) (list   0 178 238))
	    ((equal "deepskyblue3" tkcolor) (list   0 154 205))
	    ((equal "deepskyblue4" tkcolor) (list   0 104 139))
	    ((equal "skyblue1" tkcolor) (list 135 206 255))
	    ((equal "skyblue2" tkcolor) (list 126 192 238))
	    ((equal "skyblue3" tkcolor) (list 108 166 205))
	    ((equal "skyblue4" tkcolor) (list  74 112 139))
	    ((equal "lightskyblue1" tkcolor) (list 176 226 255))
	    ((equal "lightskyblue2" tkcolor) (list 164 211 238))
	    ((equal "lightskyblue3" tkcolor) (list 141 182 205))
	    ((equal "lightskyblue4" tkcolor) (list  96 123 139))
	    ((equal "slategray1" tkcolor) (list 198 226 255))
	    ((equal "slategray2" tkcolor) (list 185 211 238))
	    ((equal "slategray3" tkcolor) (list 159 182 205))
	    ((equal "slategray4" tkcolor) (list 108 123 139))
	    ((equal "lightsteelblue1" tkcolor) (list 202 225 255))
	    ((equal "lightsteelblue2" tkcolor) (list 188 210 238))
	    ((equal "lightsteelblue3" tkcolor) (list 162 181 205))
	    ((equal "lightsteelblue4" tkcolor) (list 110 123 139))
	    ((equal "lightblue1" tkcolor) (list 191 239 255))
	    ((equal "lightblue2" tkcolor) (list 178 223 238))
	    ((equal "lightblue3" tkcolor) (list 154 192 205))
	    ((equal "lightblue4" tkcolor) (list 104 131 139))
	    ((equal "lightcyan1" tkcolor) (list 224 255 255))
	    ((equal "lightcyan2" tkcolor) (list 209 238 238))
	    ((equal "lightcyan3" tkcolor) (list 180 205 205))
	    ((equal "lightcyan4" tkcolor) (list 122 139 139))
	    ((equal "paleturquoise1" tkcolor) (list 187 255 255))
	    ((equal "paleturquoise2" tkcolor) (list 174 238 238))
	    ((equal "paleturquoise3" tkcolor) (list 150 205 205))
	    ((equal "paleturquoise4" tkcolor) (list 102 139 139))
	    ((equal "cadetblue1" tkcolor) (list 152 245 255))
	    ((equal "cadetblue2" tkcolor) (list 142 229 238))
	    ((equal "cadetblue3" tkcolor) (list 122 197 205))
	    ((equal "cadetblue4" tkcolor) (list  83 134 139))
	    ((equal "turquoise1" tkcolor) (list   0 245 255))
	    ((equal "turquoise2" tkcolor) (list   0 229 238))
	    ((equal "turquoise3" tkcolor) (list   0 197 205))
	    ((equal "turquoise4" tkcolor) (list   0 134 139))
	    ((equal "cyan1" tkcolor) (list   0 255 255))
	    ((equal "cyan2" tkcolor) (list   0 238 238))
	    ((equal "cyan3" tkcolor) (list   0 205 205))
	    ((equal "cyan4" tkcolor) (list   0 139 139))
	    ((equal "darkslategray1" tkcolor) (list 151 255 255))
	    ((equal "darkslategray2" tkcolor) (list 141 238 238))
	    ((equal "darkslategray3" tkcolor) (list 121 205 205))
	    ((equal "darkslategray4" tkcolor) (list  82 139 139))
	    ((equal "aquamarine1" tkcolor) (list 127 255 212))
	    ((equal "aquamarine2" tkcolor) (list 118 238 198))
	    ((equal "aquamarine3" tkcolor) (list 102 205 170))
	    ((equal "aquamarine4" tkcolor) (list  69 139 116))
	    ((equal "darkseagreen1" tkcolor) (list 193 255 193))
	    ((equal "darkseagreen2" tkcolor) (list 180 238 180))
	    ((equal "darkseagreen3" tkcolor) (list 155 205 155))
	    ((equal "darkseagreen4" tkcolor) (list 105 139 105))
	    ((equal "seagreen1" tkcolor) (list  84 255 159))
	    ((equal "seagreen2" tkcolor) (list  78 238 148))
	    ((equal "seagreen3" tkcolor) (list  67 205 128))
	    ((equal "seagreen4" tkcolor) (list  46 139	 87))
	    ((equal "palegreen1" tkcolor) (list 154 255 154))
	    ((equal "palegreen2" tkcolor) (list 144 238 144))
	    ((equal "palegreen3" tkcolor) (list 124 205 124))
	    ((equal "palegreen4" tkcolor) (list  84 139	 84))
	    ((equal "springgreen1" tkcolor) (list   0 255 127))
	    ((equal "springgreen2" tkcolor) (list   0 238 118))
	    ((equal "springgreen3" tkcolor) (list   0 205 102))
	    ((equal "springgreen4" tkcolor) (list   0 139	 69))
	    ((equal "green1" tkcolor) (list   0 255	  0))
	    ((equal "green2" tkcolor) (list   0 238	  0))
	    ((equal "green3" tkcolor) (list   0 205	  0))
	    ((equal "green4" tkcolor) (list   0 139	  0))
	    ((equal "chartreuse1" tkcolor) (list 127 255	  0))
	    ((equal "chartreuse2" tkcolor) (list 118 238	  0))
	    ((equal "chartreuse3" tkcolor) (list 102 205	  0))
	    ((equal "chartreuse4" tkcolor) (list  69 139	  0))
	    ((equal "olivedrab1" tkcolor) (list 192 255	 62))
	    ((equal "olivedrab2" tkcolor) (list 179 238	 58))
	    ((equal "olivedrab3" tkcolor) (list 154 205	 50))
	    ((equal "olivedrab4" tkcolor) (list 105 139	 34))
	    ((equal "darkolivegreen1" tkcolor) (list 202 255 112))
	    ((equal "darkolivegreen2" tkcolor) (list 188 238 104))
	    ((equal "darkolivegreen3" tkcolor) (list 162 205	 90))
	    ((equal "darkolivegreen4" tkcolor) (list 110 139	 61))
	    ((equal "khaki1" tkcolor) (list 255 246 143))
	    ((equal "khaki2" tkcolor) (list 238 230 133))
	    ((equal "khaki3" tkcolor) (list 205 198 115))
	    ((equal "khaki4" tkcolor) (list 139 134	 78))
	    ((equal "lightgoldenrod1" tkcolor) (list 255 236 139))
	    ((equal "lightgoldenrod2" tkcolor) (list 238 220 130))
	    ((equal "lightgoldenrod3" tkcolor) (list 205 190 112))
	    ((equal "lightgoldenrod4" tkcolor) (list 139 129	 76))
	    ((equal "lightyellow1" tkcolor) (list 255 255 224))
	    ((equal "lightyellow2" tkcolor) (list 238 238 209))
	    ((equal "lightyellow3" tkcolor) (list 205 205 180))
	    ((equal "lightyellow4" tkcolor) (list 139 139 122))
	    ((equal "yellow1" tkcolor) (list 255 255	  0))
	    ((equal "yellow2" tkcolor) (list 238 238	  0))
	    ((equal "yellow3" tkcolor) (list 205 205	  0))
	    ((equal "yellow4" tkcolor) (list 139 139	  0))
	    ((equal "gold1" tkcolor) (list 255 215	  0))
	    ((equal "gold2" tkcolor) (list 238 201	  0))
	    ((equal "gold3" tkcolor) (list 205 173	  0))
	    ((equal "gold4" tkcolor) (list 139 117	  0))
	    ((equal "goldenrod1" tkcolor) (list 255 193	 37))
	    ((equal "goldenrod2" tkcolor) (list 238 180	 34))
	    ((equal "goldenrod3" tkcolor) (list 205 155	 29))
	    ((equal "goldenrod4" tkcolor) (list 139 105	 20))
	    ((equal "darkgoldenrod1" tkcolor) (list 255 185	 15))
	    ((equal "darkgoldenrod2" tkcolor) (list 238 173	 14))
	    ((equal "darkgoldenrod3" tkcolor) (list 205 149	 12))
	    ((equal "darkgoldenrod4" tkcolor) (list 139 101	  8))
	    ((equal "rosybrown1" tkcolor) (list 255 193 193))
	    ((equal "rosybrown2" tkcolor) (list 238 180 180))
	    ((equal "rosybrown3" tkcolor) (list 205 155 155))
	    ((equal "rosybrown4" tkcolor) (list 139 105 105))
	    ((equal "indianred1" tkcolor) (list 255 106 106))
	    ((equal "indianred2" tkcolor) (list 238  99	 99))
	    ((equal "indianred3" tkcolor) (list 205  85	 85))
	    ((equal "indianred4" tkcolor) (list 139  58	 58))
	    ((equal "sienna1" tkcolor) (list 255 130	 71))
	    ((equal "sienna2" tkcolor) (list 238 121	 66))
	    ((equal "sienna3" tkcolor) (list 205 104	 57))
	    ((equal "sienna4" tkcolor) (list 139  71	 38))
	    ((equal "burlywood1" tkcolor) (list 255 211 155))
	    ((equal "burlywood2" tkcolor) (list 238 197 145))
	    ((equal "burlywood3" tkcolor) (list 205 170 125))
	    ((equal "burlywood4" tkcolor) (list 139 115	 85))
	    ((equal "wheat1" tkcolor) (list 255 231 186))
	    ((equal "wheat2" tkcolor) (list 238 216 174))
	    ((equal "wheat3" tkcolor) (list 205 186 150))
	    ((equal "wheat4" tkcolor) (list 139 126 102))
	    ((equal "tan1" tkcolor) (list 255 165	 79))
	    ((equal "tan2" tkcolor) (list 238 154	 73))
	    ((equal "tan3" tkcolor) (list 205 133	 63))
	    ((equal "tan4" tkcolor) (list 139  90	 43))
	    ((equal "chocolate1" tkcolor) (list 255 127	 36))
	    ((equal "chocolate2" tkcolor) (list 238 118	 33))
	    ((equal "chocolate3" tkcolor) (list 205 102	 29))
	    ((equal "chocolate4" tkcolor) (list 139  69	 19))
	    ((equal "firebrick1" tkcolor) (list 255  48	 48))
	    ((equal "firebrick2" tkcolor) (list 238  44	 44))
	    ((equal "firebrick3" tkcolor) (list 205  38	 38))
	    ((equal "firebrick4" tkcolor) (list 139  26	 26))
	    ((equal "brown1" tkcolor) (list 255  64	 64))
	    ((equal "brown2" tkcolor) (list 238  59	 59))
	    ((equal "brown3" tkcolor) (list 205  51	 51))
	    ((equal "brown4" tkcolor) (list 139  35	 35))
	    ((equal "salmon1" tkcolor) (list 255 140 105))
	    ((equal "salmon2" tkcolor) (list 238 130	 98))
	    ((equal "salmon3" tkcolor) (list 205 112	 84))
	    ((equal "salmon4" tkcolor) (list 139  76	 57))
	    ((equal "lightsalmon1" tkcolor) (list 255 160 122))
	    ((equal "lightsalmon2" tkcolor) (list 238 149 114))
	    ((equal "lightsalmon3" tkcolor) (list 205 129	 98))
	    ((equal "lightsalmon4" tkcolor) (list 139  87	 66))
	    ((equal "orange1" tkcolor) (list 255 165	  0))
	    ((equal "orange2" tkcolor) (list 238 154	  0))
	    ((equal "orange3" tkcolor) (list 205 133	  0))
	    ((equal "orange4" tkcolor) (list 139  90	  0))
	    ((equal "darkorange1" tkcolor) (list 255 127	  0))
	    ((equal "darkorange2" tkcolor) (list 238 118	  0))
	    ((equal "darkorange3" tkcolor) (list 205 102	  0))
	    ((equal "darkorange4" tkcolor) (list 139  69	  0))
	    ((equal "coral1" tkcolor) (list 255 114	 86))
	    ((equal "coral2" tkcolor) (list 238 106	 80))
	    ((equal "coral3" tkcolor) (list 205  91	 69))
	    ((equal "coral4" tkcolor) (list 139  62	 47))
	    ((equal "tomato1" tkcolor) (list 255  99	 71))
	    ((equal "tomato2" tkcolor) (list 238  92	 66))
	    ((equal "tomato3" tkcolor) (list 205  79	 57))
	    ((equal "tomato4" tkcolor) (list 139  54	 38))
	    ((equal "orangered1" tkcolor) (list 255  69	  0))
	    ((equal "orangered2" tkcolor) (list 238  64	  0))
	    ((equal "orangered3" tkcolor) (list 205  55	  0))
	    ((equal "orangered4" tkcolor) (list 139  37	  0))
	    ((equal "red1" tkcolor) (list 255   0	  0))
	    ((equal "red2" tkcolor) (list 238   0	  0))
	    ((equal "red3" tkcolor) (list 205   0	  0))
	    ((equal "red4" tkcolor) (list 139   0	  0))
	    ((equal "deeppink1" tkcolor) (list 255  20 147))
	    ((equal "deeppink2" tkcolor) (list 238  18 137))
	    ((equal "deeppink3" tkcolor) (list 205  16 118))
	    ((equal "deeppink4" tkcolor) (list 139  10	 80))
	    ((equal "hotpink1" tkcolor) (list 255 110 180))
	    ((equal "hotpink2" tkcolor) (list 238 106 167))
	    ((equal "hotpink3" tkcolor) (list 205  96 144))
	    ((equal "hotpink4" tkcolor) (list 139  58  98))
	    ((equal "pink1" tkcolor) (list 255 181 197))
	    ((equal "pink2" tkcolor) (list 238 169 184))
	    ((equal "pink3" tkcolor) (list 205 145 158))
	    ((equal "pink4" tkcolor) (list 139  99 108))
	    ((equal "lightpink1" tkcolor) (list 255 174 185))
	    ((equal "lightpink2" tkcolor) (list 238 162 173))
	    ((equal "lightpink3" tkcolor) (list 205 140 149))
	    ((equal "lightpink4" tkcolor) (list 139  95 101))
	    ((equal "palevioletred1" tkcolor) (list 255 130 171))
	    ((equal "palevioletred2" tkcolor) (list 238 121 159))
	    ((equal "palevioletred3" tkcolor) (list 205 104 137))
	    ((equal "palevioletred4" tkcolor) (list 139  71	 93))
	    ((equal "maroon1" tkcolor) (list 255  52 179))
	    ((equal "maroon2" tkcolor) (list 238  48 167))
	    ((equal "maroon3" tkcolor) (list 205  41 144))
	    ((equal "maroon4" tkcolor) (list 139  28	 98))
	    ((equal "violetred1" tkcolor) (list 255  62 150))
	    ((equal "violetred2" tkcolor) (list 238  58 140))
	    ((equal "violetred3" tkcolor) (list 205  50 120))
	    ((equal "violetred4" tkcolor) (list 139  34	 82))
	    ((equal "magenta1" tkcolor) (list 255   0 255))
	    ((equal "magenta2" tkcolor) (list 238   0 238))
	    ((equal "magenta3" tkcolor) (list 205   0 205))
	    ((equal "magenta4" tkcolor) (list 139   0 139))
	    ((equal "orchid1" tkcolor) (list 255 131 250))
	    ((equal "orchid2" tkcolor) (list 238 122 233))
	    ((equal "orchid3" tkcolor) (list 205 105 201))
	    ((equal "orchid4" tkcolor) (list 139  71 137))
	    ((equal "plum1" tkcolor) (list 255 187 255))
	    ((equal "plum2" tkcolor) (list 238 174 238))
	    ((equal "plum3" tkcolor) (list 205 150 205))
	    ((equal "plum4" tkcolor) (list 139 102 139))
	    ((equal "mediumorchid1" tkcolor) (list 224 102 255))
	    ((equal "mediumorchid2" tkcolor) (list 209  95 238))
	    ((equal "mediumorchid3" tkcolor) (list 180  82 205))
	    ((equal "mediumorchid4" tkcolor) (list 122  55 139))
	    ((equal "darkorchid1" tkcolor) (list 191  62 255))
	    ((equal "darkorchid2" tkcolor) (list 178  58 238))
	    ((equal "darkorchid3" tkcolor) (list 154  50 205))
	    ((equal "darkorchid4" tkcolor) (list 104  34 139))
	    ((equal "purple1" tkcolor) (list 155  48 255))
	    ((equal "purple2" tkcolor) (list 145  44 238))
	    ((equal "purple3" tkcolor) (list 125  38 205))
	    ((equal "purple4" tkcolor) (list  85  26 139))
	    ((equal "mediumpurple1" tkcolor) (list 171 130 255))
	    ((equal "mediumpurple2" tkcolor) (list 159 121 238))
	    ((equal "mediumpurple3" tkcolor) (list 137 104 205))
	    ((equal "mediumpurple4" tkcolor) (list  93  71 139))
	    ((equal "thistle1" tkcolor) (list 255 225 255))
	    ((equal "thistle2" tkcolor) (list 238 210 238))
	    ((equal "thistle3" tkcolor) (list 205 181 205))
	    ((equal "thistle4" tkcolor) (list 139 123 139))
	    ((equal "gray0" tkcolor) (list   0   0   0))
	    ((equal "grey0" tkcolor) (list   0   0   0))
	    ((equal "gray1" tkcolor) (list   3   3   3))
	    ((equal "grey1" tkcolor) (list   3   3   3))
	    ((equal "gray2" tkcolor) (list   5   5   5))
	    ((equal "grey2" tkcolor) (list   5   5   5))
	    ((equal "gray3" tkcolor) (list   8   8   8))
	    ((equal "grey3" tkcolor) (list   8   8   8))
	    ((equal "gray4" tkcolor) (list  10  10  10))
	    ((equal "grey4" tkcolor) (list  10  10  10))
	    ((equal "gray5" tkcolor) (list  13  13  13))
	    ((equal "grey5" tkcolor) (list  13  13  13))
	    ((equal "gray6" tkcolor) (list  15  15  15))
	    ((equal "grey6" tkcolor) (list  15  15  15))
	    ((equal "gray7" tkcolor) (list  18  18  18))
	    ((equal "grey7" tkcolor) (list  18  18  18))
	    ((equal "gray8" tkcolor) (list  20  20  20))
	    ((equal "grey8" tkcolor) (list  20  20  20))
	    ((equal "gray9" tkcolor) (list  23  23  23))
	    ((equal "grey9" tkcolor) (list  23  23  23))
	    ((equal "gray10" tkcolor) (list  26  26  26))
	    ((equal "grey10" tkcolor) (list  26  26  26))
	    ((equal "gray11" tkcolor) (list  28  28  28))
	    ((equal "grey11" tkcolor) (list  28  28  28))
	    ((equal "gray12" tkcolor) (list  31  31  31))
	    ((equal "grey12" tkcolor) (list  31  31  31))
	    ((equal "gray13" tkcolor) (list  33  33  33))
	    ((equal "grey13" tkcolor) (list  33  33  33))
	    ((equal "gray14" tkcolor) (list  36  36  36))
	    ((equal "grey14" tkcolor) (list  36  36  36))
	    ((equal "gray15" tkcolor) (list  38  38  38))
	    ((equal "grey15" tkcolor) (list  38  38  38))
	    ((equal "gray16" tkcolor) (list  41  41  41))
	    ((equal "grey16" tkcolor) (list  41  41  41))
	    ((equal "gray17" tkcolor) (list  43  43  43))
	    ((equal "grey17" tkcolor) (list  43  43  43))
	    ((equal "gray18" tkcolor) (list  46  46  46))
	    ((equal "grey18" tkcolor) (list  46  46  46))
	    ((equal "gray19" tkcolor) (list  48  48  48))
	    ((equal "grey19" tkcolor) (list  48  48  48))
	    ((equal "gray20" tkcolor) (list  51  51  51))
	    ((equal "grey20" tkcolor) (list  51  51  51))
	    ((equal "gray21" tkcolor) (list  54  54  54))
	    ((equal "grey21" tkcolor) (list  54  54  54))
	    ((equal "gray22" tkcolor) (list  56  56  56))
	    ((equal "grey22" tkcolor) (list  56  56  56))
	    ((equal "gray23" tkcolor) (list  59  59  59))
	    ((equal "grey23" tkcolor) (list  59  59  59))
	    ((equal "gray24" tkcolor) (list  61  61  61))
	    ((equal "grey24" tkcolor) (list  61  61  61))
	    ((equal "gray25" tkcolor) (list  64  64  64))
	    ((equal "grey25" tkcolor) (list  64  64  64))
	    ((equal "gray26" tkcolor) (list  66  66  66))
	    ((equal "grey26" tkcolor) (list  66  66  66))
	    ((equal "gray27" tkcolor) (list  69  69  69))
	    ((equal "grey27" tkcolor) (list  69  69  69))
	    ((equal "gray28" tkcolor) (list  71  71  71))
	    ((equal "grey28" tkcolor) (list  71  71  71))
	    ((equal "gray29" tkcolor) (list  74  74  74))
	    ((equal "grey29" tkcolor) (list  74  74  74))
	    ((equal "gray30" tkcolor) (list  77  77  77))
	    ((equal "grey30" tkcolor) (list  77  77  77))
	    ((equal "gray31" tkcolor) (list  79  79  79))
	    ((equal "grey31" tkcolor) (list  79  79  79))
	    ((equal "gray32" tkcolor) (list  82  82  82))
	    ((equal "grey32" tkcolor) (list  82  82  82))
	    ((equal "gray33" tkcolor) (list  84  84  84))
	    ((equal "grey33" tkcolor) (list  84  84  84))
	    ((equal "gray34" tkcolor) (list  87  87  87))
	    ((equal "grey34" tkcolor) (list  87  87  87))
	    ((equal "gray35" tkcolor) (list  89  89  89))
	    ((equal "grey35" tkcolor) (list  89  89  89))
	    ((equal "gray36" tkcolor) (list  92  92  92))
	    ((equal "grey36" tkcolor) (list  92  92  92))
	    ((equal "gray37" tkcolor) (list  94  94  94))
	    ((equal "grey37" tkcolor) (list  94  94  94))
	    ((equal "gray38" tkcolor) (list  97  97  97))
	    ((equal "grey38" tkcolor) (list  97  97  97))
	    ((equal "gray39" tkcolor) (list  99  99  99))
	    ((equal "grey39" tkcolor) (list  99  99  99))
	    ((equal "gray40" tkcolor) (list 102 102 102))
	    ((equal "grey40" tkcolor) (list 102 102 102))
	    ((equal "gray41" tkcolor) (list 105 105 105))
	    ((equal "grey41" tkcolor) (list 105 105 105))
	    ((equal "gray42" tkcolor) (list 107 107 107))
	    ((equal "grey42" tkcolor) (list 107 107 107))
	    ((equal "gray43" tkcolor) (list 110 110 110))
	    ((equal "grey43" tkcolor) (list 110 110 110))
	    ((equal "gray44" tkcolor) (list 112 112 112))
	    ((equal "grey44" tkcolor) (list 112 112 112))
	    ((equal "gray45" tkcolor) (list 115 115 115))
	    ((equal "grey45" tkcolor) (list 115 115 115))
	    ((equal "gray46" tkcolor) (list 117 117 117))
	    ((equal "grey46" tkcolor) (list 117 117 117))
	    ((equal "gray47" tkcolor) (list 120 120 120))
	    ((equal "grey47" tkcolor) (list 120 120 120))
	    ((equal "gray48" tkcolor) (list 122 122 122))
	    ((equal "grey48" tkcolor) (list 122 122 122))
	    ((equal "gray49" tkcolor) (list 125 125 125))
	    ((equal "grey49" tkcolor) (list 125 125 125))
	    ((equal "gray50" tkcolor) (list 127 127 127))
	    ((equal "grey50" tkcolor) (list 127 127 127))
	    ((equal "gray51" tkcolor) (list 130 130 130))
	    ((equal "grey51" tkcolor) (list 130 130 130))
	    ((equal "gray52" tkcolor) (list 133 133 133))
	    ((equal "grey52" tkcolor) (list 133 133 133))
	    ((equal "gray53" tkcolor) (list 135 135 135))
	    ((equal "grey53" tkcolor) (list 135 135 135))
	    ((equal "gray54" tkcolor) (list 138 138 138))
	    ((equal "grey54" tkcolor) (list 138 138 138))
	    ((equal "gray55" tkcolor) (list 140 140 140))
	    ((equal "grey55" tkcolor) (list 140 140 140))
	    ((equal "gray56" tkcolor) (list 143 143 143))
	    ((equal "grey56" tkcolor) (list 143 143 143))
	    ((equal "gray57" tkcolor) (list 145 145 145))
	    ((equal "grey57" tkcolor) (list 145 145 145))
	    ((equal "gray58" tkcolor) (list 148 148 148))
	    ((equal "grey58" tkcolor) (list 148 148 148))
	    ((equal "gray59" tkcolor) (list 150 150 150))
	    ((equal "grey59" tkcolor) (list 150 150 150))
	    ((equal "gray60" tkcolor) (list 153 153 153))
	    ((equal "grey60" tkcolor) (list 153 153 153))
	    ((equal "gray61" tkcolor) (list 156 156 156))
	    ((equal "grey61" tkcolor) (list 156 156 156))
	    ((equal "gray62" tkcolor) (list 158 158 158))
	    ((equal "grey62" tkcolor) (list 158 158 158))
	    ((equal "gray63" tkcolor) (list 161 161 161))
	    ((equal "grey63" tkcolor) (list 161 161 161))
	    ((equal "gray64" tkcolor) (list 163 163 163))
	    ((equal "grey64" tkcolor) (list 163 163 163))
	    ((equal "gray65" tkcolor) (list 166 166 166))
	    ((equal "grey65" tkcolor) (list 166 166 166))
	    ((equal "gray66" tkcolor) (list 168 168 168))
	    ((equal "grey66" tkcolor) (list 168 168 168))
	    ((equal "gray67" tkcolor) (list 171 171 171))
	    ((equal "grey67" tkcolor) (list 171 171 171))
	    ((equal "gray68" tkcolor) (list 173 173 173))
	    ((equal "grey68" tkcolor) (list 173 173 173))
	    ((equal "gray69" tkcolor) (list 176 176 176))
	    ((equal "grey69" tkcolor) (list 176 176 176))
	    ((equal "gray70" tkcolor) (list 179 179 179))
	    ((equal "grey70" tkcolor) (list 179 179 179))
	    ((equal "gray71" tkcolor) (list 181 181 181))
	    ((equal "grey71" tkcolor) (list 181 181 181))
	    ((equal "gray72" tkcolor) (list 184 184 184))
	    ((equal "grey72" tkcolor) (list 184 184 184))
	    ((equal "gray73" tkcolor) (list 186 186 186))
	    ((equal "grey73" tkcolor) (list 186 186 186))
	    ((equal "gray74" tkcolor) (list 189 189 189))
	    ((equal "grey74" tkcolor) (list 189 189 189))
	    ((equal "gray75" tkcolor) (list 191 191 191))
	    ((equal "grey75" tkcolor) (list 191 191 191))
	    ((equal "gray76" tkcolor) (list 194 194 194))
	    ((equal "grey76" tkcolor) (list 194 194 194))
	    ((equal "gray77" tkcolor) (list 196 196 196))
	    ((equal "grey77" tkcolor) (list 196 196 196))
	    ((equal "gray78" tkcolor) (list 199 199 199))
	    ((equal "grey78" tkcolor) (list 199 199 199))
	    ((equal "gray79" tkcolor) (list 201 201 201))
	    ((equal "grey79" tkcolor) (list 201 201 201))
	    ((equal "gray80" tkcolor) (list 204 204 204))
	    ((equal "grey80" tkcolor) (list 204 204 204))
	    ((equal "gray81" tkcolor) (list 207 207 207))
	    ((equal "grey81" tkcolor) (list 207 207 207))
	    ((equal "gray82" tkcolor) (list 209 209 209))
	    ((equal "grey82" tkcolor) (list 209 209 209))
	    ((equal "gray83" tkcolor) (list 212 212 212))
	    ((equal "grey83" tkcolor) (list 212 212 212))
	    ((equal "gray84" tkcolor) (list 214 214 214))
	    ((equal "grey84" tkcolor) (list 214 214 214))
	    ((equal "gray85" tkcolor) (list 217 217 217))
	    ((equal "grey85" tkcolor) (list 217 217 217))
	    ((equal "gray86" tkcolor) (list 219 219 219))
	    ((equal "grey86" tkcolor) (list 219 219 219))
	    ((equal "gray87" tkcolor) (list 222 222 222))
	    ((equal "grey87" tkcolor) (list 222 222 222))
	    ((equal "gray88" tkcolor) (list 224 224 224))
	    ((equal "grey88" tkcolor) (list 224 224 224))
	    ((equal "gray89" tkcolor) (list 227 227 227))
	    ((equal "grey89" tkcolor) (list 227 227 227))
	    ((equal "gray90" tkcolor) (list 229 229 229))
	    ((equal "grey90" tkcolor) (list 229 229 229))
	    ((equal "gray91" tkcolor) (list 232 232 232))
	    ((equal "grey91" tkcolor) (list 232 232 232))
	    ((equal "gray92" tkcolor) (list 235 235 235))
	    ((equal "grey92" tkcolor) (list 235 235 235))
	    ((equal "gray93" tkcolor) (list 237 237 237))
	    ((equal "grey93" tkcolor) (list 237 237 237))
	    ((equal "gray94" tkcolor) (list 240 240 240))
	    ((equal "grey94" tkcolor) (list 240 240 240))
	    ((equal "gray95" tkcolor) (list 242 242 242))
	    ((equal "grey95" tkcolor) (list 242 242 242))
	    ((equal "gray96" tkcolor) (list 245 245 245))
	    ((equal "grey96" tkcolor) (list 245 245 245))
	    ((equal "gray97" tkcolor) (list 247 247 247))
	    ((equal "grey97" tkcolor) (list 247 247 247))
	    ((equal "gray98" tkcolor) (list 250 250 250))
	    ((equal "grey98" tkcolor) (list 250 250 250))
	    ((equal "gray99" tkcolor) (list 252 252 252))
	    ((equal "grey99" tkcolor) (list 252 252 252))
	    ((equal "gray100" tkcolor) (list 255 255 255))
	    ((equal "grey100" tkcolor) (list 255 255 255))
	    ((equal "dark grey" tkcolor) (list 169 169 169))
	    ((equal "darkgrey" tkcolor) (list 169 169 169))
	    ((equal "dark gray" tkcolor) (list 169 169 169))
	    ((equal "darkgray" tkcolor) (list 169 169 169))
	    ((equal "dark blue" tkcolor) (list 0     0 139))
	    ((equal "darkblue" tkcolor) (list 0     0 139))
	    ((equal "dark cyan" tkcolor) (list 0   139 139))
	    ((equal "darkcyan" tkcolor) (list 0   139 139))
	    ((equal "dark magenta" tkcolor) (list 139   0 139))
	    ((equal "darkmagenta" tkcolor) (list 139   0 139))
	    ((equal "dark red" tkcolor) (list 139   0   0))
	    ((equal "darkred" tkcolor) (list 139   0   0))
	    ((equal "light green" tkcolor) (list 144 238 144))
	    ((equal "lightgreen" tkcolor) (list 144 238 144))
	    (t (error "Unknown tkcolor ~a" tkcolor))
	    ;;(t (list 0 0 255))
            ))))


;;(setq color-num 0)
;;(defun random-tk-color ()
;;  (nth (setq color-num (mod (inc color-num) (length colors))) colors))
;;(setq colors '("#a696e4" "#97c7fd" "#b517ea" "#ebb283" "#fb0e63" "#b3282e" 
;;	       "#f124ee" "#7e2810" "#0a7579" "#189417" "#1cf195" "#a696e4" 
;;	       "#97c7fd" "#b517ea" "#ebb283" "#fb0e63" "#b3282e" "#f124ee" 
;;	       "#7e2810" "#0a7579" "#189417" "#1cf195" ))

(defun random-tk-color (&optional seed)
  ;;(format nil "#~2,'0x~2,'0x~2,'0x" (krandom 256) (krandom 256) (krandom 256))
  (if seed
      (seed-random seed))
  (multiple-value-bind (r g b)
      ;;(r-hsv-to-rgb (knuth-random) 1.0 0.5)
      (r-hsv-to-rgb (knuth-random) 1.0 0.5)    
    (format nil "#~2,'0x~2,'0x~2,'0x" r g b)))

(defun primary-then-random-tk-colors (n)
  (let* ((cluster-primary-colors (firstn n '("blue" "red" "green")))
	 (cluster-random-colors (loop for i from (length cluster-primary-colors) below n collect (random-tk-color)))
	 (cluster-colors (append cluster-primary-colors cluster-random-colors)))
    cluster-colors))

;;;----------------------------------------------------------------------
;;;                           BETWEEN
;;;----------------------------------------------------------------------

(defun between (x a b)
  (let ((sorted-ab (my-sort (list a b))))
    (and (> x (nth 0 sorted-ab))
	 (< x (nth 1 sorted-ab)))))


;;;----------------------------------------------------------------------
;;;                      FUNCTION COMPOSITION
;;;----------------------------------------------------------------------

(defun cfs (&rest fs)
  (if (= 1 (length fs))
      (car fs)
    (^ (x)
       (funcall (car fs)
		(funcall (apply #'cfs (cdr fs)) x)))))


;;;----------------------------------------------------------------------
;;;                             DIFF
;;;----------------------------------------------------------------------

(defun tkdiff (a b)
  (fi a "/tmp/delete-a" :supersede)
  (fi b "/tmp/delete-b" :supersede)
  (run-shell-command "tkdiff /tmp/delete-a /tmp/delete-b"))


;;;----------------------------------------------------------------------
;;;                          COORDINATES
;;;----------------------------------------------------------------------

(defun bounding-box (coordss &optional (scale 1.0))
  "Returns the x-min x-max y-min y-max z-min z-max etc for any number of dimensions."
  (let ((bounding-box (loop for i below (length (car coordss)) collect
			    (let ((dimension (nths i coordss)))
			      (list (apply-min dimension)
				    (apply-max dimension))))))
    (loop for (min max) in bounding-box collect
	  (let* ((half-range (/ (- max min) 2))
		 (midpoint (+ min half-range)))
	    (list (- midpoint (* half-range scale))
		  (+ midpoint (* half-range scale)))))))
    
(defun random-coords-in-bounding-box (bounding-box)
  "Bounding box is in form ((x-min x-max) (y-min y-max) (z-min z-max)), for any number of dimensions,
as supplied by #'bounding-box for example."
  (loop for (min max) in bounding-box collect
	(random-in-range min max)))


;;;----------------------------------------------------------------------
;;;                      keyword arguments
;;;----------------------------------------------------------------------

(defun snoop-keyword-arg (keyword args &key (not-found-action :error))
  (let ((position (position keyword args)))
    (if position
	(nth (inc position) args)
      (case not-found-action
	(:return-nil nil)
	(:return-t   t)    ;; so we can do default arg to t
	(:return-not-passed 'not-passed)
	(:error (error "no such keyword as ~a in ~a" keyword args))
	(t (error "non-found-action ~a option not recognized" not-found-action))))))

(defun remove-keyword-and-arg (keyword args &key (not-found-action :error))
  (let ((position (position keyword args)))
    (if position
	(append (firstn position args)
		(nthcdr (+ position 2) args))
      (case not-found-action
	(:error (error "no such keyword as ~a in ~a" keyword args))
	(:ignore args)
	(t (error "unexpected not-found-action ~a, expected one of :error :add or :ignore" not-found-action))))))

(defun remove-keywords-and-args (keywords args &key (not-found-action :error))
  (if (null keywords)
      args
    (remove-keywords-and-args
     (cdr keywords)
     (remove-keyword-and-arg (car keywords) args :not-found-action not-found-action)
     :not-found-action not-found-action)))

(defun subst-keyword-arg (keyword args new-arg &key (not-found-action :error))
  (let ((position (position keyword args)))
    (if position
	(append (firstn (inc position) args)
		(list new-arg)
		(nthcdr (+ position 2) args))
      (case not-found-action
	(:error (error "no such keyword as ~a in ~a" keyword args))
	(:add   (append args (list keyword new-arg)))
	(:ignore args)
	(t (error "unexpected not-found-action ~a, expected one of :error :add or :ignore" not-found-action))))))



;;;----------------------------------------------------------------------
;;;                         unix
;;;----------------------------------------------------------------------

(defun file-or-directory-exists-p (pathname)
  ;;(not (bit->bool (run-shell-command (format nil "test -e ~a" pathname) :wait t)))
  (probe-file pathname)  ;; replace unix-specific above with a cltl2 function
  )

(defun pathname-exists-p (pathname)
  (file-or-directory-exists-p pathname))

(defun directoryp (pathname)
  (file-directory-p pathname))  ;; allegro, not cltl2 function, i think

(defun run-shell-command-and-wait (command)
  (run-shell-command command :wait t))

(defun run-shell-command-wait-and-collect-output (command)
  (let ((results-filename (format nil "/tmp/.lisp-shell-output-~d" (krandom 10000000))))
    (run-shell-command (format nil "~a > ~a" command results-filename):wait t)
    (fi-in-readline results-filename)))

(defun ls (pathname)
  (run-shell-command-wait-and-collect-output (format nil "ls ~a" pathname)))

(defun mkdir (pathname)
  ;;(run-shell-command-and-wait (print (format nil "mkdir ~a" pathname)))
  (make-directory pathname)  ;; an allegro, not a cltl2 function, i think.  better for windows compatibility
                             ;; in Oct 2005 was only called by the newly minted regression code in the main mds source
  )

(defun cp (a b)
  (run-shell-command-and-wait (format nil "cp -a ~a ~a" a b)))

(defun mv (a b)
  (run-shell-command-and-wait (format nil "mv ~a ~a" a b)))

(defun rm (a)
  (run-shell-command-and-wait (format nil "rm ~a" a)))


;;;----------------------------------------------------------------------
;;;                      filesystem utilities
;;;----------------------------------------------------------------------

(defun make-resized-image-directory-copy (input-directory output-directory resize)
  (mkdir output-directory)
  (run-shell-command (format nil "cp ~a/* ~a/" input-directory output-directory) :wait t)
  (run-shell-command (format nil "pushd ~a; mogrify -resize ~a *; popd" output-directory resize :wait t)))


;;;----------------------------------------------------------------------
;;;                          unix/mac
;;;----------------------------------------------------------------------

(defun unix-to-mac (input-filename output-filename)
  (run-shell-command
   (format nil "unix-to-mac ~a > ~a" input-filename output-filename)
   :wait t))

(defun mac-to-unix (input-filename output-filename)
  (run-shell-command
   (format nil "mac-to-unix ~a > ~a" input-filename output-filename)
   :wait t))



;;;----------------------------------------------------------------------
;;;                          dates
;;;----------------------------------------------------------------------

(defun leap-year-p (year)
  (and (not (zerop (mod year 100)))
       (zerop (mod year 4))))

(defun days-in-year (year)
  (if (leap-year-p year)
      366
    365))

(defun year-from-days-since-1900 (day &optional (year-aux 1900))
  (if (<= day (days-in-year year-aux))
      (values 
       year-aux
       day)
    (year-from-days-since-1900 
     (- day (days-in-year year-aux))
     (inc year-aux))))

(defun days-in-month (month year)
  (case month
    ( 1 31)
    ( 2 (if (leap-year-p year) 29 28))
    ( 3 31)    
    ( 4 30)
    ( 5 31)
    ( 6 30)    
    ( 7 31)
    ( 8 31)
    ( 9 30)    
    (10 31)
    (11 30)
    (12 31)
    (t (error "Month should be in range 1 to 12 but was ~a" month))))

(defun month-from-days-since-start-of-year (day-in-year year &optional (month-aux 1))
  (if (<= day-in-year (days-in-month month-aux year))
      (values
       month-aux
       day-in-year)
    (month-from-days-since-start-of-year 
     (- day-in-year 
        (days-in-month month-aux year))
     year
     (inc month-aux))))

(defun date-from-days-since-1900 (day)
  (multiple-value-bind (year day-in-year)
      (year-from-days-since-1900 day)
    (multiple-value-bind (month day-in-month)
        (month-from-days-since-start-of-year day-in-year year)
      (list year month day-in-month))))
