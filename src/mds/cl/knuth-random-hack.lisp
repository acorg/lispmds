(defmacro knuth-random (&optional generator)
  (if generator
      `(if ,generator
	   (knuth_random)
	 (knuth_random))
    `(knuth_random)))

(defmacro seed-random (seed &optional generator)
  (if generator
      `(if ,generator
	   (seed_random ,seed)
	 (seed_random ,seed))
    `(seed_random ,seed)))

(defmacro krandom (n &optional generator)
  (if generator
      `(if ,generator
	   (floor (* (knuth_random) ,n))
	 (floor (* (knuth_random) ,n)))
    `(floor (* (knuth_random) ,n))))


(defvar *num-generators*)
(setq *num-generators* 10)

(seed_random 467739585)
