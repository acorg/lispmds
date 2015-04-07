(in-package user)

(defmacro knuth-random (&optional generator)
  (if generator
      `(if ,generator
	   (knuth_random_i ,generator)
	 (knuth_random))
    `(knuth_random)))

(defmacro seed-random (seed &optional generator)
  (if generator
      `(if ,generator
	   (seed_random_i ,generator ,seed)
	 (seed_random ,seed))
    `(seed_random ,seed)))

(defmacro krandom (n &optional generator)
  (if generator
      `(if ,generator
	   (floor (* (knuth_random_i ,generator) ,n))
	 (floor (* (knuth_random) ,n)))
    `(floor (* (knuth_random) ,n))))


(defvar *num-generators*)
(setq *num-generators* 10)

(seed_random 467739585)
(loop for generator below *num-generators* do
      (seed_random_i generator 467739585))

(defvar *unique-seed-each-restart-rng*)
(setq *unique-seed-each-restart-rng* 9)
(seed-random -1 *unique-seed-each-restart-rng*)

