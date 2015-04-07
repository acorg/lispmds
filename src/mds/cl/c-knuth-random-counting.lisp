(in-package user)

#|
gcc ~/cl/c-knuth-random.c -c -o ~/cl/c-knuth-random.o
gcc ~/cl/c-knuth-random-multiple.c -c -o ~/cl/c-knuth-random-multiple.o
|#

(load "~/cl/c-knuth-random.o")
(load "~/cl/c-knuth-random-multiple.o")
(ff:defforeign 'knuth_random :return-type :double-float)
(ff:defforeign 'knuth_random_i :return-type :double-float)
(ff:defforeign 'seed_random :return-type :integer)
(ff:defforeign 'seed_random_i :return-type :integer)

(defvar *num-calls-to-knuth-random*)
(defvar *num-calls-to-knuth-random-g*)
(setq *num-calls-to-knuth-random* 0)
(setq *num-calls-to-knuth-random-g* 0)

(defmacro knuth-random (&optional generator)
  (if generator
      `(progn (setq *num-calls-to-knuth-random-g* (inc *num-calls-to-knuth-random-g*))
	      (knuth_random_i ,generator))
    `(progn (setq *num-calls-to-knuth-random* (inc *num-calls-to-knuth-random*))
	    (knuth_random))))

(defmacro seed-random (seed &optional generator)
  (if generator
      `(seed_random_i generator ,seed)
    `(seed_random ,seed)))

(defvar *num-calls-to-krandom*)
(setq *num-calls-to-krandom* 0)
(defvar *num-calls-to-krandom-g*)
(setq *num-calls-to-krandom-g* 0)

(defmacro krandom (n &optional generator)
  (if generator
      `(progn
	 (setq *num-calls-to-krandom-g* (inc *num-calls-to-krandom-g*))
	 (if ,generator
	   (floor (* (knuth_random_i ,generator) ,n))
	 (floor (* (knuth_random) ,n))))
    `(progn 
       (setq *num-calls-to-krandom* (inc *num-calls-to-krandom*))
       (floor (* (knuth_random) ,n)))))

(print (list *num-calls-to-knuth-random* *num-calls-to-knuth-random-g* *num-calls-to-krandom* *num-calls-to-krandom-g*))

(seed_random 467739585)
(defvar *num-generators*)
(setq *num-generators* 10)
(loop for generator below *num-generators* do
      (seed_random_i generator 467739585))
