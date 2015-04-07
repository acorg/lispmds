(in-package user)

#|
gcc ~/cl/c-knuth-random-multiple.c -c -o ~/cl/c-knuth-random-multiple.o
|#

(load "~/cl/c-knuth-random-multiple.o")
(ff:defforeign 'knuth_random_i :return-type :double-float)
(ff:defforeign 'seed_random_i :return-type :integer)

(defmacro knuth-random ()
  `(knuth_random_i 0))

(defmacro seed-random (seed)
  `(seed_random_i 0 ,seed))

(defun krandom (n)
  (floor (* (knuth_random_i 0) n)))

(seed_random_i 0 467739585)
