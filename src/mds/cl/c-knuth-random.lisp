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

