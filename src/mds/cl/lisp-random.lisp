(in-package user)

(defmacro knuth-random (&optional generator)
  (random 1.0))

(defmacro seed-random (seed &optional generator)
  nil)

(defmacro krandom (n &optional generator)
  (random n))

