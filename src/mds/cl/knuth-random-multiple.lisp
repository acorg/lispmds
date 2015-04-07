(in-package user)

(defvar num_generators 10)

(defvar index-array (make-array num_generators))
(defvar indexp-array (make-array num_generators))
(defvar ma-array (make-array (list num_generators 56)))

;;potential problem here, this once returned 1.0, see the bottom for potential reconstruction
(defun knuth_random_i (generator)  ;;0.46 secs for 1000 calls.
  (setf (aref index-array generator) (+ 1 (aref index-array generator)))
  (if (= (aref index-array generator) 56) (setf (aref index-array generator) 1))
  (setf (aref indexp-array generator) (+ 1 (aref indexp-array generator)))
  (if (= (aref indexp-array generator) 56) (setf (aref indexp-array generator) 1))
  (let ((mj (- (aref ma-array generator (aref index-array generator)) (aref ma-array generator (aref indexp-array generator)))))
    (if (minusp mj) (setq mj (+ mj MBIG)))
    (setf (aref ma-array generator (aref index-array generator)) mj)
    (* mj FAC)))

(defun seed_random_i (generator seed)
  (if (minusp seed)
      (setq seed (mod (get-internal-real-time) 1000000000)))
  ;;(- (get-universal-time) 2500000000)))
  (if (or (minusp seed) (> seed MBIG))
    (error "Seed ~a is out of the range [0, ~a]." seed MBIG))
  (let ((mj seed)
        (mk 1))
    (setf (aref ma-array generator 55) seed)
    (loop for i from 1 to 54 do
          (let ((ii (mod (* i 21) 55)))
            (setf (aref ma-array generator ii) mk)
            (setf mk (- mj mk))
            (if (minusp mk) (setq mk (+ mk MBIG)))
            (setf mj (aref ma-array generator ii))))
    (loop for k below 4 do
          (loop for i from 1 to 55 do
                (setf (aref ma-array generator i) (- (aref ma-array generator i) (aref ma-array generator (+ 1 (mod (+ i 30) 55)))))
                (if (minusp (aref ma-array generator i)) (setf (aref ma-array generator i) (+ (aref ma-array generator i) MBIG)))))
    (setf (aref index-array generator) 0)
    (setf (aref indexp-array generator) 31)
    seed))

