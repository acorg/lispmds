;;
;; These are tests that use the lisp-unit testing framework at
;; https://github.com/OdonataResearchLLC/lisp-unit
;;
;; For some guidance on how to write tests, see
;; https://github.com/OdonataResearchLLC/lisp-unit/wiki
;;


(make-system :test)

(use-package :lisp-unit)

(setq *print-failures* t)

(defun broken-max (x y) x)
(defun my-max (x y) (max x y))

;; (define-test test-broken-max
;;     (assert-equal 5 (broken-max 2 5))
;;     (assert-equal 5 (broken-max 5 2))
;;     (assert-equal 10 (broken-max 10 10))
;;     (assert-equal 0 (broken-max -5 0)))

(define-test test-my-max
    (assert-equal 5 (my-max 2 5))
    (assert-equal 5 (my-max 5 2))
    (assert-equal 10 (my-max 10 10))
    (assert-equal 0 (my-max -5 0)))

(setq result (run-tests :all))

(if
    (and
     (zerop (length (failed-tests result)))
     (zerop (length (error-tests result))))
    (progn
      (print (format t "Zero failures, exiting lisp.~%"))
      (exit)))
