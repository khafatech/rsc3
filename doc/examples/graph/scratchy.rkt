#lang racket
;; status - working as of 20220820

(require rsc3)

;; scratchy (jmcc)

(define scratchy
  (let* ((n (mul (clone 2 (brown-noise ar)) 0.5))
         (f (mul (u:max (sub n 0.49) 0) 20)))
    (rhpf f 5000 1)))

(audition (out 0 scratchy))