#lang racket
;; status - working as of 20220820

(require rsc3)

;; moto rev (jmcc)

(define moto-rev
  (let* ((f (mul-add (sin-osc kr 0.2 0) 10 21))
         (s (lf-pulse ar f (mce2 0 0.1) 0.1)))
    (clip2 (rlpf s 100 0.1) 0.4)))

(audition (out 0 moto-rev))
