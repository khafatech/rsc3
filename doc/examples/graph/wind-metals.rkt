#lang racket
;; status - working as of 20220820

(require rsc3)

;; wind-metals (jmcc)

(define wind-metals
  (let* ((n 6)
         (base (exp-rand 60 4000))
         (range (rand 500 8000))
         (n0 (clone 2 (brown-noise ar)))
         (r0 (exp-rand 0.125 0.5))
         (n1 (lf-noise1 kr r0))
         (f (replicate-m n (rand base (add base range))))
         (dt (replicate-m n (rand 0.1 2)))
         (exc (mul3 n0 0.007 (u:max 0 (mul-add n1 0.75 0.25))))
         (k (klank-data f (replicate n 1) dt))
         (s (klank exc 1 0 1 k)))
    (soft-clip (mul s 0.1))))

(audition (out 0 wind-metals))
