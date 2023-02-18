#lang racket
;; status - working as of 20220820

(require rsc3)

;; plucked strings (jmcc)

(define plucked-strings
  (let ((s (lambda (_)
             (let* ((r0 (rand 2 2.2))
                    (n0 (dust ar 0.5))
                    (r1 (rand 0.05 0.15))
                    (r2 (rand 0 (* pi 2)))
                    (r3 (i-rand 0 2))
                    (s0 (impulse ar r0 0.3))
                    (s1 (mul n0 0.3))
                    (f (mul-add (sin-osc kr r1 r2) 5 5.2))
                    (s2 (impulse ar f 0.3))
                    (im (select r3 (mce3 s0 s1 s2)))
                    (dt (fdiv 1 (midi-cps (u:floor (rand 60 90)))))
                    (t (mul3 (decay im 0.1) (pink-noise ar) 0.1)))
               (pan2 (comb-l t dt dt 4) (rand -1 1) 1)))))
    (leak-dc (mix-fill 5 s) 0.96)))

(audition (out 0 plucked-strings))
