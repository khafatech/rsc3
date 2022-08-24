#lang racket
;; status - working as of 20220820

(require rsc3)

;; strummable guitar (jmcc)

(define sg
  (let* ((scale (list 52 57 62 67 71 76))
         (str (lambda (i)
                (let* ((x (mouse-x kr 0 1 0 0.2))
                       (t (u:abs (hpz1 (gt x (add 0.25 (mul i 0.1))))))
                       (e (decay t 0.05))
                       (n (mul (pink-noise ar) e))
                       (dt (recip (midi-cps (list-ref scale i))))
                       (s (comb-l n dt dt 4)))
                  (pan2 s (sub (mul i 0.2) 0.5) 1))))
         (strs (mix-fill (length scale) str)))
    (leak-dc (lpf strs 12000) 0.995)))

(audition (out 0 sg))
