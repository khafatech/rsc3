;; sprinkler (jmcc)

(import (rnrs) (rsc3))

(define sprinkler
  (let* ((f (mul-add (lf-pulse kr 0.09 0 0.16) 10 7))
         (t (mul (lf-pulse kr f 0 0.25) 0.1)))
    (bpz2 (mul (white-noise ar) t))))

(audition (out 0 sprinkler))
