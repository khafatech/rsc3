;; cymbalism (jmcc)

(import (rnrs) (rsc3) (rhs))

(define cymbalism
  (let* ((p 15)
         (f1 (rand 500 2500))
         (f2 (rand 0 8000))
         (y
          (lambda ()
            (let ((f (replicate-m p (add f1 (rand 0 f2))))
                  (rt (replicate-m p (rand 1 5)))
                  (a (replicate p 1)))
              (klank-data f a rt))))
         (z (mce2 (y) (y)))
         (t (impulse ar (rand 0.5 3.5) 0))
         (n (mul (white-noise ar) 0.03))
         (s (mul (decay t 0.004) n)))
    (klank s 1 0 1 (mce-transpose z))))

(audition (out 0 cymbalism))
