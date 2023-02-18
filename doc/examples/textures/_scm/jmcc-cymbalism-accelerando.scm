; cymbalism accelerando (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 4 4 4 +inf.0)
  (let* ((p 15)
         (f1 (Rand 500 2500))
         (f2 (Rand 0 8000))
         (mk-y (lambda (_)
                 (let ((f (Add f1 (RandN p 0 f2)))
                       (rt (RandN p 1 5))
                       (a (asMce (replicate p 1))))
                   (klankDataMce f a rt))))
         (z (mceFill 2 mk-y))
         (tf (XLine (LinRand 0.5 4.5 0) (Rand 0.5 35.5) 12 doNothing))
         (t (Impulse tf 0))
         (n (Mul (WhiteNoise) 0.02))
         (s (Mul (Decay t 0.004) n)))
    (Klank s 1 0 1 (mceTranspose z)))))
