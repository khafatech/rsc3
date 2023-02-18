; ring modulated klank (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 4 4 4 +inf.0)
  (let* ((p 8)
         (z (klankDataMce (RandN p 100 10000)
                          (asMce (replicate p 1))
                          (RandN p 0.2 1)))
         (k (Klank (Mul (Dust 20) 0.02) 1 0 1 z))
         (f (MulAdd (LFNoise2 (Rand 0.1 0.4)) 200 (Rand 350 400))))
    (Pan2 (Mul (SinOsc f 0) k) (Rand -1 1) 1))))

