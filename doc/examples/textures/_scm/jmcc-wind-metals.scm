; wind-metals (jmcc)
(withSc3
 (overlapTextureUgen
  (list 5 2 12 +inf.0)
  (let* ((n 6)
         (baseFreq (ExpRand 60 4000))
         (freq (RandN n baseFreq (Add baseFreq (Rand 500 8000))))
         (exc (Mul3 (mceFill 2 (lambda (_) (BrownNoise)))
                    0.007
                    (Max 0 (MulAdd (LFNoise1 (ExpRand 0.125 0.5)) 0.75 0.25))))
         (k (klankDataMce freq (asMce (replicate n 1)) (RandN n 0.1 2)))
         (s (Klank exc 1 0 1 k)))
    (SoftClip (Mul s 0.1)))))
