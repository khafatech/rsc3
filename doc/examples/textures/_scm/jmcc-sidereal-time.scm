; sidereal time (jmcc) #9
(withSc3
 (overlapTextureUgen
  (list 4 4 6 dinf)
  (let* ((p 15)
	 (z (let ((mk-y (lambda (_)
                          (let ((fr (ExpRandN p 100 6000))
                                (rt (RandN p 2 6)))
                            (klankDataMce fr (asMce (replicate p 1)) rt)))))
	      (mceFill 2 mk-y)))
	 (f (XLine (ExpRand 40 300) (ExpRand 40 300) 12 doNothing))
	 (t (let ((e (LFNoise2 (Rand 0 8))))
	      (Mul (Mul (LFPulse f 0 (Rand 0.1 0.9)) 0.002) (Max 0 e))))
	 (o (Mul (Distort (Klank t 1 0 1 (mceTranspose z))) 0.1)))
    (Add (CombN o 0.6 (Rand 0.1 0.6) 8) (mceReverse o)))))
