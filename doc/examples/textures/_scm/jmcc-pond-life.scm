; pond life (jmcc) #1
(withSc3
 (overlapTextureUgen
  (list 8 8 4 +inf.0)
  (let* ((f0 (Add 20 (Rand 0 30)))
         (f1 (MulAdd (FSinOsc f0 0) (Rand 100 400) (LinRand 500 2500 0)))
         (a (Mul (LFPulse (Fdiv 3 (Rand 1 9)) 0 (Rand 0.2 0.5)) 0.04)))
    (Pan2 (SinOsc f1 0) (Rand -1 1) a))))
