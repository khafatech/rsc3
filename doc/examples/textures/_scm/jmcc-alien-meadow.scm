; alien meadow (jmcc) #6
(withSc3
 (overlapTextureUgen
  (list 2 6 6 +inf.0)
  (let ((f (Add (Mul3 (SinOsc (Rand 0 20) 0) (Rand 0 5000) 0.1) (Rand 0 5000))))
    (Pan2 (SinOsc f 0) (Rand -1 1) (MulAdd (SinOsc (Rand 0 20) 0) 0.05 0.05)))))
