; birdies (jmcc)
(withSc3
 (overlapTextureUgen
  (list 7 4 4 dinf)
  (let* ((p1 (Add (Mul (LFPulse (Rand 0.4 1.4) 0 (Rand 0.1 0.9)) (Rand 4 7)) 2))
	 (p2 (Mul (LFPulse (Rand 0.4 1.4) 0 (Rand 0.1 0.9)) (Rand 4 7)))
	 (p3 (Mul (LFPulse (Rand 0.2 0.7) 0 0.4) 0.02))
	 (sw (Add (Mul (LFSaw (Add p1 p2) 0) (Neg (Rand 1000 1800))) (Add 4000 (Rand2 1200))))
	 (freq (Lag sw 0.05))
	 (amp (Lag p3 0.3)))
    (Pan2 (Mul (SinOsc freq 0) amp) (Rand2 1) 1))))

