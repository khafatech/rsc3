; deep-trip (jmcc) #9
(withSc3
 (overlapTextureUgen
  (list 4 12 4 dinf)
  (let* ((f (MidiCps (Add (Mul (LFNoise1 (Rand 0 0.3)) 60) 70)))
	 (a2 (Mul (SinOsc (Rand 0 40) 0) 0.1))
	 (a1 (Max 0 (Mul (LFNoise1 (Rand 0 8)) a2)))
	 (a (Mul (LFNoise2 (Mul f (Rand 0 0.5))) a1))
	 (z (Mul (SinOsc f 0) a))
	 (s (Pan2 z (LFNoise1 (Rand 0 5)) 1))
	 (c0 (CombN s 0.5 (RandN 2 0.3 0.5) 20))
	 (c1 (CombN s 0.5 (RandN 2 0.3 0.5) 20)))
    (Add (Add s c0) c1))))
