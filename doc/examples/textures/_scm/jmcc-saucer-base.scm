; saucer base - jmcc
(withSc3
 (overlapTextureUgen
  (list 2 6 4 dinf)
  (let* ((b (Rand 0 1000))
	 (c (Rand 0 5000))
	 (o (Add (Mul (SinOsc (Rand 0 20) 0) b) (Mul 1.1 b)))
	 (p (Add (Mul (SinOsc o 0) c) (Mul 1.1 c))))
    (Pan2 (Mul (SinOsc p 0) 0.1) (Rand -1 1) 1))))
