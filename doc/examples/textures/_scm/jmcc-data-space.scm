; data space (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 1 6 4 +inf.0)
  (let* ((r (lambda (n) (Rand 0 n)))
         (lp (lambda (f m a) (MulAdd (LFPulse (r f) 0 (r 1)) m a)))
         (p0 (lp 200 1 0))
         (p1 (lp 40 (r 8000) (r 2000)))
         (p2 (lp 20 1 0))
         (p3 (lp 4 (r 8000) (r 2000)))
         (p4 (lp 20 1 0))
         (p5 (lp 4 (r 8000) (r 2000)))
         (f (Add3 (Mul p0 p1) (Mul p2 p3) (Mul p4 p5)))
         (dt (Rand 0.15 0.35))
         (o (Mul (LFPulse f 0 0.5) 0.04))
         (l (Mul (LFNoise0 (r 3)) 0.8)))
    (CombL (Pan2 o l 1) dt dt 3))))
