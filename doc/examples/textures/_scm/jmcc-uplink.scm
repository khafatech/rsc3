; uplink (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 4 1 5 +inf.0)
  (let ((f (mixFill 2 (lambda (_)
                         (let* ((r (lambda (n) (Rand 0 n)))
                                (p0 (LFPulse (r 20) 0 (r 1)))
                                (p1 (MulAdd (LFPulse (r 4) 0 (r 1)) (r 8000) (r 2000))))
                           (Mul p0 p1))))))
    (Pan2 (Mul (LFPulse f 0 0.5) 0.04) (Rand -0.8 0.8) 1))))

