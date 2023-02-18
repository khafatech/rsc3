; random panning sines (jmcc) #1
(withSc3
 (overlapTextureUgen
  (list 8 8 2 dinf)
  (let ((mkNode
         (lambda (_)
           (let ((o (FSinOsc (LinRand 80 2000 0) 0))
                 (l (LFNoise1 (Rand 0.8 1.2)))
                 (a (LFNoise1 (Rand 0.82 0.98))))
             (Pan2 o l a)))))
    (Mul (Mix (mceFill 8 mkNode)) (/ 0.4 8)))))

