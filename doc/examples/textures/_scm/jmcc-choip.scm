; choip (jmcc) #10
(withSc3List
 (list
  (postProcessUgen
   2
   (let ((f (lambda (x) (AllpassN x 0.1 (RandN 2 0 0.05) 4))))
     (foldl1 compose (replicate 4 f))))
  (overlapTextureUgen
   (list 10 1 8 +inf.0)
   (let* ((t 12)
          (xl (lambda (l r t) (XLine (ExpRand l r) (ExpRand l r) t doNothing)))
          (i (Impulse (xl 1 30 t) 0))
          (f (xl 600 8000 t))
          (a (SinOsc (Add (Mul3 (Decay2 i 0.05 0.5) -0.9 f) f) 0))
          (r2 (lambda (n) (Rand (Neg n) n)))
          (l (Line (r2 1) (r2 1) t doNothing)))
     (Pan2 (Mul (Decay2 (Mul i (xl 0.01 0.5 t)) 0.01 0.2) a) l 1)))))
