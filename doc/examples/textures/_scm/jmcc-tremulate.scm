; tremulate (jmcc) #1
(withSc3List
 (list
  (postProcessUgen 2 (lambda (i) (CombN i 0.1 0.1 1)))
  (xfadeTextureUgen
   (list 0.5 2 +inf.0)
   (let* ((f (Rand 500 900))
          (o (FSinOsc (Mul f (asMce '(1 1.2 1.5 1.8))) 0))
          (r (RandN 4 30 90))
          (a (Max 0 (Mul (LFNoise2 r) 0.1)))
          (l (RandN 4 -1 1)))
     (Mix (Pan2 o l a))))))

