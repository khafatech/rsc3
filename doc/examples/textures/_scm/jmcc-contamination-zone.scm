; jmcc
(withSc3List
 (list
  (postProcessUgen
   2
   (let ((f (lambda (x) (AllpassN x 0.04 (RandN 2 0 0.04) 16)))
         (useq (lambda (n f) (foldl1 compose (replicate n f)))))
     (useq 6 f)))
  (overlapTextureUgen
   (list 3 8 4 dinf)
   (let* ((f (ExpRand 800 8000))
          (p (let ((e (MulAdd (LFNoise1 (Rand 0 3)) 0.0008 0.0022))) (Mul (PinkNoise) e)))
          (s (Add (Mul3 (SinOsc (LinRand 0 1 0) 0) 0.7 f) f))
          (k (let ((sp (klankDataMce (RandN 4 50 2000) (asMce (list 1 1 1 1)) (RandN 4 0.2 4))))
               (Mul (Abs (Klank p 1 0 1 sp)) (Choose (Mce2 -1 1)))))
          (r (RLPF k s 0.1))
          (a (LFPulse (LinRand 0 150 0) 0 (Rand 0.2 0.4))))
     (Pan2 r (LFNoise1 (Rand 0 1)) a)))))
