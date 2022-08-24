; blips 001 (jmcc) #SC3d1.5
(withSc3List
 (list
  (postProcessUgen
   2
   (lambda (z)
     (let ((f (lambda (x) (AllpassN x 0.05 (Mce2 (Rand 0 0.05) (Rand 0 0.05)) 4))))
       (iterate 6 f (Distort z)))))
  (overlapTextureUgen
   (list 2 1 12 +inf.0)
   (let* ((c (Coin 0.8))
          (b (lambda ()
               (let ((f (XLine (ExpRand 0.25 400) (ExpRand 0.25 400) 4 0))
                     (nh (XLine (ExpRand 2 100) (ExpRand 2 100) 4 0)))
                 (Blip f nh)))))
     (Mul c (Pan2 (Mul (b) (b)) (Line (Rand2 1) (Rand2 1) 4 0) 0.3))))))
