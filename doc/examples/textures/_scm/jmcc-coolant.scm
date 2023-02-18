; coolant (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 4 4 2 +inf.0)
  (let* ((p 10)
         (n (asMce (replicate p 1)))
         (sp (Mce2 (klankDataMce (RandN p 40 2400) n n)
                   (klankDataMce (RandN p 40 2400) n n)))
         (s (OnePole (Mul (mceFill 2 (lambda (_) (BrownNoise))) 0.002) 0.95)))
    (Klank s 1 0 1 (mceTranspose sp)))))
