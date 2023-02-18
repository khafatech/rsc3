; resonators harmonic series (jmcc) #2
(withSc3
 (xfadeTextureUgen
  (list 1 7 +inf.0)
  (let* ((p 2)
         (noise (Mul (BrownNoise) 0.001))
         (rat (list 1.0 1.125 1.25 1.333 1.5 1.667 1.875 2.0 2.25 2.5 2.667 3.0 3.333 3.75 4.0))
         (freq (Mul (listChoose rat) 120))
         (resFreqs (zipWith Add ((series* Add) p freq freq) (replicateM p (lambda () (Rand2 0.5)))))
         (spec (klankData
                resFreqs
                (map (lambda (i) (Fdiv 1 (Add i 1))) (enumFromTo 0 (- p 1)))
                (replicateM p (lambda () (Rand 0.5 4.5))))))
    (mceFill 2 (lambda (_) (Klank noise 1 0 1 spec))))))
