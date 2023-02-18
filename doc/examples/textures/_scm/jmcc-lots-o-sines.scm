; lots-o-sines (jmcc) #2
(withSc3
 (xfadeTextureUgen
  (list 4 4 +inf.0)
  (let* ((n 60)
         (z (klangData
	     (replicateM n (lambda () (LinRand 40 10040 0)))
             (replicate n 1)
             (replicate n 0)))
         (k (mceFill 2 (lambda (_) (Klang 1 0 z)))))
    (Mul k (Fdiv 0.1 n)))))

