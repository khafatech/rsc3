; clustered sines (jmcc) #2
(withSc3
 (xfadeTextureUgen
  (list 4 4 +inf.0)
  (let* ((n 80)
         (f1 (Rand 100 1100))
         (f2 (Mul 4 f1))
         (y (replicateM n (lambda () (Add f1 (Rand 0 f2)))))
         (z (klangData y (map (lambda (e) (Fdiv f1 e)) y) (replicate n 0)))
         (k (mceFill 2 (lambda (_) (Klang 1 0 z)))))
    (Mul k (Fdiv 0.3 n)))))
