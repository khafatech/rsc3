; resonant Dust (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 5 2 9 +inf.0)
  (let* ((rf (let* ((st (Rand 80 2080))
                    (en (Add st (Mul (Rand -0.5 0.5) st))))
               (XLine st en 9 doNothing)))
         (d (Mul (Dust (Rand 50 850)) 0.3)))
    (Pan2 (Resonz d rf 0.1) (Rand -1 1) 1))))
