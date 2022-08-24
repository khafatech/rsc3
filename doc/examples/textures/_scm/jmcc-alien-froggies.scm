; alien froggies (jmcc) #1
(withSc3
 (overlapTextureUgen
  (list 0.25 0.5 5 +inf.0)
  (let* ((r 11)
         (r* (Fold (Mul r (Exp (LinRand -0.2 0.2 0))) 1 30))
         (o (Formant r* (ExpRand 200 3000) (MulAdd (Rand 0 9) r* r*))))
    (Mul o 0.05))))
