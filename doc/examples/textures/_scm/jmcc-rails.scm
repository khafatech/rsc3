; rails (jmcc) #2
(withSc3
 (overlapTextureUgen
 (list 3 2 4 +inf.0)
  (let* ((n 20) ; resonant modes
         (e (Mul (Dust 100) 0.04)) ; excitation
         (f (XLine 3000 300 8 doNothing)) ; Sweep filter down
         (l (Line (Rand2 1) (Rand2 1) 8 doNothing)) ; Sweep pan
         (r (mceFill n (lambda (_) (Add 200 (LinRand 0 3000 0))))) ; resonant frequencies
         (a (asMce (replicate n 1)))
         (t (Add 0.2 (RandN n 0 1))) ; ring times
         (k (Klank (Resonz e f 0.2) 1 0 1 (klankDataMce r a t))))
    (Pan2 k l 1))))

