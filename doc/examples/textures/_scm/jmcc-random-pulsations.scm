; random pulsations (jmcc #1)
(withSc3
 (spawnUgen
  (list (/ 9 8) +inf.0)
  (let* ((e (EnvLinen 2 5 2 0.02 (list)))
         (o1 (Mul (FSinOsc (Rand 0 2000) 0) (EnvGen 1 1 0 1 doNothing e)))
         (o2 (SinOsc (LinRand 8 88 0) 0))
         (o3 (Mul (SinOsc (Rand 0.3 0.8) (Rand 0 twoPi)) 0.7)))
    (Pan2 (AmClip o1 o2) o3 1))))

