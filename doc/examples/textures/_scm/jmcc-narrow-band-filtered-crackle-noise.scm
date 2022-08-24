; narrow band filtered Crackle noise (jmcc #2)
(withSc3
 (spawnUgen
  (list 2 +inf.0)
  (let* ((e (EnvLinen 2 5 2 1 (list)))
         (rf1 (Add (Rand 0 2000) 80))
         (rf2 (MulAdd (Rand2 0.2) rf1 rf1))
         (rf (XLine rf1 rf2 9 doNothing))
         (c (Mul (Add (Crackle 1.97) (Rand 0 0.03)) 0.15)))
    (Pan2 (Resonz c rf 0.2) (Rand2 1) (EnvGen 1 1 0 1 removeSynth e)))))

