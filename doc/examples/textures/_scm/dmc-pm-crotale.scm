; pm-crotale (dmc) #1.7
(withSc3
 (spawnUgen
  (list (lambda () (s:rand 0.25 4.0)) inf)
  (let* ((art (Rand 2 6))
         (freq (MidiCps (IRand 48 72)))
         (env (EnvPerc 0 art 1 (list -4 -4)))
         (mod (Add 5 (Fdiv 1 (IRand 2 6))))
         (amp1 (EnvGen 1 (Rand 0.1 0.3) 0 art 0 env))
         (amp2 (EnvGen 1 (Rand 0.1 0.5) 0 (Mul 1.3 art) 2 env))
         (sig (PMOsc freq (Mul mod freq) (EnvGen 1 (Rand 1 6) 0 art 0 env) 0)))
    (Pan2 sig (Rand2 1) (Mul amp1 amp2)))))
