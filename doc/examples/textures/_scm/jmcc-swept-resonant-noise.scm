; swept resonant noise (jmcc) #2
(withSc3
 (overlapTextureUgen
  (list 4 4 5 +inf.0)
  (let* ((p 10)
         (n (Mul (WhiteNoise) 0.007))
         (f (MidiCps (MulAdd (FSinOsc (Rand 0.1 0.3) 0) (Rand 0 24) (Rand 36 84))))
         (sweep (Resonz n f 0.1))
         (spec-f (lambda ()
                   (klankDataMce
		    (LinRandN p 80 10080 0)
                    (asMce (replicate p 1))
                    (RandN p 0.5 2.5)))))
    (asMce (replicateM 2 (lambda () (Klank sweep 1 0 1 (spec-f))))))))
