; zizle (jmcc) #SC3d1.5
(withSc3
 (overlapTextureUgen
 (list 4 4 12 +inf.0)
 (let* ((a (lambda (f)
             (let ((freq (Mul f (Mce2 (RandRange 0.7 1.3) 1)))
                   (ph (Mce2 (Rand_ twoPi) (Rand_ twoPi))))
               (Mix (Mul (SinOsc freq ph) 0.1)))))
        (a1 (Max (a (ExpRand 0.38 8)) 0))
        (a2 (Abs (a (ExpRand 6 24)))))
   (Pan2 (SinOsc (MidiCps (RandRange 24 108)) (Rand_ twoPi)) (Rand2 1) (Mul a1 a2)))))
