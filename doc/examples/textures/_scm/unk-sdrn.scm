; unk-sdrn
(withSc3
 (overlapTextureUgen
  (list 4 6 3 dinf)
  (let* ((scale (list 0 2 3 5 7 9 10))
         (m (Add3 (listChoose (list 60 72)) (listChoose scale) (RandN 2 -0.05 0.05))))
    (Mul3 (SinOsc (MidiCps m) 0) (Lt (Rand 0 1) 0.8) (Rand 0.04 0.07)))))
