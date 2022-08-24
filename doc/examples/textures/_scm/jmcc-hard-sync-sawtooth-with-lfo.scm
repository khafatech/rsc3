; hard sync sawtooth with lfo (jmcc) #6
(withSc3List
 (list
  (postProcessUgen 2 (lambda (z) (Add (CombN z 0.3 0.3 4) (mceReverse z))))
  (overlapTextureUgen
   (list 4 4 4 +inf.0)
   (let* ((f (MidiCps (Add 30 (IRand 0 50))))
          (o (MulAdd (SinOsc 0.2 (Mce2 0 (Rand 0 (* 2 pi)))) (Mul 2 f) (Mul 3 f))))
     (Mul (SyncSaw (Mce2 f (Add f 0.2)) o) 0.05)))))
