; unk-rhyth
(withSc3List
 (list
  (postProcessUgen 2 (lambda (z) (Add (CombN z 0.5 0.5 6) (mceReverse z))))
  (overlapTextureUgen
   (list 6 6 6 dinf)
     (let* ((scale (list 0 2 3 5 7 9 10))
            (iseqr (lambda (s tr) (Mul tr (Demand tr 0 (Dxrand dinf (make-mce s))))))
            (m (Add (Add (listChoose (list 48 60 72 84)) (listChoose scale)) (RandN 2 -0.03 0.03)))
            (sq (iseqr (list 0 1 0 1 1 0) (Impulse (listChoose (list 1.5 3 6)) 0)))
            (sg (Mul (LFPulse (MidiCps m) 0 0.4) (Rand 0.01 0.04))))
       (RLPF (Mul (Decay2 sq 0.004 (Rand 0.2 0.7)) sg) (ExpRand 800 2000) 0.1)))))
