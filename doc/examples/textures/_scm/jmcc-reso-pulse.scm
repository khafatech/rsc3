; reso-pulse (jmcc) #1
(withSc3List
 (list
  (postProcessUgen
   1
   (lambda (z)
     (let* ((lfo-freq 6)
            (lfo (MulAdd (LFNoise0 lfo-freq) 1000 1200))
            (x (MouseX 0.2 0.02 exponential 0.2))
            (left (RLPF z lfo x))
            (delay-time (Fdiv 2 lfo-freq))
            (right (DelayN left delay-time delay-time)))
       (Mce2 left right))))
  (overlapTexture
   (list 4 2 4 +inf.0)
   (lambda ()
     (let* ((f (MidiCps (s:l-choose (list 25 30 34 37 41 42 46 49 53 54 58 61 63 66))))
            (f* (MulAdd 2 f (Rand2 0.5))))
       (Mul (Add (LFPulse f 0 0.2) (LFPulse f* 0 0.2)) 0.02))))))
