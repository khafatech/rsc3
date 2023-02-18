; chrd (rd) ; texture=21,0,3,inf
(withSc3
 (overlapTexture
  (list 21 0 3 inf)
  (lambda ()
    (let* ((chrd
            (lambda (m)
              (let* ((ds 3)
                     (d (asMce (map (lambda (x) (* x ds)) (list 5 4 5 7 4 5))))
                     (f (MidiCps (XLine m (Add m (Rand 0.05 0.5)) d doNothing)))
                     (z (EnvTrapezoid 0 (Rand 0.15 0.35) d (Rand 0.005 0.01)))
                     (e (EnvGen 1 1 0 1 doNothing z))
                     (p (XLine (Rand -1 1) (Rand -1 1) d doNothing))
                     (o (FSinOsc f 0)))
                (Mix (Pan2 o p e)))))
           (scale (list 0 2 4 5 7 9 11))
           (octaves (list 4 5 6 7))
           (mnn (concat (map (lambda (n) (map (lambda (o) (+ n (* o 12))) octaves)) scale)))
           (chd (asMce (replicateM 6 (lambda () (s:l-choose mnn))))))
      (mixFill 5 (lambda (_) (chrd chd)))))))
