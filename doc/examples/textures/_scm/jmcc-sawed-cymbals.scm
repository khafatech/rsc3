; sawed cymbals - jmcc
(withSc3
 (overlapTextureUgen
  (list 4 4 6 dinf)
  (let* ((mk-y (lambda (_)
                 (let* ((f1 (Rand 500 2500))
                        (f2 (Rand 0 8000))
                        (f (RandN 15 f1 (Add f1 f2)))
                        (rt (RandN 15 2 6)))
                   (klankDataMce f (asMce (replicate 15 1)) rt))))
         (z (mceFill 2 mk-y))
         (f-s (XLine (Rand 0 600) (Rand 0 600) 12 doNothing)))
    (Klank (Mul (LFSaw f-s 0) 0.0005) 1 0 1 (mceTranspose z)))))
