; seqr-n (rd)
(withSc3
 (overlapTextureUgen
  (list 6 6 3 +inf.0)
  (let*
      ((seqr-f
        (lambda (f e)
          (let ((n (/ (length (mceChannels e)) 2)))
            (kr: (Select (MulAdd (LFSaw f 0) n n) e)))))
       (seqr-n
        (lambda (n)
          (let ((f (Fdiv (IRand 9 18) n)))
            (Mul (Blip
                       (Mce2
			(seqr-f f (MidiCps (IRandN n 72 96)))
                        (seqr-f f (MidiCps (IRandN n 72 84))))
                       (Mce2
			(seqr-f f (RandN n 1 3))
			(seqr-f f (RandN n 3 6))))
                 (Mce2
		  (seqr-f f (RandN n 0.05 0.10))
                  (seqr-f f (RandN n 0.05 0.15))))))))
    (seqr-n 13))))
