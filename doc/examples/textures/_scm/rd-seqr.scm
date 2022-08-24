; seqr (rd)
(withSc3
 (overlapTexture
  (list 6 6 3 +inf.0)
  (lambda ()
    (let* ((nfreq
            (lambda (n l r)
              (map MidiCps (replicateM n (lambda () s:irand l r)))))
           (seqr-f
            (lambda (f e)
              (let ((n (/ (length e) 2)))
                (kr: (Select (MulAdd (LFSaw f 0) n n) (asMce e))))))
           (n (s:irand 6 18))
           (f (/ (s:irand 9 18) n)))
      (Mul (Blip
                 (Mce2
		  (seqr-f f (nfreq n 72 96))
                  (seqr-f f (nfreq n 72 84)))
                 (Mce2
		  (seqr-f f (s:rand-n n 1 3))
                  (seqr-f f (s:rand-n n 3 6))))
           (Mce2
	    (seqr-f f (s:rand-n n 0.05 0.10))
            (seqr-f f (s:rand-n n 0.05 0.15))))))))
