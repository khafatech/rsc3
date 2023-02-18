; nharm (rd)

(define nharm
  (lambda (n f)
    (if (<= n 0)
        (list)
        (cons f (nharm (- n 1) (Add f f))))))

(define klg
  (lambda (m u)
    (let* ((n (s:irand 4 u))
           (d (s:irand 9 12))
           (a 0.5)
           (e (EnvGen 1 0.9 0 1 removeSynth (EnvSine d a)))
           (s (klangData
	       (nharm n (MidiCps (s:rand m (+ m 2))))
               (replicateM n (lambda () (s:rand 0.01 0.02)))
               (replicate n 0))))
      (Pan2 (Klang 1 0 s) (s:rand -1 1) e))))

(define pattern
  (lambda (fd)
    (begin
      (play fd (Out 0 (klg (s:rand 32 92) (s:irand 9 24))))
      (thread-sleep (s:rand 0.25 0.75))
      (pattern fd))))

(withSc3 pattern)
