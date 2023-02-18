; e-lamell (rd)

(define e-lamell
  (letc ((f 440) (n 12) (d 0.1) (p 0) (a 1.0))
    (let* ((h (Line n (TChoose 1 (mce2 1 32)) d doNothing))
           (s (Blip f h))
           (e (EnvGen 1 a 0 1 removeSynth (EnvPerc 0.005 d 1 (list -4 -4)))))
      (Pan2 s p e))))

(define r-note
  (lambda (o p)
    (+ (* (s:l-choose o) 12) (s:l-choose p))))

(define l-sel
  (lambda ()
    (r-note (list 2 3) (list 0))))

(define h-sel
  (lambda ()
    (r-note (list 2 3 4) (list 0))))

(define pattern
  (lambda (fd)
    (sendBundle
     fd
     (bundle
      -1
      (list (s_new "blip" -1 addToTail 1
                   (list "f" (s:midi-cps (l-sel))
                         "n" (s:irand 2 36)
                         "d" (s:exp-rand 0.01 0.4)
                         "a" (s:rand 0 0.75)
                         "p" (s:rand -1 1)))
            (s_new "blip" -1 addToTail 1
                   (list "f" (s:midi-cps (h-sel))
                         "n" (s:irand 2 36)
                         "d" (s:exp-rand 0.01 0.4)
                         "a" (s:l-choose (list 0 0.25 0.5 1.0))
                         "p" (s:rand -1 1))))))
    (thread-sleep 0.1)
    (pattern fd)))

(withSc3
 (lambda (fd)
   (sendSynth fd "blip" (Out 0 e-lamell))
   (pattern fd)))
