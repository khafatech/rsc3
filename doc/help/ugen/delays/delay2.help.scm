;; (delay2 in)

;; Fixed two sample delay.

(let ((s (impulse ar 1 0)))
  (audition
   (out 0 (add s (delay2 s)))))

