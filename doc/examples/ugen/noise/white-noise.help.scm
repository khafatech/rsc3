;; (white-noise rate)

;; Generates noise whose spectrum has equal power at all frequencies.

(audition (out 0 (mul (white-noise ar) 0.15)))

;; Noise generators constructors are unique, to share noise UGens
;; values must be explictly stored and reused.

(audition (out 0 (mul (sub (white-noise ar) (white-noise ar)) 0.15)))

(let ((n (white-noise ar)))
  (audition (out 0 (sub n n))))
