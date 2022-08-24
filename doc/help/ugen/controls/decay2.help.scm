;; (decay2 in attackTime decayTime)			

;; Exponential decay.  decay has a very sharp attack and can produce
;; clicks. decay2 rounds off the attack by subtracting one decay from
;; another.

;; (decay2 in a d) is equivalent to (sub (decay in d) (Decay in a)).

;; Used as an envelope

(audition
 (out 0 (mul (decay2 (impulse ar (x-line kr 1 50 20 remove-synth) 0.25)
		     0.01
		     0.2)
	     (mul (f-sin-osc ar 600 0) 0.25))))

;; Compare the above with decay used as the envelope.

(audition
 (out 0 (mul (decay (impulse ar (x-line kr 1 50 20 remove-synth) 0.25)
		    0.01)
	     (mul (f-sin-osc ar 600 0) 0.25))))
