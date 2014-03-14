;; (allpass-n in maxDelayTime delayTime decayTime)

;; All pass delay line. allpass-n uses no interpolation, allpass-l uses
;; linear interpolation, allpass-c uses all pass interpolation.  All
;; time values are in seconds.  The decay time is the time for the
;; echoes to decay by 60 decibels. If this time is negative then the
;; feedback coefficient will be negative, thus emphasizing only odd
;; harmonics at an octave lower.

;; Since the allpass delay has no audible effect as a resonator on
;; steady state sound ...

(define z (mul (white-noise ar) 0.1))

(audition (out 0 (allpass-c z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2)))

;; ...these examples add the input to the effected sound so that you
;; can hear the effect of the phase comb.

(audition
 (out 0 (add z (allpass-n z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2))))

(audition
 (out 0 (add z (allpass-l z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2))))

(audition
 (out 0 (add z (allpass-c z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2))))

;; Used as an echo - doesn't really sound different than Comb, but it
;; outputs the input signal immediately (inverted) and the echoes are
;; lower in amplitude.

(audition
 (out 0 (allpass-n (mul (decay (dust ar 1) 0.2) z) 0.2 0.2 3)))
