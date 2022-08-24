#lang scribble/manual
@(require (for-label racket))

@title{(comb-n in maxDelayTime delayTime decayTime)}

(comb-l in maxDelayTime delayTime decayTime)
(comb-c in maxDelayTime delayTime decayTime)

Comb delay line. comb-n uses no interpolation, comb-l uses linear
interpolation, comb-c uses all pass interpolation.  All times are in
seconds.  The decay time is the time for the echoes to decay by 60
decibels. If this time is negative then the feedback coefficient
will be negative, thus emphasizing only odd harmonics at an octave
lower.

Comb used as a resonator. The resonant fundamental is equal to
reciprocal of the delay time.

(define src (mul (white-noise ar) 0.01))
(define ctl (x-line kr 0.0001 0.01 20 remove-synth))
(define hear (lambda (u) (audition (out 0 u))))

(hear (comb-n src 0.01 ctl 0.2))
(hear (comb-l src 0.01 ctl 0.2))
(hear (comb-c src 0.01 ctl 0.2))

With negative feedback:

(hear (comb-n src 0.01 ctl -0.2))
(hear (comb-l src 0.01 ctl -0.2))
(hear (comb-c src 0.01 ctl -0.2))

Used as an echo.

(hear (comb-n (mul (decay (mul (dust ar 1) 0.5) 0.2) (white-noise ar))
       0.2 0.2 3))

