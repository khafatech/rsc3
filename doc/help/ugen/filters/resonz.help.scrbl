#lang scribble/manual
@(require (for-label racket))

@title{(resonz in freq bwr)}


Resonant filter.

A two pole resonant filter with zeroes at z = +/- 1. Based on
K. Steiglitz, "A Note on Constant-Gain Digital Resonators,"
Computer Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.  The
reciprocal of Q is used rather than Q because it saves a divide
operation inside the unit generator.

in - input signal to be processed
freq - resonant frequency in Hertz
rq - bandwidth ratio (reciprocal of Q). rq = bandwidth / centerFreq


@racketblock[
(audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 0.1)))
]

Modulate frequency


@racketblock[
(let ((f (x-line kr 1000 8000 10 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) f 0.05))))
]

Modulate bandwidth


@racketblock[
(let ((rq (x-line kr 1 0.001 8 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 rq))))
]

Modulate bandwidth opposite direction


@racketblock[
(let ((rq (x-line kr 0.001 1 8 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 rq))))
]

random resonator at a random location, run as often as you like...


@racketblock[
(let ((freq (choose (map (lambda (z) (* z 120)) (enum-from-to 1 16))))
      (bw 1/4)
      (gain 8))
  (audition (out 0 (pan2 (resonz (white-noise ar) freq (/ bw freq))
			 (rand -1 1)
			 gain))))
]


