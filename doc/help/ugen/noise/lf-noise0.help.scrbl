#lang scribble/manual
@(require (for-label racket))

@title{(lf-noise0 rate freq)}

(lf-noise1 rate freq)
(lf-noise2 rate freq)

lf-noise0 is step noise.  Generates random values at a rate given by
the nearest integer division of the sample rate by the freq
argument.

lf-noise1 is ramp noise.  Generates linearly interpolated random
values at a rate given by the nearest integer division of the
sample rate by the freq argument.

lf-noise2 is quadratic noise.  Generates quadratically interpolated
random values at a rate given by the nearest integer division of
the sample rate by the freq argument.


@racketblock[
(audition (out 0 (mul (lf-noise0 ar 1000) 0.25)))
]

@racketblock[
(audition (out 0 (mul (lf-noise1 ar 1000) 0.25)))
]

@racketblock[
(audition (out 0 (mul (lf-noise2 ar 1000) 0.25)))
]

Modulate frequency.


@racketblock[
(audition (out 0 (mul (lf-noise0 ar (x-line kr 1000 10000 10 remove-synth)) 0.25)))
]

@racketblock[
(audition (out 0 (mul (lf-noise1 ar (x-line kr 1000 10000 10 remove-synth)) 0.25)))
]

@racketblock[
(audition (out 0 (mul (lf-noise2 ar (x-line kr 1000 10000 10 remove-synth)) 0.25)))
]

Use as frequency control.


@racketblock[
(audition (out 0 (mul (sin-osc ar (mul-add (lf-noise0 kr 4) 400 450) 0) 0.2)))
]

@racketblock[
(audition (out 0 (mul (sin-osc ar (mul-add (lf-noise1 kr 4) 400 450) 0) 0.2)))
]

@racketblock[
(audition (out 0 (mul (sin-osc ar (mul-add (lf-noise2 kr 4) 400 450) 0) 0.2)))
]


