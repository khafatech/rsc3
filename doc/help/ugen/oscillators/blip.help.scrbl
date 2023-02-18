#lang scribble/manual
@(require (for-label racket))

@title{blip}


@racketblock[
(import (rsc3))
]


@racketblock[
(audition (out 0 (mul (blip ar 440 200) 0.15)))
]

Modulate frequency

@racketblock[
(let ((f (x-line kr 20000 200 6 remove-synth)))
  (audition (out 0 (mul (blip ar f 100) 0.2))))
]

Modulate number of harmonics.

@racketblock[
(let ((h (line kr 1 100 20 remove-synth)))
  (audition (out 0 (mul (blip ar 200 h) 0.2))))
]


