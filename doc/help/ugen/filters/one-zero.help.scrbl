#lang scribble/manual
@(require (for-label racket))

@title{(one-zero in coef)}


One zero filter


@racketblock[
(audition
 (out 0 (one-zero (mul (white-noise ar) 0.5) 0.5)))
]


@racketblock[
(audition
 (out 0 (one-zero (mul (white-noise ar) 0.5) -0.5)))
]


@racketblock[
(audition
 (out 0 (one-zero (mul (white-noise ar) 0.5) 
		  (line kr -0.5 0.5 10 remove-synth))))
]


