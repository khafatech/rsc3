#lang scribble/manual
@(require (for-label racket))

@title{(add a b)}


addition, written '+' in sclang.


@racketblock[
(audition
 (out 0 (add (mul (f-sin-osc ar 800 0) 0.1)
	     (mul (pink-noise ar) 0.1))))
]

DC offset.


@racketblock[
(audition
 (out 0 (add (f-sin-osc ar 440 0) 0.1)))
]


