#lang scribble/manual
@(require (for-label racket))

@title{(hpz2 in)}


Two zero fixed highpass filter.


@racketblock[
(audition (out 0 (hpz2 (mul (white-noise ar) 0.25))))
]


