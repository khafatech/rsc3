#lang scribble/manual
@(require (for-label racket))

@title{(bpz2 in)}


Two zero fixed midpass.  This filter cuts out 0 Hz and the Nyquist
frequency.


@racketblock[
(audition (out 0 (bpz2 (mul (white-noise ar) 0.25))))
]



