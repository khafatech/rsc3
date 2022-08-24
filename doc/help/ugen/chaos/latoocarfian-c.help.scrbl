#lang scribble/manual
@(require (for-label racket))

@title{(latoocarfian-c rate freq a b c d xi yi)}


@racketblock[
(latoocarfian-l rate freq a b c d xi yi)
]

@racketblock[
(latoocarfian-n rate freq a b c d xi yi)
]

This is a function given in Clifford Pickover's book Chaos in
Wonderland, pg 26.  The function has four parameters a, b, c, and
d.  The function is:

xnew = sin
@racketblock[
(y * b)
]
