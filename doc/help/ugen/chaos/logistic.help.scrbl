#lang scribble/manual
@(require (for-label racket))

@title{(logistic rate chaosParam freq)}


UNDOCUMENTED.

Implements the equation: y1 = param * y1 * (1.0 - y1)


@racketblock[
(audition
 (out 0 (mul (logistic ar 2.9 1000) 0.2)))
]

