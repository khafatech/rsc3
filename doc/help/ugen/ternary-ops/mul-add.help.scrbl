#lang scribble/manual
@(require (for-label racket))

@title{(mul-add a b c)}


Functionally equivalent to (add (mul a b) c).


@racketblock[
(let ((f (mul-add (lf-saw kr (mce2 10 9) 0) 200 400)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]


@racketblock[
(let ((f (add (mul (lf-saw kr (mce2 10 9) 0) 200) 400)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]


