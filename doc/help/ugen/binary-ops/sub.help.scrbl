#lang scribble/manual
@(require (for-label racket))

@title{(sub a b)}


subtraction, written '-' in sclang.

Silence.


@racketblock[
(let ((z (f-sin-osc ar 800 0)))
  (audition
   (out 0 (sub z z))))
]




