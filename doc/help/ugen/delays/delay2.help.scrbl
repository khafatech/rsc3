#lang scribble/manual
@(require (for-label racket))

@title{(delay2 in)}


Fixed two sample delay.


@racketblock[
(let ((s (impulse ar 1 0)))
  (audition
   (out 0 (add s (delay2 s)))))
]



