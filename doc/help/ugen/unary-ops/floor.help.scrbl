#lang scribble/manual
@(require (for-label racket))

@title{(u:floor a)}


Round signal down.


@racketblock[
(let* ((x (mouse-x kr 65 95 0 0.1))
       (f (midi-cps (u:floor x))))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
]


