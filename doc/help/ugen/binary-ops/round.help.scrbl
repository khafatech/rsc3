#lang scribble/manual
@(require (for-label racket))

@title{(round a b)}


Rounds a to the nearest multiple of b.


@racketblock[
(let* ((x (mouse-x kr 60 4000 0 0.1))
       (f (u:round x 100)))
  (audition
   (out 0 (mul (sin-osc ar f 0) 0.1))))
]


@racketblock[
(let ((n (line kr 24 108 6 remove-synth)))
  (audition
   (out 0 (mul (saw ar (midi-cps (u:round n 1))) 0.2))))
]


