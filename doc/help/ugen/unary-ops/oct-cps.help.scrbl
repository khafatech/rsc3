#lang scribble/manual
@(require (for-label racket))

@title{(oct-cps a)}


Convert decimal octaves to cycles per second.


@racketblock[
(audition
 (let ((f (oct-cps (line kr 2 9 6 remove-synth))))
   (out 0 (mul (saw ar f) 0.2))))
]


@racketblock[
(audition 
 (let ((f (oct-cps (u:round (line kr 2 9 6 remove-synth) (/ 1 12)))))
   (out 0 (mul (saw ar f) 0.2))))
]


