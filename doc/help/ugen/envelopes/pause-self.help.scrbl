#lang scribble/manual
@(require (for-label racket))

@title{(pause-self src)}


pause enclosing synth when input signal crosses from non-positive
to positive.  If the synth is restarted and the gate reset the
synthesis *not* paused a second time.


@racketblock[
(audition
 (mrg2 (pause-self (mouse-x kr -1 1 0 0.1))
       (out 0 (mul (sin-osc ar 440 0) 0.1))))
]


