#lang scribble/manual
@(require (for-label racket))

@title{(Mod a b)}


Modulo, written % in sclang.  outputs a modulo b.


@racketblock[
(audition
   (out 0 (u:mod (f-sin-osc ar 100 4) 1)))
]


