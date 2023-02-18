#lang scribble/manual
@(require (for-label racket))

@title{(lin-pan2 in pos level)}


Two channel linear pan.  See pan2.


@racketblock[
(audition (out 0 (lin-pan2 (pink-noise ar) (f-sin-osc kr 2 0) 0.1)))
]


@racketblock[
(audition (out 0 (lin-pan2 (f-sin-osc ar 800 0.1) (f-sin-osc kr 3 0) 0.1)))
]


