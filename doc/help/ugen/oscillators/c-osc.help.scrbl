#lang scribble/manual
@(require (for-label racket))

@title{c-osc}


@racketblock[
(import (rsc3))
]


@racketblock[
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 512 1))
   (async fd (b-gen1 10 "sine1" (list (+ 1 2 4) 1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)))))
]


@racketblock[
(audition (out 0 (mul (c-osc ar 10 200 0.7) 0.25)))
]


@racketblock[
(audition (out 0 (mul (c-osc ar 10 200 (mouse-x* kr 0 4 0 0.1)) 0.25)))
]

Compare with:

@racketblock[
(audition (out 0 (mul (osc ar 10 200 0.0) 0.25)))
]


