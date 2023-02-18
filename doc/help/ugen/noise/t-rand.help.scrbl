#lang scribble/manual
@(require (for-label racket))

@title{(t-rand lo hi trig)}


Generates a random float value in uniform distribution from lo each
time the trig signal changes from nonpositive to positive values


@racketblock[
(let* ((t (dust kr (mce2 5 12)))
       (f (t-rand (mce2 200 1600) (mce2 500 3000) t)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
]


