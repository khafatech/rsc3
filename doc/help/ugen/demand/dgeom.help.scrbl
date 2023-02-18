#lang scribble/manual
@(require (for-label racket))

@title{(dgeom length start grow)}


demand rate geometric series ugen.

start   - start value
grow    - value by which to grow ( x = x[-1] * grow )
length  - number of values to create

The arguments can be a number or any other ugen


@racketblock[
(let* ((a (dgeom 15 1 1.2))
       (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]

demand rate UGens are not shared...


@racketblock[
(let* ((a  (dgeom 15 1 1.2))
       (t  (impulse ar (mouse-x kr 1 40 1 0.1) 0))
       (f0 (mul-add (demand (delay1 t) 0 a) 30 340))
       (f1 (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar (mce2 f0 f1) 0) 0.1))))
]


@racketblock[
(let* ((a0 (dgeom 15 1 1.2))
       (a1 (dgeom 15 1 1.2))
       (t  (impulse ar (mouse-x kr 1 40 1 0.1) 0))
       (f0 (mul-add (demand (delay1 t) 0 a0) 30 340))
       (f1 (mul-add (demand t 0 a1) 30 340)))
  (audition (out 0 (mul (sin-osc ar (mce2 f0 f1) 0) 0.1))))
]


