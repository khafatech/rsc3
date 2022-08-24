#lang scribble/manual
@(require (for-label racket))

@title{(dseries length start step)}

demand rate arithmetic series ugen.

length   - number of values to create
start    - start value
step     - step value

The arguments can be a number or any other ugen

(let* ((a (dseries 15 0 1))
       (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

