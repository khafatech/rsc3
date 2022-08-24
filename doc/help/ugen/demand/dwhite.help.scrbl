#lang scribble/manual
@(require (for-label racket))

@title{(dwhite  length lo hi)}

(diwhite length lo hi)

demand rate white noise random generators.

length	number of values to create
lo		minimum value
hi 		maximum value

dwhite returns numbers in the continuous range between lo and hi,
diwhite returns integer values.  The arguments can be a number or
any other ugen

(let* ((a (dwhite dinf 0 15))
       (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

(let* ((a (diwhite dinf 0 15))
       (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

