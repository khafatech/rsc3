#lang scribble/manual
@(require (for-label racket))

@title{(dser length array)}

demand rate sequence generator.

array  - array of values or other ugens
length - number of values to return

(let* ((a (dser dinf (make-mce (list 1 3 2 7 8))))
       (x (mouse-x kr 1 40 1 0.1))
       (t (impulse kr x 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

