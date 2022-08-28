#lang scribble/manual
@(require (for-label racket))

@title{(dseq length array)}


demand rate sequence generator.

array  - array of values or other ugens
length - number of repeats


@racketblock[
(let* ((a (dseq 3 (make-mce (list 1 3 2 7 8))))
       (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]


@racketblock[
(let* ((a (dseq dinf (make-mce (replicate-m 32 (random 0 10)))))
       (t (impulse ar (mouse-x kr 1 10000 1 0.1) 0))
       (f (mul-add (demand t 0 a) 30 340)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
]


