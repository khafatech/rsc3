#lang scribble/manual
@(require (for-label racket))

@title{(drand  length array)}

(dxrand length array)

demand rate random sequence generators.

length - number of values to return
array  - array of values or other ugens

dxrand never plays the same value twice, whereas drand chooses any
value in the list.


@racketblock[
(let ((f (lambda (u)
           (let* ((a (u dinf (make-mce (list 1 3 2 7 8))))
                  (t (impulse kr (mouse-x kr 1 400 1 0.1) 0))
                  (f (mul-add (demand t 0 a) 30 340)))
             (mul (sin-osc ar f 0) 0.1)))))
  (audition (out 0 (mce2 (f drand)
                         (f dxrand)))))
]


