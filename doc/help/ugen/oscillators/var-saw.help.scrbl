#lang scribble/manual
@(require (for-label racket))

@title{(var-saw rate freq iphasewidth)}


Variable duty saw

freq - frequency in Hertz
iphase - initial phase offset in cycles ( 0..1 )
width - duty cycle from zero to one.


@racketblock[
(let ((f (mul-add (lf-pulse kr (mce2 3 3.03) 0 0.3) 200 200))
      (w (lin-lin (lf-tri kr 1 0) -1 1 0 1)))
  (audition (out 0 (mul (var-saw ar f 0 w) 0.1))))
]


