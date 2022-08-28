#lang scribble/manual
@(require (for-label racket))

@title{(pluck in tr maxdelaytime delaytime decaytime coef)}


Karplus-Strong synthesis.

in - an excitation signal

tr - upon a negative to positive transition, the excitation signal
     will be fed into the delay line

maxdelaytime - the max delay time in seconds (initializes the
               internal delay buffer).

delaytime - delay time in seconds.

decaytime - time for the echoes to decay by 60 decibels. Negative
            times emphasize odd partials.

coef - the coef of the internal one-pole filter. Values should be
       between -1 and +1 (larger values will be unstable... so be
       careful!).

Excitation signal is white-noise, triggered twice a second with
varying one-pole coef.


@racketblock[
(let ((n (mul (white-noise ar) 0.1))
      (t (impulse kr 2 0))
      (x (mouse-x kr -0.999 0.999 0 0.1))
      (dl (/ 1 440)))
  (audition (out 0 (pluck n t dl dl 10 x))))
]


@racketblock[
(let* ((n 25)
       (gen (lambda (n f) (mce-fill n (lambda (_) (f)))))
       (f (gen n (lambda () (rand 0.05 0.2))))
       (p (gen n (lambda () (rand 0 1))))
       (x (mouse-x kr 60 1000 1 0.1))
       (o (lin-lin (sin-osc kr f p) -1 1 x 3000))
       (w (clone n (mul (white-noise ar) 0.1)))
       (i (impulse kr (gen n (lambda () (rand 10 12))) 0))
       (ks (pluck w i 0.01 (fdiv 1 o) 2 (rand 0.01 0.2)))
       (l (gen n (lambda () (rand -1 1)))))
  (audition (out 0 (leak-dc (mix (pan2 ks l 1)) 0.995))))
]


