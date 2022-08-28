#lang scribble/manual
@(require (for-label racket))

@title{(cusp-n rate freq a b xi)}

(cusp-l rate freq a b xi)

freq - iteration frequency in Hertz
a, b - equation variables
xi   - initial value of x

Cusp map chaotic generator.  Non- and linear- interpolating sound
generator based on the difference equation:

xn+1 = a - b*sqrt(|xn|)


@racketblock[
(define cusp_ cusp-l)
]

vary frequency


@racketblock[
(let ((x (mouse-x kr 20 sample-rate 0 0.1)))
  (audition (out 0 (mul (cusp_ ar x 1.0 1.99 0) 0.3))))
]

mouse-controlled params


@racketblock[
(let ((x (mouse-x kr 0.9 1.1 1 0.1))
      (y (mouse-y kr 1.8 2 1 0.1)))
  (audition (out 0 (mul (cusp_ ar (fdiv sample-rate 4) x y 0) 0.3))))
]

as a frequency control


@racketblock[
(let* ((x (mouse-x kr 0.9 1.1 1 0.1))
       (y (mouse-y kr 1.8 2 1 0.1))
       (f (mul-add (cusp_ ar 40 x y 0) 800 900)))
  (audition (out 0 (mul (sin-osc ar f 0.0) 0.4))))
]


