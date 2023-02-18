#lang scribble/manual
@(require (for-label racket))

@title{(henon-n rate freq a b x0 x1)}

(henon-l rate freq a b x0 x1)
(henon-c rate freq a b x0 x1)

Henon map chaotic generator.

freq   - iteration frequency in Hertz   -- 22050
a, b   - equation variables             -- 1.4, 0.3
x0, x1 - initial and second values of x -- 0, 0

A non-interpolating sound generator based on the difference
equation:

    xn + 2 = 1 - axn + 12 + bxn

This equation was discovered by French astronomer Michel Henon
while studying the orbits of stars in globular clusters.

With default initial parameters.


@racketblock[
(audition
 (out 0 (mul (henon-n ar (mouse-x kr 20 sample-rate 0 0.1) 1.4 0.3 0 0)
	     0.1)))
]

With mouse-control of parameters.


@racketblock[
(audition
 (out 0 (mul (henon-n ar
		      (fdiv sample-rate 4)
		      (mouse-x kr 1 1.4 0 0.1)
		      (mouse-y kr 0 0.3 0 0.1)
		      0
		      0)
	     0.1)))
]

With randomly modulate parameters.


@racketblock[
(audition
 (out 0 (mul (henon-n ar
		      (fdiv sample-rate 8)
		      (mul-add (lf-noise2 kr 1) 0.20 1.20)
		      (mul-add (lf-noise2 kr 1) 0.15 0.15)
		      0
		      0)
	     0.1)))
]

As a frequency control.


@racketblock[
(let ((x (mouse-x kr 1 1.4 0 0.1))
      (y (mouse-y kr 0 0.3 0 0.1))
      (f 40))
  (audition
   (out 0 (mul (sin-osc ar (mul-add (henon-n ar f x y 0 0) 800 900) 0)
	       0.4))))
]


