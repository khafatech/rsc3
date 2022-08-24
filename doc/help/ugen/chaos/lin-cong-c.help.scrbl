#lang scribble/manual
@(require (for-label racket))

@title{(lin-cong-c rate freq a c m xi)}

(lin-cong-l rate freq a c m xi)
(lin-cong-n rate freq a c m xi)

linear congruential chaotic generator.

freq - iteration frequency in Hertz
a    - multiplier amount
c    - increment amount
m    - modulus amount
xi   - initial value of x

A cubic-interpolating sound generator based on the difference
equation:

	xn+1 = (axn + c) % m

The output signal is automatically scaled to a range of [-1, 1].


Default initial parameters.

(audition
 (out 0 (let ((x (mouse-x kr 20 sample-rate 0 0.1)))
	  (mul (lin-cong-c ar x 1.1 0.13 1 0) 0.2))))


randomly modulate parameters.

(audition
 (out 0 (mul (lin-cong-c ar
			 (mul-add (lf-noise2 kr 1.0) 1e4 1e4)
			 (mul-add (lf-noise2 kr 0.1) 0.5 1.4)
			 (mul-add (lf-noise2 kr 0.1) 0.1 0.1)
			 (lf-noise2 kr 0.1)
			 0)
	     0.2)))

As frequency control...

(audition
 (out 0 (mul (sin-osc ar (mul-add (lin-cong-c ar
					      40 
					      (mul-add (lf-noise2 kr 0.1) 0.1 1.0)
					      (mul-add (lf-noise2 kr 0.1) 0.1 0.1)
					      (lf-noise2 kr 0.1)
					      0)
				  500 600) 0)
	     0.4)))

