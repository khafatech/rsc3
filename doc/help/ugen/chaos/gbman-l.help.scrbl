#lang scribble/manual
@(require (for-label racket))

@title{(gbman-c rate freq xi yi)}

(gbman-l rate freq xi yi)
(gbman-n rate freq xi yi)

Gingerbreadman map chaotic generator.  Cubic, linear and
non-interpolating variants.

freq - iteration frequency in Hertz
xi   - initial value of x
yi   - initial value of y

A linear-interpolating sound generator based on the difference
equations:

	xn+1 = 1 - yn + |xn|
	yn+1 = xn

The behavior of the system is dependent only on its initial conditions
qand cannot be changed once it's started.

Reference: Devaney, R. L. "The Gingerbreadman." Algorithm 3, 15-16,
Jan. 1992.

sclang default initial parameters.

(audition
 (out 0 (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) 1.2 2.1) 0.1)))

Different initial parameters.

(audition
 (out 0 (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) -0.7 -2.7) 0.1)))

Wait for it...

(audition
 (out 0 (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) 1.2 2.0002) 0.1)))

As a frequency control

(audition
 (out 0 (mul (sin-osc ar (mul-add (gbman-l ar 40 1.2 2.1) 400 500) 0) 0.4)))

