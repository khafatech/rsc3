#lang scribble/manual
@(require (for-label racket))

@title{(quad-n rate freq a b c xi)}

(quad-l rate freq a b c xi)
(quad-c rate freq a b c xi)

freq - iteration frequency in Hertz
a, b, c - equation variables
xi - initial value of x

General quadratic map chaotic generator.  Non-, linear- and cubic-
interpolating sound generators based on the difference equation:
xn+1 = axn2 + bxn + c

(define quad_ quad-c)

(audition
 (out 0 (mul (quad_ ar 4000 1 -1 -0.75 0) 0.2)))

(let ((r (mouse-x kr 3.5441 4 0 0.1)))
  (audition
   (out 0 (mul (quad_ ar 4000.0 (neg r) r 0 0.1) 0.4))))

(let ((r (mouse-x kr 3.5441 4 0 0.1)))
  (audition
   (out 0 (mul (sin-osc ar (mul-add (quad_ ar 4 (neg r) r 0 0.1) 800 900) 0)
	       0.4))))

