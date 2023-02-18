#lang scribble/manual
@(require (for-label racket))

@title{(latoocarfian-c rate freq a b c d xi yi)}

(latoocarfian-l rate freq a b c d xi yi)
(latoocarfian-n rate freq a b c d xi yi)

This is a function given in Clifford Pickover's book Chaos in
Wonderland, pg 26.  The function has four parameters a, b, c, and
d.  The function is:

xnew = sin(y * b) + c * sin(x * b);
ynew = sin(x * a) + d * sin(y * a);
x = xnew;
y = ynew;
output = x;

According to Pickover, parameters a and b should be in the range
from -3 to +3, and parameters c and d should be in the range from
0.5 to 1.5.  The function can, depending on the parameters given,
give continuous chaotic output, converge to a single value
(silence) or oscillate in a cycle (tone).  This UGen is
experimental and not optimized currently, so is rather hoggish of
CPU.

Default initial parameters.


@racketblock[
(let ((x (mouse-x kr 20 sample-rate 0 0.1)))
  (audition
   (out 0 (mul (latoocarfian-c ar x 1 3 0.5 0.5 0.5 0.5) 0.2))))
]

randomly modulate all parameters.


@racketblock[
(audition
 (out 0 (mul (latoocarfian-c ar
			     (fdiv sample-rate 4)
			     (mul-add (lf-noise2 kr 2) 1.5 1.5)
			     (mul-add (lf-noise2 kr 2) 1.5 1.5)
			     (mul-add (lf-noise2 kr 2) 0.5 1.5)
			     (mul-add (lf-noise2 kr 2) 0.5 1.5)
			     0.5
			     0.5)
	     0.2)))
]


