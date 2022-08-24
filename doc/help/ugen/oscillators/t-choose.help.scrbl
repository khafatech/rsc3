#lang scribble/manual
@(require (for-label racket))

@title{(t-choose trig array)}

The output is selected randomly on recieving a trigger from an
array of inputs.  t-choose is a composite of ti-rand and select.

(audition
 (let* ((t (dust ar (mouse-x kr 1 1000 1 0.1)))
	(f (midi-cps (ti-rand 48 60 t)))
	(a (mce3 (sin-osc ar f 0)
		 (saw ar (mul f 2))
		 (pulse ar (mul f 0.5) 0.1))))
   (out 0 (mul (t-choose t a) 0.1))))

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

