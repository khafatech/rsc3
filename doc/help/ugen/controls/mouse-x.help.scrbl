#lang scribble/manual
@(require (for-label racket))

@title{(mouse-x rate minval maxval warp lag)}

Cursor UGen.  Report mouse location on root window of the machine
that the synthesis server is running on.  For a linear mapping set
warp to 0, for an exponential mapping set warp to 1.

(import (rsc3))

(audition
 (out 0 (mul (sin-osc ar (mouse-x kr 40 10000 1 0.1) 0) 0.1)))

(audition
 (out 0 (mce2 (mul (sin-osc ar (mouse-x kr 20 2000 1 0.1) 0)
		   (mouse-y kr 0.01 0.1 0 0.1))
	      (mul (sin-osc ar (mouse-y kr 20 2000 1 0.1) 0)
		   (mouse-x kr 0.01 0.1 0 0.1)))))

Auto-pilot variant
(audition
 (out 0 (mul (sin-osc ar (mouse-x* kr 40 10000 1 0.1) 0) 0.1)))

