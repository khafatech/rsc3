#lang scribble/manual
@(require (for-label racket))

@title{(tw-choose trig array weights normalize)}

The output is selected randomly on recieving a trigger from an
array of inputs.  The weights of this choice are determined from
the weights array.  If normalize is set to 1 the weights are
continuously normalized, which means an extra calculation overhead.
When using fixed values the normalizeSum method can be used to
normalize the values.  TWChoose is a composite of TWindex and
select

(let ((a (mce3 (sin-osc ar 220 0)
	       (saw ar 440)
	       (pulse ar 110 0.1)))
      (t (dust ar (mouse-x kr 1 1000 1 0.1)))
      (w (mce3 0.6 0.15 0.05)))
  (audition (out 0 (mul (tw-choose t a w 1) 0.1))))

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

