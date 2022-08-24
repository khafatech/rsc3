#lang scribble/manual
@(require (for-label racket))

@title{FOS}
 First order filter section.@section{related}
  Classes/SOS
@section{categories}
   UGens>Filters>Linear


@section{description}


A standard first order filter section. Filter coefficients are given
directly rather than calculated for you. Formula is equivalent to:


@racketblock[
out(i) = (a0 * in(i)) + (a1 * in(i-1)) + (b1 * out(i-1))
::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
Signal input.

@section{argument}
 a0
See formula above.

@section{argument}
 a1
See formula above.

@section{argument}
 b1
See formula above.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[

(
// same as OnePole
{	var x;
	x = LFTri.ar(0.4, 0, 0.99);
	FOS.ar(LFSaw.ar(200, 0, 0.2), 1 - x.abs, 0.0, x)
}.play;
)

(
// same as OneZero
{	var x;
	x = LFTri.ar(0.4, 0, 0.99);
	FOS.ar(LFSaw.ar(200, 0, 0.2), 1 - x.abs, x, 0.0)
}.play;
)

(
// same as OnePole, kr
{	var x, ctl;
	x = LFTri.kr(0.2, 0, 0.99);
	ctl = FOS.kr(LFSaw.kr(8, 0, 0.2), 1 - x.abs, 0.0, x);
	LFTri.ar(ctl * 200 + 500);
}.play;
)

::

]


