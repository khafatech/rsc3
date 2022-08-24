#lang scribble/manual
@(require (for-label racket))

@title{SOS}
 Second order filter section (biquad).@section{related}
  Classes/FOS
@section{categories}
   UGens>Filters>Linear


@section{description}


A standard second order filter section. Filter coefficients are given
directly rather than calculated for you. Formula is equivalent to:


@racketblock[

out(i) = (a0 * in(i)) + (a1 * in(i-1)) + (a2 * in(i-2)) + (b1 * out(i-1)) + (b2 * out(i-2))

::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

signal input

@section{argument}
 a0
See formula above.

@section{argument}
 a1
See formula above.

@section{argument}
 a2
See formula above.

@section{argument}
 b1
See formula above.

@section{argument}
 b2
See formula above.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[

// example: same as TwoPole
(
{
	var rho, theta, b1, b2;
	theta = MouseX.kr(0.2pi, pi);
	rho = MouseY.kr(0.6, 0.99);
	b1 = 2.0 * rho * cos(theta);
	b2 = rho.squared.neg;
	SOS.ar(LFSaw.ar(200, 0, 0.1), 1.0, 0.0, 0.0, b1, b2)
}.play
)


(
{
	var rho, theta, b1, b2;
	theta = MouseX.kr(0.2pi, pi);
	rho = MouseY.kr(0.6, 0.99);
	b1 = 2.0 * rho * cos(theta);
	b2 = rho.squared.neg;
	SOS.ar(WhiteNoise.ar(0.1 ! 2), 1.0, 0.0, 0.0, b1, b2)
}.play
)

// example with SOS.kr kr as modulator
(
{
	var rho, theta, b1, b2, vib;
	theta = MouseX.kr(0.2pi, pi);
	rho = MouseY.kr(0.6, 0.99);
	b1 = 2.0 * rho * cos(theta);
	b2 = rho.squared.neg;

	vib = SOS.kr(LFSaw.kr(3.16), 1.0, 0.0, 0.0, b1, b2);
	SinOsc.ar( vib * 200 + 600) * 0.2
}.play
)

::

]


