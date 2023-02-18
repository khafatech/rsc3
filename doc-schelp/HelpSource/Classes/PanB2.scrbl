#lang scribble/manual
@(require (for-label racket))

@title{PanB2}
 2D Ambisonic B-format panner.@section{related}
  Classes/BiPanB2, Classes/DecodeB2, Classes/PanB, Classes/Rotate2
@section{categories}
   UGens>Multichannel>Ambisonics


@section{description}


Encodes a mono signal to 2-dimensional ambisonic B-format.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 azimuth

Position around the circle from -1 to +1. -1 is behind, -0.5 is
left, 0 is forward, +0.5 is right, +1 is behind.


@section{argument}
 gain

Amplitude control.


@section{Examples}
 


@racketblock[

(
{
	var w, x, y, p, a, b, c, d;

	p = PinkNoise.ar; // source

	// B-format encode
	#w, x, y = PanB2.ar(p, MouseX.kr(-1,1), 0.1);

	// B-format decode to quad
	#a, b, c, d = DecodeB2.ar(4, w, x, y);

	[a, b, d, c] // reorder to my speaker arrangement: Lf Rf Lr Rr
}.play;
)

::

]


