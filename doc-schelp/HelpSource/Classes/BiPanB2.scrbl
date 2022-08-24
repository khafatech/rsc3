#lang scribble/manual
@(require (for-label racket))

@title{BiPanB2}
 2D Ambisonic B-format panner.@section{related}
  Classes/DecodeB2, Classes/PanB, Classes/PanB2, Classes/Rotate2
@section{categories}
   UGens>Multichannel>Ambisonics


@section{description}


Encode a two channel signal to two dimensional ambisonic B-format. This
puts two channels at opposite poles of a 2D ambisonic field. This is one
way to map a stereo sound onto a soundfield. It is equivalent to:


@racketblock[
PanB2(inA, azimuth, gain) + PanB2(inB, azimuth + 1, gain)
::

]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 inA
Input signal A

@section{argument}
 inB
Input signal B.

@section{argument}
 azimuth
Position around the circle from -1 to +1.

-1 is behind, -0.5 is left, 0 is forward, +0.5 is right, +1 is behind.

@section{argument}
 gain
Amplitude control.


@section{Examples}
 


@racketblock[

(
{
	var w, x, y, p, q, a, b, c, d;

	p = LFSaw.ar(200);
	q = LFSaw.ar(301);

	// B-format encode
	#w, x, y = BiPanB2.ar(p, q, MouseX.kr(-1,1), 0.1);

	// B-format decode to quad
	#a, b, c, d = DecodeB2.ar(4, w, x, y);

	[a, b, d, c] // reorder to my speaker arrangement: Lf Rf Lr Rr
}.play;
)

::

]


