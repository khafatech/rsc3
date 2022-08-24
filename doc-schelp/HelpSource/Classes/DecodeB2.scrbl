#lang scribble/manual
@(require (for-label racket))

@title{DecodeB2}
 2D Ambisonic B-format decoder.@section{related}
  Classes/BiPanB2, Classes/PanB, Classes/PanB2, Classes/Rotate2
@section{categories}
   UGens>Multichannel>Ambisonics


@section{description}


Decode a two dimensional ambisonic B-format signal to a set of speakers
in a regular polygon. The outputs will be in clockwise order. The
position of the first speaker is either center or left of center.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 numChans

Number of output speakers. Typically 4 to 8.


@section{argument}
 w

The B-format signal.


@section{argument}
 x

The B-format signal.


@section{argument}
 y

The B-format signal.


@section{argument}
 orientation

Should be zero if the front is a vertex of the polygon. The first
speaker will be directly in front. Should be 0.5 if the front
bisects a side of the polygon. Then the first speaker will be the
one left of center.

@section{returns}
 
An array of channels, one for each speaker.

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


