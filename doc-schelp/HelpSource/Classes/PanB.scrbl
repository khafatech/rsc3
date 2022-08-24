#lang scribble/manual
@(require (for-label racket))

@title{PanB}
 Ambisonic B-format panner.@section{related}
  Classes/BiPanB2, Classes/DecodeB2, Classes/PanB2, Classes/Rotate2
@section{categories}
   UGens>Multichannel>Ambisonics


@section{description}


Ambisonic B format panner. Output channels are in order W, X, Y, Z.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 azimuth

Azimuth in radians, -π to +π.


@section{argument}
 elevation

Elevation in radians, -0.5π to +0.5π.


@section{argument}
 gain

A control rate level input.


@section{Examples}
 


@racketblock[

// You'll only hear the first two channels on a stereo setup.
play({
	#w, x, y, z = PanB.ar(WhiteNoise.ar, LFSaw.kr(0.5,pi), FSinOsc.kr(0.31, 0.5pi), 0.3);
	//decode for 4 channels
	DecodeB2.ar(4, w, x, y, 0.5);
});

::

]


