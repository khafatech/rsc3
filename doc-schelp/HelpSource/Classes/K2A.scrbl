#lang scribble/manual
@(require (for-label racket))

@title{K2A}
 Control to audio rate converter.@section{related}
  Classes/A2K
@section{categories}
   UGens>Conversion


@section{description}


To be able to play a control rate UGen into an audio rate UGen,
sometimes the rate must be converted. K2A converts via linear
interpolation.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in
The input signal.

@section{Examples}
 


@racketblock[
{ K2A.ar(WhiteNoise.kr(0.3)) }.scope;


// compare:
(
{
	[
		K2A.ar(WhiteNoise.kr(0.3)),
		WhiteNoise.ar(0.3)
	]
}.scope;
)

(
{
	var freq, blockSize, sampleRate;
	blockSize = s.options.blockSize; // usually 64
	sampleRate = s.sampleRate;
	freq = MouseX.kr(0.1, 40, 1) / blockSize * sampleRate;
	[
		K2A.ar(LFNoise0.kr(freq)),
		LFNoise0.ar(freq)
	] * 0.3
}.scope;
)
::

]


