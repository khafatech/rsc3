#lang scribble/manual
@(require (for-label racket))

@title{LFNoise2}
 Quadratic noise.@section{related}
  Classes/LFClipNoise, Classes/LFDClipNoise, Classes/LFDNoise0, Classes/LFDNoise1, Classes/LFDNoise3, Classes/LFNoise0, Classes/LFNoise1
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates quadratically interpolated random values at a rate given by
the nearest integer division of the sample rate by the

@racketblock[freq::  argument.

]
@section{note}
 
quadratic interpolation means that the noise values can occasionally extend beyond the normal range of +-1, if the freq varies in certain ways. If this is undesirable then you might like to clip2 the values, or use a linearly-interpolating unit instead.
::

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Approximate rate at which to generate random values.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ LFNoise2.ar(1000, 0.25) }.play;

// modulate frequency

{ LFNoise2.ar(XLine.kr(1000, 10000, 10), 0.25) }.play;

// as frequency modulator
(
{ SinOsc.ar(
		LFNoise2.ar(4, 400, 450),
		0, 0.2
	)
}.play;
)

// freq is the rate of interpolation points
{ var freq = 1000; [LFNoise2.ar(freq), Impulse.ar(freq)] }.plot
::

]


