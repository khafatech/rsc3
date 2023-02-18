#lang scribble/manual
@(require (for-label racket))

@title{LFNoise0}
 Step noise@section{related}
  Classes/LFClipNoise, Classes/LFDClipNoise, Classes/LFDNoise0, Classes/LFDNoise1, Classes/LFDNoise3, Classes/LFNoise1, Classes/LFNoise2
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates random values at a rate given by the nearest integer division
of the sample rate by the  
@racketblock[freq::  argument.


]
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
{ LFNoise0.ar(1000, 0.25) }.play;

// modulate frequency

{ LFNoise0.ar(XLine.kr(1000, 10000, 10), 0.25) }.play;

// as frequency modulator
(
{ SinOsc.ar(
		LFNoise0.ar(4, 400, 450),
		0, 0.2
	)
}.play;
)

// freq is the rate of starting points
{ var freq = 1000; [LFNoise0.ar(freq), Impulse.ar(freq)] }.plot
::

]


