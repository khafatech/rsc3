#lang scribble/manual
@(require (for-label racket))

@title{SampleRate}
 Server sample rate.@section{related}
  Classes/ControlRate, Classes/RadiansPerSample, Classes/SampleDur, Classes/SubsampleOffset
@section{categories}
   UGens>Info


@section{description}


Returns the current sample rate of the server.


@section{classmethods}
 

@section{method}
 ir

@section{Examples}
 


@racketblock[

// compares a 441 Hz sine tone derived from sample rate (44100 * 0.01, left)
// with a 440 Hz tone (right), resulting in a 1 Hz beating
(
{
	var freq;
	freq = [ SampleRate.ir * 0.01, 440];
	SinOsc.ar(freq, 0, 0.1)
}.play;
)

::

]


