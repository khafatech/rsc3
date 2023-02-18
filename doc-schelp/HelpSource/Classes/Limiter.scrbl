#lang scribble/manual
@(require (for-label racket))

@title{Limiter}
 Peak limiter@section{related}
  Classes/Amplitude, Classes/Compander, Classes/CompanderD, Classes/Normalizer
@section{categories}
  UGens>Dynamics


@section{description}


Limits the input amplitude to the given level. Limiter will not overshoot
like  link::Classes/Compander::  will, but it needs to look ahead in the
audio. Thus there is a delay equal to twice the value of the

@racketblock[dur::  parameter.

Limiter, unlike Compander, is completely transparent for an in range signal.

]
@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in
The signal to be processed.

@section{argument}
 level
The peak output amplitude level to which to normalize the input.

@section{argument}
 dur
aka lookAheadTime.
The buffer delay time. Shorter times will produce smaller delays
and quicker transient response times, but may introduce amplitude
modulation artifacts.


@section{Examples}
 


@racketblock[
(
// example signal to process
Synth.play({
	var z;
	z = Decay2.ar(
		Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)),
		0.001, 0.3, FSinOsc.ar(500));
}, 0.8)
)

(
Synth.play({
	var z;
	z = Decay2.ar(
		Impulse.ar(8, LFSaw.kr(0.25, -0.6, 0.7)),
		0.001, 0.3, FSinOsc.ar(500));
	[z, Limiter.ar(z, 0.4, 0.01)]
}, 0.5)
)
::

]


