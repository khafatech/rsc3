#lang scribble/manual
@(require (for-label racket))

@title{Normalizer}
 Flattens dynamics.@section{related}
  Classes/Amplitude, Classes/Compander, Classes/CompanderD, Classes/Limiter
@section{categories}
  UGens>Dynamics


@section{description}


Normalizes the input amplitude to the given level. Normalizer will not
overshoot like  link::Classes/Compander::  will, but it needs to look
ahead in the audio. Thus there is a delay equal to twice the value
of the  
@racketblock[dur::  parameter.


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
	[z, Normalizer.ar(z, 0.4, 0.01)]
}, 0.5)
)

::

]


