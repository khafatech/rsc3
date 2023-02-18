#lang scribble/manual
@(require (for-label racket))

@title{TChoose}
 Randomly select one of several inputs@section{categories}
  UGens>Triggers, UGens>Random
@section{related}
  Classes/TWChoose

@section{description}

An output is selected randomly on receiving a trigger from an array of inputs.

TChoose returns a combination of link::Classes/Select:: and link::Classes/TIRand::.

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  trig
@section{argument}
  array

@section{examples}
 

@racketblock[
(
{
	var a;
	a = [
			SinOsc.ar,
			Saw.ar,
			Pulse.ar
		];
	TChoose.ar(Dust.ar(MouseX.kr(1, 1000, 1)), a) * 0.2

}.play;
)
::

]
@section{note}
  all the ugens are continuously running. This may not be the most efficient way if each input is  cpu-expensive.::


