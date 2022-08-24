#lang scribble/manual
@(require (for-label racket))

@title{TWChoose}
 Randomly select one of several inputs@section{categories}
  UGens>Triggers, UGens>Random
@section{related}
  Classes/TChoose

@section{description}

An output is selected randomly on receiving a trigger from an array of inputs.

the 
@racketblock[weights:: of this choice are determined from the weights array.

If ]

@racketblock[normalize:: is set to 1 the weights are continuously normalized (this is an extra overhead) when using fixed values the ]

@racketblock[normalizeSum:: method can be used to normalize the values.

TWChoose is a composite of link::Classes/TWindex:: and link::Classes/Select::.

]
@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  trig
@section{argument}
  array
@section{argument}
  weights
@section{argument}
  normalize

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
	TWChoose.ar(Dust.ar(MouseX.kr(1, 1000, 1)), a, [0.99, 0.05, 0.05].normalizeSum) * 0.2

}.play;
)
::

]
@section{note}
  all the ugens are continuously running. This may not be the most efficient way if each input is  cpu-expensive.::


