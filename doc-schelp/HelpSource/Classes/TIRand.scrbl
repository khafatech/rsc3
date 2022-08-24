#lang scribble/manual
@(require (for-label racket))

@title{TIRand}
 Triggered integer random number generator.@section{related}
  Classes/ExpRand, Classes/IRand, Classes/LinRand, Classes/NRand, Classes/Rand, Classes/TExpRand, Classes/TRand, Classes/TChoose
@section{categories}
  UGens>Random, UGens>Triggers


@section{description}


Generates a random integer value in uniform distribution from

@racketblock[lo::  to  ]

@racketblock[hi::  each time the
trigger signal changes from nonpositive to positive values.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 lo
Lower limit of the output range.

@section{argument}
 hi
Upper limit of the output range.

@section{argument}
 trig

The trigger. Trigger can be any signal. A trigger happens when
the signal changes from non-positive to positive.


@section{Examples}
 


@racketblock[

(
SynthDef("help-TIRand", {
	var trig, outBus;
	trig = Dust.kr(10);
	outBus = TIRand.kr(0, 1, trig); //play on random channel between 0 and 1
	Out.ar(outBus, PinkNoise.ar(0.2))

}).play;
)

(
{
	var trig = Dust.kr(10);
	SinOsc.ar(
			TIRand.kr(4, 12, trig) * 100
		) * 0.1
}.play;
)

(
{
	var trig = Dust.ar(MouseX.kr(1, 8000, 1));
	SinOsc.ar(
			TIRand.ar(4, 12, trig) * 100
		) * 0.1
}.play;
)

::

]


