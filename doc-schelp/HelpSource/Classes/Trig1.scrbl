#lang scribble/manual
@(require (for-label racket))

@title{Trig1}
 Timed trigger.@section{related}
  Classes/Trig
@section{categories}
   UGens>Triggers


@section{description}


When a nonpositive to positive transition occurs at the input, Trig1
outputs 1 for the specified duration, otherwise outputs 0.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Trigger. Trigger can be any signal. A trigger happens when the
signal changes from non-positive to positive.


@section{argument}
 dur

Duration of the trigger output.


@section{Examples}
 


@racketblock[

{ Trig1.ar(Dust.ar(1), 0.2) * FSinOsc.ar(800, 0.5) }.play


To create a fixed duration gate
(

SynthDef("trig1",{ arg dur=0.125;
	var gate;
	gate = Trig1.kr(1.0,dur);
	OffsetOut.ar(0,
		SinOsc.ar(800, 0.3)
		* EnvGen.kr(
			Env([0,0.1,0.1,0],[0.01,1.0,0.01],[-4,4],2),
			gate,
			doneAction: Done.freeSelf)
	)
}).add;

Routine({
	1.0.wait;
	100.do({
		s.sendBundle(0.05,["s_new", "trig1" ,-1,0,0,0,rrand(0.02,0.25)]);
		0.25.wait
	})
}).play(SystemClock)

)

This should sound like a continuous sine wave, although it is actually a series of 0.25 second synths.
(
SynthDef("trig1",{
	var gate;
	gate = Trig1.kr(1.0,0.25);
	OffsetOut.ar(0,
		SinOsc.ar(800, 0.3)
		* EnvGen.kr(
			Env([0,0.1,0.1,0],[0.01,1.0,0.01],[-4,4],2),
			gate,
			doneAction: Done.freeSelf)
	)
}).add;

Routine({
	1.0.wait;
	100.do({
		s.sendBundle(0.05,["s_new", "trig1" ,-1]);
		0.25.wait
	})
}).play(SystemClock)

)

::

]


