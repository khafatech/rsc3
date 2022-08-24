#lang scribble/manual
@(require (for-label racket))

@title{PulseDivider}
 Pulse divider.@section{categories}
   UGens>Triggers


@section{description}


Outputs one impulse each time it receives a certain number of triggers at
its input.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 trig

Trigger. Trigger can be any signal. A trigger happens when the
signal changes from non-positive to positive.


@section{argument}
 div

Number of triggers to count before outputting an impulse.


@section{argument}
 start

Starting value for the trigger count. This lets you start
somewhere in the middle of a count. If start is negative
it adds that many counts to the first time the output is
triggered.


@section{Examples}
 


@racketblock[

SynthDef("help-PulseDivider",{ arg out=0;
	var p, a, b;
	p = Impulse.ar(8);
	a = SinOsc.ar(1200, 0, Decay2.ar(p, 0.005, 0.1));
	b = SinOsc.ar(600,  0, Decay2.ar(PulseDivider.ar(p, 4), 0.005, 0.5));

	Out.ar(out,(a + b) * 0.4)
}).play;

::
]


