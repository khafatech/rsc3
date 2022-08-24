#lang scribble/manual
@(require (for-label racket))

@title{PulseCount}
 Pulse counter.@section{related}
  Classes/Stepper
@section{categories}
   UGens>Triggers


@section{description}


Each trigger increments a counter which is output as a signal.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 trig

Trigger. Trigger can be any signal. A trigger happens when the
signal changes from non-positive to positive.


@section{argument}
 reset

Resets the counter to zero when triggered.


@section{Examples}
 


@racketblock[

SynthDef("help-PulseCount",{ arg out=0;
	Out.ar(out,
		SinOsc.ar(
			PulseCount.ar(Impulse.ar(10), Impulse.ar(0.4)) * 200,
			0, 0.05
		)
	)
}).play;

::

]


