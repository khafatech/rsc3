#lang scribble/manual
@(require (for-label racket))

@title{Pgbrown}
 geometric brownian motion pattern@section{related}
  Classes/BrownNoise, Classes/Pbrown
@section{categories}
  Streams-Patterns-Events>Patterns>Random

@section{description}


Returns a stream that behaves like a geometric brownian motion.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 lo
lower boundary of values.

@section{argument}
 hi
upper boundary of values.

@section{argument}
 step
maximum multiplication factor per step (omega) - the distribution is xrand2.

@section{argument}
 length
number of values produced.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pgbrown(0.0, 1.0, 0.2, inf);
b = a.asStream;
7.do({ b.next.postln; });
)


// sound example
(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
var a;
a = Pgbrown(1.0, 2.0, 0.1, inf).asStream;
Routine({
	loop({
	Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
	0.02.wait;
	})
}).play;
)

// compare with normal brownian motion:

(
var a;
a = Pbrown(1.0, 2.0, 0.1, inf).asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	})
}).play;
)
::
]

