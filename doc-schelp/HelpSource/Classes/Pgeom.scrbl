#lang scribble/manual
@(require (for-label racket))

@title{Pgeom}
 geometric series pattern@section{related}
  Classes/Pseries
@section{categories}
  Streams-Patterns-Events>Patterns>List

@section{description}


Returns a stream that behaves like a geometric series.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 start
start value.

@section{argument}
 grow
multiplication factor.

@section{argument}
 length
number of values produced.

@section{Examples}
 


@racketblock[
(
var a;
a = Pgeom(1.0, 1.1, inf);
a.asStream.nextN(100).plot;
)


// sound example
(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05, amp=0.1;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2, amp), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)


(
var a;
a = Pgeom(300, 1.03, 70).asStream;
{
	a.do { |val|
		Synth(\help_sinegrain, [\freq, val]);
		0.02.wait;
	}
}.fork;
)

(
Pbind(
	\dur, 0.01,
	\instrument, \help_sinegrain,
	\freq, Pgeom(800, Pbrown(0.99, 1.01, 0.01, inf), inf)
).play;
)
::
]


