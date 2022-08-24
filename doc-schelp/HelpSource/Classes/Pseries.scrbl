#lang scribble/manual
@(require (for-label racket))

@title{Pseries}
 arithmetic series pattern@section{related}
  Classes/Pgeom
@section{categories}
  Streams-Patterns-Events>Patterns>List

@section{description}


Returns a stream that behaves like an arithmetic series.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 start
start value.

@section{argument}
 step
addition factor.

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
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
var a;
a = Pseries(300, 20, 70).asStream;
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
	\freq, Pseries(800.0, Pbrown(-1.0, 3.0, 0.1, inf), inf)
).play;
)
::
]


