#lang scribble/manual
@(require (for-label racket))

@title{Pclutch}
 sample and hold a pattern@section{related}
  Classes/StreamClutch, Classes/Pstutter, Classes/Latch
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
any pattern.

@section{argument}
 connected
a pattern that returns either a series of link::Classes/Boolean::s or the equivalent 0 and 1. If true (or 1), the pattern plays as usual, if false (or 0), the previous value is kept.

@section{Examples}
 


@racketblock[
Pclutch(Pseq([1, 2, 3, 4, 5], 3), Pseq([0, 0, 1, 0, 0, 0, 1, 1])).asStream.nextN(10);

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
a = Pclutch(
		Pseq([1, 2, 3, 4, 5], inf),
		Pseq([0, 0, 1, 0, Pn(0, {30.rand}), 0, 1, 1], inf)
	).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 200 + 200]);
		0.02.wait;
	}
}.fork;
)
::
]


