#lang scribble/manual
@(require (for-label racket))

@title{Plambda}
 create a scope (namespace) for enclosed streams@section{related}
  Classes/Penvir, Classes/Pkey
@section{categories}
  Streams-Patterns-Events>Patterns>Data Sharing

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
an event stream.

@section{argument}
 scope
an event with default bindings (can be nil).

@section{Examples}
 


@racketblock[
// Plet, Pget and Plambda.
// Plet(key, stream, return)
// Pget(key, default, repeats)

(
SynthDef(\sine,
	{ arg out=0, freq=440, sustain=0.05, pan=0, amp=0.1;
		var env;
		env = EnvGen.kr(Env.perc(Rand(0.001, 0.02), sustain, AmpCompA.kr(freq)*amp), doneAction: Done.freeSelf);
		Out.ar(out, Pan2.ar(SinOsc.ar(freq), pan, env))
	}).add;
)

(
a = Plambda(
	Pseq([
	Pfindur(5,
		Ppar([
			Pbind(\note, Plet(\x, Prand([1, 5, 1, [10, 14]], inf)), \dur, 8/3, \pan, -1),
			Pbind(\note, Plet(\y, Pseq([5, 3, 2, 0, [0, 5, 6, 9]], inf)), \dur, 0.5, \pan,1),
			Pbind(\note, Pseq([Pget(\x), Pget(\y)], inf) + 12, \pan, 0, \dur, 2/3)
		])
	),
	Pbind(\note, Pget(\x, 0, 6) + [0, 5], \dur, Pstutter(inf, Prand([2/3, 1/6])))
	], inf).trace(\eventScope) // internally, the values are shared via \eventScope
);
b = Pbindf(a, \instrument, \sine, \legato, 0.1);
b.play
)

// this structure remains parallelizable

Ppar([b, Pbindf(b, \ctranspose, 24, \dur, Pkey(\dur) * 0.25)]).play;
::
]


