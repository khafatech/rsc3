#lang scribble/manual
@(require (for-label racket))

@title{Punop}
 unary operator pattern@section{related}
  Classes/Pbinop, Classes/Pnaryop, Classes/UnaryOpFunction, Overviews/Operators
@section{categories}
  Streams-Patterns-Events>Patterns>Math

@section{description}


Returns a stream that applies the unary operator to the stream values of the receiver. Usually, this is the result of applying a unary operator (i.e. a method with one argument) to a pattern.

Examples of unary operators are: squared, sqrt, sin, tan ...

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 operator
operator to be applied

@section{argument}
 a
a pattern (or compatible pattern input)

@section{Examples}
 


@racketblock[
(
var a;
a = Punop(\sqrt, Pseries(0, 1, 12));
a.asStream.all;
)

// this is the same as:
(
var a;
a = Pseries(0, 1, 12).sqrt;
a.asStream.all;
)

// some common cases:
Pseq([1, 2, 3]).squared;
Pseq([0.2, 0.5, 0.8]).coin;
Pwhite(-100, 100, inf).abs;



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
a = Pn(Punop(\sqrt, Pseries(0, 1, 12))).asStream;
{
	a.do { |val|
		Synth(\help_sinegrain, [\freq, a * 200 + 300].postln);
		0.5.wait;
	}
}.fork;
)

(
Pbind(
	\dur, 0.01,
	\instrument, \help_sinegrain,
	\note, Pn(Punop(\sqrt, Pseries(0, 1, 12)))
).play;
)


// these are the same as:

(
var a;
a = Pn(Pseries(0, 1, 12).sqrt).asStream;
{
	a.do { |val|
		Synth(\help_sinegrain, [\freq, a * 200 + 300].postln);
		0.05.wait;
	}
}.fork;
)

(
Pbind(
	\dur, 0.1,
	\instrument, \help_sinegrain,
	\note, Pn(Pseries(0, 1, 12).sqrt)
).play;
)
::
]


