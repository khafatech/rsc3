#lang scribble/manual
@(require (for-label racket))

@title{Pif}
 Pattern-based conditional expression@section{related}
  Classes/Pwhile
@section{categories}
  Streams-Patterns-Events>Patterns>Language Control

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 condition
A pattern or stream returning a link::Classes/Boolean:: value.

@section{argument}
 iftrue
This stream is evaluated if the link::Classes/Boolean:: is true.

@section{argument}
 iffalse
This stream is evaluated if the link::Classes/Boolean:: is false.

@section{argument}
 default
This value (not stream) is returned if "iftrue" or "iffalse" return nil at any time.

@section{Examples}
 


@racketblock[
p = Pif(Pfunc({ 0.3.coin }), Pwhite(0, 9, inf), Pwhite(100, 109, inf)).asStream;
p.nextN(20);

// 7 of the 20 values, or roughly 30%, are in the 0-9 range:

[ 105, 107, 107, 8, 100, 3, 105, 5, 107, 106, 1, 104, 8, 102, 102, 4, 108, 8, 109, 101 ]


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
a = Pif(Pfunc({ 0.3.coin }), Pn(Pseries(0.5, 0.1, 10)), Pn(Pseries(6, -0.1, 10))).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.2.wait;
	}
}.fork;
)
::
]


