#lang scribble/manual
@(require (for-label racket))

@title{Pn}
 repeatedly embed a pattern@section{related}
  Classes/Pstutter
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
the pattern to repeat

@section{argument}
 repeats
Repeats the enclosed pattern strong::repeats:: times.

@section{argument}
 key
If strong::key:: is non-nil, it sets the value of that key to true whenever it restarts the pattern. This can be used to advance Patterns enclosed by link::Classes/Pgate::.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pn(Pseq(#[1, 2, 3], 1), 4);	// repeat pattern four times
b = a.asStream;
16.do({ b.next.postln; });
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
a = Pn(Pshuf([1, 2, 2, 3, 3, 3], 4)).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.2.wait;
	}
}.fork;
)
::
]


