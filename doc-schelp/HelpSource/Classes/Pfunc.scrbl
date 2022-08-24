#lang scribble/manual
@(require (for-label racket))

@title{Pfunc}
 Function pattern@section{categories}
  Streams-Patterns-Events>Patterns>Function
@section{related}
  Classes/Pfuncn

@section{description}

Returns a link::Classes/Stream:: that returns values from the 
@racketblock[nextFunc::.


]
@section{classmethods}
 

@section{method}
  new
@section{argument}
  nextFunc
Stream function. In an event stream receives the current link::Classes/Event:: as argument.
@section{argument}
  resetFunc
Function that is called when the stream is reset. In an event stream receives the current link::Classes/Event:: as argument.


@section{examples}
 

@racketblock[
(
var a, x;
a = Pfunc({ exprand(0.1, 2.0) + #[1, 2, 3, 6].choose }, { \reset.postln });
x = a.asStream;
x.nextN(20).postln;
x.reset;
)
::

Sound example
]

@racketblock[
(
SynthDef(\help_sinegrain,
	{ arg out = 0, freq = 440, sustain = 0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
var a;
a = Pfunc({ exprand(0.1, 0.3) + #[1, 2, 3, 6, 7].choose }).asStream;
{
	a.do { |val|
		Synth(\help_sinegrain, [\freq, val * 100 + 300]);
		0.02.wait;
	}
}.fork;
)
::
]


