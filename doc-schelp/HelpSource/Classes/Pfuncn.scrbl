#lang scribble/manual
@(require (for-label racket))

@title{Pfuncn}
 Function pattern of given length@section{categories}
  Streams-Patterns-Events>Patterns>Function
@section{related}
  Classes/Pfunc

@section{description}

Returns a stream that returns values from the 
@racketblock[func::.


]
@section{classmethods}
 

@section{method}
  new
@section{argument}
  func
Stream function. In an event stream receives the current link::Classes/Event:: as argument.
@section{argument}
  repeats
End after this number of times.


@section{examples}
 

@racketblock[
(
var a, b, c;
a = Pfuncn({ exprand(0.1, 2.0) + #[1, 2, 3, 6].choose }, 2);
b = Pfuncn({ #[-2, -3].choose }, 2);
Pseq([a, b], inf).asStream.nextN(20).postln;
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
var a, b, c;
a = Pfuncn({ exprand(0.1, 2.0) + #[1, 2, 3, 6].choose }, 2);
b = Pfuncn({ #[-2, -3].choose }, 7);
c = Pseq([a, b], inf).asStream;
{
	c.do { |val|
		Synth(\help_sinegrain, [\freq, val * 100 + 300]);
		0.02.wait;
	}
}.fork;
)
::
]


