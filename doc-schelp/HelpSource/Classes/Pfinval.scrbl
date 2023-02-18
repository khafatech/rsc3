#lang scribble/manual
@(require (for-label racket))

@title{Pfinval}
 limit number of items embedded in a stream@section{related}
  Classes/Pfindur, Classes/Pfin, Classes/Pconst
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{description}


@section{note}
 
Pfinval is not appropriate for wrapping link::Classes/Pmono::, link::Classes/Pfx:: etc. For these types of event patterns, you should use link::Classes/Pfin::.
::

@section{ClassMethods}
 

@section{method}
 new
Embeds strong::count:: elements of the strong::pattern:: into the stream.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pfinval(5, Pseq(#[1, 2, 3],inf));
b = a.asStream;
9.do({ b.next.postln; });
)


//Pfinval used as a sequence of pitches

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
var c, b;
c = Pn(Pfinval({ rrand(3, 5)}, Pseq([1, 2, 3, 4, 5, 6],inf)*4+65),inf);
b = c.asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, b.next.midicps]);
		0.12.wait;
	})
}).play;
)
::
]


