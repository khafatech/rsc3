#lang scribble/manual
@(require (for-label racket))

@title{Pfin}
 limit number of events embedded in a stream@section{related}
  Classes/Pfinval, Classes/Pfindur, Classes/Pconst
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{description}


Limits the number of output values from the given pattern or stream. If the stream is able to output more values, Pfin will terminate it early.

The number may be given as a constant, or calculated on the fly from a function or stream. See the "sequence of pitches" example below.

@section{ClassMethods}
 

@section{method}
 new
embeds strong::count:: elements of the strong::pattern:: into the stream.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pfin(5, Pseq(#[1, 2, 3],inf));
b = a.asStream;
9.do({ b.next.postln; });
)


//Pfin used as a sequence of pitches

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
c = Pn(Pfin({ rrand(3, 5) }, Pseq([1, 2, 3, 4, 5, 6], inf) * 4 + 65), inf);
b = c.asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, b.next.midicps]);
		0.12.wait;
	})
}).play;
)

// or, more concisely:

p = Pbind(
	\instrument, \help_sinegrain,
	\midinote, Pn(Pfin({ rrand(3, 5) }, Pseq([1, 2, 3, 4, 5, 6], inf) * 4 + 65), inf),
	\dur, 0.12
).play;

p.stop;
::
]


