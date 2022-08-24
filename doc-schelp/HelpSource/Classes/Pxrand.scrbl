#lang scribble/manual
@(require (for-label racket))

@title{Pxrand}
 embed values randomly chosen from a list@section{related}
  Classes/Prand, Classes/Pwrand
@section{categories}
  Streams-Patterns-Events>Patterns>List

@section{description}

Like link::Classes/Prand::, returns one item from the list at random for each repeat, but Pxrand never repeats the same element twice in a row.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pxrand.new(#[1, 2, 3], 10);	// return 10 items
b = a.asStream;
11.do({ b.next.postln; });
)

//Pxrand used as a sequence of pitches:

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)


(
a = Pxrand(#[60, 61, 63, 65, 72], inf).asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, a.next.midicps]);
		0.1.wait;
	})
}).play;
)
::
]


