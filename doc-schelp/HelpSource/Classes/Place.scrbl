#lang scribble/manual
@(require (for-label racket))

@title{Place}
 interlaced embedding of subarrays@section{related}
  Classes/Ppatlace
@section{categories}
  Streams-Patterns-Events>Patterns>List

@section{description}


Returns elements in the list. If an element is an array itself, it embeds the first element when it comes by first time, the second element when it comes by the second time... The nth when it comes by the nth time.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Place(#[1, [2,5], [3, 6]], inf);
x = a.asStream;
8.do({ x.next.postln; });
)

1
2
3
1
5
6
1
2



//Place used as a sequence of pitches

(
SynthDef("help-sinegrain",
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)


(
c = Place(#[0, 0, [0, 4, 7], [1, 5, 8], [2, 6, 9]], inf) + 67;
x = c.asStream;
Routine({
	loop({
		Synth("help-sinegrain", [\freq, x.next.midicps]);
		0.17.wait;
	})
}).play;
)
::
]


