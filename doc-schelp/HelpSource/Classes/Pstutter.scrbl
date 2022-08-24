#lang scribble/manual
@(require (for-label racket))

@title{Pstutter}
 repeat input stream values@section{related}
  Classes/Pn
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{description}


repeat each element n times.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 n
may be a pattern, so the number of times can vary each iteration.

@section{argument}
 pattern
the pattern

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pstutter(2, Pseq([1, 2, 3],inf));
x = a.asStream;
13.do({ x.next.postln; });
)


//Pstutter used as a sequence of pitches

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)


(
c = Pstutter(3, Prand([1, 2, 3],inf)*4+65);
x = c.asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, x.next.midicps]);
		0.12.wait;
	})
}).play;
)
::
]


