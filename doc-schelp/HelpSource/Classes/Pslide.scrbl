#lang scribble/manual
@(require (for-label racket))

@title{Pslide}
 slide over a list of values and embed them@section{related}
  Classes/Ptuple
@section{categories}
  Streams-Patterns-Events>Patterns>List

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 list

@section{argument}
 repeats
number of segments.

@section{argument}
 len
length of each segment.

@section{argument}
 step
how far to step the start of each segment from previous. step can be negative.

@section{argument}
 start
what index to start at.

@section{argument}
 wrapAtEnd
if true (default), indexing wraps around if goes past beginning or end. If false, the pattern stops if it hits a nil element or goes outside the list bounds.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pslide([1, 2, 3, 4, 5], inf, 3, 1, 0);
x = a.asStream;
13.do({ x.next.postln; });
)

//Pslide used as a sequence of pitches:

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
c = Pslide(#[1, 2, 3, 4, 5], inf, 3, 1, 0) * 3 + 67;
x = c.asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, x.next.midicps]);
		0.17.wait;
	})
}).play;
)
::
]


