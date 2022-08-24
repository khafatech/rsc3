#lang scribble/manual
@(require (for-label racket))

@title{Pfindur}
 limit total duration of events embedded in a stream@section{related}
  Classes/Pfinval, Classes/Pfin, Classes/Pconst
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{ClassMethods}
 

@section{method}
 new
Embeds elements of the strong::pattern:: into the stream until the duration comes close enough to strong::dur::.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Pfindur(5, Pbind(\dur, Prand([1, 2, 0.5, 0.1], inf)));
x = a.asStream;
9.do({ x.next(Event.default).postln; });
)


//Pfindur used as a sequence of pitches

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
var c;
c = Pbind(
	\dur, Prand([1, 0.02, 0.2], inf),
	\instrument, \help_sinegrain,
	\degree, Pseries(0, 1, inf),
	\octave, 6
);

Pn(
	Pfindur(1, c)
).play;
)
::
]


