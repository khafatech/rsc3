#lang scribble/manual
@(require (for-label racket))

@title{Pcauchy}
 random values that follow a Cauchy Distribution@section{related}
  Classes/BrownNoise, Classes/Pbrown
@section{categories}
  Streams-Patterns-Events>Patterns>Random

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 mean
The mean of the distribution.

@section{argument}
 spread
The horizontal dispersion of the random values. The distribution is unbounded.

@section{argument}
 length
Number of values produced.

@section{Examples}
 


@racketblock[
(
var a, c, w;
a = Pcauchy(0.0, 1.0, inf);
c = a.asStream.nextN(500);
w = Window.new("Pcauchy", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);
)

(
var a, c, w;
a = Pcauchy(0.0, 10.0, inf);
c = a.asStream.nextN(500);
w = Window.new("Pbetarand", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);
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
a = Pcauchy(0.0, 1.0,inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)
::
]


