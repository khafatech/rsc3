#lang scribble/manual
@(require (for-label racket))

@title{Pbeta}
 random values that follow a Eulerian Beta Distribution@section{categories}
  Streams-Patterns-Events>Patterns>Random

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 lo
lower boundary of values.

@section{argument}
 hi
upper boundary of values.

@section{argument}
 prob1
The probability that a value will occur near lo. <1, probability of a value near lo increases. =1, uniform dist. >1 will create a bounded Gaussian-like distribution.

@section{argument}
 prob2
The probability that a value will occur near hi. <1, probability of a value near lo increases. =1, uniform dist. >1 will create a bounded Gaussian-like distribution.

@section{argument}
 length
number of values produced (default: inf).

@section{Examples}
 


@racketblock[
(
var a, c, w;
a = Pbeta(0.0, 1.0, 0.1, 0.1, inf);
c = a.asStream.nextN(500);
w = Window.new("Pbetarand", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);
)

(
var a, c, w;
a = Pbeta(0.0, 1.0, 1, 1, inf);
c = a.asStream.nextN(500);
w = Window.new("Pbetarand", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);(discrete: true);
)

(
var a, c, w;
a = Pbeta(0.0, 1.0, 3, 3, inf);
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
a = Pbeta(0.0, 1.0, 0.1, 0.1, inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)

(
var a;
a = Pbeta(0.0, 1.0, 1.0, 1.0, inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)

(
var a;
a = Pbeta(0.0, 1.0, 3.0, 3.0, inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)
::
]

