#lang scribble/manual
@(require (for-label racket))

@title{Ppoisson}
 random values that follow a Poisson Distribution (positive integer values)@section{related}
  Classes/Pgauss
@section{categories}
  Streams-Patterns-Events>Patterns>Random

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 mean
The mean of the distribution.

@section{argument}
 length
Number of values produced.

@section{Examples}
 


@racketblock[
(
var a, c, w;
a = Ppoisson(1.0, inf);
c = a.asStream.nextN(500);
w = Window.new("Ppoisson", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);
)

(
var a, c, w;
a = Ppoisson(10.0, inf);
c = a.asStream.nextN(500);
w = Window.new("Ppoisson", Rect(10, 10, 540, 800));
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
a = Ppoisson(1.0,inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, (a.next + 72).midicps]);
		0.02.wait;
	}
}.fork;
)

(
var a;
a = Ppoisson(7.0,inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, (a.next + 72).midicps]);
		0.02.wait;
	}
}.fork;
)
::
]


