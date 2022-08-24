#lang scribble/manual
@(require (for-label racket))

@title{Pmeanrand}
 random values that tend toward ((lo + hi) / 2)@section{related}
  Classes/Plprand, Classes/Phprand
@section{categories}
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
 length
number of values produced.

@section{Examples}
 


@racketblock[
(
var a, c, w;
a = Pmeanrand(0.0, 1.0, inf);
c = a.asStream.nextN(500);
w = Window.new("Pmeanrand", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);)


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
a = Pmeanrand(0.0, 1.0, inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)

// this is equivalent to:
(
{
	loop {
		Synth(\help_sinegrain, [\freq, (rrand(0.0, 1.0) + rrand(0.0, 1.0) * 0.5) * 600 + 300]);
		0.02.wait;
	}
}.fork;
)
::
]


