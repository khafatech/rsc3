#lang scribble/manual
@(require (for-label racket))

@title{Pbrown}
 brownian motion pattern@section{related}
  Classes/BrownNoise, Classes/Pgbrown
@section{categories}
  Streams-Patterns-Events>Patterns>Random

@section{description}


Returns a stream that behaves like a brownian motion.

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
 step
maximum change per step - the distribution is xrand2.

@section{argument}
 length
number of values produced.

@section{Examples}
 


@racketblock[
(
var a;
a = Pbrown(0.0, 1.0, 0.1, inf);
c = a.asStream.nextN(500);
w = Window.new("Pbrown", Rect(10, 10, 540, 800));
// plot the values
c.plot(bounds: Rect(10, 10, 520, 380), discrete: true, parent: w);
// a histogram of the values
c.histo(500).plot(bounds: Rect(10, 410, 520, 380), parent: w);
)

(
var a;
a = Pbrown(0.0, 1.0, 0.2, inf);
a.asStream.nextN(1000).plot2;
)
::

]

@racketblock[
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
a = Pbrown(0.0, 1.0, 0.1, inf).asStream;
{
	loop {
		Synth(\help_sinegrain, [\freq, a.next * 600 + 300]);
		0.02.wait;
	}
}.fork;
)



// parallel browninan motions
(
var a, x, f;
a = Pbrown(0.0, 1.0, 0.1, inf);

f = { |pattern, dt=0.02, min=300, max=900|
	{
		var x = pattern.asStream;
		loop {
			Synth.grain(\help_sinegrain, [\freq, x.next.linexp(0, 1, min, max), \sustain, dt]);
			dt.wait;
		}
	}.fork;
}.flop;

// 3 parallel motions
f.(a, [0.02, 0.08, 0.16]);
)
::
]


