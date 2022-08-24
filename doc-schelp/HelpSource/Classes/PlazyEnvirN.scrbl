#lang scribble/manual
@(require (for-label racket))

@title{PlazyEnvirN}
 instantiate new patterns from a function and multichannel expand them@section{related}
  Classes/Plazy, Classes/PlazyEnvir, Classes/Pfunc
@section{categories}
  Streams-Patterns-Events>Patterns>Function

@section{description}


Evaluates a function that returns a pattern and embeds it in a stream.
@section{list}
 
## In difference to link::Classes/Plazy::, the function is evaluated using the environment passed in by the stream.
## In difference to link::Classes/PlazyEnvir::, PlayzEnvirN expands to strong::multiple parallel patterns:: if the function arguments receive multiple channels. This works only with event streams.
::

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 func
A link::Classes/Function:: that returns a pattern or any other valid pattern input.


@racketblock[
(
f = { arg g=0, h=0, dur=1;
	Pbind(\degree, Pseq([g, g, h, g, h], 2), \dur, Pseries(dur, 0.1))
};
// compare:
a = Pchain(PlazyEnvirN(f), (g: [1, 2], h: 3, dur:0.2)).trace(\degree).asStream;
c = Pchain(Plazy(f), (g: [1, 2], h: 3, dur:0.2)).asStream;
)

a.nextN(4);
c.nextN(4); // no degrees, because stream ends
::


]
@section{Examples}
 


@racketblock[
(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05, pan=0;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, Pan2.ar(SinOsc.ar(freq, 0, env), pan))
	}).add;

a = PlazyEnvirN({ arg g=0, h=0, dur=1;
	postf("g: %, h: %, dur: %\n", g, h, dur);

	Pbind(
		\instrument, \help_sinegrain,
		\dur, dur,
		\degree, Pseq([g, g, h, g, h], 2)
	)
});
)

// different variants
(a <> (g: 0, h: 3, dur:0.2)).play; // single stream
(a <> (g: [0, 4], h: [3, -1], dur:0.2)).play; // same durations, two streams
(a <> (g: [0, 4], h: [3, -1], dur: [0.2, 0.3])).play; // different durations, two streams
::

For more about the composition operator ]

@racketblock[<>:: see: link::Classes/Pchain::.
]


