#lang scribble/manual
@(require (for-label racket))

@title{Pevent}
 Provide an inval for an event stream.@section{related}
  Classes/Pkey, Classes/Pchain
@section{categories}
  Streams-Patterns-Events>Patterns

This pattern is mainly used in two cases:
@section{definitionlist}
 
## using an event pattern as a stream directly
||

@racketblock[
a = Pbind(\note, Prand([1, 2, 3], inf)).asStream;
a.next; // returns nil
a = Pevent(Pbind(\note, Prand([1, 2, 3], inf))).asStream;
a.next; // returns an event
::

## setting some default values before they are processed by the pattern
||
]

@racketblock[
Pevent(Pbind(\harmonic, Pseq([24, 25, 29, 30], inf)), (octave: 3, dur: 0.2)).trace.play;
::
::

Pevent is a simpler form of link::Classes/Pchain::, so that these are equivalent:

]

@racketblock[
Pevent(pattern, event)
Pchain(pattern, event)
pattern <> event
::

By contrast to Pchain, Pevent uses an  link::Classes/Event#*default:: if nothing is provided.

]
@section{ClassMethods}
 

@section{method}
  new

@section{argument}
 pattern
pattern or stream that returns or modifies events.

@section{argument}
  event
an event with objects to embed.  If none is provided, it uses link::Classes/Event#*default::.

@section{examples}
 


@racketblock[
(
SynthDef(\help_sinegrain,
	    { arg out=0, freq=440, sustain=0.05;
		        var env;
		        env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		        Out.ar(out, SinOsc.ar(freq, 0, env))
    }).add;

a = Pn(
	Plazy({
		Pbind(
			\instrument, \help_sinegrain,
			\freq, Pgeom(rrand(200, 600), rrand(1.001, 1.01), rrand(10, 100)).wrap(100, 15000),
			\dur, Pgeom(rrand(0.2, 0.3), rrand(0.99, 0.92))
		)
	})
);


Tdef(\x, {
	var str = Pevent(a).asStream, event;
	loop {
		event = str.next;
		if(event[\freq] % 50 < 20) { event[\freq] = event[\freq] * [1, 1.2, 1.5] };
		event.play;
		event[\dur].wait;
	}

}).play;
)

::
]


