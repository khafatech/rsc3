#lang scribble/manual
@(require (for-label racket))

@title{Prewrite}
 rewriting system@section{related}
  Classes/Pfsm
@section{categories}
  Streams-Patterns-Events>Patterns>List>Indexing

@section{description}


Lindenmayer system pattern for selfsimilar structures. Its strong::dictionary (or event):: maps one element to an array of child elements. The algorithm replaces iteratively (strong::levels:: deep) elements by arrays of elements starting with the values in the strong::pattern::.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
starting value

@section{argument}
 dict
a dictionary or an event.

@section{argument}
 levels
number of levels


@racketblock[
IdentityDictionary[
	elem1 -> [ otherElements ],
	elem2 -> [ otherElements ],
	elem2 -> [ otherElements ]
]
::

]
@section{Examples}
 

The examples use the 
@racketblock[():: shortcut for link::Classes/Event::.

]

@racketblock[
(
a = Prewrite(0, // start with 0
		(	0: #[2,0],
			1: #[0,0,1],
			2: #[1,0,1]
		), 4);
x = a.asStream;
30.do({ x.next.postln });
)


//Prewrite used as a sequence of pitches:

(
SynthDef(\help_sinegrain,
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.2), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)

(
a = Prewrite(0,	(
	0: #[2,0],
	1: #[0,0,1],
	2: #[1,0,1]
), 4).asStream;
Routine({
	loop({
		Synth(\help_sinegrain, [\freq, (a.next * 5 + 70).midicps]);
		0.1.wait;
	})
}).play;
)
::
]


