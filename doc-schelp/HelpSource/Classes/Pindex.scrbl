#lang scribble/manual
@(require (for-label racket))

@title{Pindex}
 pattern that indexes into an array@section{related}
  Classes/Pswitch
@section{categories}
  Streams-Patterns-Events>Patterns>List>Indexing

@section{description}


This allows an link::Classes/ArrayedCollection:: to be accessed within patterns.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 listPat
the array. Can be a link::Classes/Pattern::.

@section{argument}
 indexPat
the value to retrieve. Can be a link::Classes/Pattern::.

@section{argument}
 repeats
specifies the number of repeats.

@section{Examples}
 


@racketblock[
(
SynthDef(\help_pindex, { | out, amp=0.1, freq=440, gate=1 |
	var son = Saw.ar(freq * [0.99, 1, 1.01]).mean;
	son = son * EnvGen.ar(Env.adsr, gate: gate, doneAction: Done.freeSelf);
	Out.ar(out, son.dup * amp);
}).add;
)

(
var data = [7, 13, 12, 2, 2, 2, 5];
var indices = [0, 0, 2, 0, 4, 6, 7];
Pbind(
	\instrument, \help_pindex,
	\choice, Prand(indices, inf),
	\degree, Pindex(data, Pkey(\choice), inf),
	\dur, 0.7
).play
)
::
]


