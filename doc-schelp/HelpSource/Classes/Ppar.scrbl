#lang scribble/manual
@(require (for-label racket))

@title{Ppar}
 embed event streams in parallel@section{related}
  Classes/Pgpar, Classes/Pbus, Classes/Pgroup
@section{categories}
  Streams-Patterns-Events>Patterns>Parallel

@section{description}


Embeds several event streams so that they form a single output stream with all their events in temporal order. When one stream ends, the other streams are further embedded until all have ended.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 list
list of patterns or streams.

@section{argument}
 repeats
repeat the whole pattern n times.

@section{Examples}
 


@racketblock[
// see the delta values in the resulting events
(
var a, b, c, t;
a = Pbind(\x, Pseq([1, 2, 3, 4]), \dur, 1);
b = Pbind(\x, Pseq([10, 20, 30, 40]), \dur, 0.4);
c = Ppar([a, b]);
t = c.asStream;
20.do({ t.next(Event.default).postln; });
)

// sound example
(
var a, b;
a = Pbind(\note, Pseq([7, 4, 0], 4), \dur, Pseq([1, 0.5, 1.5], inf));
b = Pbind(\note, Pseq([5, 10, 12], 4), \dur, 1);
Ppar([ a, b ]).play;
)
::
]


