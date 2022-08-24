#lang scribble/manual
@(require (for-label racket))

@title{Paddp}
 add each value of a pattern to the value at a key in event stream@section{related}
  Classes/Padd, Classes/Pmulp
@section{categories}
  Streams-Patterns-Events>Patterns>Math

@section{description}


Adds a value to a named value in an event pattern or stream until it ends. Repeats this with new values until the value stream ends.

@section{ClassMethods}
 

@section{method}
 new
@section{argument}
 name
the named value in the event pattern or stream to add to.
@section{argument}
 value
The value, pattern, stream or array to add. The resulting stream ends when this incoming stream ends.
@section{argument}
 pattern
The event pattern or stream within which to add the new values.

@section{Examples}
 


@racketblock[
(
var a, b;
a = Paddp(\freq, Pseq([2, 3, pi],inf), Pbind(\freq, Pseq([100, 200, 300])));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)
::

]

@racketblock[
//sound example
(
SynthDef(\sinegrain,
	{ arg out=0, freq=440, sustain=0.02;
		var env;
		env = EnvGen.kr(Env.perc(0.001, sustain), 1, doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env * 0.1))
	}).add;
)

(
a = Pbind(\freq, Pseq([500, 600, 700]), \instrument, \sinegrain);
a = Paddp(\freq, Pseq([30, 90, -100], inf), a);
a.play;
)
::
]


