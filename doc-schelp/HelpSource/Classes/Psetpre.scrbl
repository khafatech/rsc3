#lang scribble/manual
@(require (for-label racket))

@title{Psetpre}
 set values of one key in an event before it is passed up@section{related}
  Classes/Pset, Classes/Psetp
@section{categories}
  Streams-Patterns-Events>Patterns>Filter

@section{description}


Sets a value in an event, before it is passed up the stream. To set the value after it has been passed to the stream, use link::Classes/Pset::.

@racketblock[
(
var a, b;
a = Psetpre(\freq, 801, Pbind(\dur, 0.5));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)
::

]
@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 name
the key

@section{argument}
 value
can be a pattern or a stream. The resulting stream ends when that incoming stream ends.

@section{argument}
 pattern
the pattern

@section{Examples}
 


@racketblock[
//Psetpre does not override incoming values:

(
var a, b;
a = Psetpre(\freq, 801, Pbind(\freq, 108));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)


(
var a, b;
a = Psetpre(\freq, Pseq([401, 801], 2), Pbind(\dur, 0.5));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)


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
a = Pbind(\dur, 0.5, \instrument, \sinegrain);
a = Psetpre(\freq, Pseq([500, 600, 700], inf), a);
a = Psetpre(\legato, Pseq([0.01, 1],inf), a);
a.play;
)
::
]


