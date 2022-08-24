#lang scribble/manual
@(require (for-label racket))

@title{Padd}
 add to value of a key in event stream@section{related}
  Classes/Paddp, Classes/Pmul
@section{categories}
  Streams-Patterns-Events>Patterns>Math

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 name

@section{argument}
 value
can be a pattern or a stream. The resulting stream ends when that incoming stream ends.

@section{argument}
 pattern

@section{Examples}
 


@racketblock[
(
var a, b;
a = Padd(\freq, 801, Pbind(\freq, 100));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)
::

]

@racketblock[
(
var a, b;
a = Padd(\freq, Pseq([401, 801], 2), Pbind(\freq, 100));
x = a.asStream;
9.do({ x.next(Event.new).postln; });
)
::

]

@racketblock[
//sound example
(
SynthDef(\sinegrain,
	{ arg out=0, freq=440, gate=1;
		var env;
		env = EnvGen.kr(Env.asr(0.001, 1, 0.2), gate, doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env * 0.1))
	}).add;
)

(
a = Pbind(\dur, 0.5, \instrument, \sinegrain, \freq, 440);
b = Padd(\freq, Pseq([10, 30, 100], inf), a);
b.play;
)
::
]


