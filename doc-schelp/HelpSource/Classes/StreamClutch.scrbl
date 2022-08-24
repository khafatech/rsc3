#lang scribble/manual
@(require (for-label racket))

@title{StreamClutch}
 buffers a streamed value@section{related}
  Classes/Routine, Classes/FuncStream, Classes/EventStreamPlayer
@section{categories}
  Streams-Patterns-Events


@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
a pattern or stream to be buffered.

@section{argument}
 connected
if true it will call the next stream value for each time next is called. if false it returns the last value.

@section{Examples}
 


@racketblock[
a = Pseq([1, 2, 3], inf);
b = StreamClutch(a);

6.do({ b.next.postln });
b.connected = false;
6.do({ b.next.postln });


//statistical clutch
a = Pseq([1, 2, 3], inf);
b = StreamClutch(a, { 0.5.coin });
12.do({ b.next.postln });
::

]

@racketblock[
s.boot;
//sound example:
(
var clutch, pat, decision;
decision = Pseq([Pn(true,10), Prand([true, false], 10)], inf).asStream;
pat = Pbind(\freq, Pseq([200, [300, 302], 400, 450], inf), \dur, 0.3);
clutch = StreamClutch(pat, decision);
clutch.asEventStreamPlayer.play;
)
::

]

@racketblock[
// independant stepping
(
var clutch, pat, decision;
pat = Pbind(\freq, Pseq([200, [300, 302], 400, 450], inf), \dur, 0.3);
b = StreamClutch(pat);
b.connected = false;
b.asEventStreamPlayer.play;
)

b.step;
::
]


