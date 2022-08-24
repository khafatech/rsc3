#lang scribble/manual
@(require (for-label racket))

@title{SendReply}
 Send an array of values from the server to all notified clients@section{categories}
  UGens>Triggers
@section{related}
  Classes/SendTrig, Classes/OSCFunc

@section{description}

A message is sent to all notified clients. See link::Classes/Server::.

@section{list}
 
## strong::cmdName::
	@section{list}
 
	## int - node ID
	## int - reply ID
	## ... floats - values.
	::
::

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  trig
a non-positive to positive transition triggers a message.
@section{argument}
  cmdName
a string or symbol, as a message name.
@section{argument}
  values
array of ugens, or valid ugen inputs.
@section{argument}
  replyID
integer id (similar to link::Classes/SendTrig::).

@section{examples}
 

@racketblock[
(
{
	SendReply.kr(Impulse.kr(3), '/the_answer', [40, 41, 42, 43] + MouseX.kr, 1905);
}.play(s);
)

o = OSCFunc({ |msg| msg.postln }, '/the_answer');


// multichannel expansion
(
{
	SendReply.kr(Impulse.kr(3),
		'/the_answer',
		values: [[40, 80], [41, 56], 42, [43, 100, 200]],
		replyID: [1905, 1906, 1907, 1908]
	);
}.play(s);
)

o.free;
::
]


