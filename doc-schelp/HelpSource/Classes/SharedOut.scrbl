#lang scribble/manual
@(require (for-label racket))

@title{SharedOut}
 Write to a shared control bus.@section{related}
  Classes/SharedIn
@section{categories}
   UGens>InOut


@section{description}


@section{warning}
 
SharedIn has been deprecated. Synchronous access to busses on local servers is possible via
link::Classes/Bus#-getSynchronous:: and link::Classes/Bus#-setSynchronous::
::


Reads from a control bus shared between the internal server and the SC
client. Control rate only. Reading from a shared control bus on the
client is synchronous. When not using the internal server use the get
method of Bus (or /c_get in messaging style) or
link::Classes/SendTrig:: with an link::Classes/OSCFunc::.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 bus

The index of the shared control bus to write to.


@section{argument}
 channelsArray

An Array of channels or single output to write out. You cannot
change the size of this once a SynthDef has been built.


@section{Examples}
 


@racketblock[

(
// only works with the internal server
s = Server.internal;
s.boot;
)

(
SynthDef("help-SharedOut", {
	SharedOut.kr(0, SinOsc.kr(0.2));
}).add;
)

(
s.sendMsg(\s_new, "help-SharedOut", x = s.nextNodeID, 0, 1);
s.sendMsg(\n_trace, x);

// poll the shared control bus
Routine({
	30.do({
		s.getSharedControl(0).postln;
		0.2.wait;
	});
}).play;
)


s.quit;

::

]


