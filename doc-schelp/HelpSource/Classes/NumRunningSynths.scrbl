#lang scribble/manual
@(require (for-label racket))

@title{NumRunningSynths}
 Number of currently running synths.@section{related}
  Classes/NumAudioBuses, Classes/NumControlBuses, Classes/NumBuffers, Classes/NumInputBuses, Classes/NumOutputBuses
@section{categories}
   UGens>Info


@section{description}


Number of currently running synths.


@section{classmethods}
 

@section{method}
 ir

@section{Examples}
 


@racketblock[

// example: frequency is derived from the number of synths running
(
SynthDef("numRunning", { arg out;
	Out.ar(out, SinOsc.ar(NumRunningSynths.ir * 200 + 400, 0, 0.1));
}).add;
)

s.sendMsg("/s_new", "numRunning", -1, 0, 0);
s.sendMsg("/s_new", "numRunning", -1, 0, 0);
s.sendMsg("/s_new", "numRunning", -1, 0, 0);
s.sendMsg("/s_new", "numRunning", -1, 0, 0);

::

]


