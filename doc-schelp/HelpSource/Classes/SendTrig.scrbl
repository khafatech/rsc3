#lang scribble/manual
@(require (for-label racket))

@title{SendTrig}
 Send a trigger message from the server back to the client.@section{categories}
   UGens>Triggers
@section{related}
  Classes/OSCFunc, Classes/SendReply


@section{description}


On receiving a trigger (a non-positive to positive transition), send a
trigger message from the server back to the client.


The trigger message sent back to the client is this:

@section{table}
 

## /tr || A trigger message.

## int: || Node ID.

## int: || Trigger ID.

## float: || Trigger value.

::


This command is the mechanism that synths can use to trigger events in
clients. The node ID is the node that is sending the trigger. The trigger
ID and value are determined by inputs to the SendTrig unit generator
which is the originator of this message.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The trigger.


@section{argument}
 id

An integer that will be passed with the trigger message. This is
useful if you have more than one SendTrig in a SynthDef.


@section{argument}
 value

A UGen or float that will be polled at the time of trigger, and
its value passed with the trigger message.


@section{Examples}
 


@racketblock[

s.boot;

(
SynthDef("help-SendTrig",{
	SendTrig.kr(Dust.kr(1.0),0,0.9);
}).add;

// register to receive this message
o = OSCFunc({ arg msg, time;
	[time, msg].postln;
},'/tr', s.addr);
)

Synth("help-SendTrig");

o.free;
::
]


