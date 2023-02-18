#lang scribble/manual
@(require (for-label racket))

@title{OSCBundle}
 network bundle object@section{related}
  Guides/OSC_communication
@section{categories}
  External Control>OSC

@section{description}

A bundle object that allows to add preparation messages for async processes. If this feature is not needed, a list object can be used instead.

@section{InstanceMethods}
 

@section{private}
 prSend

@section{method}
 add
Add an osc message to the bundle.

@section{method}
 addAll
Add an array of osc messages to the bundle.

@section{method}
 addPrepare
Add a preparation osc message, which is sent before the bundle is sent.

@section{method}
 send
Send the bundle to a server. If preparation messages are given, they are sent, the process waits for their reception abd then sends the bundle.

@section{method}
 schedSend
Like send, but the sending is synced to a given clock to the next beat.

@section{argument}
 server
A link::Classes/Server::.

@section{argument}
 clock
A link::Classes/TempoClock::.

@section{argument}
 quant
Can be a pair of values: [quant, offset].

@section{Examples}
 


@racketblock[
// create a new, empty instance
a = OSCBundle.new;

// a synthdef that needs to be sent to the server, an operation that is asynchronous,
// i.e. we have to wait until it is finished.
x = SynthDef("test", { OffsetOut.ar(0, BPF.ar(Impulse.ar(4) * 10, Rand(9000, 1000), 0.1)) });
// this is why addPrepare is used.
a.addPrepare(["/d_recv", x.asBytes]);
// add is used with synchronous operations, like starting synths.
a.add(["/s_new", "test", -1]);

// the bundle has now the synchronous separated from the asynchronous bundles:
a.messages;
a.preparationMessages;

// this can be simply sent - the bundle takes care of the server client communication
// like waiting for the synthdef to be loaded. the synth is started when the preparation
// is finished.

s.boot; // boot the server
a.send(s);

s.freeAll; // free all nodes on the server

// scheduled sending: the synths are started on the next beat.

a.schedSend(s, TempoClock.default, 1);
a.schedSend(s, TempoClock.default, 1);
a.schedSend(s, TempoClock.default, 1);

s.freeAll; // free all nodes on the server

// the bundle can contain several preparation messages and messages at a time.
// the proparationMessages are sent first and only when they are all completed,
// the other bundles are sent.
// the bundle can also be reused, if there is no specific allocated buffers/node ids.
::
]


