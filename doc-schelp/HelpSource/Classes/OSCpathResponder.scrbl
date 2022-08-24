#lang scribble/manual
@(require (for-label racket))

@title{OSCpathResponder}
 client side network responder@section{related}
  Classes/OSCFunc, Classes/OSCdef, Guides/OSC_communication, Classes/OSCresponderNode
@section{categories}
  External Control>OSC

@section{description}


@section{note}
  As of SC 3.5 link::Classes/OSCFunc:: and link::Classes/OSCdef:: are the recommended way of registering for incoming OSC messages. They are faster, safer, and have more logical argument order than the old responder classes, and they support pattern matching and custom listening ports. Use of the older classes OSCresponder, OSCresponderNode, and OSCpathResponder can be unsafe, since responders in user and class code can override each other unintentionally. They are maintained for legacy code only.

The replacement for path matching is to be found in the template argument of OSCFunc and OSCDef (see example below).

::

Register a function to be called upon receiving a command with a specific path.

Other applications sending messages to SuperCollider should distinguish between sending messages to the server or the language. Server messages are documented in the link::Reference/Server-Command-Reference:: and should be sent to the server's port - 
@racketblock[s.addr.port:: - which is strong::57110:: by default. Messages sent to the server will not be processed by any link::Classes/OSCresponder:: in the language.

Messages from external clients that should be processed by OSCresponders must be sent to the language port, strong::57120:: by default. Use ]

@racketblock[NetAddr.langPort:: to confirm which port the SuperCollider language is listening on.

See link::Guides/OSC_communication:: for more details.

]
@section{subsection}
 Command paths

OSC commands sometimes include additional parameters to specify the right responder.

For example 
@racketblock[/tr:: commands, which are generated on the server by the link::Classes/SendTrig:: Ugen create an OSC packet consisting of: ]

@racketblock[ [ /tr, nodeID, triggerID, value] ::.
This array actually specifies the source of value: ]

@racketblock[ [ /tr, nodeID, triggerID] ::.
We will refer to that array as a command path.

To create an OSCpathResponder for a specific trigger, the strong::cmdName:: parameter is simply replaced by the complete command path.

]
@section{subsection}
 Path defaults

Any element of the command path array can be set to nil to create a responder that will handle multiple command paths.

For example, setting the commandpath: 
@racketblock[ ['/tr', nil, triggerID] :: makes a responder that responds to ]

@racketblock[/tr:: messages from any Synth but with a specific triggerID.

]
@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 addr
An instance of link::Classes/NetAddr::, usually obtained from your server: server-addr. An address of nil will respond to messages from anywhere.

@section{argument}
 cmdPath
A command path, such as ['\c_set', bus index].

@section{argument}
 action
A link::Classes/Function:: that will be evaluated when a cmd of that name is received from addr. arguments: time, theResponder, message
@section{note}
 
OSCresponderNode evaluates its function in the system process. In order to access the application process (e.g. for GUI access ) use { ... }.defer; within the action function, e.g., 
@racketblock[ { |time, resp, msg| { gui.value_(msg[3]) }.defer } ::
::

]
@section{Examples}
 


@racketblock[
s.boot;

(
	var commandpath, response, aSynth, nodeID, triggerID;
	triggerID = 1;
	aSynth = { arg freq = 1, triggerID = 1;
		SendTrig.kr(SinOsc.kr(freq), triggerID, 666)
	}.play;
	nodeID = aSynth.nodeID;
	commandpath = ['/tr', nodeID, triggerID];
	response = { arg time, responder, message; message.postln };

	o = OSCpathResponder(s.addr, commandpath, response);
	o.add;
)

// switch on and off:
o.remove;
o.add;


// this can be written now conveniently and efficiently with OSCFunc.
// the argTemplate is like the "path" of OSCpathResponder, but without the OSC-path itself.
(
	var commandpath, response, aSynth, nodeID, triggerID;
	triggerID = 1;
	aSynth = { arg freq = 1, triggerID = 1;
		SendTrig.kr(SinOsc.kr(freq), triggerID, 666)
	}.play;
	nodeID = aSynth.nodeID;
	commandpath = [nodeID, triggerID];
	response = { arg message; message.postln };

	o = OSCFunc(response, '/tr', s.addr, argTemplate: commandpath);

)

// an OSCFunc is removed with cmd-Period.
// in order to switch it on and off programmatically:
o.remove;
o.add;

::
]


