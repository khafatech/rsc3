#lang scribble/manual
@(require (for-label racket))

@title{OSCresponder}
 client side network responder@section{related}
  Classes/OSCFunc, Classes/OSCdef, Guides/OSC_communication, Classes/OSCresponderNode, Classes/NetAddr
@section{categories}
  External Control>OSC

@section{description}

@section{note}
  As of SC 3.5 link::Classes/OSCFunc:: and link::Classes/OSCdef:: are the recommended way of registering for incoming OSC messages. They are faster, safer, and have more logical argument order than the old responder classes, and they support pattern matching and custom listening ports. Use of the older classes OSCresponder, OSCresponderNode, and OSCpathResponder can be unsafe, since responders in user and class code can override each other unintentionally. They are maintained for legacy code only.::

Register a function to be called upon receiving a specific command from a specific OSC address.

Other applications sending messages to SuperCollider should distinguish between sending messages to the server or the language. Server messages are documented in the link::Reference/Server-Command-Reference:: and should be sent to the server's port - 
@racketblock[s.addr.port:: - which is strong::57110:: by default. Messages sent to the server will not be processed by any OSCresponder in the language.

Messages from external clients that should be processed by OSCresponders must be sent to the language port, strong::57120:: by default. Use ]

@racketblock[NetAddr.langPort:: to confirm which port the SuperCollider language is listening on.

See link::Guides/OSC_communication:: for more details.

]
@section{note}
 
It is highly recommended to use link::Classes/OSCresponderNode:: or link::Classes/OSCpathResponder:: instead of OSCresponder directly. OSCresponders can overwrite each other, but OSCresponderNodes with the same address and command name can coexist peacefully.
::

@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new

@section{argument}
 addr
The address the responder strong::receives from:: (an instance of link::Classes/NetAddr::, e.g. 
@racketblock[Server.default.addr::). An address of nil will respond to messages from anywhere. An address with a port of nil will respond to messages from any port from that specific IP.

]
@section{argument}
 cmdName
An OSC command eg. 
@racketblock['/done'::.

]
@section{argument}
 action
A link::Classes/Function:: that will be evaluated when a cmd of that name is received from addr. arguments: time, theResponder, message, addr
@section{note}
 
OSCresponder evaluates its function in the system process. In order to access the application process (e.g. for GUI access ) use { ... }.defer; within the action function, e.g., 
@racketblock[ { |time, resp, msg| { gui.value_(msg[3]) }.defer } ::
::

]
@section{Examples}
 

see link::Classes/OSCresponderNode::.


