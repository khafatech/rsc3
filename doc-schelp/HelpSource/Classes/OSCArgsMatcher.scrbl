#lang scribble/manual
@(require (for-label racket))

@title{OSCArgsMatcher}
 Test for specific OSC arguments before evaluating a Function@section{categories}
  External Control>OSC>Matchers
@section{related}
  Classes/OSCFunc, Guides/OSC_communication

@section{description}

OSCArgMatcher matches an argument template to a link::Classes/Function:: or similar object. When its value method is called, it evaluates the function if all of the arguments in its template pass a link::Reference/matchItem:: test.  This is used by link::Classes/OSCMessageDispatcher:: and link::Classes/OSCMessagePatternDispatcher:: to match incoming OSC messages to instances of link::Classes/OSCFunc:: or link::Classes/OSCdef:: using sender address. This class is private, and generally users should not need to address instances directly.

@section{CLASSMETHODS}
 

@section{METHOD}
  new
Make a new OSCArgsMatcher

@section{argument}
  argTemplate
An link::Classes/Array:: comprising a template for determining if incoming arguments match. For each argument that you wish to test, you may include a constant (for exact matching), 
@racketblock[nil:: (indicating that all possible values and types will match), or a link::Classes/Function:: to test the incoming argument (see link::Reference/matchItem:: for examples). These should be in the same order as the items in the incoming OSC message, starting from index 1. (Index 0 is the OSC address.)

]
@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated it will be passed the arguments msg, time, addr, and recvPort, corresponding to the message as an link::Classes/Array:: in the form 
@racketblock[[OSCAddress, ...otherArgs]::, the time that the message was sent, a link::Classes/NetAddr:: corresponding to the IP address of the sender, and an link::Classes/Integer:: corresponding to the port on which the message was received.


]
@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{METHOD}
  value
Test if an incoming message's arguments match, and if so evaluate this object's function. In normal usage (within an OSCFunc) this is done behind the scenes.

@section{argument}
  testMsg
An link::Classes/Array:: in the form 
@racketblock[[OSCAddress, …msgArgs]::.

]
@section{argument}
  time
The time that the message was sent as a link::Classes/Float::.

@section{argument}
  addr
A link::Classes/NetAddr:: corresponding to the IP address of the sender.

@section{argument}
  recvPort
An link::Classes/Integer:: corresponding to the port on which the message was received.


@section{EXAMPLES}
 


@racketblock[
// Basic example (standalone use)
m = OSCArgsMatcher([1, nil, 2], {'matches!'.postln});
m.value(['/myAddress', 1, 3, 2], 0.0, NetAddr.localAddr, NetAddr.langPort); // matches!
::
]


