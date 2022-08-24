#lang scribble/manual
@(require (for-label racket))

@title{OSCFuncAddrMessageMatcher}
 Matches incoming messages to responder funcs based on sender address@section{categories}
  External Control>OSC>Matchers
@section{related}
  Classes/AbstractMessageMatcher, Classes/OSCFuncRecvPortMessageMatcher, Classes/OSCFuncBothMessageMatcher

@section{description}

This is used by link::Classes/OSCMessageDispatcher:: and link::Classes/OSCMessagePatternDispatcher:: to match incoming OSC messages to instances of link::Classes/OSCFunc:: or link::Classes/OSCdef:: using sender address. This class is private, and generally users should not need to address instances directly.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Make a new instance.

@section{argument}
  addr
The link::Classes/NetAddr:: to attempt to match.

@section{argument}
  func
The link::Classes/Function:: to evaluate if a match is found.

@section{returns}
  An OSCFuncAddrMessageMatcher.


@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{METHOD}
  value
Check to see if a message matches, and evaluate func if it does.

@section{argument}
  msg
The OSC message as an link::Classes/Array:: in the form 
@racketblock[[OSCAddress, other args]::.

]
@section{argument}
  time
A link::Classes/Float:: indicating the time the incoming message was sent.

@section{argument}
  testAddr
A link::Classes/NetAddr:: indicating the source of the message.

@section{argument}
  recvPort
An link::Classes/Integer:: indicating the port on which the message was received.



