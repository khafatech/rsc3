#lang scribble/manual
@(require (for-label racket))

@title{OSCFuncRecvPortMessageMatcher}
 Matches incoming messages to responder funcs based on receive port@section{categories}
  External Control>OSC>Matchers
@section{related}
  Classes/AbstractMessageMatcher, Classes/OSCFuncAddrMessageMatcher, Classes/OSCFuncRecvPortMessageMatcher, Classes/OSCFuncBothMessageMatcher

@section{description}

This is used by link::Classes/OSCMessageDispatcher:: and link::Classes/OSCMessagePatternDispatcher:: to match incoming OSC messages  to instances of link::Classes/OSCFunc:: or link::Classes/OSCdef:: using receive port. This class is private, and generally users should not need to address instances directly.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Make a new instance.

@section{argument}
  recvPort
The receive port to attempt to match, in the form of an link::Classes/Integer::.

@section{argument}
  func
The link::Classes/Function:: to evaluate if a match is found.

@section{returns}
  An OSCFuncRecvPortMessageMatcher.


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
  addr
A link::Classes/NetAddr:: indicating the source of the message.

@section{argument}
  testRecvPort
An link::Classes/Integer:: indicating the port on which the message was received.



