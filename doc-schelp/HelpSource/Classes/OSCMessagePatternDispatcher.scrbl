#lang scribble/manual
@(require (for-label racket))

@title{OSCMessagePatternDispatcher}
 The default pattern matching dispatcher for OSCFunc and OSCdef.@section{categories}
  External Control>OSC>Dispatchers
@section{related}
  Classes/OSCFunc, Classes/OSCdef, Classes/AbstractWrappingDispatcher, Classes/AbstractDispatcher, Classes/AbstractMessageMatcher, Classes/OSCMessageDispatcher, Classes/OSCFuncAddrMessageMatcher, Classes/OSCFuncRecvPortMessageMatcher, Classes/OSCFuncBothMessageMatcher, Guides/OSC_communication

@section{description}

OSCMessageDispatcher dispatches incoming OSC messages to matching functions, using pattern matching to see if regular expressions wildcards in the incoming message's address pattern match one of this dispatcher's OSCFuncs' paths. Normally users should not have to create or message instances of this class directly. For details on OSC pattern matching, see http://opensoundcontrol.org/spec-1_0


@section{CLASSMETHODS}
 


@section{INSTANCEMETHODS}
 

@section{METHOD}
  value
Attempt to match an incoming OSC message with this dispatcher's responder funcs, and evaluate their functions for all matches found.

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
  recvPort
An link::Classes/Integer:: indicating the port on which the message was received.

@section{METHOD}
  typeKey
Returns 
@racketblock['OSC unmatched'::.

]
@section{returns}
  A link::Classes/Symbol::.



