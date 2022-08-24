#lang scribble/manual
@(require (for-label racket))

@title{OSCMessageDispatcher}
 The default dispatcher class for OSCFunc and OSCdef.@section{categories}
  External Control>OSC>Dispatchers
@section{related}
  Classes/OSCFunc, Classes/OSCdef, Classes/AbstractWrappingDispatcher, Classes/AbstractDispatcher, Classes/AbstractMessageMatcher, Classes/OSCMessagePatternDispatcher, Classes/OSCFuncAddrMessageMatcher, Classes/OSCFuncRecvPortMessageMatcher, Classes/OSCFuncBothMessageMatcher, Guides/OSC_communication

@section{description}

OSCMessageDispatcher dispatches incoming OSC messages to matching functions. Normally users should not have to create or message instances of this class directly.


@section{CLASSMETHODS}
 


@section{INSTANCEMETHODS}
 

@section{METHOD}
  wrapFunc
Called internally to wrap functions in message matcher objects, if needed.

@section{argument}
  funcProxy
An instance of link::Classes/OSCFunc:: or link::Classes/OSCdef:: whose function(s) are to be wrapped.

@section{METHOD}
  getKeysForFuncProxy
Get the keys at which a responder func's functions are stored in this dispatcher's active dictionary. The keys will be an OSC path.

@section{argument}
  funcProxy
The link::Classes/OSCFunc:: or link::Classes/OSCdef:: whose keys should be returned.

@section{returns}
  An link::Classes/Array:: containing the funcProxy's path as a link::Classes/Symbol::.

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
  register
Adds this dispatcher to thisProcess.recvOSCfunc.

@section{METHOD}
  unregister
Removes this dispatcher from thisProcess.recvOSCfunc.

@section{METHOD}
  typeKey
Returns 
@racketblock['OSC unmatched'::.

]
@section{returns}
  A link::Classes/Symbol::.



