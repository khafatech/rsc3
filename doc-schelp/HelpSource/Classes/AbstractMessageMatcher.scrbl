#lang scribble/manual
@(require (for-label racket))

@title{AbstractMessageMatcher}
 Matches incoming messages to Functions@section{categories}
  External Control>Abstract Classes
@section{related}
  Classes/AbstractResponderFunc, Classes/AbstractWrappingDispatcher

@section{description}

Instances of subclasses of AbstractMessageMatcher are used by subclasses of link::Classes/AbstractWrappingDispatcher:: to match multiple parameters of incoming messages (i.e. OSC or MIDI) to instances of subclasses of link::Classes/AbstractResponderFunc::. This class and its subclasses are private and generally users should not need to address them directly.


@section{CLASSMETHODS}
 


@section{INSTANCEMETHODS}
 

@section{METHOD}
  func
Get or set this object's response link::Classes/Function::.

@section{returns}
  A link::Classes/Function:: or similar object.

@section{METHOD}
  value
Evaluate an incoming message to see if it matches. Subclasses should override this message to take appropriate arguments. If a match is found, this method should call value on this object's func, passing the message as appropriate arguments.

@section{METHOD}
  valueArray
As link::#-value:: above, but with the arguments passed as a single link::Classes/Array::. This method is needed so that subclasses can work in FunctionLists.

@section{argument}
  args
An link::Classes/Array:: containing the message and appropriate arguments.


