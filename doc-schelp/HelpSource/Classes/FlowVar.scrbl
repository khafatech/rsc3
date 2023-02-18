#lang scribble/manual
@(require (for-label racket))

@title{FlowVar}
 A place holder, which when accessed pauses a thread until the place holder has a value@section{categories}
 Scheduling
@section{related}
  Classes/Ref, Classes/Thunk, Classes/Condition, Classes/Routine

@section{description}




@racketblock[
(
a = FlowVar.new;
fork {
	"I am waiting, please enter a value ...".postln;
	a.value.postln;
	"...... ok, done.".postln;
}
)

// later, set the value. Then we can continue ...
a.value = 2;
::


]
@section{CLASSMETHODS}
 

@section{METHOD}
  new
Return a new instance,

@section{ARGUMENT}
  inVal
If a value is given here, the FlowVar will not block execution.


@section{INSTANCEMETHODS}
 


@section{METHOD}
  value
Set the value bound to the FlowVar.

The getter returns the value bound to the FlowVar. If the value is not yet available, hold execution (this requires the method to be called from within a link::Classes/Routine:: or similar thread.).


@section{ARGUMENT}
  inVal
Any object.



