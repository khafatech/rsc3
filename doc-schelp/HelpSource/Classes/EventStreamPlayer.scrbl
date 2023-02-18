#lang scribble/manual
@(require (for-label racket))

@title{EventStreamPlayer}
 two streams combined by a binary operator@section{related}
  Classes/Event, Classes/Pbind
@section{categories}
  Streams-Patterns-Events

@section{description}


An EventStreamPlayer is used by link::Classes/Event:: based Patterns.

The EventStreamPlayer holds a stream which returns a series of Events, and a protoEvent. At each call to next, it copies the protoEvent, passes that to the stream, and calls strong::play:: on the link::Classes/Event:: returned.

For more on EventStreamPlayer see link::Tutorials/Streams-Patterns-Events4::

EventStreamPlayer uses the same control methods and status notifications as link::Classes/Task::.

@section{ClassMethods}
 

@section{method}
 new
@section{note}
 
You do not explicitly create an EventStreamPlayers, they are created for you when you call link::Classes/Pattern#-play::.
::

@section{InstanceMethods}
 

@section{private}
 prStop, prNext

@section{method}
 play

@section{argument}
 argClock
(optional) Override the clock assigned in Task.new.

@section{argument}
 doReset
If true, the task will start over from the beginning. Default is false (task will resume where it was when it was last stopped).

@section{argument}
 quant
See the link::Classes/Quant:: helpfile.

@section{method}
 start
Restart the task from the beginning.

@section{method}
 resume
Resume the task where it left off.

@section{method}
 pause
Stop playing now.

@section{method}
 stop
Stop playing now. (Pause and stop have the same implementation.)

@section{method}
 reset
Set the stream to restart from the beginning the next time it's played.


