#lang scribble/manual
@(require (for-label racket))

@title{ToggleFF}
 Toggle flip flop.@section{related}
  Classes/SetResetFF
@section{categories}
   UGens>Triggers


@section{description}


Toggles between 0 and 1 upon receiving a trigger.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 trig

Trigger input.


@section{Examples}
 


@racketblock[

(
play({
	SinOsc.ar((ToggleFF.ar(Dust.ar(XLine.kr(1,1000,60))) * 400) + 800, 0, 0.1)
}))

::

]


