#lang scribble/manual
@(require (for-label racket))

@title{Trig}
 Timed trigger.@section{related}
  Classes/Trig1
@section{categories}
   UGens>Triggers


@section{description}


When a nonpositive to positive transition occurs at the input, Trig
outputs the level of the triggering input for the specified duration,
otherwise it outputs zero.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Trigger. Trigger can be any signal. A trigger happens when the
signal changes from non-positive to positive.


@section{argument}
 dur

Duration of the trigger output.


@section{Examples}
 


@racketblock[

{ Trig.ar(Dust.ar(1), 0.2) * FSinOsc.ar(800, 0.5) }.play

{ Trig.ar(Dust.ar(4), 0.1) }.play

::

]


