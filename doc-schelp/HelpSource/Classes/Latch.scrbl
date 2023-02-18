#lang scribble/manual
@(require (for-label racket))

@title{Latch}
 Sample and hold@section{related}
  Classes/Gate
@section{categories}
   UGens>Triggers

@section{description}

Holds input signal value when triggered. Latch will output 0 until it receives its first trigger.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 trig

Trigger. Trigger can be any signal. A trigger happens when the
signal changes from non-positive to positive.


@section{Examples}
 


@racketblock[
{ Blip.ar(Latch.ar(WhiteNoise.ar, Impulse.ar(9)) * 400 + 500, 4, 0.2) }.play;
::

The above is just meant as example. LFNoise0 is a faster way to generate random steps:
]

@racketblock[
{ Blip.ar(LFNoise0.kr(9, 400 ,500), 4, 0.2) }.play;
::

]


