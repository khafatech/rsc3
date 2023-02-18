#lang scribble/manual
@(require (for-label racket))

@title{Gate}
 Gate or hold.@section{related}
  Classes/Latch
@section{categories}
   UGens>Triggers


@section{description}


Allows input signal value to pass when gate is positive, otherwise holds last value.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 trig

Gate - can be any signal. The output is held fixed when this is non-positive.


@section{Examples}
 


@racketblock[

s.boot;
// Control rate so as not to whack your speakers with DC
{ Gate.kr(WhiteNoise.kr(1, 0), LFPulse.kr(1.333, 0.5))}.scope(zoom: 20);

::

]


