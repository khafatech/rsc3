#lang scribble/manual
@(require (for-label racket))

@title{XLine}
 Exponential line generator.@section{related}
  Classes/Line
@section{categories}
   UGens>Envelopes


@section{description}


Generates an exponential curve from the start value to the end value.
Both the start and end values must be non-zero and have the same sign.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 start

Starting value.


@section{argument}
 end

Ending value.


@section{argument}
 dur

Duration in seconds.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{argument}
 doneAction

A doneAction to be evaluated when the line is completed. See

link::Classes/Done::  for more detail.


@section{Examples}
 


@racketblock[

play({ SinOsc.ar(XLine.kr(200,17000,10),0,0.1) });

::

]


