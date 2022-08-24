#lang scribble/manual
@(require (for-label racket))

@title{Line}
 Line generator.@section{related}
  Classes/XLine
@section{categories}
   UGens>Envelopes


@section{description}


Generates a line from the start value to the end value.


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

A doneAction to be evaluated when the Line is completed. See

link::Classes/Done::  for more detail.


@section{Examples}
 


@racketblock[

// XLine is usually better than Line for frequency
play({ SinOsc.ar(Line.kr(200,17000,10),0,0.1) });

::

]


