#lang scribble/manual
@(require (for-label racket))

@title{OnePole}
 One pole filter.@section{related}
  Classes/OneZero
@section{categories}
   UGens>Filters>Linear


@section{description}


A one pole filter. Implements the formula:


@racketblock[

out(i) = ((1 - abs(coef)) * in(i)) + (coef * out(i-1)).

::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 coef

Feedback coefficient. Should be between -1 and +1


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ OnePole.ar(WhiteNoise.ar(0.5), 0.95) }.play

{ OnePole.ar(WhiteNoise.ar(0.5), -0.95) }.play

{ OnePole.ar(WhiteNoise.ar(0.5), Line.kr(-0.99, 0.99, 10)) }.play

::

]


