#lang scribble/manual
@(require (for-label racket))

@title{LPZ2}
 Two zero fixed lowpass@section{related}
  Classes/BPZ2, Classes/BRZ2, Classes/HPZ2
@section{categories}
   UGens>Filters>Linear


@section{description}


A special case fixed filter. Implements the formula:

@racketblock[
out(i) = 0.25 * (in(i) + (2 * in(i - 1)) + in(i - 2)).
::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
//Compare:

{ WhiteNoise.ar(0.25) }.play;

{ LPZ2.ar(WhiteNoise.ar(0.25)) }.play;
::

]


