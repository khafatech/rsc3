#lang scribble/manual
@(require (for-label racket))

@title{LPZ1}
 Two point average filter@section{related}
  Classes/HPZ1
@section{categories}
   UGens>Filters>Linear


@section{description}


A special case fixed filter. Implements the formula:

@racketblock[
out(i) = 0.5 * (in(i) + in(i-1))
::
which is a two point averager.


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

{ LPZ1.ar(WhiteNoise.ar(0.25)) }.play;
::

]


