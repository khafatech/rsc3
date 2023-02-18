#lang scribble/manual
@(require (for-label racket))

@title{BPZ2}
 Two zero fixed midpass.@section{related}
  Classes/BRZ2, Classes/HPZ2, Classes/LPZ2
@section{categories}
   UGens>Filters>Linear


@section{description}


A special case fixed filter. Implements the formula:


@racketblock[
out(i) = 0.5 * (in(i) - in(i - 2)).
::

This filter cuts out 0 Hz and the Nyquist frequency.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.

@section{Examples}
 


@racketblock[
//Compare:

{ WhiteNoise.ar(0.25) }.play;

{ BPZ2.ar(WhiteNoise.ar(0.25)) }.play;
::

]


