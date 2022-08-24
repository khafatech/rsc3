#lang scribble/manual
@(require (for-label racket))

@title{TwoZero}
 Two zero filter.@section{related}
  Classes/TwoPole
@section{categories}
   UGens>Filters>Linear


@section{description}


A two zero filter.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Frequency of zero angle.


@section{argument}
 radius

Radius of zero.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ TwoZero.ar(WhiteNoise.ar(0.125), XLine.kr(20,20000,8), 1) }.play

::

]


