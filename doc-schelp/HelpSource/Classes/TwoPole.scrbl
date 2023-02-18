#lang scribble/manual
@(require (for-label racket))

@title{TwoPole}
 Two pole filter.@section{related}
  Classes/TwoZero
@section{categories}
   UGens>Filters>Linear


@section{description}


A two pole filter. This provides lower level access to setting of pole
location. For general purposes  link::Classes/Resonz::  is better.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Frequency of pole angle.


@section{argument}
 radius

Radius of pole. Should be between 0 and +1.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ TwoPole.ar(WhiteNoise.ar(0.005), 2000, 0.95) }.play

{ TwoPole.ar(WhiteNoise.ar(0.005), XLine.kr(800,8000,8), 0.95) }.play

{ TwoPole.ar(WhiteNoise.ar(0.005), MouseX.kr(800,8000,1), 0.95) }.play

::

]


