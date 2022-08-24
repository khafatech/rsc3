#lang scribble/manual
@(require (for-label racket))

@title{Crackle}
 Chaotic noise function.@section{related}
  Classes/LatoocarfianN, Classes/Logistic
@section{categories}
   UGens>Generators>Stochastic


@section{description}


A noise generator based on a chaotic function.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 chaosParam

A parameter of the chaotic function with useful values from
just below 1.0 to just above 2.0. Towards 2.0 the sound crackles.


@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ Crackle.ar(1.95, 0.5) }.play;

// modulate chaos parameter
{ Crackle.ar(Line.kr(1.0, 2.0, 3), 0.5, 0.5) }.play;
::

]


