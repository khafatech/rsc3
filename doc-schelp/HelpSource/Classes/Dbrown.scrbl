#lang scribble/manual
@(require (for-label racket))

@title{Dbrown}
 Demand rate brownian movement generator.@section{related}
  Classes/Dibrown, Classes/Demand, Classes/Duty, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}


Dbrown returns numbers in the continuous range between

@racketblock[lo::  and  ]

@racketblock[hi:: ,
link::Classes/Dibrown::  returns integer values.


The arguments can be a number or any other UGen.


See link::Classes/Pbrown::,  link::Classes/BrownNoise::  for structurally related
equivalents.


]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 lo

Minimum value.


@section{argument}
 hi

Maximum value.


@section{argument}
 step

Maximum step for each new value.


@section{argument}
 length

Number of values to create. Use 
@racketblock[inf:: for an infinite number.


]
@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dbrown(0, 15, 1, inf);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


