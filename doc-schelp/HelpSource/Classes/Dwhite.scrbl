#lang scribble/manual
@(require (for-label racket))

@title{Dwhite}
 Demand rate white noise random generator.@section{related}
  Classes/Diwhite, Classes/Demand, Classes/Duty, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}


Dwhite returns numbers in the continuous range between

@racketblock[lo::  and  ]

@racketblock[hi:: .
link::Classes/Diwhite::  returns integer values.


The arguments can be a number or any other UGen.


See link::Classes/Pwhite::,  link::Classes/WhiteNoise::  for structurally related
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
 length

Number of values to create.


@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dwhite(0, 15, inf);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


