#lang scribble/manual
@(require (for-label racket))

@title{Dser}
 Demand rate sequence generator.@section{related}
  Classes/Demand, Classes/Drand, Classes/Dseq, Classes/Duty, Classes/Dxrand, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}


Demand rate sequence generator.

See link::Classes/Pser:: for structurally related equivalent.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 list

An array of values or other UGens.


@section{argument}
 repeats

Number of values to return.


@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dser([1, 3, 2, 7, 8], 8);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


