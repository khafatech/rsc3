#lang scribble/manual
@(require (for-label racket))

@title{Drand}
 Demand rate random sequence generator.@section{related}
  Classes/Demand, Classes/Dseq, Classes/Dser, Classes/Duty, Classes/Dxrand, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}

link::Classes/Dxrand::  never plays the same value twice, whereas Drand
chooses any value in the list.


See link::Classes/Prand:: for structurally related equivalent.


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
	a = Drand([1, 3, 2, 7, 8], inf);
	trig = Impulse.kr(MouseX.kr(1, 400, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]

