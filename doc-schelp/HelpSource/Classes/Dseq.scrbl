#lang scribble/manual
@(require (for-label racket))

@title{Dseq}
 Demand rate sequence generator.@section{related}
  Classes/Demand, Classes/Drand, Classes/Dser, Classes/Duty, Classes/Dxrand, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}


Demand rate sequence generator.


See link::Classes/Pseq:: for structurally related equivalent.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 list

An array of values or other UGens.


@section{argument}
 repeats

Number of repeats.


@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dseq([1, 3, 2, 7, 8], 3);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

// audio rate
(
{
	var a, freq, trig;
	a = Dseq({ 10.rand } ! 32, inf);
	trig = Impulse.ar(MouseX.kr(1, 10000, 1));
	freq = Demand.ar(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


