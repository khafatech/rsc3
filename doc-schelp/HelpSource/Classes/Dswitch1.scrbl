#lang scribble/manual
@(require (for-label racket))

@title{Dswitch1}
 Demand rate generator for switching between inputs.@section{related}
  Classes/Demand, Classes/Duty, Classes/TDuty, Classes/Dswitch
@section{categories}
  UGens>Demand

@section{description}


Demand rate generator for switching between inputs.

See link::Classes/Pswitch1:: for structurally related equivalent.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 list

Array of values or other UGens.


@section{argument}
 index

Which of the inputs to return.


@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dswitch1([1, 3, MouseY.kr(1, 15), 2, Dwhite(0, 3, 2)], MouseX.kr(0, 4));
	trig = Impulse.kr(3);
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

(
{
	var a, freq, trig;
	a = Dswitch1({ |i| Dseq((0..i*3), inf) } ! 5, MouseX.kr(0, 4));
	trig = Impulse.kr(6);
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


