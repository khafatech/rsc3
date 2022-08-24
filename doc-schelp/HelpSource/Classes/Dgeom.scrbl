#lang scribble/manual
@(require (for-label racket))

@title{Dgeom}
 Demand rate geometric series UGen.@section{related}
  Classes/Demand, Classes/Dseries, Classes/Duty, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}

Demand rate geometric series UGen.

See link::Classes/Pgeom:: for structurally related equivalent.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 start
Start value. Can be a number or any other UGen.


@section{argument}
 grow
Value by which to grow (x = x[-1] * grow).  Can be a number or
any other UGen.

@section{argument}
 length
Number of values to create.

@section{discussion}
 
The arguments can be a number or any other ugen

@section{Examples}
 


@racketblock[

// example

(
{
	var a, freq, trig;
	a = Dgeom(1, 1.2, 15);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

(
{
	var a, freq, trig;
	a = Dgeom(1, 1.2, inf);
	trig = Dust.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


