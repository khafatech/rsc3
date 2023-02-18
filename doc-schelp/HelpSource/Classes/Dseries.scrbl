#lang scribble/manual
@(require (for-label racket))

@title{Dseries}
 Demand rate arithmetic series UGen.@section{related}
  Classes/Demand, Classes/Dgeom, Classes/Duty, Classes/TDuty
@section{categories}
  UGens>Demand

@section{description}


Demand rate arithmetic series UGen.

See link::Classes/Pseries:: for structurally related equivalent.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 start

Start value. Can be a number or any other UGen.


@section{argument}
 step

Step value. Can be a number or any other UGen.


@section{argument}
 length

Number of values to create.  Can be a number or any other UGen.


@section{Examples}
 


@racketblock[

(
{
	var a, freq, trig;
	a = Dseries(0, 1, 15);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

(
{
	var a, freq, trig;
	a = Dseries(0, 1, inf);
	trig = Dust.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) % 15 * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

::

]


