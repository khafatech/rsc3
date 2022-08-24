#lang scribble/manual
@(require (for-label racket))

@title{Dwrand}
 Demand rate weighted random sequence generator@section{categories}
  UGens>Demand, UGens>Random
@section{related}
  Classes/Demand

@section{classmethods}
 
@section{method}
  new
@section{argument}
  list
array of values or other ugens
@section{argument}
  weights
array of values (should sum up to 1.0)
@section{argument}
  repeats
number of values to return
@section{discussion}
 
structurally related: link::Classes/Pwrand::, link::Classes/TWindex::, link::Classes/TWChoose::

@section{examples}
 

@racketblock[
(
{
	var a, freq, trig;
	a = Dwrand([0, 1, 2, 7], [0.4, 0.4, 0.1, 0.1], inf).dpoll;
	trig = Impulse.kr(MouseX.kr(1, 400, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)
::

]


