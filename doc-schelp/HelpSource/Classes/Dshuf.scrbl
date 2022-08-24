#lang scribble/manual
@(require (for-label racket))

@title{Dshuf}
 Demand rate random sequence generator@section{categories}
  UGens>Demand, UGens>Random

@section{classmethods}
 
@section{method}
  new
@section{argument}
  list
array of values or other ugens
@section{argument}
  repeats
number of repeats
@section{discussion}
 
structurally related: link::Classes/Pshuf::

@section{examples}
 

@racketblock[
(
{
	var a, freq, trig;
	a = Dseq([Dshuf([1, 3, 2, 7, 8.5], 3)], inf);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

// audio rate
(
{
	var a, freq, trig;
	a = Dseq([Dshuf({ 10.rand } ! 81, 5)], inf).poll;
	trig = Impulse.ar(MouseX.kr(1, 10000, 1));
	freq = Demand.ar(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)

// embedded structures
(
{
	var a, freq, trig;
	a = Dseq([Dshuf([Drand([1, 2, 3], 1), 3, Drand([20, 23, 56], 1), 7, 8.5], 8)], inf);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, a) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)
::

]


