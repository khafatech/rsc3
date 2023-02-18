#lang scribble/manual
@(require (for-label racket))

@title{Dstutter}
 Demand rate input replicator@section{categories}
  UGens>Demand

@section{classmethods}
 
@section{method}
  new
@section{argument}
  n
number of repeats (can be a demand ugen)
@section{argument}
  in
input ugen
@section{discussion}
 
structurally related: link::Classes/Pstutter::

@section{examples}
 

@racketblock[
(
{
	var freq, trig;
	var in = Dseq([1, 2, 3], inf);
	var rep = Dstutter(Diwhite(2, 8, inf), in);
	trig = Impulse.kr(MouseX.kr(1, 40, 1));
	freq = Demand.kr(trig, 0, rep).poll(trig) * 30 + 340;
	SinOsc.ar(freq) * 0.1

}.play;
)
::

]


