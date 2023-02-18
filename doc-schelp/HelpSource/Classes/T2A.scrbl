#lang scribble/manual
@(require (for-label racket))

@title{T2A}
 Control rate trigger to audio rate trigger converter@section{categories}
  UGens>Conversion, UGens>Triggers
@section{related}
  Classes/T2K, Classes/K2A, Classes/A2K

@section{description}

Converts control rate trigger into audio rate trigger (maximally one per control period).

@section{classmethods}
 
@section{method}
  ar

@section{argument}
  in
input signal.
@section{argument}
  offset
sample offset within control period.

@section{examples}
 

@racketblock[
// example
(
{
	var trig = Impulse.kr(MouseX.kr(1, 100, 1));
	Ringz.ar(T2A.ar(trig), 800, 0.01) * 0.4
}.play;
)

// compare with K2A
(
{
	var trig = Impulse.kr(200);
	[T2A.ar(trig), K2A.ar(trig)].lag(0.001)
}.plot2(10/200);
)

// removing jitter by randomising offset
(
{
	var trig = Impulse.kr(MouseX.kr(1, 100, 1));
	Ringz.ar(T2A.ar(trig, WhiteNoise.kr.range(0, s.options.blockSize - 1)), 800, 0.01) * 0.4
}.play;
)
::
]


