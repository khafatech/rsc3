#lang scribble/manual
@(require (for-label racket))

@title{VarSaw}
 Variable duty saw@section{related}
  Classes/Saw, Classes/SyncSaw, Classes/LFSaw
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Sawtooth-triangle oscillator with variable duty.


@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 freq
frequency in Hertz

@section{argument}
 iphase
initial phase offset in cycles ( 0..1 )

@section{argument}
 width
duty cycle from zero to one.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[

play({
	VarSaw.ar(
		LFPulse.kr(3, 0, 0.3, 200, 200),
		0,
		LFTri.kr(1.0).range(0,1), //width
		0.1)
});


play({ VarSaw.ar(LFPulse.kr(3, 0, 0.3, 200, 200), 0, 0.2, 0.1) });

// compare:

play({ LFPulse.ar(LFPulse.kr(3, 0, 0.3, 200, 200), 0, 0.2, 0.1) });

::

]


