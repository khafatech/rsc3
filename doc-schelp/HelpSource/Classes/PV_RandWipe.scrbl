#lang scribble/manual
@(require (for-label racket))

@title{PV_RandWipe}
 Crossfade in random bin order.@section{related}
  Classes/PV_RandComb
@section{categories}
  UGens>FFT

@section{description}


Crossfades between two sounds by copying bins in a random order.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 bufferA

FFT buffer A.


@section{argument}
 bufferB

FFT buffer B.


@section{argument}
 wipe

Copies bins from bufferB in a random order as wipe goes from 0
to 1.


@section{argument}
 trig

A trigger, that selects a new random ordering.


@section{Examples}
 


@racketblock[

s.boot;

(
//trig with MouseY
SynthDef("help-randWipe", { arg out=0;
	var inA, chainA, inB, chainB, chain;
	inA = Mix.arFill(6, { LFSaw.ar(exprand(400, 1000), 0, 0.1) });
	inB = Mix.arFill(6, { LFPulse.ar(exprand(80, 400), 0, 0.2, SinOsc.kr(8.0.rand, 0, 0.2).max(0)) });
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_RandWipe(chainA, chainB, MouseX.kr.poll, MouseY.kr.poll > 0.5);
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s);
)

::

]


