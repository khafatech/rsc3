#lang scribble/manual
@(require (for-label racket))

@title{PV_Add}
 Complex addition.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_CopyPhase, Classes/PV_MagMul, Classes/PV_Max, Classes/PV_Min, Classes/PV_Mul
@section{categories}
  UGens>FFT

@section{description}


Complex addition:


@racketblock[

RealA + RealB, ImagA + ImagB

::


]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 bufferA
FFT buffer A.

@section{argument}
 bufferB
FFT buffer B.

@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
SynthDef("help-add", { arg out=0, soundBufnum;
	var inA, chainA, inB, chainB, chain ;
	inA = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	inB =  PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum) * 0.5, loop: 1);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_Add(chainA, chainB);
	Out.ar(out,  0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b.bufnum]);
)

b.free

::
]


