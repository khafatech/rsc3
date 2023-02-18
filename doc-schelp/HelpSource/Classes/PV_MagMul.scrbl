#lang scribble/manual
@(require (for-label racket))

@title{PV_MagMul}
 Multiply magnitudes.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_Add, Classes/PV_CopyPhase, Classes/PV_Max, Classes/PV_Min, Classes/PV_Mul
@section{categories}
  UGens>FFT

@section{description}


Multiplies magnitudes of two inputs and keeps the phases of the first input.


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
SynthDef("help-magMul", { arg out=0;
	var inA, chainA, inB, chainB, chain;
	inA = WhiteNoise.ar(0.2);
	inB = LFSaw.ar(100, 0, 0.2);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_MagMul(chainA, chainB);
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magMul2", { arg out=0, soundBufnum=2;
	var inA, chainA, inB, chainB, chain;
	inA = LFSaw.ar([100, 150], 0, 0.2);
	inB = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_MagMul(chainA, chainB);
	Out.ar(out,  0.03 * IFFT(chain));
}).play(s, [\soundBufnum, b.bufnum]);
)

b.free

::

]


