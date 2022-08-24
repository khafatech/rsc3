#lang scribble/manual
@(require (for-label racket))

@title{PV_CopyPhase}
 Copy magnitudes and phases.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_Add, Classes/PV_MagMul, Classes/PV_Max, Classes/PV_Min, Classes/PV_Mul
@section{categories}
  UGens>FFT

@section{description}


Combines magnitudes of first input and phases of the second input.


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
SynthDef("help-copyPhase", { arg out=0;
	var inA, chainA, inB, chainB, chain;
	inA = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08, 0, 6, 6.2).squared, 0, 100, 800));
	inB = WhiteNoise.ar(0.2);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_CopyPhase(chainA, chainB);
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-copyPhase2", { arg out=0, soundBufnum=2;
	var inA, chainA, inB, chainB, chain;
	inA = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	inB =  SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08, 0, 6, 6.2).squared, 0, 100, 800));
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_CopyPhase(chainA, chainB);
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);

)

::

]


