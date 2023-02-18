#lang scribble/manual
@(require (for-label racket))

@title{PV_Mul}
 Complex multiply.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_Add, Classes/PV_CopyPhase, Classes/PV_MagMul, Classes/PV_Max, Classes/PV_Min
@section{categories}
  UGens>FFT

@section{description}


Complex Multiplication:


@racketblock[

(RealA * RealB) - (ImagA * ImagB),
(ImagA * RealB) + (RealA * ImagB)

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

(
SynthDef("help-mul", { arg out=0;
	var inA, chainA, inB, chainB, chain ;
	inA = SinOsc.ar(500, 0, 0.5);
	inB =  SinOsc.ar(Line.kr(100, 400, 5), 0, 0.5);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_Mul(chainA, chainB);
	Out.ar(out,  0.1 * IFFT(chain).dup);
}).play(s);
s.scope;
)

(
SynthDef("help-mul2", { arg out=0;
	var inA, chainA, inB, chainB, chain ;
	inA = SinOsc.ar(500, 0, 0.5) * Line.kr;
	inB = LFNoise1.ar(20);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_Mul(chainA, chainB);
	Out.ar(out,  0.1 * IFFT(chain).dup);
}).play(s);
s.scope;
)

::

]


