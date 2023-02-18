#lang scribble/manual
@(require (for-label racket))

@title{PV_Max}
 Maximum magnitude.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_Add, Classes/PV_CopyPhase, Classes/PV_MagMul, Classes/PV_Min, Classes/PV_Mul
@section{categories}
  UGens>FFT

@section{description}


Output copies bins with the maximum magnitude of the two inputs.


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
var exBuf;
Dialog.getPaths({ arg paths; //get a second soundfile;
	paths.do({ arg p; exBuf = Buffer.read(s, p);
		SynthDef("help-max", { arg out=0, soundBufnum1=2, soundBufnum2 = 3;
			var inA, chainA, inB, chainB, chain ;
			inA = PlayBuf.ar(1, soundBufnum1, BufRateScale.kr(soundBufnum1), loop: 1);
			inB =  PlayBuf.ar(1, soundBufnum2, BufRateScale.kr(soundBufnum2), loop: 1);
			chainA = FFT(LocalBuf(2048), inA);
			chainB = FFT(LocalBuf(2048), inB);
			chain = PV_Max(chainA, chainB);
			Out.ar(out,  0.1 * IFFT(chain).dup);
		}).play(s, [\soundBufnum1, b.bufnum, \soundBufnum2, exBuf.bufnum]);
	})
},{
	"cancelled".postln;
});
)

b.free;

::

]


