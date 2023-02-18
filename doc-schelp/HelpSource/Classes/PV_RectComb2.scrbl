#lang scribble/manual
@(require (for-label racket))

@title{PV_RectComb2}
 Make gaps in spectrum.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_RectComb
@section{categories}
  UGens>FFT

@section{description}


Alternates blocks of bins between the two inputs.


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
 numTeeth

Number of teeth in the comb.


@section{argument}
 phase

Starting phase of comb pulse.


@section{argument}
 width

Pulse width of the comb.


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
			chain = PV_RectComb2(chainA, chainB,  MouseX.kr(0, 32), MouseY.kr, 0.3);
			Out.ar(out,  0.5 * IFFT(chain).dup);
		}).play(s, [\soundBufnum1, b, \soundBufnum2, exBuf]);
	})
},{
	"cancelled".postln;
});
)

::

]


