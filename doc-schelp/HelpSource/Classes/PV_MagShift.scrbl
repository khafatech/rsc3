#lang scribble/manual
@(require (for-label racket))

@title{PV_MagShift}
 shift and stretch magnitude bin position.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_BinShift
@section{categories}
  UGens>FFT

@section{description}


Shift and stretch the positions of only the magnitude of the bins. Can be used as a very crude frequency shifter/scaler.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 stretch

Scale bin location by factor.


@section{argument}
 shift

Add an offset to bin position.


@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
SynthDef("help-magStretch", { arg out=0, bufnum=0;
	var in, chain;
	in = LFSaw.ar(200, 0, 0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagShift(chain, MouseX.kr(0.25, 4, \exponential) );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magStretch2", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagShift(chain, MouseX.kr(0.25, 4, \exponential) );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)

(
SynthDef("help-magShift", { arg out=0;
	var in, chain;
	in = LFSaw.ar(200, 0, 0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagShift(chain, 1, MouseX.kr(-128, 128) );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magShift2", {  arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagShift(chain, 1, MouseX.kr(-128, 128) );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)

::

]


