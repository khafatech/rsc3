#lang scribble/manual
@(require (for-label racket))

@title{PV_BinShift}
 Shift and stretch bin position.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_MagShift
@section{categories}
  UGens>FFT

@section{description}


Shift and scale the positions of the bins. Can be used as a very crude
frequency shifter/scaler.


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

@section{argument}
 interp

Set to 0 (default) for no interpolation, or 1 to linear-interpolate between bins.


@section{Examples}
 


@racketblock[

s.boot;

(
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
)

(
SynthDef("help-binStretch", { arg out=0;
	var in, chain;
	in = LFSaw.ar(200, 0, 0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BinShift(chain, MouseX.kr(0.25, 4, \exponential) );
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play;
)

(
SynthDef("help-binStretch2", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BinShift(chain, MouseX.kr(0.25, 4, \exponential) );
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \soundBufnum, c.bufnum]);
)

(
SynthDef("help-binShift", { arg out=0, bufnum=0;
	var in, chain;
	in = LFSaw.ar(200, 0, 0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BinShift(chain, 1, MouseX.kr(-128, 128) );
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play;
)

(
SynthDef("help-binShift2", {  arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BinShift(chain, 1, MouseX.kr(-128, 128) );
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \soundBufnum, c.bufnum]);
)

::

]


