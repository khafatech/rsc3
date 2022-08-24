#lang scribble/manual
@(require (for-label racket))

@title{PV_Conj}
 Complex conjugate@section{categories}
  UGens>FFT

@section{description}

Converts the FFT frames to their complex conjugate (i.e. reverses the sign of their imaginary part). This is not usually a useful audio effect in itself, but may be a component of other analysis or transformation processes...

@section{classmethods}
 
@section{method}
  new
@section{argument}
  buffer
FFT chain.

@section{examples}
 

@racketblock[
s.boot;

(
b = Buffer.alloc(s,2048,1);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
d = Buffer.alloc(s,2048,1);
)

(
SynthDef(\help_pvconj, {  arg out=0, bufnum=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(bufnum, in);
	chain = PV_Conj(chain);
	// Original is left, conj is right
	Out.ar(out, 0.3 * [in, IFFT(chain)]);
}).play(s,[\out, 0, \bufnum, b, \soundBufnum, c]);
)

(
SynthDef(\help_pvconj2, {  arg out=0, bufnum=0, soundBufnum=2;
	var in, chainA, chainB;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chainA = FFT(bufnum, in);
	chainB = PV_Copy(chainA, d);
	chainB = PV_Conj(chainB);
	// Now we have the original and conjugate, what happens if we add them?
	Out.ar(out, 0.3 * (IFFT(PV_Add(chainA, chainB)).dup));
}).play(s,[\out, 0, \bufnum, b, \soundBufnum, c]);
)
::

]


