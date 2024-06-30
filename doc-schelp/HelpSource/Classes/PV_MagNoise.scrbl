#lang scribble/manual
@(require (for-label racket))

@title{PV_MagNoise}
 Multiply magnitudes by noise.@section{related}
  Classes/FFT, Classes/IFFT
@section{categories}
  UGens>FFT

@section{description}


Magnitudes are multiplied with noise.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{Examples}
 


@racketblock[

s.boot;

b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
SynthDef("help-magNoise", { arg out=0;
	var in, chain;
	in = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08, 0, 6, 6.2).squared, 0, 100, 800));
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagNoise(chain);
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magNoise2", {  arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagNoise(chain);
	Out.ar(out, 0.2 * IFFT(chain).dup);
}).play(s,[\soundBufnum, b]);
)

b.free;

::

]

