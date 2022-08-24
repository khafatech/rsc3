#lang scribble/manual
@(require (for-label racket))

@title{PV_MagClip}
 Clip bins to a threshold.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_MagAbove, Classes/PV_LocalMax, Classes/PV_MagBelow
@section{categories}
  UGens>FFT

@section{description}


Clips bin magnitudes to a maximum threshold.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 threshold

Magnitude threshold.


@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
	var in, chain;
	in = Mix.arFill(3, { LFSaw.ar(exprand(100, 500), 0, 0.1); });
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagClip(chain, MouseX.kr(0, 15));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magClip2", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagClip(chain, MouseX.kr(0, 50));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)

::

]


