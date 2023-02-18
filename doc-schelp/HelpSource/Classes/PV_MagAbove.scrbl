#lang scribble/manual
@(require (for-label racket))

@title{PV_MagAbove}
 Pass bins above a threshold.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_LocalMax, Classes/PV_MagBelow, Classes/PV_MagClip
@section{categories}
  UGens>FFT

@section{description}


Passes only bins whose magnitude is above a threshold.


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
SynthDef("help-magAbove", { arg out=0;
	var in, chain;
	in = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08, 0, 6, 6.2).squared, 0, 100, 800));
	//in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagAbove(chain, 310);
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magAbove2", { arg out=0;
	var in, chain;
	in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagAbove(chain, MouseX.kr(0, 10));
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-magAbove3", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagAbove(chain, MouseX.kr(0, 310));
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)

::
]


