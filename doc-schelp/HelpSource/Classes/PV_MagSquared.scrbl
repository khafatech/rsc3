#lang scribble/manual
@(require (for-label racket))

@title{PV_MagSquared}
 Square magnitudes.@section{related}
  Classes/FFT, Classes/IFFT
@section{categories}
  UGens>FFT

@section{description}


Squares the magnitudes and renormalizes to previous peak. This makes weak
bins weaker.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{Examples}
 


@racketblock[

s.boot;

(
b = Buffer.alloc(s,2048,1);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
)

(
SynthDef("help-magSquared", {  arg out=0, bufnum=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(bufnum, in);
	chain = PV_MagSquared(chain);
	Out.ar(out, 0.003 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum, \soundBufnum, c.bufnum]);
)

::

]


