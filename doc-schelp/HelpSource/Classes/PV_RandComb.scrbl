#lang scribble/manual
@(require (for-label racket))

@title{PV_RandComb}
 Pass random bins.@section{related}
  Classes/PV_RandWipe
@section{categories}
  UGens>FFT

@section{description}


Randomly clear bins.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 wipe

Clears bins from input in a random order as wipe goes from 0 to
1.


@section{argument}
 trig

A trigger, that selects a new random ordering.


@section{Examples}
 


@racketblock[

s.boot;
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
SynthDef("help-randcomb", { | out=0 |
	var sig, chain;
	sig = WhiteNoise.ar(0.8);
	chain = FFT(LocalBuf(2048), sig);
	chain = PV_RandComb(chain, 0.95, Impulse.kr(0.4));
	Out.ar(out, IFFT(chain).dup);
}).play(s);
)

(
//trig with MouseY
SynthDef("help-randcomb2", { | out=0, soundBufnum=2 |
	var sig, chain;
	sig = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), sig);
	chain = PV_RandComb(chain, MouseY.kr, Impulse.kr(0.4));
	Out.ar(out, IFFT(chain).dup);
}).play(s,[\soundBufnum, c.bufnum]);
)

::

]


