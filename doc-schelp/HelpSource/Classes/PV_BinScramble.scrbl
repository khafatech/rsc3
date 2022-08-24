#lang scribble/manual
@(require (for-label racket))

@title{PV_BinScramble}
 Scramble bins.@section{categories}
  UGens>FFT

@section{description}


Randomizes the order of the bins. The trigger will select a new random ordering.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 wipe

Scrambles more bins as wipe moves from 0 to 1.


@section{argument}
 width

A value from zero to one, indicating the maximum randomized
distance of a bin from its original location in the spectrum.


@section{argument}
 trig

A trigger, that selects a new random ordering.


@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
//trig with MouseY
SynthDef("help-binScramble", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BinScramble(chain, MouseX.kr , 0.1, MouseY.kr > 0.5 );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)

::
]


