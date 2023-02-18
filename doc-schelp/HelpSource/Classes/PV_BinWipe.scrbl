#lang scribble/manual
@(require (for-label racket))

@title{PV_BinWipe}
 Combine low and high bins from two inputs.@section{categories}
  UGens>FFT

@section{description}


Copies low bins from one input and the high bins of the other.


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
 wipe

Can range between -1 and +1.


If

@racketblock[wipe::  == 0, then the output is the same
as
]

@racketblock[bufferA:: .


If
]

@racketblock[wipe::  > 0, then it begins replacing
with bins from
]

@racketblock[bufferB::  from the bottom up.


If
]

@racketblock[wipe::  < 0, then it begins replacing
with bins from
]

@racketblock[bufferB::  from the top down.


]
@section{Examples}
 


@racketblock[
s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
SynthDef("help-binWipe", { arg out=0;
	var inA, chainA, inB, chainB, chain;
	inA = WhiteNoise.ar(0.2);
	inB = LFSaw.ar(100, 0, 0.2);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_BinWipe(chainA, chainB, MouseX.kr(-1, 1));
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)

(
SynthDef("help-binWipe2", { arg out=0, soundBufnum=2;
	var inA, chainA, inB, chainB, chain;
	inA = WhiteNoise.ar(0.2);
	inB = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chainA = FFT(LocalBuf(2048), inA);
	chainB = FFT(LocalBuf(2048), inB);
	chain = PV_BinWipe(chainA, chainB, MouseX.kr(-1, 1));
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s, [\soundBufnum, b]);
)
::
]


