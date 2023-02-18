#lang scribble/manual
@(require (for-label racket))

@title{PV_MagFreeze}
 Freeze magnitudes.@section{categories}
  UGens>FFT

@section{description}


Freezes magnitudes at current levels when  
@racketblock[freeze::  > 0.


]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 freeze

If > 0, then magnitudes are frozen at current levels.


@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");


(
SynthDef("help-magFreeze", { arg out=0;
	var in, chain;
	in = SinOsc.ar(LFNoise1.kr(5.2,250,400));
	chain = FFT(LocalBuf(2048), in);
	// moves in and out of freeze
	chain = PV_MagFreeze(chain, SinOsc.kr(0.2) );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);

)

(
//trig with MouseY
SynthDef("help-magFreeze2", { arg out=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagFreeze(chain, MouseY.kr > 0.5 );
	Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s,[\soundBufnum, b]);
)

b.free

::
]


