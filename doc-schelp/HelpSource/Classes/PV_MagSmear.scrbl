#lang scribble/manual
@(require (for-label racket))

@title{PV_MagSmear}
 Average magnitudes across bins.@section{categories}
  UGens>FFT

@section{description}


Average a bin's magnitude with its neighbors.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 bins

Number of bins to average on each side of bin. As this number
rises, so will CPU usage.


@section{Examples}
 


@racketblock[

s.boot;

(
b = Buffer.alloc(s,2048,1);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
)

(
SynthDef("help-magSmear", { arg out=0, bufnum=0;
	var in, chain;
	in = LFSaw.ar(500, 0, Decay2.ar(Impulse.ar(2,0,0.2), 0.01, 2));
	chain = FFT(bufnum, in);
	chain = PV_MagSmear(chain, MouseX.kr(0, 100));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum]);
)

(
SynthDef("help-magSmear2", { arg out=0, bufnum=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(bufnum, in);
	chain = PV_MagSmear(chain, MouseX.kr(0, 100));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum, \soundBufnum, c.bufnum]);
)

::
]


