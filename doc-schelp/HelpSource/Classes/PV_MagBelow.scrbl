#lang scribble/manual
@(require (for-label racket))

@title{PV_MagBelow}
 Pass bins below a threshold.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_MagAbove, Classes/PV_LocalMax, Classes/PV_MagClip
@section{categories}
  UGens>FFT

@section{description}


Passes only bins whose magnitude is below a threshold.


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

(
b = Buffer.alloc(s,2048,1);
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
)

(
SynthDef("help-magBelow", { arg out=0, bufnum=0;
	var in, chain;
	in = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08, 0, 6, 6.2).squared, 0, 100, 800));
	chain = FFT(bufnum, in);
	chain = PV_MagBelow(chain, 10);
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum]);
)

(
SynthDef("help-magBelow2", { arg out=0, bufnum=0;
	var in, chain;
	in = WhiteNoise.ar(0.2);
	chain = FFT(bufnum, in);
	chain = PV_MagBelow(chain, MouseX.kr(0, 7));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum]);
)

(
SynthDef("help-magBelow3", { arg out=0, bufnum=0, soundBufnum=2;
	var in, chain;
	in = PlayBuf.ar(1, soundBufnum, BufRateScale.kr(soundBufnum), loop: 1);
	chain = FFT(bufnum, in);
	chain = PV_MagBelow(chain, MouseX.kr(0, 310));
	Out.ar(out, 0.5 * IFFT(chain).dup);
}).play(s,[\out, 0, \bufnum, b.bufnum, \soundBufnum, c.bufnum]);
)

::

]


