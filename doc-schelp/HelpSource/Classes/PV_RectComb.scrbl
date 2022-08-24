#lang scribble/manual
@(require (for-label racket))

@title{PV_RectComb}
 Make gaps in spectrum.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_RectComb2
@section{categories}
  UGens>FFT

@section{description}


Makes a series of gaps in a spectrum.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 numTeeth

Number of teeth in the comb.


@section{argument}
 phase

Starting phase of comb pulse.


@section{argument}
 width

Pulse width of the comb.


@section{Examples}
 


@racketblock[


(
{
	var sig, chain;
	sig = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), sig);
	chain = PV_RectComb(chain, 8, LFTri.kr(0.097, 0, 0.4, 0.5),
	LFTri.kr(0.24, 0, -0.5, 0.5));
	IFFT(chain).dup
}.play;
)

(
{
	var sig, chain;
	sig = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), sig);
	chain = PV_RectComb(chain,  MouseX.kr(0, 32), MouseY.kr, 0.2);
	IFFT(chain).dup
}.play;
)

::

]


