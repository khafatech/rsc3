#lang scribble/manual
@(require (for-label racket))

@title{PV_BrickWall}
 Zero bins.@section{related}
  Classes/FFT, Classes/IFFT
@section{categories}
  UGens>FFT

@section{description}


Clears bins above or below a cutoff point.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 wipe

Can range between -1 and +1.


If

@racketblock[wipe::  == 0 then there is no effect.


If
]

@racketblock[wipe::  > 0 then it acts like a high
pass filter, clearing bins from the bottom up.


If
]

@racketblock[wipe::  < 0 then it acts like a low
pass filter, clearing bins from the top down.


]
@section{Examples}
 


@racketblock[

s.boot;

(
{
	var in, chain;
	in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BrickWall(chain, SinOsc.kr(0.1));
	IFFT(chain);
}.play
)

::

]


