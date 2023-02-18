#lang scribble/manual
@(require (for-label racket))

@title{PV_Diffuser}
 Random phase shifting.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_PhaseShift, Classes/PV_PhaseShift90, Classes/PV_PhaseShift270
@section{categories}
  UGens>FFT

@section{description}


Adds a different constant random phase shift to each bin. When triggered, it selects a new set of random phases.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 trig

A trigger, that selects a new set of random values.


@section{Examples}
 


@racketblock[



(
// trig with MouseY crossing center of screen
{
	var in, chain;
	in = Mix.ar(SinOsc.ar(200 * (1..10), 0, Array.fill(10, { rrand(0.1, 0.2) }) ));
	chain = FFT(LocalBuf(2048), in);
	chain = PV_Diffuser(chain, MouseY.kr > 0.5 );
	0.5 * IFFT(chain).dup;
}.play
);

(
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

// trig with MouseY crossing center of screen
{
	var in, chain;
	in = PlayBuf.ar(1, b, BufRateScale.kr(b), loop: 1);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_Diffuser(chain, MouseY.kr > 0.5 );
	0.5 * IFFT(chain).dup;
}.play
);

::

]


