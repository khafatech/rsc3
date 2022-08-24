#lang scribble/manual
@(require (for-label racket))

@title{PV_PhaseShift270}
 Shift phase by 270 degrees.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_PhaseShift, Classes/PV_PhaseShift90, Classes/PV_Diffuser
@section{categories}
  UGens>FFT

@section{description}


Shift phase of all bins by 270 degrees.


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
{  arg out=0;
	var in, fft, fft2, shifted;
	in = SinOsc.ar(500, 0, 0.1);
	fft = FFT(LocalBuf(2048), in);
	fft2 = FFT(LocalBuf(2048), in);
	shifted = PV_PhaseShift270(fft);
	Out.ar(0,  [IFFT(fft2), IFFT(shifted)]);
}.scope
)

::

]


