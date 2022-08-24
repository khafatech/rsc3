#lang scribble/manual
@(require (for-label racket))

@title{LFDNoise3}
 Dynamic cubic noise@section{related}
  Classes/LFClipNoise, Classes/LFDClipNoise, Classes/LFDNoise0, Classes/LFDNoise1, Classes/LFNoise0, Classes/LFNoise1, Classes/LFNoise2
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Similar to  link::Classes/LFNoise2:: , it generates polynomially
interpolated random values at a rate given by the

@racketblock[freq::  argument, with 3 differences:
]
@section{list}
 
## no time quantization
## fast recovery from low freq values @section{footnote}
 
link::Classes/LFNoise0:: ,  link::Classes/LFNoise1::  and
link::Classes/LFNoise2::  quantize to the nearest integer division
of the samplerate, and they poll the  
@racketblock[freq::
argument only when scheduled; thus they often seem to hang
when freqs get very low.
::
## cubic instead of quadratic interpolation
::

If you don't need very high or very low freqs, or use fixed freqs,
link::Classes/LFNoise2::  is more efficient.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Approximate rate at which to generate random values.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
// try wiggling mouse quickly:
// LFNoise2 overshoots when going from high to low freqs, LFDNoise changes smoothly.

{  SinOsc.ar(LFNoise2.ar(MouseX.kr(0.1, 1000, 1), 200, 500), 0, 0.2)  }.play

{  SinOsc.ar(LFDNoise3.ar(MouseX.kr(0.1, 1000, 1), 200, 500), 0, 0.2)  }.play


// LFNoise quantizes time steps at high freqs, LFDNoise does not:

{ LFNoise2.ar(XLine.kr(2000, 20000, 8), 0.1) }.scope;

{ LFDNoise3.ar(XLine.kr(2000, 20000, 8), 0.1) }.scope;

// use as frequency control
(
{
		SinOsc.ar(
			LFDNoise3.ar(4, 400, 450),
			0, 0.2
		)
}.play;
)
::

]


