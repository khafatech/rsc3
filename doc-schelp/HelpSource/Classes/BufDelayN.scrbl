#lang scribble/manual
@(require (for-label racket))

@title{BufDelayN}
 Buffer based simple delay line with no interpolation.@section{related}
  Classes/BufDelayC, Classes/BufDelayL, Classes/DelayN
@section{categories}
   UGens>Delays>Buffer


@section{description}


Simple delay line with no interpolation which uses a buffer for its
internal memory. See also  link::Classes/BufDelayL::  which uses linear
interpolation, and  link::Classes/BufDelayC::  which uses cubic
interpolation. Cubic interpolation is more computationally expensive
than linear, but more accurate.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 buf
Buffer number.

@section{note}
  The buffers provided to any of the BufDelay units must be one channel. If you want to delay a multichannel signal, you must provide as many separate (one-channel) buffers as there are input channels.::

@section{argument}
 in
The input signal.

@section{argument}
 delaytime
Delay time in seconds.

@section{argument}
 mul

@section{argument}
 add

@section{discussion}
 
@section{Warning}
  For reasons of efficiency, the effective buffer size is limited to the previous power of two. So, if 44100 samples are allocated, the maximum delay would be 32768 samples.
::

@section{Examples}
 


@racketblock[

// allocate buffer
b = Buffer.alloc(s,44100,1);

(
// Dust randomly triggers Decay to create an exponential
// decay envelope for the WhiteNoise input source
{
z = Decay.ar(Dust.ar(1,0.5), 0.3, WhiteNoise.ar);
BufDelayN.ar(b.bufnum, z, 0.2, 1, z); // input is mixed with delay via the add input
}.play
)

b.free;


// multichannel

// two channels, two buffers
b = Buffer.allocConsecutive(2, s, 32768, 1);

a = { |bufs = #[0, 1]|
	var sig = SinOsc.ar([440, 880]) * Decay2.kr(Impulse.kr([2, 4]), 0.01, 0.15);
	sig + BufDelayN.ar(bufs, sig, delaytime: 0.125)
}.play(args: [bufs: b]);

a.free;
b.do(_.free);

::

]


