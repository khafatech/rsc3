#lang scribble/manual
@(require (for-label racket))

@title{BufSampleRate}
 Buffer sample rate.@section{related}
  Classes/BufChannels, Classes/BufDur, Classes/BufFrames, Classes/BufRateScale, Classes/BufSamples
@section{categories}
   UGens>Buffer>Info


@section{description}

Returns the buffer's current sample rate.

@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{returns}
 
the buffer's current sample rate.

@section{discussion}
 
@section{warning}
 
The  
@racketblock[.ir::  method is not the safest choice.
Since a buffer can be reallocated at any time, using
]

@racketblock[.ir::  will not track the changes.
::

]
@section{Examples}
 


@racketblock[
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

// compares a 1102.5 Hz sine tone (11025 * 0.1, left) with a 1100 Hz tone (right)
// the apollo sample has a sample rate of 11.025 kHz
(
{
	var freq;
	freq = [ BufSampleRate.kr(b) * 0.1, 1100];
	SinOsc.ar(freq, 0, 0.1)
}.play;
)

b.free;
::

]


