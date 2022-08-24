#lang scribble/manual
@(require (for-label racket))

@title{BufRateScale}
 Buffer rate scaling in respect to server samplerate.@section{related}
  Classes/BufChannels, Classes/BufDur, Classes/BufFrames, Classes/BufSampleRate, Classes/BufSamples
@section{categories}
   UGens>Buffer>Info


@section{description}

Returns a ratio by which the playback of a soundfile is to be scaled.

@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{Returns}
  a ratio by which the playback of a soundfile is to be scaled.

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

(
x = { arg rate=1;
	BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b) * rate, 0, BufFrames.kr(b)))
}.play;
)
::

]


