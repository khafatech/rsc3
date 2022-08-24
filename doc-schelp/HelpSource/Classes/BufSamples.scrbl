#lang scribble/manual
@(require (for-label racket))

@title{BufSamples}
 Current number of samples in buffer.@section{related}
  Classes/BufChannels, Classes/BufDur, Classes/BufFrames, Classes/BufRateScale, Classes/BufSampleRate
@section{categories}
   UGens>Buffer>Info

@section{description}


Returns the current number of allocated samples. A sample is not the same as a frame (compare with link::Classes/BufFrames:: ); a frame includes the samples in each channel of the buffer. Only for a mono buffer are samples the same as frames.

@racketblock[
samples = frames * numChannels
::


]
@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{discussion}
 
@section{warning}
 
The  
@racketblock[.ir::  method is not the safest choice.
Since a buffer can be reallocated at any time, using
]

@racketblock[.ir::  will not track the changes.
::

]
@section{examples}
 

@racketblock[
// example; this buffer is mono, so the number of samples matches the number of frames
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

// indexing with a phasor
{ BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b), 0, BufSamples.kr(b))) }.play;

// indexing by hand
{ BufRd.ar(1, b, K2A.ar(MouseX.kr(0, BufSamples.kr(b)))) }.play;

b.free;
::

]


