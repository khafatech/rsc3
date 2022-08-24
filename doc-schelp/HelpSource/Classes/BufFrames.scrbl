#lang scribble/manual
@(require (for-label racket))

@title{BufFrames}
 Current number of frames allocated in the buffer.@section{related}
  Classes/BufChannels, Classes/BufDur, Classes/BufRateScale, Classes/BufSampleRate, Classes/BufSamples
@section{categories}
   UGens>Buffer>Info

@section{description}

Get the current number of allocated frames.

@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{returns}
  the current number of allocated frames.

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

// indexing with a phasor
{ BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b), 0, BufFrames.kr(b))) }.play;

// indexing by hand
{ BufRd.ar(1, b, K2A.ar(MouseX.kr(0, BufFrames.kr(b)))) }.play;

b.free

::

]


