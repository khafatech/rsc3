#lang scribble/manual
@(require (for-label racket))

@title{BufDur}
 Current duration of soundfile in buffer.@section{related}
  Classes/BufChannels, Classes/BufFrames, Classes/BufRateScale, Classes/BufSampleRate, Classes/BufSamples
@section{categories}
   UGens>Buffer>Info

@section{description}

Get the current duration of soundfile.

@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{returns}
  the current duration.

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

{ BufRd.ar(1, b,  Sweep.ar(Impulse.ar(BufDur.kr(b).reciprocal), BufSampleRate.kr(b))) }.play;

b.free

::

]


