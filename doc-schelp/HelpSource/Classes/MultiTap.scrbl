#lang scribble/manual
@(require (for-label racket))

@title{MultiTap}
 Multiple tap delay.@section{related}
  Classes/Tap
@section{categories}
   UGens>Buffer, UGens>Delays>Buffer


@section{description}


This is a wrapper which creates a multiple tap delay line using
link::Classes/RecordBuf::  and  link::Classes/PlayBuf:: .

@section{note}
  
@racketblock[RecordBuf.ar:: and ]

@racketblock[PlayBuf.ar:: operate block by block. If a delay time is greater than the buffer
size minus the server's block size, the write and read heads might interfere in unintended ways. Use a slightly larger
buffer if this happens. ::

]
@section{classmethods}
 

@section{method}
 ar

@section{argument}
 timesArray
A Ref to an Array of delay times in seconds.

@section{argument}
 levelsArray
A Ref to an Array of amplitudes.

@section{argument}
 in
The input signal.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{argument}
 bufnum

The number of the buffer to use for the delay. This must be at
least as long as the longest tap time.


@section{Examples}
 


@racketblock[

s.boot;
b = Buffer.alloc(s, s.sampleRate);
(
{
	MultiTap.ar(`[0.1, 0.2, 0.3, 0.4], `[0.1, 0.2, 0.4, 0.8],
		Decay.ar(Dust.ar(2), 0.1, PinkNoise.ar), bufnum: b.bufnum)
}.play
)

::

]


