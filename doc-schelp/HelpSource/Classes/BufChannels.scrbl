#lang scribble/manual
@(require (for-label racket))

@title{BufChannels}
 Current number of channels of soundfile in buffer.@section{related}
  Classes/BufDur, Classes/BufFrames, Classes/BufRateScale, Classes/BufSampleRate, Classes/BufSamples
@section{categories}
   UGens>Buffer>Info

@section{description}

Get the current number of channels of soundfile.

@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 bufnum
Buffer index.

@section{returns}
  the current number of channels.

@section{discussion}
 
@section{warning}
 
The  
@racketblock[.ir::  method is not the safest choice.
Since a buffer can be reallocated at any time, using
]

@racketblock[.ir::  will not track the changes. Use ]

@racketblock[.kr:: instead.
::

]


