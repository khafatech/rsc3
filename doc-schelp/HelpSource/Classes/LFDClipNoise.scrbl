#lang scribble/manual
@(require (for-label racket))

@title{LFDClipNoise}
 Dynamic clipped noise@section{related}
  Classes/LFClipNoise, Classes/LFDNoise0, Classes/LFDNoise1, Classes/LFDNoise3, Classes/LFNoise0, Classes/LFNoise1, Classes/LFNoise2
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Like link::Classes/LFClipNoise::, it generates the values -1 or +1 at a rate given
by the  
@racketblock[freq::  argument, with two differences:
]
@section{list}
 
## no time quantization
## fast recovery from low freq values @section{footnote}
 
link::Classes/LFClipNoise:: , as well as  link::Classes/LFNoise0:: ,
link::Classes/LFNoise1::  and  link::Classes/LFNoise2::  quantize to the
nearest integer division of the samplerate, and they poll the

@racketblock[freq::  argument only when scheduled; thus they often
seem to hang when freqs get very low.
::
::

If you don't need very high or very low freqs, or use fixed freqs,
link::Classes/LFDClipNoise::  is more efficient.


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
// try wiggling the mouse quickly;
// LFNoise frequently seems stuck, LFDNoise changes smoothly.

{ LFClipNoise.ar(MouseX.kr(0.1, 1000, 1), 0.1)  }.play

{ LFDClipNoise.ar(MouseX.kr(0.1, 1000, 1), 0.1)  }.play

// silent for 2 secs before going up in freq

{ LFClipNoise.ar(XLine.kr(0.5, 10000, 3), 0.1) }.scope;

{ LFDClipNoise.ar(XLine.kr(0.5, 10000, 3), 0.1) }.scope;


// LFNoise quantizes time steps at high freqs, LFDNoise does not:

{ LFClipNoise.ar(XLine.kr(1000, 20000, 10), 0.1) }.scope;

{ LFDClipNoise.ar(XLine.kr(1000, 20000, 10), 0.1) }.scope;
::

]

