#lang scribble/manual
@(require (for-label racket))

@title{BufCombC}
 Buffer based comb delay line with cubic interpolation.@section{related}
  Classes/BufCombL, Classes/BufCombN, Classes/CombC
@section{categories}
   UGens>Delays>Buffer


@section{description}


Comb delay line with cubic interpolation which uses a buffer for its
internal memory. See also  link::Classes/BufCombN::  which uses no
interpolation, and  link::Classes/BufCombL::  which uses linear
interpolation. Cubic interpolation is more computationally expensive
than linear, but more accurate.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 buf
Buffer number.

@section{argument}
 in
The input signal.

@section{argument}
 delaytime
Delay time in seconds.

@section{argument}
 decaytime
Time for the echoes to decay by 60 decibels. If this time is negative then the feedback coefficient will be negative, thus emphasizing only odd harmonics at an octave lower.

@section{discussion}
 
@section{Warning}
  For reasons of efficiency, the effective buffer size is limited to the previous power of two. So, if 44100 samples are allocated, the maximum delay would be 32768 samples.
::

@section{Examples}
 


@racketblock[

// These examples compare the variants, so that you can hear the difference in interpolation

// allocate buffer
b = Buffer.alloc(s,44100,1);

// Comb used as a resonator. The resonant fundamental is equal to
// reciprocal of the delay time.
{ BufCombN.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ BufCombL.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ BufCombC.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

// with negative feedback:
{ BufCombN.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ BufCombL.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ BufCombC.ar(b.bufnum, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

// used as an echo.
{ BufCombC.ar(b.bufnum, Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 3) }.play;

::

]

