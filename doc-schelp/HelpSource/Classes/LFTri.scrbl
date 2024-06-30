#lang scribble/manual
@(require (for-label racket))

@title{LFTri}
 Triangle oscillator@section{related}
  Classes/LFCub, Classes/LFPar, Classes/LFPulse, Classes/LFSaw
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A non-band-limited triangle oscillator. Output ranges from -1 to +1.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 iphase

Initial phase offset. For efficiency reasons this is a value
ranging from 0 to 4.


@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
// plot:
{ LFTri.ar(40) }.plot(0.1);
{ LFTri.ar(XLine.kr(1, 800, 0.1)) }.plot(0.1);

{ LFTri.ar(500, 0, 0.1) }.play

// used as both Oscillator and LFO:
{ LFTri.ar(LFTri.kr(4, 0, 200, 400), 0, 0.1) }.play

// phase. compare
{ LFTri.ar(LFTri.kr(0.1, iphase: 0) * 200 + 400, 0, 0.1) }.play
{ LFTri.ar(LFTri.kr(0.1, iphase: 1) * 200 + 400, 0, 0.1) }.play
{ LFTri.ar(LFTri.kr(0.1, iphase: 2) * 200 + 400, 0, 0.1) }.play
{ LFTri.ar(LFTri.kr(0.1, iphase: 3) * 200 + 400, 0, 0.1) }.play

// together
{ LFTri.ar(LFTri.kr(0.05, iphase: (0..3)) * 200 + 400, 0, 0.1).sum }.play
::

]

