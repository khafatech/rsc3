#lang scribble/manual
@(require (for-label racket))

@title{LFPar}
 Parabolic oscillator@section{related}
  Classes/LFCub, Classes/LFPulse, Classes/LFSaw, Classes/LFTri
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A sine-like shape made of two parabolas and the integral of a triangular wave.  It has audible odd harmonics and is non-band-limited.
Output ranges from -1 to +1.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 iphase

Initial phase in cycles ( 0..1 ).


@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
// a plot
{ LFPar.ar(Line.kr(100, 800, 0.1)) }.plot(0.1);

// 440 Hz wave
{ LFPar.ar(440) * 0.1 }.play;

// modulating frequency:
{ LFPar.ar(XLine.kr(100, 2000, 10)) * 0.1 }.play;

// amplitude modulation:
{ LFPar.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.1 }.play;

// used as both Oscillator and LFO:
{ LFPar.ar(LFPar.kr(3, 0.3, 200, 400)) * 0.1 }.play;

// used as phase modulator (behaves like a triangular modulator in FM):
// Compare:
{SinOsc.ar(440, LFPar.ar(1, 2, mul: 8pi))}.play
{SinOsc.ar(440 + LFTri.ar(1, mul: 8pi))}.play


// more examples:

{ LFPar.ar(LFPar.kr(LFPar.kr(0.2,0,8,10), 0, 400,800),0,0.1) }.play
{ LFPar.ar(LFPar.kr(0.2, 0, 400,800),0,0.1) }.play
{ LFPar.ar(800,0,0.1) }.play
{ LFPar.ar(XLine.kr(100,8000,30),0,0.1) }.play


// compare:

{ LFCub.ar(LFCub.kr(LFCub.kr(0.2,0,8,10),0, 400,800),0,0.1) }.play
{ LFCub.ar(LFCub.kr(0.2, 0, 400,800),0,0.1) }.play
{ LFCub.ar(800,0,0.1) }.play
{ LFCub.ar(XLine.kr(100,8000,30),0,0.1) }.play

{ SinOsc.ar(SinOsc.kr(SinOsc.kr(0.2,0,8,10),0, 400,800),0,0.1) }.play
{ SinOsc.ar(SinOsc.kr(0.2, 0, 400,800),0,0.1) }.play
{ SinOsc.ar(800,0,0.1) }.play
{ SinOsc.ar(XLine.kr(100,8000,30),0,0.1) }.play

{ LFTri.ar(LFTri.kr(LFTri.kr(0.2,0,8,10),0, 400,800),0,0.1) }.play
{ LFTri.ar(LFTri.kr(0.2, 0, 400,800),0,0.1) }.play
{ LFTri.ar(800,0,0.1) }.play
{ LFTri.ar(XLine.kr(100,8000,30),0,0.1) }.play
::

]


