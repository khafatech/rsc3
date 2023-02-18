#lang scribble/manual
@(require (for-label racket))

@title{LFCub}
 A sine like shape made of two cubic pieces@section{related}
  Classes/LFPar, Classes/LFPulse, Classes/LFSaw, Classes/LFTri
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A sine like shape made of two cubic pieces. Smoother than
link::Classes/LFPar:: .


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 iphase

Initial phase offset.


@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ LFCub.ar(LFCub.kr(LFCub.kr(0.2,0,8,10),0, 400,800),0,0.1) }.play
{ LFCub.ar(LFCub.kr(0.2, 0, 400,800),0,0.1) }.play
{ LFCub.ar(800,0,0.1) }.play
{ LFCub.ar(XLine.kr(100,8000,30),0,0.1) }.play

//compare:

{ LFPar.ar(LFPar.kr(LFPar.kr(0.2,0,8,10),0, 400,800),0,0.1) }.play
{ LFPar.ar(LFPar.kr(0.2, 0, 400,800),0,0.1) }.play
{ LFPar.ar(800,0,0.1) }.play
{ LFPar.ar(XLine.kr(100,8000,30),0,0.1) }.play


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


