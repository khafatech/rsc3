#lang scribble/manual
@(require (for-label racket))

@title{Pan2}
 Two channel equal power pan.@section{related}
  Classes/Balance2, Classes/LinPan2, Classes/Pan4, Classes/PanAz
@section{categories}
   UGens>Multichannel>Panners


@section{description}


Two channel equal power panner. Pan2 takes the square root of the linear scaling factor going from 1 (left or right) to 0.5.sqrt (~=0.707) in the center, which is about 3dB reduction. With linear panning (LinPan2) the signal is lowered as it approaches center using a straight line from 1 (left or right) to 0.5 (center) for a 6dB reduction in the middle. A problem inherent to linear panning is that the perceived volume of the signal drops in the middle. Pan2 solves this. 


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 pos

Pan position, -1 is left, +1 is right.


@section{argument}
 level

A control rate level input.


@section{Examples}
 

@racketblock[
// hear the difference, LinPan having a slight drop in the middle...
{LinPan2.ar(SinOsc.ar(440), Line.kr(-1,1,5))}.play

// ... whereas Pan2 is more smooth
{Pan2.ar(SinOsc.ar(440), Line.kr(-1,1,5))}.play

// other examples
SynthDef("help-Pan2", { Out.ar(0, Pan2.ar(PinkNoise.ar(0.4), FSinOsc.kr(2), 0.3)) }).play;
::

]


