#lang scribble/manual
@(require (for-label racket))

@title{LinPan2}
 Two channel linear pan.@section{related}
  Classes/Balance2, Classes/Pan2, Classes/Pan4, Classes/PanAz
@section{categories}
   UGens>Multichannel>Panners


@section{description}

Two channel linear panner. The signal is lowered as it pans from left (or right) to center using a straight line from 1 (left or right) to 0.5 (center) for a 6dB reduction in the middle. A problem inherent to linear panning is that the perceived volume of the signal drops in the middle. Pan2 solves this by taking the square root of the linear scaling factor going from 1 (left or right) to 0.5.sqrt (~=0.707) in the center, which is about 3dB reduction. This is equal power panning. 
LinPan2 sounds more like the Rhodes tremolo than Pan2.

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
// hear the difference, LinPan having a slight drop in the middle (yeah, it's subtle)...
{LinPan2.ar(SinOsc.ar(440), Line.kr(-1,1,5))}.play

// ... whereas Pan2 is more smooth
{Pan2.ar(SinOsc.ar(440), Line.kr(-1,1,5))}.play

// other examples
play({ LinPan2.ar(PinkNoise.ar(0.4), FSinOsc.kr(2)) });
SynthDef("help-LinPan2", {  Out.ar(0, LinPan2.ar(FSinOsc.ar(800, 0, 0.1), FSinOsc.kr(3))) }).play;
::

]


