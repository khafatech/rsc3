#lang scribble/manual
@(require (for-label racket))

@title{InRect}
 Test if a point is within a given rectangle.@section{related}
  Classes/InRange, Classes/Schmidt
@section{categories}
   UGens>Maths


@section{description}

A pair of signals x and y are treated as a point (x,y) in 2-D; if they fall within the bounds of the rectangle, then this UGen outputs a one; else it outputs zero.


@section{classmethods}
 
@section{method}
 ar, kr

@section{argument}
 x
X component signal

@section{argument}
 y
Y component signal

@section{argument}
 rect
A link::Classes/Rect:: which defines the rectangular region to monitor; note that Rects are in screen co-ordinates, so the top is smaller than the bottom. The Rect is created once and cannot be modulated.

@section{examples}
 

@racketblock[
//we'll hear the sawtooth wave when the two sine oscillators are both in the region x = 0.0 to 0.5, y = 0.5 to 1.0
{InRect.ar(SinOsc.ar(1),SinOsc.ar(1.3),Rect(0,0.5,0.5,0.5))*LFSaw.ar(44,0,0.1) }.play

//stereo effect
{(InRect.ar(LFNoise0.ar([140,141]),LFNoise0.ar(143),Rect(0,0,0.5,1)).lag(0.1))*LFSaw.ar(SinOsc.ar(10,0,5,400),0,0.1) }.play


//for the Rect, create as left, 'top', width, height;
r= Rect(0,0,1,1)

r.left
r.right
r.top
r.bottom
::

]


