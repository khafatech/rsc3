#lang scribble/manual
@(require (for-label racket))

@title{ModDif}
 Minimum difference of two values in modulo arithmetics@section{related}
  Classes/Clip, Classes/Wrap
@section{categories}
   UGens>Maths


@section{description}

Returns the minimum difference of two values in modulo arithmetics. On a circle, there are two distances between two points. This UGen returns the smaller value of the two.


@racketblock[
{ var a = Line.ar(0, 4, 0.01), d = ModDif.ar(a); [a, d] }.plot;
{ var a = Line.ar(0, 4, 0.01); ModDif.ar(a, 0, (1..4)) }.plot;
{ var a = Line.ar(0, 4, 0.01); ModDif.ar(a, (0, 0.25 .. 1), 1) }.plot;

::


]
@section{classmethods}
 

@section{method}
 ar, kr, ir

@section{argument}
 x
First input value

@section{argument}
 y
Second input value

@section{argument}
 mod
Modulo (maximum value, double of the maximal difference).


@racketblock[

// different moduli
(
{
	var sig = LFSaw.ar(10);
	var dist = ModDif.kr(sig, 0, (0..8) * MouseX.kr(0, 1/5));
	Splay.ar(SinOsc.ar(dist * 4000 + 400)) * 0.1
}.play;
)

// wrapping amplitude crossfade
(
{
	var numChan = 12;
	var x = SinOsc.ar({ rrand(300.0, 800.0) } ! numChan);
	var dist = ModDif.kr(MouseX.kr(0, numChan * 2), (0..numChan-1), numChan);
	x = x * max(0, 1 - dist);
	Splay.ar(x) * 0.1
}.play;
)
::


]


