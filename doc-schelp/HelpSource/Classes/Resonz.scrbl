#lang scribble/manual
@(require (for-label racket))

@title{Resonz}
 Resonant filter.@section{related}
  Classes/Formlet, Classes/RHPF, Classes/RLPF, Classes/Ringz
@section{categories}
   UGens>Filters>Linear


@section{description}


This is the same as  link::Classes/Ringz:: , except that it has a constant gain at 0 dB instead of being constant skirt.

It is a two pole resonant filter with zeroes at


@racketblock[
z = ±1
::


Based on  emphasis::K. Steiglitz,  "A Note on Constant-Gain Digital Resonators", Computer Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994::.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Resonant frequency in Hertz.
WARNING: due to the nature of its implementation frequency values close to 0 may cause glitches and/or extremely loud audio artifacts!

@section{argument}
 bwr

Bandwidth ratio (reciprocal of Q). rq = bandwidth / centerFreq.


The reciprocal of Q is used rather than Q because it saves a
divide operation inside the unit generator.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ Resonz.ar(WhiteNoise.ar(0.5), 2000, 0.1) }.play

// modulate frequency
{ Resonz.ar(WhiteNoise.ar(0.5), XLine.kr(1000,8000,10), 0.05) }.play

// modulate bandwidth
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(1, 0.001, 8)) }.play

// modulate bandwidth opposite direction
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(0.001, 1, 8)) }.play

::

]


