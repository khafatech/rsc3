#lang scribble/manual
@(require (for-label racket))

@title{Integrator}
 A leaky integrator.@section{categories}
   UGens>Filters>Linear, UGens>Maths


@section{description}


Integrates an input signal with a leak. The formula implemented is:


@racketblock[
out(0) = in(0) + (coef * out(-1))
::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 coef

Leak coefficient.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ Integrator.ar(LFPulse.ar(300, 0.2, 0.1), MouseX.kr(0.001, 0.999, 1)) }.play

// used as an envelope
{ Integrator.ar(LFPulse.ar(3, 0.2, 0.0004), 0.999, FSinOsc.ar(700), 0) }.play


// scope

{ Integrator.ar(LFPulse.ar(1500 / 4, 0.2, 0.1), MouseX.kr(0.01, 0.999, 1)) }.scope

::
]


