#lang scribble/manual
@(require (for-label racket))

@title{Decay}
 Exponential decay@section{related}
  Classes/Decay2
@section{categories}
  UGens>Filters>Linear, UGens>Envelopes

@section{description}


This is essentially the same as  link::Classes/Integrator::  except that
instead of supplying the coefficient directly, it is calculated from a 60
dB decay time. This is the time required for the integrator to lose 99.9%
of its value or -60dB. This is useful for exponential decaying envelopes
triggered by impulses.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 decayTime
60 dB decay time in seconds.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[
plot({ Decay.ar(Impulse.ar(1), 0.01) });

// used as an envelope
play({ Decay.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.2, PinkNoise.ar, 0) });
::

]


