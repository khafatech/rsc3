#lang scribble/manual
@(require (for-label racket))

@title{Decay2}
 Exponential decay@section{related}
  Classes/Decay
@section{categories}
   UGens>Filters>Linear, UGens>Envelopes


@section{description}

link::Classes/Decay::  has a very sharp attack and can produce clicks.
Decay2 rounds off the attack by subtracting one Decay from another.

@racketblock[ Decay2.ar(in, attackTime, decayTime):: is equivalent to:

]

@racketblock[
Decay.ar(in, decayTime) - Decay.ar(in, attackTime)
::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 attackTime

60 dB attack time in seconds.


@section{argument}
 decayTime

60 dB decay time in seconds.


@section{argument}
 mul

@section{argument}
 add


@section{Examples}
 


@racketblock[
// since attack and decay are a difference of two Decays,
// swapping the values, the envelope turns upside down:
plot({ Decay2.ar(Impulse.ar(1), 0.001, 0.01) })
plot({ Decay2.ar(Impulse.ar(1), 0.01, 0.001) })

// used as an envelope
{ Decay2.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.01, 0.2, FSinOsc.ar(600)) }.play;

// compare the above with Decay used as the envelope
{ Decay.ar(Impulse.ar(XLine.kr(1,50,20), 0.25), 0.2, FSinOsc.ar(600), 0)  }.play;
::

]


