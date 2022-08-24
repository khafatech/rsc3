#lang scribble/manual
@(require (for-label racket))

@title{CombN}
 Comb delay line with no interpolation.@section{related}
  Classes/CombC, Classes/CombL, Classes/BufCombN
@section{categories}
   UGens>Delays


@section{description}


Comb delay line with no interpolation. See also  link::Classes/CombL::
which uses linear interpolation, and  link::Classes/CombC::  which uses
cubic interpolation. Cubic and linear interpolation are more computationally
expensive, but more accurate.

This UGen will create aliasing artifacts if you modulate the delay time, which is also quantized to the nearest sample period. If these are undesirable properties, use CombL or CombC. But if your delay time is fixed and sub-sample accuracy is not needed, this is the most CPU-efficient choice with no loss in quality.

The feedback coefficient is given by the equation 
@racketblock[ fb == 0.001 ** (delay / decay.abs) * decay.sign :: where 0.001 is -60 dBFS.

]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 maxdelaytime
The maximum delay time in seconds. Used to initialize the delay buffer size.

@section{argument}
 delaytime
Delay time in seconds.

@section{argument}
 decaytime
Time for the echoes to decay by 60 decibels. If this time is negative, then the feedback coefficient will be negative, thus emphasizing only odd harmonics at an octave lower.

Infinite decay times are permitted. A decay time of 
@racketblock[inf:: leads to a feedback coefficient of 1, and a decay time of ]

@racketblock[-inf:: leads to a feedback coefficient of -1.

]
@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

// These examples compare the variants, so that you can hear the difference in interpolation

// Comb used as a resonator. The resonant fundamental is equal to
// reciprocal of the delay time.
{ CombN.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ CombL.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ CombC.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

// with negative feedback:
{ CombN.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ CombL.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ CombC.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

// used as an echo.
{ CombN.ar(Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 0.2, 3) }.play;

::

]


