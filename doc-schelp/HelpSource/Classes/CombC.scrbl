#lang scribble/manual
@(require (for-label racket))

@title{CombC}
 Comb delay line with cubic interpolation.@section{related}
  Classes/CombL, Classes/CombN, Classes/BufCombC
@section{categories}
   UGens>Delays


@section{description}


Comb delay line with cubic interpolation. See also  link::Classes/CombN::
which uses no interpolation, and  link::Classes/CombL::  which uses linear
interpolation. Cubic interpolation is more computationally expensive
than linear, but more accurate.

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
Time for the echoes to decay by 60 decibels. If this time is negative then the feedback coefficient will be negative, thus emphasizing only odd harmonics at an octave lower.

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

// large delay lines
(
s.options.memSize= 8192*2;
s.reboot;
)
({
	var in, del1, del2, del3, del4;
	in = SoundIn.ar(0);
	del1 = CombC.ar(in, 8, LFNoise2.kr(0.01).range(1, 8), 1);
	del2 = CombC.ar(del1, 8, LFNoise2.kr(0.01).range(1, 8), 1);
	del3 = CombC.ar(del2, 8, LFNoise2.kr(0.01).range(1, 8), 1);
	del4 = CombC.ar(del3, 8, LFNoise2.kr(0.01).range(1, 8), 1);
	(del1 + del2 + del3 + del4)!2;
}.play;
);

::

]


