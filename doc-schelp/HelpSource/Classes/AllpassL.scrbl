#lang scribble/manual
@(require (for-label racket))

@title{AllpassL}
 All pass delay line with linear interpolation.@section{related}
  Classes/AllpassC, Classes/AllpassN, Classes/BufAllpassL
@section{categories}
   UGens>Delays


@section{description}


All pass delay line with linear interpolation. See also
link::Classes/AllpassN::  which uses no interpolation, and
link::Classes/AllpassC::  which uses cubic interpolation.
Cubic interpolation is more computationally expensive than linear,
but more accurate.


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

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

// Since the allpass delay has no audible effect as a resonator on
// steady state sound ...

{ AllpassC.ar(WhiteNoise.ar(0.1), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

// ...these examples add the input to the effected sound and compare variants so that you can hear
// the effect of the phase comb:

(
{
	z = WhiteNoise.ar(0.2);
	z + AllpassN.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)

(
{
	z = WhiteNoise.ar(0.2);
	z + AllpassL.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)

(
{
	z = WhiteNoise.ar(0.2);
	z + AllpassC.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)

// used as an echo - doesn't really sound different than Comb,
// but it outputs the input signal immediately (inverted) and the echoes
// are lower in amplitude.
{ AllpassL.ar(Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 0.2, 3) }.play;

::

]


