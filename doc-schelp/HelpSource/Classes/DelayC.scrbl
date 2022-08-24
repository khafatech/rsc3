#lang scribble/manual
@(require (for-label racket))

@title{DelayC}
 Simple delay line with cubic interpolation.@section{related}
  Classes/DelayL, Classes/DelayN, Classes/BufDelayC
@section{categories}
   UGens>Delays


@section{description}


Simple delay line with cubic interpolation. See also
link::Classes/DelayN::  which uses no interpolation, and
link::Classes/DelayL::  which uses linear interpolation. Cubic
interpolation is more computationally expensive than linear,
but more accurate.

The term "delay" is often used in electronic music to refer to a delay line with feedback. If you are looking for that, try CombC.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 maxdelaytime
The maximum delay time in seconds. used to initialize the delay buffer size.

@section{argument}
 delaytime
Delay time in seconds.

@section{discussion}
 
@section{note}
 
DelayC needs at least 4 samples buffer. Therefore the minimum 
@racketblock[maxdelaytime:: and ]

@racketblock[delaytime:: must be ]

@racketblock[SampleDur.ir * 4::.
::

]
@section{Examples}
 

@racketblock[
(
// Dust randomly triggers Decay to create an exponential
// decay envelope for the WhiteNoise input source
{
z = Decay.ar(Dust.ar(1,0.5), 0.3, WhiteNoise.ar);
DelayC.ar(z, 0.2, 0.2, 1, z); // input is mixed with delay via the add input
}.play
)

(
// recursive application of delay.
{
z = Decay2.ar(Dust.ar(1, 0.5), 0.01, 0.1, Saw.ar(100 + [0, 1]));
5.do { |i| z = DelayC.ar(RLPF.ar(z, Rand(100, 3000), 0.03), 1, 1 / (2**i), 1, z * 0.5) };
z
}.play
)
::

]


