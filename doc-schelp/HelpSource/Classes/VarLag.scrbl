#lang scribble/manual
@(require (for-label racket))

@title{VarLag}
 Variable shaped lag@section{related}
  Classes/Lag, Classes/Ramp, Classes/Slew
@section{categories}
   UGens>Filters>Linear


@section{description}

Similar to link::Classes/Lag:: but with other curve shapes than exponential.
A change on the input will take the specified time to reach the new value.
Useful for smoothing out control signals.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 time

Lag time in seconds.

@section{argument}
 curvature
Control curvature if strong::warp:: input is 5 (default).
0 means linear, positive and negative numbers curve the segment up and down.

@section{argument}
 warp
Determines the shape. The possible values are:
@section{table}
 
## 
@racketblock[\step:: || || flat segment
## ]

@racketblock[\linear:: || ]

@racketblock[\lin:: || linear segment, the default
## ]

@racketblock[\exponential:: || ]

@racketblock[\exp:: || natural exponential growth and decay. In this case, the levels must all be nonzero and the have the same sign.
## ]

@racketblock[\sine:: || ]

@racketblock[\sin:: || sinusoidal S shaped segment.
## ]

@racketblock[\welch:: || ]

@racketblock[\wel:: || sinusoidal segment shaped like the sides of a Welch window.
## ]

@racketblock[\squared::  || ]

@racketblock[\sqr:: || squared segment
## ]

@racketblock[\cubed:: || ]

@racketblock[\cub:: || cubed segment
::

All values above will ignore strong::curvature:: input.

]
@section{note}
 
When controlling this from the outside, use 
@racketblock[Env.shapeNumber(symbol):: to get the numeric value for each shape.
::

]
@section{argument}
 start
Initial value. If not specified, same as the input signal.

@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
(
// used to lag pitch
{
    SinOsc.ar(                              // sine wave
        VarLag.kr(                            // lag the modulator
            LFPulse.kr(1).range(100,400),   // frequency modulator
            0.2,                            // lag time
            Line.kr(-8, 8, 15, doneAction: Done.freeSelf) // modulate shape
        ),
        0,                                  // sine phase
        0.3                                 // sine amplitude
    )
}.play
)
::

]

@racketblock[
(
x = play { |amp=0, time=0, curve=0, warp=5|
    PinkNoise.ar(VarLag.kr(amp, time, curve, warp) ! 2)
}
)

x.set(\amp, 1, \time, 5, \warp, Env.shapeNumber(\sin)) // s-shaped curve up
x.set(\amp, 0, \time, 1, \warp, Env.shapeNumber(\lin)) // linear down

x.set(\amp, 1, \time, 2, \warp, 5, \curve, 7); // slow curvature
x.set(\amp, 0, \time, 0);

x.set(\amp, 1, \time, 2, \warp, 5, \curve, -7); // fast curvature
x.set(\amp, 0, \time, 0);

x.free;
::

]


