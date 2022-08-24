#lang scribble/manual
@(require (for-label racket))

@title{Schmidt}
 Schmidt trigger.@section{related}
  Classes/InRange, Classes/InRect
@section{categories}
   UGens>Maths


@section{description}


If 
@racketblock[in > hi::, output 1. If ]

@racketblock[in < lo::, output 0. Otherwise, repeat the
last sample of output, assumed to be 0 at initialization. In sclang-flavored
pseudocode:

]

@racketblock[
out[i] = if(in[i] < lo[i]) {
    0
} {
    if(in[i] > hi[i]) {
        1
    } {
        out[i-1]
    }
};
::

]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Signal to be tested.


@section{argument}
 lo

Low threshold.


@section{argument}
 hi

High threshold.


@section{Examples}
 


@racketblock[

s.boot;

{ Schmidt.kr(SinOsc.kr(1, 0, 0.2), -0.15, 0.15)}.scope; // see the trigger

{ Schmidt.kr(MouseX.kr(0, 1), 0.2, 0.8)}.scope; // try it with the cursor

// threshold octave jumps
(
{
	var in = LFNoise1.kr(3);
	var octave = Schmidt.kr(in, -0.15, 0.15) + 1;
	SinOsc.ar(in * 200 + 500 * octave, 0, 0.1)
}.scope;
)

::

]


