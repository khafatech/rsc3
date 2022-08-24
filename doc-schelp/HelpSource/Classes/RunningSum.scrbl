#lang scribble/manual
@(require (for-label racket))

@title{RunningSum}
 Running sum over n frames@section{categories}
  UGens>Analysis, UGens>Maths

@section{description}

A running sum over a user specified number of samples, useful for running RMS power windowing.

@section{classmethods}
 
@section{method}
  ar, kr
@section{argument}
 in
Input signal
@section{argument}
 numsamp
How many samples to take the running sum over (initialisation time only, not modulatable. default: 40)

@section{examples}
 


@racketblock[
// distorts of course - would need scaling
{ RunningSum.ar(SoundIn.ar) }.play

// Running Average over x samples
(
{
    var x = 100;
    RunningSum.ar(LFSaw.ar, x) * (x.reciprocal)
 }.play
)
::

]

@racketblock[
// RMS Power
(
{
    var input, numsamp;

    input = LFSaw.ar;
    numsamp = 30;

    (RunningSum.ar(input.squared, numsamp) / numsamp).sqrt
}.play
)
::

]

@racketblock[
// shortcut in class
{ RunningSum.rms(SoundIn.ar) }.play
::

]

@racketblock[
// play around
(
{
    var input, numsamp, power;
    numsamp = 500;
    input = SoundIn.ar;
    power = MouseX.kr(0.1, 4);

    (RunningSum.ar(input ** power, numsamp)/numsamp) ** (power.reciprocal)
}.play
)
::
]


