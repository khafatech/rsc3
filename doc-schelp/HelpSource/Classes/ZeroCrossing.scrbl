#lang scribble/manual
@(require (for-label racket))

@title{ZeroCrossing}
 Zero crossing frequency follower@section{categories}
  UGens>Analysis>Pitch

@section{description}

Outputs a frequency based upon the distance between interceptions of the X axis. The X intercepts are determined via linear interpolation so this gives better than just integer wavelength resolution. This is a very crude pitch follower, but can be useful in some situations.

@section{classmethods}
 
@section{method}
  ar, kr
@section{argument}
 in
Input signal.

@section{examples}
 


@racketblock[
s.boot;
(
{
    var a;
    a = SinOsc.ar(SinOsc.kr(1, 0, 600,700), 0, 0.1);
    [a, ZeroCrossing.ar(a) * 0.0005]
}.scope;
)
::
]


