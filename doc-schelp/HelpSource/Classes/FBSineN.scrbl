#lang scribble/manual
@(require (for-label racket))

@title{FBSineN}
 Feedback sine with chaotic phase indexing@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/FBSineL, Classes/FBSineC

@section{description}

A non-interpolating sound generator based on the difference equations:

teletype::
        x(n+1) = sin(im * y(n) + fb * x(n))
        y(n+1) = (a * y(n) + c) % 2pi
::


This uses a linear congruential function to drive the phase indexing of a sine wave. For 
@racketblock[ im = 1 ::, ]

@racketblock[ fb = 0 ::, and ]

@racketblock[ a = 1 :: a normal sinewave results.

sclang code translation:

]

@racketblock[
(
var im = 1, fb = 0.1, a = 1.1, c = 0.5, xi = 0.1, yi = 0.1, size = 64;
plot(size.collect { xi = sin((im * yi) + (fb * xi)); yi = (a * yi + c) % 2pi; xi });
)
::

]
@section{classmethods}
 
@section{method}
  ar
@section{argument}
  freq
Iteration frequency in Hertz
@section{argument}
  im
Index multiplier amount
@section{argument}
  fb
Feedback amount
@section{argument}
  a
Phase multiplier amount
@section{argument}
  c
Phase increment amount
@section{argument}
  xi
Initial value of x
@section{argument}
  yi
Initial value of y
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
// default initial params
{ FBSineN.ar(SampleRate.ir/4) * 0.2 }.play(s);
::

]

@racketblock[
// increase feedback
{ FBSineN.ar(SampleRate.ir, 1, Line.kr(0.01, 4, 10), 1, 0.1) * 0.2 }.play(s);
::

]

@racketblock[
// increase phase multiplier
{ FBSineN.ar(SampleRate.ir, 1, 0, XLine.kr(1, 2, 10), 0.1) * 0.2 }.play(s);
::

]

@racketblock[
// modulate frequency and index multiplier
{ FBSineN.ar(LFNoise2.kr(1, 1e4, 1e4), LFNoise2.kr(1,16,17), 1, 1.005, 0.7) * 0.2 }.play(s);
::

]

@racketblock[
// randomly modulate params
(
{ FBSineN.ar(
	LFNoise2.kr(1, 1e4, 1e4),
	LFNoise2.kr(1, 32, 33),
	LFNoise2.kr(1, 0.5),
	LFNoise2.kr(1, 0.05, 1.05),
	LFNoise2.kr(1, 0.3, 0.3)
) * 0.2 }.play(s);
)
::
]


