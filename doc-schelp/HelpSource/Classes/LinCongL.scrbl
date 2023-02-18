#lang scribble/manual
@(require (for-label racket))

@title{LinCongL}
 Linear congruential chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/LinCongC, Classes/LinCongN

@section{description}

A linear-interpolating sound generator based on the difference equation:

teletype::
        x(n+1) = (a * x(n) + c) % m
::

The output signal is automatically scaled to a range of [-1, 1].

sclang code translation:


@racketblock[
(
var a = 1.1, c = 0.13, m = 1, xi = 0, size = 64;
plot(size.collect { xi = (a * xi + c) % m });
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
  a
Multiplier amount
@section{argument}
  c
Increment amount
@section{argument}
  m
Modulus amount
@section{argument}
  xi
Initial value of x

@section{examples}
 

@racketblock[
// default initial params
{ LinCongL.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }.play(s);
::

]

@racketblock[
// randomly modulate params
(
{ LinCongL.ar(
	LFNoise2.kr(1, 1e4, 1e4),
	LFNoise2.kr(0.1, 0.5, 1.4),
	LFNoise2.kr(0.1, 0.1, 0.1),
	LFNoise2.kr(0.1)
) * 0.2 }.play(s);
)
::

]

@racketblock[
// as frequency control...
(
{
SinOsc.ar(
	LinCongL.ar(
		40,
		LFNoise2.kr(0.1, 0.1, 1),
		LFNoise2.kr(0.1, 0.1, 0.1),
		LFNoise2.kr(0.1),
		0, 500, 600
	)
) * 0.4 }.play(s);
)
::
]


