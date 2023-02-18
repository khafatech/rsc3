#lang scribble/manual
@(require (for-label racket))

@title{QuadL}
 General quadratic map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/QuadC, Classes/QuadN

@section{description}

A linear-interpolating sound generator based on the difference equation:

teletype::
	x(n+1) = a * x(n)^2 + b * x(n) + c
::

sclang code translation:


@racketblock[
(
var a = 1, b = -1, c = -0.75, xi = 0, size = 64;
plot(size.collect { xi = (a * (xi ** 2)) + (b * xi) + c; xi });
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
Equation variable
@section{argument}
  b
Equation variable
@section{argument}
  c
Equation variable
@section{argument}
  xi
Initial value of x

@section{examples}
 

@racketblock[
// default params
{ QuadL.ar(SampleRate.ir/4) * 0.2 }.play(s);
::

]

@racketblock[
// logistic map
// equation: x1 = -r*x0^2 + r*x0
(
{ var r;
	r = MouseX.kr(3.5441, 4);	// stable range
	QuadL.ar(SampleRate.ir/4, r.neg, r, 0, 0.1) * 0.4;
}.play(s);
)
::

]

@racketblock[
// logistic map as frequency control
(
{ var r;
	r = MouseX.kr(3.5441, 4);	// stable range
	SinOsc.ar(QuadL.ar(40, r.neg, r, 0, 0.1, 800, 900)) * 0.4;
}.play(s);
)
::
]


