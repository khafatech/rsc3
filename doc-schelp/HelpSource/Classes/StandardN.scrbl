#lang scribble/manual
@(require (for-label racket))

@title{StandardN}
 Standard map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/StandardL

@section{description}

A non-interpolating sound generator based on the difference equations:

teletype::
        x(n+1) = (x(n) + y(n+1)) % 2pi
        y(n+1) = (y(n) + k * sin(x(n))) % 2pi
::

The standard map is an area preserving map of a cylinder discovered by the plasma physicist Boris Chirikov.

sclang code translation:


@racketblock[
(
var k = 1, xi = 0.5, yi = 0, size = 64;
plot(size.collect { yi = yi + (k * sin(xi)) % 2pi; xi = (xi + yi) % 2pi; xi - pi * 0.3183098861837907 });
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
  k
Perturbation amount
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
// vary frequency
{ StandardN.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }.play(s);
::

]

@racketblock[
// mouse-controlled param
{ StandardN.ar(SampleRate.ir/2, MouseX.kr(0.9,4)) * 0.3 }.play(s);
::

]

@racketblock[
// as a frequency control
{ SinOsc.ar(StandardN.ar(40, MouseX.kr(0.9,4))*800+900)*0.4 }.play(s);
::
]


