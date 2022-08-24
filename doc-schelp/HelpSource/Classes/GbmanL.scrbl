#lang scribble/manual
@(require (for-label racket))

@title{GbmanL}
 Gingerbreadman map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/GbmanN

@section{description}

A linear-interpolating sound generator based on the difference equations:

teletype::
	x(n+1) = 1 - y(n) + |x(n)|
	y(n+1) = x(n)
::

The behavior of the system is dependent only on its initial conditions and cannot be changed once it's started.

sclang code translation:


@racketblock[
(
var xi = 1.2, yi = 2.1, size = 64;
plot(size.collect { var x; xi = 1 - yi + abs(x = xi); yi = x; xi });
)
::

Reference: Devaney, R. L. "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992.

]
@section{classmethods}
 
@section{method}
  ar
@section{argument}
  freq
Iteration frequency in Hertz
@section{argument}
  xi
Initial value of x
@section{argument}
  yi
Initial value of y

@section{examples}
 

@racketblock[
// default initial params
{ GbmanL.ar(MouseX.kr(20, SampleRate.ir)) * 0.1 }.play(s);

// different initial params
{ GbmanL.ar(MouseX.kr(20, SampleRate.ir), -0.7, -2.7) * 0.1 }.play(s);

// wait for it...
{ GbmanL.ar(MouseX.kr(20, SampleRate.ir), 1.2, 2.0002) * 0.1 }.play(s);

// as a frequency control
{ SinOsc.ar(GbmanL.ar(40)*400+500)*0.4 }.play(s);
::
]


