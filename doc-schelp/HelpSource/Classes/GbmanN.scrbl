#lang scribble/manual
@(require (for-label racket))

@title{GbmanN}
 Gingerbreadman map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/GbmanL

@section{description}

A non-interpolating sound generator based on the difference equations:

teletype::
        x(n+1) = 1 - y(n) + |x(n)|
        y(n+1) = x(n)
::


The behavior of the system is only dependent on its initial conditions.

sclang code translation:


@racketblock[
(
var xi = 1.2, yi = 2.1, size = 64;
plot(size.collect { var x; xi = 1 - yi + abs(x = xi); yi = x; xi });
)
::

Reference: emphasis:: Devaney, R. L. "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992. ::

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
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
// default initial params
{ GbmanN.ar(MouseX.kr(20, SampleRate.ir)) * 0.1 }.play(s);

// change initial params
{ GbmanN.ar(MouseX.kr(20, SampleRate.ir), -0.7, -2.7) * 0.1 }.play(s);

// wait for it...
{ GbmanN.ar(MouseX.kr(20, SampleRate.ir), 1.2, 2.0002) * 0.1 }.play(s);

// as a frequency control
{ SinOsc.ar(GbmanN.ar(40)*400+500)*0.4 }.play(s);
::
]


