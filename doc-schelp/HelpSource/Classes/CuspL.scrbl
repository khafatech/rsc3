#lang scribble/manual
@(require (for-label racket))

@title{CuspL}
 Cusp map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/CuspN

@section{description}

A linear-interpolating sound generator based on the difference equation:

teletype::
	x(n+1) = a - b * sqrt(|x(n)|)
::

sclang code translation:


@racketblock[
(
var a = 1.0, b = 1.9, xi = 0, size = 64;
plot(size.collect { xi = a - (b * sqrt(abs(xi))) });
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
 xi
Initial value of x

@section{examples}
 

@racketblock[
// vary frequency
{ CuspL.ar(MouseX.kr(20, SampleRate.ir), 1.0, 1.99) * 0.3 }.play(s);

// mouse-controlled params
{ CuspL.ar(SampleRate.ir/4, MouseX.kr(0.9,1.1,1), MouseY.kr(1.8,2,1)) * 0.3 }.play(s);

// as a frequency control
{ SinOsc.ar(CuspL.ar(40, MouseX.kr(0.9,1.1,1), MouseY.kr(1.8,2,1))*800+900)*0.4 }.play(s);
::
]


