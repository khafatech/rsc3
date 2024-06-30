#lang scribble/manual
@(require (for-label racket))

@title{HenonC}
 Henon map chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/HenonN, Classes/HenonL

@section{description}

A cubic-interpolating sound generator based on the difference equation:

teletype::
	x(n+2) = 1 - a * x(n+1)^2 + b * x(n)
::

This equation was discovered by French astronomer Michel Hénon while studying the orbits of stars in globular clusters.

for more information on chaos theory henon formulas:

"https://en.wikipedia.org/wiki/Chaos_theory"

"https://en.wikipedia.org/wiki/Hénon_map"

sclang code translation:


@racketblock[
(
var a = 1.4, b = 0.3, x0 = 0, x1 = 1, size = 64;
plot(size.collect { var aux = 1 - (a * (x1 ** 2)) + (b * x0); x0 = x1; x1 = aux; aux });
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
  x0
Initial value of x
@section{argument}
  x1
Second value of x

@section{examples}
 

@racketblock[
// default initial params
{ HenonC.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 !2}.scope(s);

// mouse-control of params
{ HenonC.ar(SampleRate.ir/4, MouseX.kr(1,1.4), MouseY.kr(0,0.3)) * 0.2 !2}.scope(s);

// randomly modulate params
({ HenonC.ar(SampleRate.ir/8, LFNoise2.kr(1, 0.2, 1.2), LFNoise2.kr(1, 0.15, 0.15)) ! 2 * 0.02 }).scope(s);

// as a frequency control
{ SinOsc.ar(HenonC.ar(40, MouseX.kr(1,1.4), MouseY.kr(0,0.3))*800+900) * 0.04 ! 2 }.scope(s);
::
]

