#lang scribble/manual
@(require (for-label racket))

@title{LorenzL}
 Lorenz chaotic generator@section{categories}
  UGens>Generators>Chaotic

@section{description}

A strange attractor discovered by Edward N. Lorenz while studying mathematical models of the atmosphere. The system is composed of three ordinary differential equations:

teletype::
	x' = s * (y - x)
	y' = x * (r - z) - y
	z' = x * y - b * z
::

The time step amount 
@racketblock[h:: determines the rate at which the ODE is evaluated. Higher values will increase the rate, but cause more instability. A safe choice is the default amount of 0.05.

]
@section{classmethods}
 
@section{method}
  ar
@section{argument}
  freq
Iteration frequency in Hertz
@section{argument}
  s
Equation variable
@section{argument}
  r
Equation variable
@section{argument}
  b
Equation variable
@section{argument}
  h
Integration time step
@section{argument}
  xi
Initial value of x
@section{argument}
  yi
Initial value of y
@section{argument}
  zi
Initial value of z
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
// vary frequency
{ LorenzL.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }.play(s);
::

]

@racketblock[
// randomly modulate params
(
{ LorenzL.ar(
	SampleRate.ir,
	LFNoise0.kr(1, 2, 10),
	LFNoise0.kr(1, 20, 38),
	LFNoise0.kr(1, 1.5, 2)
) * 0.2 }.play(s);
)
::

]

@racketblock[
// as a frequency control
{ SinOsc.ar(Lag.ar(LorenzL.ar(MouseX.kr(1, 200)),3e-3)*800+900)*0.4 }.play(s);
::
]


