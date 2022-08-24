#lang scribble/manual
@(require (for-label racket))

@title{Ball}
 physical model of bouncing object@section{categories}
  UGens>Filters>Nonlinear, UGens>Generators>PhysicalModels
@section{related}
  Classes/TBall, Classes/Spring

@section{description}

Ball models the path of a bouncing object that is reflected by a vibrating surface.

@section{classmethods}
 
@section{private}
  categories

@section{method}
  ar, kr

@section{argument}
 in
modulated surface level

@section{argument}
 g
gravity

@section{argument}
 damp
damping on impact

@section{argument}
 friction
proximity from which on attraction to surface starts

@section{examples}
 
mouse x controls switch of level:

@racketblock[
(
{
	var f, sf;
	sf = K2A.ar(MouseX.kr > 0.5) > 0;
	f = Ball.ar(sf, MouseY.kr(0.01, 20, 1), 0.01);
	f = f * 10 + 500;
	SinOsc.ar(f, 0, 0.2)
}.play;
)
::

mouse x controls modulation rate, mouse y controls gravity:
]

@racketblock[
(
{
	var f, sf, g;
	sf = LFNoise0.ar(MouseX.kr(1, 100, 1));
	g = MouseY.kr(0.1, 10, 1);
	f = Ball.ar(sf, g, 0.01, 0.01);
	f = f * 140 + 500;
	SinOsc.ar(f, 0, 0.2)
}.play;
)
::

the general german police choir. mouse x controls damping, mouse y controls gravity:
]

@racketblock[
(
{
	var f, sf, g;
	sf = LFPulse.ar(0.6, 0.2, 0.5);
	g = MouseY.kr(0.1, 10, 1);
	d = MouseX.kr(0.0001, 1, 1);
	f = Ball.ar(sf, g, d);
	f = f * 140 + 400;
	SinOsc.ar(f, 0, 0.2)
}.play;
)
::

]


