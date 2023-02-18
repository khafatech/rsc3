#lang scribble/manual
@(require (for-label racket))

@title{LinExp}
 Map a linear range to an exponential range@section{related}
  Classes/LinLin
@section{categories}
   UGens>Maths


@section{description}


Converts a linear range of values to an exponential range of values.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal to convert.


@section{argument}
 srclo

Lower limit of input range.


@section{argument}
 srchi

Upper limit of input range.


@section{argument}
 dstlo

Lower limit of output range.


@section{argument}
 dsthi

Upper limit of output range.

@section{discussion}
 
The 
@racketblock[dstlo::  and  ]

@racketblock[dsthi::  arguments
must be nonzero and have the same sign.


]
@section{Examples}
 


@racketblock[
// compare:
(
{
	var mod = SinOsc.kr(Line.kr(1, 10, 10));
	SinOsc.ar(mod * 400 + 500) * 0.1
}.play;
)

(
{
	var mod = SinOsc.kr(Line.kr(1, 10, 10));
	SinOsc.ar(LinExp.kr(mod, -1,1, 100, 900)) * 0.1
}.play;
)

// modulating destination values.
(
{
	var mod = LFNoise2.ar(80);
	SinOsc.ar(LinExp.ar(mod, -1,1, MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1))) * 0.1
}.play;
)
::

]

@racketblock[linexp:: and ]

@racketblock[exprange:: can be used to create a LinExp implicitly from a ugen, mapping its output values from linear range to an exponential one. The rate is derived from the ugen.

]

@racketblock[
// linexp
(
{
	var mod = LFNoise2.ar(80);
	SinOsc.ar(mod.linexp(-1,1, MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1))) * 0.1
}.play;
)

// exprange
(
{
	var mod = LFNoise2.ar(80).exprange(MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1));
	SinOsc.ar(mod) * 0.1
}.play;
)
::

]


