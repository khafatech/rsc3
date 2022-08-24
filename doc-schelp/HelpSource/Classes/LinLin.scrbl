#lang scribble/manual
@(require (for-label racket))

@title{LinLin}
 Map a linear range to another linear range@section{related}
  Classes/LinExp
@section{categories}
   UGens>Maths


@section{description}


Maps a linear range of values to another linear range of values.


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


@section{Examples}
 


@racketblock[
// examples:

(
{
	var mod = SinOsc.kr(Line.kr(1, 10, 10));
	SinOsc.ar(LinLin.kr(mod, -1,1, 100, 900)) * 0.1
}.play;
)

// modulating destination values.
(
{
	var mod = LFNoise2.ar(80);
	SinOsc.ar(LinLin.ar(mod, -1,1, MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1))) * 0.1
}.play;
)

// modulating source and destination values.
(
{
	var mod = LFNoise2.ar(80);
	SinOsc.ar(
		LinLin.ar(mod,
			SinOsc.kr(0.2), SinOsc.kr(0.2543),
			MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1)
		)
	) * 0.1
}.play;
)
::

linlin and range can be used to create a LinLin implicitly from a ugen, mapping its output values from linear range to an exponential one. The rate is derived from the ugen.

]

@racketblock[
// linlin
(
{
	var mod = LFNoise2.ar(80);
	SinOsc.ar(mod.linlin(-1,1, MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1))) * 0.1
}.play;
)

// range
(
{
	var mod = LFNoise2.ar(80).range(MouseX.kr(200, 8000, 1), MouseY.kr(200, 8000, 1));
	SinOsc.ar(mod) * 0.1
}.play;
)
::

]


