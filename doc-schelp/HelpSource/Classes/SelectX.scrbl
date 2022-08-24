#lang scribble/manual
@(require (for-label racket))

@title{SelectX}
 Mix one output from many sources@section{categories}
  UGens>Multichannel>Select
@section{related}
  Classes/Select, Classes/SelectXFocus, Classes/LinSelectX

@section{description}

The output is mixed from an array of inputs, performing an equal power crossfade between two adjacent channels.

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  which
@section{argument}
  array
@section{argument}
  wrap
wrap does not work yet.

@section{examples}
 

@racketblock[
(
{
	var a;
	a = [
			SinOsc.ar,
			Saw.ar(300),
			Pulse.ar(230)
		];

	SelectX.ar(MouseX.kr(0, 1) * a.size, a) * 0.2
}.play;
)

(
{
	var a;
	a = [
			SinOsc.kr(0.25),
			LFSaw.kr(10),
			LFPulse.kr(0.3)
		];

	SinOsc.ar(SelectX.kr(MouseX.kr(0, 1) * a.size, a) * 300 + 400) * 0.2
}.play;
)
::

]
@section{note}
 
all the ugens are continuously running. This may not be the most efficient way if each input is  cpu-expensive. The array is fixed at the time of writing the SynthDef, and the whole array is embedded in the SynthDef file itself.  For small arrays this is more efficient than reading from a buffer.
::

wrap does not work yet.

(by adc)


