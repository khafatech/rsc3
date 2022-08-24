#lang scribble/manual
@(require (for-label racket))

@title{BPF}
 2nd order Butterworth bandpass filter.@section{related}
  Classes/BRF, Classes/HPF, Classes/LPF
@section{categories}
   UGens>Filters>Linear


@section{description}


A second order band pass filter.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Centre frequency in Hertz.
WARNING: due to the nature of its implementation frequency values close to 0 may cause glitches and/or extremely loud audio artifacts!

@section{argument}
 rq

The reciprocal of Q. Q is conventionally defined as cutoffFreq / bandwidth, meaning rq = (bandwidth / cutoffFreq).

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[
{ BPF.ar(Saw.ar(200,0.5), FSinOsc.kr(XLine.kr(0.7,300,20),0,3600,4000), 0.3) }.play;

{ BPF.ar(Saw.ar(200,0.5), MouseX.kr(100, 10000, 1), 0.3) }.play;

	// BPF on control signals:
(
{ 	var vib = BPF.kr(PinkNoise.kr, MouseX.kr(1, 100, 1), 0.3) * 10;
	SinOsc.ar(vib * 200 + 600) * 0.2 }.play;
)
::

]


