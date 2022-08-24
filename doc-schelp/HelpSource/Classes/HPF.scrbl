#lang scribble/manual
@(require (for-label racket))

@title{HPF}
 2nd order Butterworth highpass filter.@section{related}
  Classes/BPF, Classes/BRF, Classes/LPF
@section{categories}
   UGens>Filters>Linear


@section{description}

A second order high pass filter.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Cutoff frequency in Hertz.
WARNING: due to the nature of its implementation frequency values close to 0 may cause glitches and/or extremely loud audio artifacts!


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
{ HPF.ar(Saw.ar(200, 0.1), FSinOsc.kr(XLine.kr(0.7, 300, 20), 0, 3600, 4000), 5) }.play;

(
{ 	var ctl = HPF.kr(LFSaw.kr(5), SinOsc.kr(XLine.kr(0.07, 30, 20), 0, 35, 40)) ;
	SinOsc.ar(ctl * 200 + 500);
}.play;
)

(
{ 	var ctl = HPF.kr(LFSaw.kr(5, 0.1), MouseX.kr(2, 200, 1));
	SinOsc.ar(ctl * 200 + 400) * 0.1;
}.play;
)
::

]


