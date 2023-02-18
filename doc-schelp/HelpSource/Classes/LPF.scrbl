#lang scribble/manual
@(require (for-label racket))

@title{LPF}
 2nd order Butterworth lowpass filter@section{related}
  Classes/BPF, Classes/BRF, Classes/HPF
@section{categories}
   UGens>Filters>Linear


@section{description}


A second order low pass filter.


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
{ LPF.ar(Saw.ar(200,0.1), SinOsc.kr(XLine.kr(0.7,300,20),0,3600,4000)) }.play;

// kr:
(
{ var ctl = LPF.kr(LFPulse.kr(8), SinOsc.kr(XLine.kr(1, 30, 5)) + 2);
	SinOsc.ar(ctl * 200 + 400)
}.play;
)

(
{ var ctl = LPF.kr(LFPulse.kr(8), MouseX.kr(2, 50, 1));
	SinOsc.ar(ctl * 200 + 400)
}.play;
)
::

]


