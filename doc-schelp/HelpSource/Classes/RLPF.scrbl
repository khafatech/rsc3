#lang scribble/manual
@(require (for-label racket))

@title{RLPF}
 A resonant low pass filter.@section{related}
  Classes/Formlet, Classes/RHPF, Classes/Resonz, Classes/Ringz
@section{categories}
   UGens>Filters>Linear


@section{description}


A resonant low pass filter.


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
 rq

The reciprocal of Q (bandwidth / cutoffFreq).


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ RLPF.ar(Saw.ar(200, 0.1), SinOsc.ar(XLine.kr(0.7, 300, 20), 0, 3600, 4000), 0.2) }.play;


(
{ var ctl = RLPF.ar(Saw.ar(5, 0.1), 25, 0.03);
	SinOsc.ar(ctl * 200 + 400) * 0.1;
}.play;
)


(
{ var ctl = RLPF.ar(Saw.ar(5,0.1), MouseX.kr(2, 200, 1), MouseY.kr(0.01, 1, 1));
	SinOsc.ar(ctl * 200 + 400) * 0.1;
}.play;
)

::


]


