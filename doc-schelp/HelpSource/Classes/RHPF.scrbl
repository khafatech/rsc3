#lang scribble/manual
@(require (for-label racket))

@title{RHPF}
 A resonant high pass filter.@section{related}
  Classes/Formlet, Classes/RLPF, Classes/Resonz, Classes/Ringz
@section{categories}
   UGens>Filters>Linear


@section{description}


A resonant high pass filter.


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

{ RHPF.ar(Saw.ar(200,0.1), FSinOsc.kr(XLine.kr(0.7,300,20), 0, 3600, 4000), 0.2) }.play;

(
{ 	var ctl = RHPF.kr(LFSaw.kr(2), SinOsc.kr(XLine.kr(0.07,30,20), 0, 35, 40), 0.05);
	SinOsc.ar(ctl * 200 + 500);
}.play;
)

::

]


