#lang scribble/manual
@(require (for-label racket))

@title{XFade2}
 Equal power two channel cross fade.@section{related}
  Classes/LinXFade2
@section{categories}
   UGens>Multichannel>Select


@section{description}


Two channel equal power crossfader.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 inA

Input signal A.


@section{argument}
 inB

Input signal B.


@section{argument}
 pan

Crossfade position from -1 to +1.


@section{argument}
 level

A control rate level input.


@section{Examples}
 


@racketblock[

(
SynthDef("help-XFade2", {
	Out.ar(0, XFade2.ar( Saw.ar, SinOsc.ar , LFTri.kr(0.1) ));
}).play
)

::

]


