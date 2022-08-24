#lang scribble/manual
@(require (for-label racket))

@title{LinXFade2}
 Two channel linear crossfade.@section{related}
  Classes/XFade2
@section{categories}
   UGens>Multichannel>Select


@section{description}


Two channel linear crossfader.


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

Cross fade position from -1 to +1.


@section{argument}
 level

A control rate level input.


@section{Examples}
 


@racketblock[

play({ LinXFade2.ar(FSinOsc.ar(800, 0, 0.2), PinkNoise.ar(0.2), FSinOsc.kr(1)) });

::

]


