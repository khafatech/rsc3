#lang scribble/manual
@(require (for-label racket))

@title{ClipNoise}
 Clip Noise.@section{related}
  Classes/BrownNoise, Classes/GrayNoise, Classes/PinkNoise, Classes/WhiteNoise
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates noise whose values are either -1 or 1. This produces
the maximum energy for the least peak to peak amplitude.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ ClipNoise.ar(0.2) }.play;
::

]


