#lang scribble/manual
@(require (for-label racket))

@title{BrownNoise}
 Brown Noise.@section{related}
  Classes/ClipNoise, Classes/GrayNoise, Classes/PinkNoise, Classes/WhiteNoise
@section{categories}
   UGens>Generators>Stochastic

@section{description}

Generates noise whose spectrum falls off in power by 6 dB per octave.

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
 

compare:

@racketblock[
{ BrownNoise.ar(0.1) }.play;
{ WhiteNoise.ar(0.1) }.play;
::

brownian noise as a frequency modulator:
]

@racketblock[
{ SinOsc.ar(BrownNoise.ar(100, 200)) * 0.1 }.play;
::

filtered brown noise:
]

@racketblock[
{ BPF.ar(BrownNoise.ar(0.1.dup), MouseX.kr(40, 17000, 1), 0.2) }.play;
::
]


