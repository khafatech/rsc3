#lang scribble/manual
@(require (for-label racket))

@title{WhiteNoise}
 White noise.@section{related}
  Classes/BrownNoise, Classes/GrayNoise, Classes/ClipNoise, Classes/PinkNoise
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates noise whose spectrum has equal power at all frequencies.


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

(
SynthDef("help-WhiteNoise", { arg out=0;
	Out.ar(out,
		WhiteNoise.ar(0.25)
	)
}).play;
)

::

]


