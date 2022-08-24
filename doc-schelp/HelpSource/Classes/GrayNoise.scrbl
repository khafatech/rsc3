#lang scribble/manual
@(require (for-label racket))

@title{GrayNoise}
 Gray Noise.@section{related}
  Classes/BrownNoise, Classes/ClipNoise, Classes/PinkNoise, Classes/WhiteNoise
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates noise which results from flipping random bits in a word.
This type of noise has a high RMS level relative to its peak to peak
level. The spectrum is emphasized towards lower frequencies.


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
SynthDef("help-GrayNoise", { arg out=0;
	Out.ar(out,
		GrayNoise.ar(0.1)
	)
}).play;
)

::

]


