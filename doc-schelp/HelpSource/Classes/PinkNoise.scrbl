#lang scribble/manual
@(require (for-label racket))

@title{PinkNoise}
 Pink Noise.@section{related}
  Classes/BrownNoise, Classes/GrayNoise, Classes/ClipNoise, Classes/WhiteNoise
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates noise whose spectrum falls off in power by 3 dB per octave, which gives equal power over the span of each octave.
This version is band-limited to 8 octaves.

Internally, this UGen calculates its output by means of the Voss-McCartney algorithm.
link::http://www.firstpr.com.au/dsp/pink-noise/allan-2/spectrum2.html::

@section{note}
 
The values produced by this UGen were observed to lie with very high probability between approximately -0.65 and +0.81 (before being multiplied by mul). The signal's RMS is approximately -16 dB.
::


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
SynthDef("help-PinkNoise", { arg out=0;
	Out.ar(out,
		PinkNoise.ar(0.4)
	)
}).play;
)

::

]


