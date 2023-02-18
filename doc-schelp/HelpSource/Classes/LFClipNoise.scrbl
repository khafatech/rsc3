#lang scribble/manual
@(require (for-label racket))

@title{LFClipNoise}
 Clipped noise@section{related}
  Classes/LFDClipNoise, Classes/LFDNoise0, Classes/LFDNoise1, Classes/LFDNoise3, Classes/LFNoise0, Classes/LFNoise1, Classes/LFNoise2
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Randomly generates the values -1 or +1 at a rate given by the nearest
integer division of the sample rate by the  
@racketblock[freq::
argument. It is probably pretty hard on your speakers!


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Approximate rate at which to generate random values.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

(
SynthDef("help-LFClipNoise", { arg out=0;
	Out.ar(out,
		LFClipNoise.ar(1000, 0.25)
	)
}).play;
)

//modulate frequency
(
SynthDef("help-LFClipNoise", { arg out=0;
	Out.ar(out,
		LFClipNoise.ar(XLine.kr(1000, 10000, 10), 0.25)
	)
}).play;
)

//use as frequency control
(
SynthDef("help-LFClipNoise", { arg out=0;
	Out.ar(out,
		SinOsc.ar(
			LFClipNoise.ar(4, 200, 600),
			0, 0.2
		)
	)
}).play;
)

::

]


