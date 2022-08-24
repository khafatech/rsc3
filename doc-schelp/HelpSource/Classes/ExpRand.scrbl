#lang scribble/manual
@(require (for-label racket))

@title{ExpRand}
 Exponential single random number generator.@section{related}
  Classes/IRand, Classes/LinRand, Classes/NRand, Classes/Rand, Classes/TExpRand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random

@section{description}


Generates a single random float value in an exponential distributions
from  
@racketblock[lo::  to  ]

@racketblock[hi:: . It generates
this when the SynthDef first starts playing, and remains fixed for
the duration of the synth's existence.


]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 lo
Lower limit of the output range.

@section{argument}
 hi
Upper limit of the output range.

@section{Examples}
 


@racketblock[

(
SynthDef("help-ExpRand", { arg out=0, n=0;
	Out.ar(out,
		FSinOsc.ar(
			ExpRand(100.0, 8000.0, n),
			0, Line.kr(0.2, 0, 0.01, doneAction: Done.freeSelf))
	)
}).add;
)

(
Routine({
	inf.do({ arg i;
		Synth.new("help-ExpRand"); 0.05.wait;
	})
}).play;
)

::

]


