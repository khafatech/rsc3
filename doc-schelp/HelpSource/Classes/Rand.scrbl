#lang scribble/manual
@(require (for-label racket))

@title{Rand}
 Single random number generator.@section{related}
  Classes/ExpRand, Classes/IRand, Classes/LinRand, Classes/NRand, Classes/TExpRand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random

@section{description}


Generates a single random float value in uniform distribution from

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
SynthDef("help-Rand", { arg out=0;
	Out.ar(out,
		FSinOsc.ar(
			Rand(200.0, 400.0),
			0, Line.kr(0.2, 0, 1, doneAction: Done.freeSelf))
	)
}).add;
)

(
Routine({
	8.do({
		Synth.new("help-Rand"); 1.0.wait;
	})
}).play;
)

::

]


