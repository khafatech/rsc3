#lang scribble/manual
@(require (for-label racket))

@title{IRand}
 Single integer random number generator.@section{related}
  Classes/ExpRand, Classes/LinRand, Classes/NRand, Classes/Rand, Classes/TExpRand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random

@section{description}


Generates a single random integer value in uniform distribution from

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
SynthDef("help-IRand", {
	Out.ar(
		IRand(0, 1), //play on random channel between 0 and 1
		FSinOsc.ar(500,
			0, Line.kr(0.2, 0, 0.1, doneAction: Done.freeSelf))
	)
}).add;
)

(
Routine({
	16.do({
		Synth.new("help-IRand"); 0.5.wait;
	})
}).play;
)
::

]


