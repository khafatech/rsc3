#lang scribble/manual
@(require (for-label racket))

@title{NRand}
 Sum of uniform distributions.@section{related}
  Classes/ExpRand, Classes/IRand, Classes/LinRand, Classes/Rand, Classes/TExpRand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random

@section{description}


Generates a single random float value in a sum of

@racketblock[n::  uniform distributions from
]

@racketblock[lo::  to  ]

@racketblock[hi:: .


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

@section{argument}
 n

@section{table}
 

## n = 1: || Uniform distribution - same as link::Classes/Rand::.

## n = 2: || Triangular distribution.

## n = 3: || Smooth hump.

::

As

@racketblock[n::  increases, distribution converges
towards gaussian.


]
@section{Examples}
 


@racketblock[

(
SynthDef("help-NRand", { arg out=0, n=0;
	Out.ar(out,
		FSinOsc.ar(
			NRand(1200.0, 4000.0, n),
			0, Line.kr(0.2, 0, 0.01, doneAction: Done.freeSelf))
	)
}).add;
)

(
n = 0;
Routine({
	inf.do({ arg i;
		Synth.new("help-NRand", [\n, n]); 0.05.wait;
	})
}).play;
)

n = 1;
n = 2;
n = 4;

::

]


