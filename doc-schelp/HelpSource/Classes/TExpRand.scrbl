#lang scribble/manual
@(require (for-label racket))

@title{TExpRand}
 Triggered exponential random number generator.@section{related}
  Classes/ExpRand, Classes/IRand, Classes/LinRand, Classes/NRand, Classes/Rand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random, UGens>Triggers


@section{description}


Generates a random float value in exponential distribution from

@racketblock[lo::  to  ]

@racketblock[hi::  each time the
trigger signal changes from nonpositive to positive values
]

@racketblock[lo::  and  ]

@racketblock[hi::  must both have
the same sign and be non-zero.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 lo
Lower limit of the output range.

@section{argument}
 hi
Upper limit of the output range.

@section{argument}
 trig

The trigger. Trigger can be any signal. A trigger happens when
the signal changes from non-positive to positive.


@section{Examples}
 


@racketblock[

(
{
	var trig = Dust.kr(10);
	SinOsc.ar(
			TExpRand.kr(300.0, 3000.0, trig)
		) * 0.1
}.play;
)

(
{
	var trig = Dust.ar(MouseX.kr(1, 8000, 1));
	SinOsc.ar(
			TExpRand.ar(300.0, 3000.0, trig)
		) * 0.1
}.play;
)

::

]


