#lang scribble/manual
@(require (for-label racket))

@title{LinRand}
 Skewed random number generator.@section{related}
  Classes/ExpRand, Classes/IRand, Classes/NRand, Classes/Rand, Classes/TExpRand, Classes/TIRand, Classes/TRand
@section{categories}
  UGens>Random

@section{description}


Generates a single random float value in linear distribution from

@racketblock[lo::  to  ]

@racketblock[hi:: , skewed towards
]

@racketblock[lo::  if  ]

@racketblock[minmax::  < 0, otherwise
skewed towards  ]

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
 minmax
The output is skewed towards 
@racketblock[lo:: if ]

@racketblock[minmax:: < 0, otherwise skewed towards ]

@racketblock[hi::.

]
@section{Examples}
 


@racketblock[

(
SynthDef("help-LinRand", { arg out=0, minmax=1;
	Out.ar(out,
		FSinOsc.ar(
			LinRand(200.0, 10000.0, minmax),
			0, Line.kr(0.2, 0, 0.01, doneAction: Done.freeSelf))
	)
}).add;
)

//towards hi
(
Routine({
	loop({
		Synth.new("help-LinRand"); 0.04.wait;
	})
}).play;
)

//towards lo (doesn't work like that yet)
(
Routine({
	loop({
		Synth.new("help-LinRand", [\minmax, -1]); 0.04.wait;
	})
}).play;
)

::

]


