#lang scribble/manual
@(require (for-label racket))

@title{Dust2}
 Random impulses.@section{related}
  Classes/Dust
@section{categories}
   UGens>Generators>Stochastic


@section{description}


Generates random impulses from -1 to +1.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 density
Average number of impulses per second.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

(
SynthDef("help-Dust2", { arg out=0;
	Out.ar(out,
		Dust2.ar(200, 0.5)
	)
}).play;
)

(
SynthDef("help-Dust2", { arg out=0;
	Out.ar(out,
		Dust2.ar(XLine.kr(20000, 2, 10), 0.5)
	)
}).play;
)

::

]


