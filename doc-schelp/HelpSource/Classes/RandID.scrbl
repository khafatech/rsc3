#lang scribble/manual
@(require (for-label racket))

@title{RandID}
 Set the synth's random generator ID.@section{related}
  Classes/RandSeed
@section{categories}
   UGens>Generators>Stochastic, UGens>Random


@section{description}


Choose which random number generator to use for this synth.
All synths that use the same generator reproduce the same sequence
of numbers when the same seed is set again.


@section{classmethods}
 

@section{method}
 kr, ir

@section{argument}
 id
The random number generator ID.

@section{Examples}
 


@racketblock[

//start a noise patch and set the id of the generator
(
SynthDef("help-RandID", { arg out=0, id=1;
	RandID.ir(id);
	Out.ar(out,
		WhiteNoise.ar(0.05) + Dust2.ar(70)
	)
}).add;
)

//reset the seed of my rgen at a variable rate
(
SynthDef("help-RandSeed", { arg seed=1910, id=1;
		RandID.kr(id);
		RandSeed.kr(Impulse.kr(FSinOsc.kr(0.2, 0, 10, 11)), seed);
}).add;

)

//start two noise synths on left and right channel with a different randgen id
a = Synth("help-RandID", [\out, 0, \id, 1]);
b = Synth("help-RandID", [\out, 1, \id, 2]);

//reset the seed of randgen 1
x = Synth("help-RandSeed", [\id, 1]);

//change the target randgen to 2 (affects right channel)
x.set(\id, 2);

::

]


