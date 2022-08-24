#lang scribble/manual
@(require (for-label racket))

@title{InTrig}
 Generate a trigger anytime a bus is set.@section{categories}
   UGens>InOut, UGens>Triggers


@section{description}


Any time the bus is "touched", ie. has its value set (using "/c_set"
etc.), a single impulse trigger will be generated. Its amplitude is the
value that the bus was set to.

If the bus is set link::Classes/Bus#Synchronous Control Bus Methods#synchronously:: no trigger will be generated.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 bus
The index of the bus to read in from.

@section{argument}
 numChannels
The number of channels (i.e. adjacent buses) to read in. You
cannot modulate this number by assigning it to an argument in a
SynthDef.


@section{Examples}
 


@racketblock[
s = Server.local;
b = Bus.control(s,1);

SynthDef("help-InTrig",{arg out=0,busnum=0;
	var inTrig;
	inTrig = InTrig.kr( busnum );
	Out.ar(out,
		EnvGen.kr(Env.perc,gate: inTrig,levelScale: inTrig ) * SinOsc.ar
	)
}).play(s,[\out, 0, \busnum, b.index]);


b.set(1.0);

b.value = 1.0;

b.value = 0.2;

b.value = 0.1;
::

compare with link::Classes/In:: example.

]


