#lang scribble/manual
@(require (for-label racket))

@title{PMOsc}
 Phase modulation oscillator pair.@section{categories}
   UGens>Generators>Deterministic


@section{description}


Phase modulation sine oscillator pair.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 carfreq
Carrier frequency in cycles per second.

@section{argument}
 modfreq
Modulator frequency in cycles per second.

@section{argument}
 pmindex
Modulation index in radians.

@section{argument}
 modphase
A modulation input for the modulator's phase in radians.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

play({ PMOsc.ar(Line.kr(600, 900, 5), 600, 3, 0, 0.1) }); // modulate carfreq

play({ PMOsc.ar(300, Line.kr(600, 900, 5), 3, 0, 0.1) }); // modulate modfreq

play({ PMOsc.ar(300, 550, Line.ar(0,20,8), 0, 0.1) }); // modulate index

(
e = Env.linen(2, 5, 2);
Routine.run({
	loop({
		play({
			LinPan2.ar(EnvGen.ar(e) *
				PMOsc.ar(2000.0.rand,800.0.rand, Line.kr(0.0, 12.0.rand,9),0,0.1), 1.0.rand2)});
		2.wait;
	})
}))

::
]


