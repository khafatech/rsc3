#lang scribble/manual
@(require (for-label racket))

@title{CoinGate}
 Statistical gate.@section{categories}
   UGens>Generators>Stochastic


@section{description}


When CoinGate receives a trigger, it tosses a coin
and either passes the trigger or doesn't.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 prob

Value between 0.0 and 1.0 determines probability of either possibilities.


@section{argument}
 in
The trigger input.

@section{Examples}
 


@racketblock[

(
a = SynthDef("help-TCoin", { arg out=0, prob=0.5;
	var trig;
	trig = CoinGate.kr(prob, Impulse.kr(10));
	Out.ar(out,
		SinOsc.ar(
			TRand.kr(300.0, 400.0, trig),0,0.2
		)
	)
}).play;
)

a.set(\prob, 1.0);
a.set(\prob, 0.0);
a.set(\prob, 0.1);


(
a = SynthDef("help-TCoin", { arg out=0, prob=0.5;
	var trig;
	trig = Impulse.ar(20, 0, SinOsc.kr(0.5,0,1,1));
	Out.ar(out,
		Mix.fill(3, {Ringz.ar(CoinGate.ar(prob, trig*0.5), #[1,1.5]*Rand(1000, 9000), 0.01)})
	)
}).play;
)

a.set(\prob, 1.0);
a.set(\prob, 0.0);
a.set(\prob, 0.1);

::
]


