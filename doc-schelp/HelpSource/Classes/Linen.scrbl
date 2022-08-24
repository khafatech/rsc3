#lang scribble/manual
@(require (for-label racket))

@title{Linen}
 Simple linear envelope generator.@section{categories}
   UGens>Envelopes
@section{related}
  Classes/EnvGen

@section{description}


Simple linear envelope generator.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 gate

This triggers the envelope and holds it open while > 0.

If strong::gate:: < 0, force release with time 
@racketblock[ -1.0 - gate ::. See link::Classes/EnvGen#Forced release::.

]
@section{argument}
 attackTime

The duration of the attack portion.


@section{argument}
 susLevel

The level of the sustain portion.


@section{argument}
 releaseTime

The duration of the release portion.


@section{argument}
 doneAction

An integer representing an action to be executed when the
envelope is finished. See
link::Classes/Done::  for
more detail.


@section{Examples}
 


@racketblock[

// trigged
(
SynthDef("help-Linen",{ arg out = 0;
	Out.ar(out,
		Linen.kr(Impulse.kr(2), 0.01, 0.6, 1.0, doneAction: Done.none) * SinOsc.ar(440, 0, 0.1)
	)
}).play;
)

// play once and end the synth
(
SynthDef("help-Linen",{ arg out=0;
	Out.ar(out,
		Linen.kr(Impulse.kr(0), 0.01, 0.6, 1.0, doneAction: Done.freeSelf) * SinOsc.ar(440, 0, 0.1)
	)
}).play;
)

// play once and sustain
(
x = SynthDef("help-Linen",{ arg gate = 1, out = 0; // use gate arg for release
	Out.ar(out,
		Linen.kr(gate, 0.01, 0.6, 1.0, doneAction: Done.freeSelf) * SinOsc.ar(440, 0, 0.1)
	)
}).play;
)
x.release(4); // change the release time

// longer gate, can pass in duration
(
SynthDef("help-Linen",{ arg out = 0, dur = 0.1;
	var gate;
	gate = Trig.kr(1.0, dur);
	Out.ar(out,
		Linen.kr(gate, 0.01, 0.6, 1.0, doneAction: Done.freeSelf) * SinOsc.ar(440, 0, 0.1)
	)
}).play(nil, [\out, 0, \dur, 2.0]);
)



// used below in a Routine varying the releaseTime
(
SynthDef("help-Linen",{ arg out=0,freq=440,attackTime=0.01,susLevel=0.6,releaseTime=0.1;
	Out.ar(out,
		Linen.kr(Impulse.kr(0), attackTime, susLevel, releaseTime, doneAction: Done.freeSelf)
			* SinOsc.ar(freq, 0, 0.1)
	)
}).add;
)

(
// debussey sleeping through math class
x = Pbrown(0.01, 2.0, 0.2, inf).asStream;
Routine({
	loop({
		Synth.grain("help-Linen",[\freq, (rrand(20, 50) * 2).midicps, \releaseTime, x.next]);
		0.25.wait;
	})
}).play(TempoClock.default)
)





(
SynthDef("help-Linen",{ arg out = 0;
	Out.ar(out,

		Linen.kr(Impulse.kr(2),
			0.01,
			// sustain level is polled at time of trigger
			FSinOsc.kr(0.1).range(0, 1),
			1.0,
			doneAction: Done.none)

			* SinOsc.ar(440, 0, 0.1)
	)
}).play;
)

::
]


