#lang scribble/manual
@(require (for-label racket))

@title{FreeVerb}
 A reverb@section{categories}
  UGens>Reverbs
@section{related}
  Classes/FreeVerb2, Classes/GVerb

@section{description}

Coded from experiments with faust.

@section{classmethods}
 

@section{method}
  ar
@section{argument}
  in
input signal.
@section{argument}
  mix
dry/wet balance. range 0..1.
@section{argument}
  room
room size. rage 0..1.
@section{argument}
  damp
Reverb HF damp. range 0..1.
@section{argument}
  mul
@section{argument}
  add

@section{discussion}
 
Valid parameter range from 0 to 1. Values outside this range are clipped by the UGen.

@section{examples}
 

@racketblock[
s.boot;

// FreeVerb - 1x1 ugen
(
z = SynthDef(\src, {|mix = 0.25, room = 0.15, damp = 0.5|
	Out.ar(0,
		FreeVerb.ar(
			Decay.ar(Impulse.ar(1), 0.25, LFCub.ar(1200, 0, 0.1)), // mono src
			mix, // mix 0-1
			room, // room 0-1
			damp // damp 0-1 duh
		) ! 2 // fan out...
	);
}).play
)
z.set(\room, 0.7)
z.set(\mix, 0.4)
z.set(\damp, 0.2)

z.free

// it expands as any ugen does
(
z = SynthDef(\src, {|mix = 0.25, room = 0.15, damp = 0.5|
	Out.ar(0,
		FreeVerb.ar(
			Pan2.ar(
				Decay.ar(Impulse.ar(1), 0.25, LFCub.ar(1200, 0, 0.1)),
				LFNoise1.ar(1).range(-1, 1)
			),
			mix,
			room,
			damp
		)
	);
}).play
)
z.set(\room, 0.7)
z.set(\mix, 0.4)
z.set(\damp, 0.2)

z.free
::
]


