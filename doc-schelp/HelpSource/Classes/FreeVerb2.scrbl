#lang scribble/manual
@(require (for-label racket))

@title{FreeVerb2}
 A two-channel reverb@section{categories}
  UGens>Reverbs
@section{related}
  Classes/FreeVerb, Classes/GVerb

@section{description}

Coded from experiments with faust.

@section{classmethods}
 

@section{method}
  ar
@section{argument}
  in
input signal channel 1.
@section{argument}
  in2
input signal channel 2.
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

// FreeVerb2 - demo synthdef
(
SynthDef(\FreeVerb2x2, {|outbus, mix = 0.25, room = 0.15, damp = 0.5, amp = 1.0|
	var signal;

	signal = In.ar(outbus, 2);

	ReplaceOut.ar(outbus,
		FreeVerb2.ar( // FreeVerb2 - true stereo UGen
			signal[0], // Left channel
			signal[1], // Right Channel
			mix, room, damp, amp)); // same params as FreeVerb 1 chn version

}).add;
)

// 2ch source
(
a = SynthDef(\src2x2, {
	Out.ar(0,
		Decay.ar(Impulse.ar(1), 0.25, LFCub.ar(1200, 0, 0.1)) ! 2 +
		Pan2.ar(
			Decay.ar(Impulse.ar(1, pi), 0.1, WhiteNoise.ar(0.1)),
			LFNoise1.kr(0.5).range(-1, 1)
		)
	)
}).play
)

// kick it in
z = Synth(\FreeVerb2x2, [\outbus, 0], addAction:\addToTail)
// experiment with some settings
z.set(\room, 0.7)
z.set(\mix, 0.33)
z.set(\damp, 0.9)

// silence
[a, z].do(_.free)

// crucial lib example
(
Patch({|mix =0.33, room = 0.25, damp = 0.7, amp = 0.1|
	var signal;

	signal = Decay.ar(Impulse.ar(1), 0.25, LFCub.ar(1200)) ! 2 +
		Pan2.ar(
			Decay.ar(Impulse.ar(1, pi), 0.1, WhiteNoise.ar),
			LFNoise1.kr(0.5).range(-1, 1)
		);

	FreeVerb2.ar(
		signal[0], // Left channel
		signal[1], // Right Channel
		mix, room, damp, amp)

}).gui
)
::
]


