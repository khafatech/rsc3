#lang scribble/manual
@(require (for-label racket))

@title{Understanding Streams, Patterns and Events - Part 3}
 ListPatterns@section{related}
  Tutorials/Streams-Patterns-Events1, Tutorials/Streams-Patterns-Events2, Tutorials/Streams-Patterns-Events4, Tutorials/Streams-Patterns-Events5, Tutorials/Streams-Patterns-Events6, Tutorials/Streams-Patterns-Events7
@section{categories}
  Tutorials>Streams-Patterns-Events

@section{section}
 ListPatterns

ListPatterns are link::Classes/Pattern::s that iterate over arrays of objects in some fashion. All ListPatterns have in common the instance variables list and repeats. The list variable is some link::Classes/Array:: to be iterated over. The repeats variable is some measure of the number of times to do something, whose meaning varies from subclass to subclass. The default value for repeats is 1.

A link::Classes/Pseq:: is a Pattern that cycles over a list of values. The repeats variable gives the number of times to repeat the entire list.


@racketblock[
//////////////////////////////////////////////////////////////
// Note: This SynthDef used throughout this document
(
s = Server.local;
SynthDef( \help_SPE3_SimpleSine, {
	arg freq, sustain=1.0;
	var osc;
	osc = SinOsc.ar( [freq,freq+0.05.rand] ) * EnvGen.ar(
		Env.perc, doneAction: Done.freeSelf, levelScale: 0.3, timeScale: sustain
	);
	Out.ar(0,osc);
}).add;
)
//////////////////////////////////////////////////////////////

(
var a, b;
a = Pseq.new(#[1, 2, 3], 2);	// repeat twice
b = a.asStream;
7.do({ b.next.postln; });
)
::

Pseq also has an offset argument which gives a starting offset into the list.

]

@racketblock[
(
var a, b;
a = Pseq.new(#[1, 2, 3, 4], 3, 2);	// repeat 3, offset 2
b = a.asStream;
13.do({ b.next.postln; });
)
::

You can pass a function for the repeats variable that gets evaluated when the stream is created.

]

@racketblock[
(
var a, b;
a = Pseq.new(#[1, 2], { rrand(1, 3) });	// repeat 1,2, or 3 times
b = a.asStream;
7.do({ b.next.postln; });
)
::

If you specify the value ]

@racketblock[inf:: for the repeats variable, then it will repeat indefinitely.

]

@racketblock[
(
var a, b;
a = Pseq.new(#[1, 2, 3], inf);	// infinite repeat
b = a.asStream;
10.do({ b.next.postln; });
)
::

Pseq used as a sequence of pitches:

Remember that math operations like ]

@racketblock[midicps:: can be used on streams.

The alternative ]

@racketblock[Pseq(...).midicps.asStream:: is also possible because both pattern and stream inherit from link::Classes/AbstractFunction:: for which midicps is a method. ( midicps converts a midi value to cycles per second or Hz )

]

@racketblock[
(
var a, d;
a = Pseq(#[60, 61, 63, 65, 67, 63], inf ).asStream.midicps;
d = 0.3;
Task({
	12.do({
		Synth(\help_SPE3_SimpleSine, [ \freq, a.next, \sustain, d ]);
		d.wait;
	});
}).play
)
::

link::Classes/Pser:: is like Pseq, however the repeats variable gives the number of items returned instead of the number of complete cycles.

]

@racketblock[
(
var a, b;
a = Pser.new(#[1, 2, 3], 5);	// return 5 items
b = a.asStream;
6.do({ b.next.postln; });
)
::

link::Classes/Prand:: returns one item from the list at random for each repeat.

]

@racketblock[
(
var a, b;
a = Prand.new(#[1, 2, 3, 4, 5], 6);	// return 6 items
b = a.asStream;
7.do({ b.next.postln; });
)
::

Prand used as a sequence of pitches:

]

@racketblock[
(
var a, d;
a = Prand(#[60, 61, 63, 65], inf).midicps.asStream;
d = 0.3;
Task({
	12.do({
		Synth(\help_SPE3_SimpleSine,[\freq, a.next]);
		d.wait;
	});
}).play;
)
::

link::Classes/Pxrand::, like Prand, returns one item from the list at random for each repeat, but Pxrand never repeats the same element twice in a row.

]

@racketblock[
(
var a, b;
a = Pxrand.new(#[1, 2, 3], 10);	// return 10 items
b = a.asStream;
11.do({ b.next.postln; });
)
::

Pxrand used as a sequence of pitches:

]

@racketblock[
(
var a;
a = Pxrand(#[60, 61, 63, 65], inf).midicps.asStream;
Task({
	12.do({
		Synth(\help_SPE3_SimpleSine, [\freq, a.next]);
		0.8.wait;
	});
}).play;
)
::

link::Classes/Pshuf:: iterates over the list in scrambled order. The entire scrambled list is repeated in the same order the number of times given by the repeats variable.

]

@racketblock[
(
var a, b;
a = Pshuf.new(#[1, 2, 3, 4], 3);
b = a.asStream;
13.do({ b.next.postln; });
)
::

Pshuf used as a sequence of pitches:

]

@racketblock[
(
var a, b;
a = Pshuf(#[60, 61, 65, 67], inf).midicps.asStream;
Task({
	12.do({
		Synth(\help_SPE3_SimpleSine,[\freq, a.next]);
		0.5.wait;
	});
}).play;
)
::

]
@section{section}
 Nesting Patterns

If a link::Classes/Pattern:: encounters another Pattern in its list, it embeds that pattern in its output. That is, it creates a stream on that pattern and iterates that pattern until it ends before moving on.

For example here is one pattern nested in another.


@racketblock[
(
var a, b;
a = Pseq.new([1, Pseq.new([100,200], 2), 3], 3);
b = a.asStream;
19.do({ b.next.postln; });
)
::

Pseqs nested in a Prand:

]

@racketblock[
(
var a, b;
a = Prand.new([
		Pseq.new([1, 2], 2),
		Pseq.new([3, 4], 2),
		Pseq.new([5, 6], 2)
	], 3);
b = a.asStream;
13.do({ b.next.postln; });
)
::

Nested sequences of pitches:

]

@racketblock[
(
var a;
a = Prand([
		Pseq(#[60, 61, 63, 65, 67, 63]),
		Prand(#[72, 73, 75, 77, 79], 6),
		Pshuf(#[48, 53, 55, 58], 2)
	], inf
).midicps.asStream;
Task({
	loop({
		Synth( \help_SPE3_SimpleSine, [\freq, a.next] );
		0.3.wait;
	});
}).play;
)
::

]
@section{section}
 Math operations on ListPatterns

Pattern 
@racketblock[b:: plays pattern a once normally, once transposed up a fifth and once transposed up a fourth.

]

@racketblock[
(
var a, b;
a = Pseq(#[60, 62, 63, 65, 67, 63]);
b = Pseq([ a, a + 7, a + 5], inf).asStream;
Task({
	24.do({
		Synth(\help_SPE3_SimpleSine, [ \freq, b.next.midicps ]);
		0.3.wait;
	});
}).play;
)
::

Adding two patterns together. The second pattern transposes each fifth note of the first pattern down an octave.

]

@racketblock[
(
var a;
a = Pseq(#[60, 62, 63, 65, 67, 63], inf) + Pseq(#[0, 0, 0, 0, -12], inf);
a = a.asStream.midicps;
Task({
	25.do({
		Synth(\help_SPE3_SimpleSine,[\freq, a.next]);
		0.3.wait;
	});
}).play;
)
::

]
@section{section}
 Making Music with ListPatterns

Here is the same example given in part 2 rewritten to use ListPatterns. It uses nested patterns and results in much more concise code. SuperCollider allows you to write 
@racketblock[SomeClass.new(params):: as ]

@racketblock[SomeClass(params):: eliminating the ".new". This can make code like the pattern examples below, which create a lot of objects, more readable.

]

@racketblock[
(
SynthDef( \help_SPE3_Allpass6, { arg freq;
	var out, env;
	out = RLPF.ar(
		LFSaw.ar( freq, mul: EnvGen.kr( Env.perc, levelScale: 0.3, doneAction: Done.freeSelf ) ),
		LFNoise1.kr(1, 36, 110).midicps,
		0.1
	);
	6.do({ out = AllpassN.ar(out, 0.05, [0.05.rand, 0.05.rand], 4) });
	Out.ar( 0, out );
}).add
)

(
var freqStream;

freqStream = Pseq([
	Prand([
		nil,	// a nil item reached in a pattern causes it to end
		Pseq(#[24, 31, 36, 43, 48, 55]);
	]),
	Pseq([ 60, Prand(#[63, 65]), 67, Prand(#[70, 72, 74]) ], { rrand(2, 5) }),
	Prand(#[74, 75, 77, 79, 81], { rrand(3, 9) })
], inf).asStream.midicps;

Task({
	loop({
		Synth( \help_SPE3_Allpass6, [\freq, freqStream.next ]);
		0.13.wait;
	});
}).play;
)
::

Here is an example that uses a Pattern to create a rhythmic solo. The values in the pattern specify the amplitudes of impulses fed to the link::Classes/Decay2:: generator.

]

@racketblock[
(
SynthDef( \help_SPE3_Mridangam, { arg t_amp;
	var out;

	out = Resonz.ar(
		WhiteNoise.ar(70) * Decay2.kr( t_amp, 0.002, 0.1 ),
		60.midicps,
		0.02,
		4
	).distort * 0.4;

	Out.ar( 0, out );
	DetectSilence.ar( out, doneAction: Done.freeSelf );
}).add;

SynthDef( \help_SPE3_Drone, {
	var out;
	out = LPF.ar(
		Saw.ar([60, 60.04].midicps)
		+
		Saw.ar([67, 67.04].midicps),
		108.midicps,
		0.007
	);
	Out.ar( 0, out );
}).add;
)

(
// percussion solo in 10/8

var stream, pat, amp;

pat = Pseq([
	Pseq(#[0.0], 10),

	// intro
	Pseq(#[0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], 2),
	Pseq(#[0.9, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0], 2),
	Pseq(#[0.9, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0, 0.2, 0.0, 0.0], 2),
	Pseq(#[0.9, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.2, 0.0, 0.2], 2),

	// solo
	Prand([
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.0, 0.2, 0.0, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.2, 0.0, 0.7, 0.0, 0.2, 0.0, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.0, 0.2, 0.0, 0.7, 0.0, 0.2]),
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.2, 0.2, 0.0, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.0, 0.2, 0.2, 0.7, 0.2, 0.0]),
		Pseq(#[0.9, 0.2, 0.2, 0.7, 0.2, 0.2, 0.2, 0.7, 0.2, 0.2]),
		Pseq(#[0.9, 0.2, 0.2, 0.7, 0.2, 0.2, 0.2, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.2, 0.2, 0.2, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.4, 0.0, 0.4, 0.0, 0.4, 0.0, 0.4, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.4, 0.0, 0.0, 0.4, 0.2, 0.4, 0.2]),
		Pseq(#[0.9, 0.0, 0.2, 0.7, 0.0, 0.2, 0.0, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.7, 0.0, 0.0, 0.0, 0.7, 0.0, 0.0]),
		Pseq(#[0.9, 0.7, 0.7, 0.0, 0.0, 0.2, 0.2, 0.2, 0.0, 0.0]),
		Pseq(#[0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
	], 30),

	// tehai : 7 beat motif 3 times sharing 1st beat with next 7x3
	// and again the third time:
	//   123456712345671234567                   123456712345671234567
	//                       123456712345671234567
	//   !                   !                   !                   !
	//   1234567890123456789012345678901234567890123456789012345678901
	Pseq(#[2.0, 0.0, 0.2, 0.5, 0.0, 0.2, 0.9,
		1.5, 0.0, 0.2, 0.5, 0.0, 0.2, 0.9,
		1.5, 0.0, 0.2, 0.5, 0.0, 0.2], 3),
	Pseq(#[5], 1),	// sam

	Pseq(#[0.0], inf)
]);

stream = pat.asStream;

Task({
	Synth(\help_SPE3_Drone);
	loop({
		if( ( amp = stream.next ) > 0,
			{ Synth(\help_SPE3_Mridangam, [ \t_amp, amp ]) }
		);
		(1/8).wait;
	})
}).play
)
::

To go to the next file:
link::Tutorials/Streams-Patterns-Events4::
]


