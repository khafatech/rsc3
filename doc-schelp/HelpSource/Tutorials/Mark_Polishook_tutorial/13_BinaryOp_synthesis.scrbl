#lang scribble/manual
@(require (for-label racket))

@title{13_BinaryOp_synthesis}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Binary messages

The pattern for a binary message is

@section{table}
 
## RECEIVER || OPERATOR || OPERAND
::

For example


@racketblock[
2 * 3
::

is a receiver (the object to which a message is sent), a binary operator, and an operand.

]
@section{section}
 Mixing = addition

Use addition (a binary operation) to mix two or more ugens.


@racketblock[
(
	// mix 2 sawtooth waves
	{
		Saw.ar(500, 0.05)	// receiver
		+			// operator
		Saw.ar(600, 0.06)	// operand
	}.scope;
)


(
// mix 3 unit generators.
	{
		Saw.ar(500, 0.05)	// receiver
		+			// operator
		Saw.ar(600, 0.06)	// operand
						// when evaluated produce
						// a BinaryOpUGen
						// this BinaryOpUGen is then a receiver for an
		+				// addition operator followed by
		Saw.ar(700, 0.07)		// an operand
	}.scope;
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

Rewrite the previous example with the Mix ugen.

]

@racketblock[
(
{
	Mix.ar(
		// the ugens that will be mixed go into an array
		[
			Saw.ar(500, 0.05),
			Saw.ar(600, 0.06),
			Saw.ar(700, 0.06)
		]
	)
}.scope
)
::

Or use Mix.arFill to create the same result.

]

@racketblock[
{ Mix.arFill(3, { arg i; Saw.ar(500 + (i * 100), 0.05) }) }.scope;
::

Every time the function is evaluated, the argument i is incremented. So i equals 0 the first time the function is evaluated, i equals 1 the second time, i equals 2, the third time, and so on.

]
@section{section}
 Scaling = multiplication

Apply an envelope, in the form of a low-frequency sine wave, to a WhiteNoise generator.


@racketblock[
{ WhiteNoise.ar(0.1) * SinOsc.kr(1, 1) }.scope;

(
	// scaling and mixing
	// ... imitates a train?
	{
		(WhiteNoise.ar(0.1) * SinOsc.kr(1, 1))
		+
		(BrownNoise.ar(0.1) * SinOsc.kr(2, 1))

	}.scope;
)
::

]
@section{section}
 Envelopes

Dynamically modulate any parameter in a ugen (such as frequency, phase, or amplitude) with an envelope.


@racketblock[
// modulate amplitude
{ SinOsc.ar(440, 0, 0.1) * EnvGen.kr(Env.sine(1), doneAction: Done.freeSelf) }.scope;
::

Setting the doneAction argument (control) to 2 insures that after the envelope reaches its endpoint, SuperCollider will release the memory it used for the instances of the SinOsc and the EnvGen.

]
@section{section}
 Keyword arguments

Keywords make code easier to read and they allow arguments to be presented in any order. Here, the doneAction and the timeScale arguments are expressed in keyword style.


@racketblock[
(
SynthDef("timeScale", { arg ts = 1;
	Out.ar(
		0,
		SinOsc.ar(440, 0, 0.4)
		*
		EnvGen.kr(
			Env.sine(1),
			doneAction: Done.freeSelf,
			timeScale: ts	// scale the duration of an envelope
		)
	)
}).add;
)

Synth("timeScale", [\ts, 0.1]); // timeScale controls the duration of the envelope
::

////////////////////////////////////////////////////////////////////////////////////////////////////

]

@racketblock[
// scale the duration of the envelope for every new synth
(
r = Routine({
	loop({
		Synth("timeScale", [\ts, 0.01.rrand(0.3)]);
		0.5.wait;
	})
});
)
r.play
::

]
@section{section}
 Additive Synthesis

Additive synthesis is as its name says. Components are added (mixed) together.


@racketblock[
(
{	// evaluate the function 12 times
	var n = 12;
	Mix.arFill(
		n,
		{
			SinOsc.ar(
				[67.0.rrand(2000), 67.0.rrand(2000)],
				0,
				n.reciprocal * 0.75
			)
		}
	)
	*
	EnvGen.kr(Env.perc(11, 6), doneAction: Done.freeSelf)
}.scope
)
::

]
@section{section}
 Envelopes

The promise of additive synthesis is that one can add sine waves to create any sound that can be imagined.

The problem of additive synthesis is that each and every sine wave and their envelopes have to be specified explicitly.

Create nuanced textures by scaling sine waves with envelopes and then mixing the result.


@racketblock[
(
{	var n = 12;

	Mix.arFill(
			n,						// generate n sine waves
			{
			SinOsc.ar(					// each with a possible frequency between
				[67.0.rrand(2000), 67.0.rrand(2000)],	// low.rrand(high) ... floating point values
				0,
				n.reciprocal				// scale the amplitude of each sine wave
									// according to the value of n
			)
			*
			EnvGen.kr(					// put an envelope on each of the sine waves
				Env.sine(2.0.rrand(17)),
				doneAction: Done.none 				// deallocate envelopes only when the
									// entire sound is complete (why?)
			)
		}
	)
	*								// put an envelope over the whole patch
	EnvGen.kr(
		Env.perc(11, 6),
		doneAction: Done.freeSelf,
		levelScale: 0.75
	)

}.scope
)
::

(Or use the link::Classes/Klang:: ugen to produce a similar effect).

]
@section{section}
 Ring modulation

Multiply two UGens.


@racketblock[
{ SinOsc.ar(440, 0, 0.571) * SinOsc.kr(880) }.scope

// use an lfo to modulate the amplitude of the modulator
(
	{
		SinOsc.ar(440, 0, 0.571)
		*
		(SinOsc.kr(880)				// wrap the modulator and the lfo in parenthese
		* 					// why ... ?
		SinOsc.kr([6.99, 8.01].reciprocal)
		)
	}.scope
)
::

]
@section{section}
 Amplitude modulation

Multiply two UGens and restrict the value of the modulator to positive values (use the .abs message to calculate 'absolute' value) to create what Charles Dodge calls "classic" amplitude modulation.


@racketblock[
// use an lfo to modulate the amplitude of the modulator
(
	{
		SinOsc.ar(440, 0, 0.571)
		*
		(SinOsc.kr(880).abs			// wrap the modulator and the lfo in parenthese
		* 					// why ... ?
		SinOsc.kr([6.99, 8.01].reciprocal)
		)
	}.scope
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

Compare "classic" amplitude modulation and ring modulation

]

@racketblock[
// "classic"
{ SinOsc.ar(440, 0, 0.571) * SinOsc.kr(880).abs }.scope

// "ring"
// ... what's the difference?
{ SinOsc.ar(440, 0, 0.571) * SinOsc.kr(880) }.scope
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/14_Subtractive_synthesis::
]


