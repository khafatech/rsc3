#lang scribble/manual
@(require (for-label racket))

@title{Understanding Streams, Patterns and Events - Part 5}
 Event.default@section{related}
  Tutorials/Streams-Patterns-Events1, Tutorials/Streams-Patterns-Events2, Tutorials/Streams-Patterns-Events3, Tutorials/Streams-Patterns-Events4, Tutorials/Streams-Patterns-Events6, Tutorials/Streams-Patterns-Events7
@section{categories}
  Tutorials>Streams-Patterns-Events

More about the default Event:

@section{section}
 protoEvents

The protoEvent contains default values for many useful parameters.

The default protoEvent is 
@racketblock[Event.default::. It provides default bindings for duration, envelope, instrument, making a very simple Pattern directly playable:

]

@racketblock[
(
// an endless sequence of middle Cs
Pbind.new.play
)
::

By adding other bindings, you can override the defaults in the protoEvent.

]

@racketblock[
(
// duration 0.25 beats (16th notes)
Pbind( \dur, 0.25 ).play
)
::

]

@racketblock[
(
// specifying the pitch in terms of midinote
// see also The pitch model below
Pbind(
	\dur, 0.125,
	\legato, 0.2,
	\midinote, Pseq(#[60, 62, 64, 65, 67, 69, 71, 72], inf)
).play
)
::

]
@section{section}
 ~finish

Event.default contains a function bound to the Symbol 
@racketblock['finish':: which is called for each new event generated in order to complete any computations that depend on the other values in the event.

]
@section{section}
 The pitch model

Event.default implements a multi level pitch model which allows composition using modal scale degrees, equal division note values, midi note values, or frequencies in Hertz. These different ways of specifying the pitch can all be used interchangeably.

The way this works is due to the default values bound to the Symbols of the pitch model.

The lowest level Symbol in the pitch model is 
@racketblock['freq'::. The default binding for ]

@racketblock['freq':: is a link::Classes/Function:: which calculates the frequency by getting the value of ]

@racketblock['midinote'::, adding a transpose value and converting it to Hertz using ]

@racketblock[midicps::.

]

@racketblock[
	~freq = {
		(~midinote.value + ~ctranspose).midicps;
	};
::

If you compose with ]

@racketblock['freq':: directly then this default function is overridden.

]

@racketblock[
(
Pbind(
	\dur, 0.25,
	\freq, Pseq(#[300, 400, 500, 700, 900], inf)
).play;
)
::

Event.default's ]

@racketblock['finish':: function sends the value message to the current binding of ]

@racketblock['freq':: in order to get the value for the frequency and adds a detune value to it which transposes the frequency in Hertz.

]

@racketblock[
(
Pbind(
	\dur, 0.25,
	\detune, -20,
	\freq, Pseq(#[300, 400, 500, 700, 900], inf)
).play
)
::

The next level is ]

@racketblock['midinote':: which is by default bound to this function:

]

@racketblock[
	~midinote = {
		(~note.value + ~gtranspose + (~octave * divs) + ~root)
				* 12.0 / ~stepsPerOctave;
	};
::

This function gets the value bound to ]

@racketblock['note':: which is a value expressed in some equal temperament, not necessarily 12. It adds a gamut transpose value ]

@racketblock['gtranspose'::, and scales from the number of notes per octave being used into 12 notes per octave MIDI key values. If you compose with ]

@racketblock['midinote':: directly then that will override this function.

]

@racketblock[
(
Pbind(
	\dur, 0.2,
	\midinote, Pseq([ Pshuf(#[60, 61, 62, 63, 64, 65, 66, 67], 3) ], inf)
).play
)
::

Another level higher is ]

@racketblock['note':: which is defined by default by this function:

]

@racketblock[
	~note = {
		var divs;
		divs = ~stepsPerOctave;
		(~degree + ~mtranspose).degreeToKey(~scale, divs);
	};
::

This function derives the note value from the next higher level variables which specify a pitch from a scale. These variables are defined as follows:

]

@racketblock[
	~stepsPerOctave = 12.0;
::

The number of equal divisions of an octave for this tuning. The equal temperament defined by this variable is known as the gamut. If you wanted to work in cents for example you could set this to 1200.0.

]

@racketblock[
	~octave = 5.0;
::

The current octave. Middle C is the lowest note in octave 5.

]

@racketblock[
	~root = 0.0;
::

The root of the scale given in equal divisions defined by ]

@racketblock[~stepsPerOctave::.

]

@racketblock[
	~scale = #[0, 2, 4, 5, 7, 9, 11]; // diatonic major scale
::

A set of scale pitches given in equal divisions defined by ]

@racketblock[~stepsPerOctave::.

]

@racketblock[
	~degree = 0;
::

A scale degree index into the ]

@racketblock[~scale::. 0 is the root and the scale wraps in the manner defined by ]

@racketblock[degreeToKey::.

]

@racketblock[
	~mtranspose = 0;
::

A modal transposition value that is added to the scale degree.

]

@racketblock[
	~gtranspose = 0;
::

A gamut transposition value that is added to the gamut pitch.

]

@racketblock[
	~ctranspose = 0;
::

A chromatic transposition value expressed in semitones.

]
@section{section}
 Pitch model Examples


@racketblock[
(
// a simple scale degree sequence
Pbind(
		// -7 is 8ve below, -3 is a 4th below,
		// 0 is root, 2 is 3rd above, 4 is 5th above, 7 is 8ve above.
	\degree, Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], inf),
	\dur, 0.15
).play
)


(
// change the octave
Pbind(
	\dur, 0.15,
	\octave, 4,
	\degree, Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], inf)
).play
)


(
// change the scale
Pbind(
	\dur, 0.15,
	\scale, [0, 2, 3, 5, 7, 8, 10],
	\degree, Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], inf)
).play
)


(
// modal transposition
var notes;
notes = Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], 1);
Pseq([
	Pbind(
		\dur, 0.15,
		\mtranspose, 0,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\mtranspose, 1,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\mtranspose, 2,
		\degree, notes
	)
], inf).play
)


(
// chromatic transposition
var notes;
notes = Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], 1);
Pseq([
	Pbind(
		\dur, 0.15,
		\ctranspose, 0,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\ctranspose, 3,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\ctranspose, -3,
		\degree, notes
	)
], inf).play
)


(
// frequency detuning
var notes;
notes = Pseq([ Pshuf(#[-7,-3,0,2,4,7], 4), Pseq([0,1,2,3,4,5,6,7]) ], 1);
Pseq([
	Pbind(
		\dur, 0.15,
		\detune, 0,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\detune, 20,
		\degree, notes
	),
	Pbind(
		\dur, 0.15,
		\detune, 40,
		\degree, notes
	)
], inf).play
)


(
// chords. If an Array of pitches is returned by a Stream for pitch, then a chord
// will be played.
Pbind(
	\dur, 0.15,
	\degree, Pseq([
		Pshuf(#[-7,-3,0,2,4,7], 4)+[0,4],
		Pseq( [0,1,2,3,4,5,6,7] )+[0,2]
	], inf)
).play
)


(
// composing in non 12 equal temperaments. 72 tone equal temp.
Pbind(
	\stepsPerOctave, 72,
	\note, Pseq([
			// 1/1, 7/6, 3/2, 7/4, 9/8
		Pseq([ [0,16,42,58,84], Pseq([ 0, 16, 42, 58, 72, 84 ], 2), [0,16,42,58,84] ], 1),
			// 1/1, 6/5, 3/2, 9/5, 9/8
		Pseq([ [0,19,42,61,84], Pseq([ 0, 19, 42, 61, 72, 84 ], 2), [0,19,42,61,84] ], 1),
			// 1/1, 5/4, 3/2, 15/8, 9/8
		Pseq([ [0,23,42,65,84], Pseq([ 0, 23, 42, 65, 72, 84 ], 2), [0,23,42,65,84] ], 1),
			// 1/1, 9/7, 3/2, 27/14, 9/8
		Pseq([ [0,26,42,68,84], Pseq([ 0, 26, 42, 68, 72, 84 ], 2), [0,26,42,68,84] ], 1)
		], inf),
	\dur, Pseq([ 1.2, Pseq([0.15], 12), 1.2], inf)
).play
)
::

]
@section{section}
 The duration model

Duration is expressed in beats and is bound to the 
@racketblock['dur':: symbol. The sustain time of a note can be expressed directly in beats or by using a legato value which is multiplied by the note duration to get the sustain time.

]

@racketblock[
(
// changing duration
Pbind(
	\dur, Pseq([ Pgeom(0.05, 1.1, 24), Pgeom(0.5, 0.909, 24) ], inf),
	\midinote, Pseq(#[60, 58], inf)
).play
)


(
// changing legato value
Pbind(
	\dur, 0.2,
	\legato, Pseq([ Pseries(0.05, 0.05, 40), Pseries(2.05, -0.05, 40) ], inf),
	\midinote, Pseq(#[48, 51, 55, 58, 60, 58, 55, 51], inf)
).play
)
::

To go to the next file:
link::Tutorials/Streams-Patterns-Events6::

]


