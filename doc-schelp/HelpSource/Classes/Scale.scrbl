#lang scribble/manual
@(require (for-label racket))

@title{Scale}
represents a musical scale@section{related}
 Classes/Tuning
@section{categories}
 Math, Tuning

@section{description}

Scale supports arbitrary octave divisions and ratios, and (in conjunction with link::Classes/Tuning::) can generate pitch information in various ways, including as input to Patterns.


@racketblock[
s.boot;

a = Scale.major;
a.degrees;		// [ 0, 2, 4, 5, 7, 9, 11 ]
a.semitones;		// [ 0, 2, 4, 5, 7, 9, 11 ]
a.cents;		// [ 0, 200, 300, 500, 700, 900, 1100 ]
a.ratios;		// [ 1, 1.1224620483089, 1.2599210498937, 1.3348398541685, etc. ]

Pbind(\scale, a, \degree, Pseq((0..7) ++ (6..0) ++ [\rest], 1), \dur, 0.25).play;

// use non-standard tuning
a.tuning_(\just);
a.degrees;		// no change; degrees are independent of tuning
a.semitones;		// [ 0, 2.0391000173077, 3.1564128700055, 4.9804499913461, etc. ]
a.ratios.collect(_.round(0.001));	// [ 1, 1.125, 1.2, 1.333, 1.5, 1.667, 1.875 ]

Pbind(\scale, a, \degree, Pseq((0..7) ++ (6..0) ++ [\rest], 1), \dur, 0.25).play;
::

]
@section{subsection}
 Creation

strong::*major, *minor, *dorian, *chromatic, *todi, *hijaz, *partch_o1, etc.::

Creates a scale from the library stored in the instance variable "all". Each scale comes with an appropriate default link::Classes/Tuning::, but alternate tunings can be specified at creation time:

@racketblock[
Scale.phrygian(\pythagorean)
::
If the tuning size does not match the scale's link::#-pitchesPerOctave::, a warning will be thrown, and the scale will use its default tuning.

For a complete list of available scales, execute
]

@racketblock[
Scale.directory
::

]
@section{CLASSMETHODS}
 

@section{method}
 all
The scale repository, to which new scales can be added.


@racketblock[
Scale.all.put(\catastrophic, Scale([0, 0.01, 0.04, 11.2]));
Scale.at(\catastrophic); // access the scale
::

]
@section{method}
 at
Access from the scale repository.

@racketblock[
Scale.all.put(\catastrophic, Scale([0, 0.01, 0.04, 11.2]));
Scale.at(\ionian);
Scale.newFromKey(\ionian); // access a copy of the scale for modification
::

]
@section{method}
 choose
Creates a random scale from the library, constrained by size and pitchsPerOctave if desired.

@racketblock[
Scale.choose;		// could be anything
Scale.choose(7);	// will be a seven-note scale in its default tuning (could be any)
Scale.choose(7, 12);	// will be a seven-note scale in a twelve-tone tuning (usually ET!2)

// Random seven-note scale in random twelve-tone tuning
a = Scale.choose(7, 12).tuning_(Tuning.choose(12));
a.tuning.name;
::

]
@section{method}
 new
Creates a Scale from scratch. strong::degrees:: should be an array of Integers or scale name. If strong::pitchesPerOctave:: is nil, will guess the most appropriate number based on degrees. strong::tuning:: can be an instance of link::Classes/Tuning:: or a symbol; if nil, will be equal temperament of pitchesPerOctave. Specify strong::descDegrees:: if the Scale should play differently when descending than when ascending; otherwise it should be nil.

@racketblock[
Scale.new(#[0, 1, 3, 6, 8, 10, 11], name: "My ET12");		// will be in ET12
Scale.new(#[0, 3, 7, 10, 15, 19, 22], name: "My Quarter-Tone");	// will be in ET24
Scale.new(#[0, 6, 17, 21, 30, 39], 43, \partch, "My Partch");
::

]
@section{method}
 chromatic

Returns a chromatic scale for a specific tuning.

@section{INSTANCEMETHODS}
 

@section{private}
 storeOn, storedKey, storeArgs, printOn

@section{method}
 tuning
Sets or gets the tuning of the Scale.
@section{argument}
 inTuning
can be either an instance of link::Classes/Tuning:: or a symbol matching a library tuning.

@section{method}
 semitones
Returns a tuned array of semitone values. link::#-as::(Array) is equivalent; link::#-as::(List) returns it as a list, etc.

@section{method}
 cents
Returns a tuned array of cent values.

@section{method}
 ratios
Returns a tuned array of ratios.

@section{method}
 as
Converting. For example 
@racketblock[as(Array)::, ]

@racketblock[as(List):: and ]

@racketblock[as(LocalBuf):: which is useful for server-side work.
]

@racketblock[
(
r = {
	var scale = Scale.choose.postln;
	SinOsc.ar(
		(
			DegreeToKey.kr(
				scale.as(LocalBuf),
				MouseX.kr(0,15), // mouse indexes into scale
				scale.stepsPerOctave,
				1, // mul = 1
				60 // offset by 72 notes
			)
			+ LFNoise1.kr([3,3], 0.04) // add some low freq stereo detuning
		).midicps, // convert midi notes to hertz
		0,
		0.25
	)
}.play;
)

r.free;
::

]
@section{method}
 size
Returns the length of the scale.

@racketblock[
Scale.ionian.size; // 7
Scale.minorPentatonic.size; // 5
Scale.ajam.size; // 7
Scale.partch_o1.size; // 6
::

]
@section{method}
 pitchesPerOctave
Returns the size of the pitch class set from which the tuning is drawn.

@racketblock[
Scale.ionian.pitchesPerOctave; // 12
Scale.minorPentatonic.pitchesPerOctave; // 12
Scale.ajam.pitchesPerOctave; // 24--this is a quarter-tone scale
Scale.partch_o1.pitchesPerOctave; // 43
::

]
@section{method}
 stepsPerOctave
Usually 12, but may be different if the current tuning has a stretched or compressed octave. Needed for degreeToKey.

@racketblock[
Scale.new((0..14), 15, tuning: \wcAlpha).stepsPerOctave;	// ~ 11.7
Scale.new(#[0, 3, 6, 9, 12], 13, tuning: \bp).stepsPerOctave;	// ~ 19.02
::
but note:
]

@racketblock[
Scale.ajam.stepsPerOctave;	// 12 -- quarter-tone scales have normal octaves
::

]
@section{method}
 at, wrapAt
These access the array generated by semitones.

@racketblock[
a = Scale.major;
a.wrapAt(4);	// 7
a.wrapAt(5);	// 9
a.wrapAt(6);	// 11
a.wrapAt(7);	// 0
::

]
@section{method}
 degreeToFreq
Returns a frequency based on current tuning and rootFreq argument.

@racketblock[
Scale.major.degreeToFreq(2, 60.midicps, 1);		// 659.25511...
Scale.major(\just).degreeToFreq(2, 60.midicps, 1);	// 654.06391...
::

]
@section{method}
 degreeToRatio
Returns a ratio based on current tuning.

@racketblock[
Scale.major.degreeToRatio(2, 1).round(0.001);		// 2.52
Scale.major(\just).degreeToRatio(2, 1).round(0.001);	// 2.5
::

]
@section{EXAMPLES}
 


@racketblock[
(
s.waitForBoot({
	a = Scale.ionian;

	p = Pbind(
		\degree, Pseq([0, 1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 0, \rest], inf),
		\scale, Pfunc({ a }, inf),
		\dur, 0.25
	);

	q = p.play;
})
)

// change scale
a = Scale.phrygian;

// change tuning
a.tuning_(\just);

// can also set tuning at creation time
a = Scale.ionian(\pythagorean);

// if you use a tuning with the wrong number of pitches per octave,
// you get a warning and the scale reverts to default tuning
a.tuning_(\partch);

// random scale
(
a = Scale.choose(7, 12);
[a.name, a.tuning.name].postln;
)

(
// or make up your own arbitrary scales and tunings
a = Scale.new(
	#[0, 2, 4, 5, 7, 9, 10],
	12,
	Tuning.new([0, 0.8, 2.1, 3, 4.05, 5.2, 6, 6.75, 8.3, 9, 10.08, 11.5]),
	"Custom"
);
)

// tuning has its own class
t = Tuning.werckmeister;

a = Scale.lydian(t);

q.stop;

// getting info
a.name;
a.degrees;
a.semitones;
a.ratios;

a.tuning.name;
a.tuning.semitones;
a.tuning.ratios;
::

]

@racketblock[
// for ascending/descending scales, use Pavaroh
(
Pbind(\note, Pavaroh(
	Pseq([0, 1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 0, \rest], 2),
		Scale.melodicMinor,
		Scale.melodicMinorDesc
	),
	\dur, 0.25
).play;
)
::

]

@racketblock[
// note that the root pitch is not stored in the Scale (which should arguably be called a Mode for that reason)
// instead you supply it at play time:

// key of A
Pbind(
	\degree, Pseq((0..7), inf), // your melody goes here
	\scale, Scale.major, // your scale goes here
	\root, -3 // semitones relative to 60.midicps, so this is A
).play;
::
]


