#lang scribble/manual
@(require (for-label racket))

@title{18_Frequency_modulation}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Carriers and modulators

In its simplest form, frequency modulation (FM) synthesis - famous since the Yamaha DX7 of the 1980's - uses one oscillator to modulate the frequency of another. The modulating oscillator in FM synthesis usually runs at the audio rate and its amplitude often is shaped by an envelope or other controller, such as a low frequency oscillator.


@racketblock[
(
SynthDef("fm1", { arg bus = 0, freq = 440, carPartial = 1, modPartial = 1, index = 3, mul = 0.05;

	// index values usually are between 0 and 24
	// carPartial :: modPartial => car/mod ratio

	var mod;
	var car;

	mod = SinOsc.ar(
		freq * modPartial,
		0,
		freq * index * LFNoise1.kr(5.reciprocal).abs
	);

	car = SinOsc.ar(
		(freq * carPartial) + mod,
		0,
		mul
	);

	Out.ar(
		bus,
		car
	)
}).add;
)

(
Synth("fm1", [\bus, 0, \freq, 440, \carPartial, 1, \modPartial, 2.4]);
Synth("fm1", [\bus, 1, \freq, 442, \carPartial, 1, \modPartial, 2.401]);
)

(
s.queryAllNodes;
)
::

]
@section{section}
 FM synthesis and reverb


@racketblock[
// ... a reverb adapted from the "01 Why SuperCollider document" in the SC2 distribution
(
SynthDef("preDelay", { arg inbus = 2;
	ReplaceOut.ar(
		4,
		DelayN.ar(In.ar(inbus, 1), 0.048, 0.048)
	)
}).add;

SynthDef("combs", {
	ReplaceOut.ar(
		6,
		Mix.arFill(7, { CombL.ar(In.ar(4, 1), 0.1, LFNoise1.kr(Rand(0, 0.1), 0.04, 0.05), 15) })
	)
}).add;

SynthDef("allpass", { arg gain = 0.2;
	var source;
	source = In.ar(6, 1);
	4.do({ source = AllpassN.ar(source, 0.050, [Rand(0, 0.05), Rand(0, 0.05)], 1) });
	ReplaceOut.ar(
		8,
		source * gain
	)
}).add;

SynthDef("theMixer", { arg gain = 1;
	ReplaceOut.ar(
		0,
		Mix.ar([In.ar(2, 1), In.ar(8, 2)]) * gain
	)
}).add;
)

(
Synth("fm1", [\bus, 2, \freq, 440, \carPartial, 1, \modPartial, 1.99, \mul, 0.071]);
Synth("fm1", [\bus, 2, \freq, 442, \carPartial, 1, \modPartial, 2.401, \mul, 0.071]);
Synth.tail(s, "preDelay");
Synth.tail(s, "combs");
Synth.tail(s, "allpass");
Synth.tail(s, "theMixer", [\gain, 0.64]);
)

(
s.queryAllNodes;
)
::

]
@section{section}
 Components

Dividing the "fm" synth def into two pieces, a synthdef for a modulator and a synthdef for the carrier, gives more functionality - carrier signals can shaped by two or more modulators.


@racketblock[
(
SynthDef("carrier", { arg inbus = 2, outbus = 0, freq = 440, carPartial = 1, index = 3, mul = 0.2;

	// index values usually are between 0 and 24
	// carPartial :: modPartial => car/mod ratio

	var mod;
	var car;

	mod = In.ar(inbus, 1);

	Out.ar(
		outbus,
		SinOsc.ar((freq * carPartial) + mod, 0, mul);
	)
}).add;

SynthDef("modulator", { arg outbus = 2, freq, modPartial = 1, index = 3;
	Out.ar(
		outbus,
		SinOsc.ar(freq * modPartial, 0, freq)
		*
		LFNoise1.kr(Rand(3, 6).reciprocal).abs
		*
		index
	)
}).add;
)

(
var freq = 440;
// modulators for the left channel
Synth.head(s, "modulator", [\outbus, 2, \freq, freq, \modPartial, 0.649, \index, 2]);
Synth.head(s, "modulator", [\outbus, 2, \freq, freq, \modPartial, 1.683, \index, 2.31]);

// modulators for the right channel
Synth.head(s, "modulator", [\outbus, 4, \freq, freq, \modPartial, 0.729, \index, 1.43]);
Synth.head(s, "modulator", [\outbus, 4, \freq, freq, \modPartial, 2.19, \index, 1.76]);

// left and right channel carriers
Synth.tail(s, "carrier", [\inbus, 2, \outbus, 0, \freq, freq, \carPartial, 1]);
Synth.tail(s, "carrier", [\inbus, 4, \outbus, 1, \freq, freq, \carPartial, 0.97]);
)

(
s.queryAllNodes;
)
::

]
@section{section}
 Reverberation and frequency modulation


@racketblock[
(
var freq;
// generate a random base frequency for the carriers and the modulators
freq = 330.0.rrand(500);

// modulators for the left channel
Synth.head(s, "modulator", [\outbus, 60, \freq, freq, \modPartial, 0.649, \index, 2]);
Synth.head(s, "modulator", [\outbus, 60, \freq, freq, \modPartial, 1.683, \index, 2.31]);

// modulators for the right channel
Synth.head(s, "modulator", [\outbus, 62, \freq, freq, \modPartial, 1.11, \index, 1.43]);
Synth.head(s, "modulator", [\outbus, 62, \freq, freq, \modPartial, 0.729, \index, 1.76]);

// left and right channel carriers
Synth.tail(s, "carrier", [\inbus, 60, \outbus, 100, \freq, freq, \carPartial, 1]);
Synth.tail(s, "carrier", [\inbus, 62, \outbus, 100, \freq, freq+1, \carPartial, 2.91]);

Synth.tail(s, "preDelay", [\inbus, 100]);
Synth.tail(s, "combs");
Synth.tail(s, "allpass");
Synth.tail(s, "theMixer", [\gain, 0.2]);
)

(
s.queryAllNodes;
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/19_Scheduling::
]


