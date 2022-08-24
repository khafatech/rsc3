#lang scribble/manual
@(require (for-label racket))

@title{EnvGen}
 Envelope generator@section{related}
  Classes/Linen, Classes/Env
@section{categories}
   UGens>Envelopes


@section{description}


Plays back break point envelopes. The envelopes are instances of the
link::Classes/Env:: class. The envelope and the arguments for  
@racketblock[levelScale:: ,
]

@racketblock[levelBias:: , and  ]

@racketblock[timeScale::
are polled when the EnvGen is triggered and remain constant for the
duration of the envelope.

]

@racketblock[
{ PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: Done.freeSelf)) }.play
::

]
@section{classmethods}
 

@section{private}
 convertEnv

@section{method}
 ar, kr

@section{argument}
 envelope

An link::Classes/Env:: instance, or an Array of Controls.
(See link::Classes/Control::  and the example below for how to use
this.)

The envelope is polled when the EnvGen is triggered. The Env inputs can be other UGens.


@section{argument}
 gate

This triggers the envelope and holds it open while > 0. If the
Env is fixed-length (e.g. Env.linen, Env.perc), the gate argument
is used as a simple trigger. If it is an sustaining envelope
(e.g. Env.adsr, Env.asr), the envelope is held open until the
gate becomes 0, at which point is released.

If strong::gate:: < 0, force release with time 
@racketblock[ -1.0 - gate ::. See link::#Forced release:: below.

]
@section{argument}
 levelScale

Scales the levels of the breakpoints.


@section{argument}
 levelBias

Offsets the levels of the breakpoints.


@section{argument}
 timeScale

Scales the durations of the segments.


@section{argument}
 doneAction

An integer representing an action to be executed when the env is
finished playing. This can be used to free the enclosing synth,
etc. See link::Classes/Done::  for more detail.

@section{discussion}
 
@section{note}
 
The actual minimum duration of a segment is not zero, but one sample step for audio rate and one block for control rate. This may result in asynchronicity when in two envelopes of different number of levels, the envelope times add up to the same total duration. Similarly, when modulating times, the new time is only updated at the end of the current segment - this may lead to asynchronicity of two envelopes with modulated times.
::


@racketblock[

// as amplitude envelope
(
{
	var env = Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]);
	SinOsc.ar(470) * EnvGen.kr(env, doneAction: Done.freeSelf)
}.play
)

// as amplitude and modulation envelope
(
{
	var env = Env([0, 1, 0.5, 0.8, 0, 1.2, 0], [0.01, 0.5, 0.02, 0.5, 0.2, 0.5]);
	var gate = Impulse.kr(MouseX.kr(0.2, 3), 0.5);
	var gen = EnvGen.kr(env, gate);
	SinOsc.ar(270, SinOsc.ar(gen * 473)) * gen * 0.2
}.play
)
// EnvGen multichannel expands when passed a multichannel envelope
(
{
	SinOsc.ar(
		EnvGen.kr(
			Env.circle([0, 1, 0, (2..4), 0, LFNoise1.kr(0.1 ! 5) * 10, 0], [0.01, 0.6])
		)
		* 240 + 300
	).sum * 0.2
}.play;
)
::

]
@section{Examples}
 


@racketblock[
// retriggered envelope by Dust
(
{
	var env = Env([0.0, 0.5, 0.0, 1.0, 0.9, 0.0], [0.05, 0.1, 0.01, 1.0, 1.5], -4);
	var envgen = EnvGen.ar(env, Dust.ar(1));
	SinOsc.ar(
		envgen * 1000 + 440
	) * envgen * 0.1
}.play
);

// two channels
(
{
	var env = Env([0.0, [-0.2, 0.5], 0.0, 1.0, [-0.4, 0.9], 0.0], [0.05, 0.1, 0.01, 1.0, 1.5], -4);
	var envgen = EnvGen.ar(env, Dust.ar([1, 1]));
	SinOsc.ar(
		envgen * 440 + 550
	) * envgen * 0.1
}.play
);

// an envelope in a SynthDef can be used to limit the synth's lifetime (doneAction: Done.freeSelf)

(
SynthDef(\env_help, { | out, gate = 0, freq = 440 |
    var z;
    z = EnvGen.kr(Env.perc, doneAction: Done.freeSelf) * SinOsc.ar(freq, 0, 0.1);
    Out.ar(out, z)
}).add;
)

(
fork {
	10.do {
		Synth(\env_help);
		0.2.rand.wait;
	}
}
)


// using a gated envelope to gate a sound:
(
SynthDef(\env_help, { | out, gate = 0, freq = 440, doneAction = 0 |
    var z = EnvGen.kr(Env.adsr, gate, doneAction: doneAction) * SinOsc.ar(freq, 0, 0.1);
    Out.ar(out, z)
}).add;
)

a = Synth(\env_help);


// turn on
a.set(\gate, 1);

// turn off
a.set(\gate, 0);

// it does not matter to what value the gate is set, as long as it is > 0
a.set(\gate, 2);

a.set(\doneAction, 2, \gate, 0); // set doneAction to two to let the synth free itself

a.free; // alternatively, free it directly.
::

]
@section{subsection}
  Specifying an envelope for each new synth

@racketblock[
(
SynthDef(\help_Env_newClear, { |out = 0|
	var env, envctl;
	// make an empty 4 segment envelope
	env = Env.newClear(4);
	// create a control argument array
	envctl = \env.kr(env.asArray);
	Out.ar(out,
		SinOsc.ar(EnvGen.kr(envctl, \gate.tr), 0, 0.3) // the gate control is a trigger
	);
}).add;
)

Synth(\help_Env_newClear, [\gate, 1, \env, Env([700,900,900,800], [1,1,1], \exp)]); // 3 segments

// reset then play again:
Synth(\help_Env_newClear, [\gate, 1, \env, Env({ rrand(60, 70).midicps } ! 4, [1,1,1], \exp)]);

// the same written as an event:
(instrument: \help_Env_newClear, gate: 1, env: Env({ rrand(60, 70).midicps } ! 4, [1,1,1], \exp)).play;

::

]
@section{subsection}
  Forced release
If the gate of an EnvGen is set to -1 or below, then the envelope will cutoff immediately.
The time for it to cutoff is the amount less than -1, with -1 being as fast as possible, -1.5 being a cutoff in 0.5 seconds, etc.
The cutoff shape is linear.

@racketblock[
(
SynthDef(\stealMe, { |out, gate = 1|
    Out.ar(out, {BrownNoise.ar}.dup * EnvGen.kr(Env.asr, gate, doneAction: Done.freeSelf))
}).add;
)

a = Synth(\stealMe);
a.release(3); //  // cutoff in 3 seconds

// this is how the OSC data looks like:
s.sendMsg(\s_new, \stealMe, 1001, 1, 0);
s.sendMsg(\n_set, 1001, \gate, -1.1); // cutoff in 0.1 seconds
::

If the synthDef has an arg named "gate", the convenience method of Node can be used: ]

@racketblock[node.release(releaseTime)::
]

@racketblock[
d = { arg gate=1; {BrownNoise.ar}.dup * EnvGen.kr(Env.asr, gate, doneAction: Done.freeSelf) }.play;
d.release(3);
::

]
@section{subsection}
  Fast triggering tests

@racketblock[
(
{
    EnvGen.kr(
        Env.new([ 0.001, 1, 0.5, 0 ], [ 0.01, 0.3, 1 ], -4, 2, nil),
        Impulse.kr(10)
    ) * SinOsc.ar(440, 0, 0.1)
}.play;
)

(
{
    EnvGen.kr(
        Env.perc( 0.1, 0.0, 0.5, 1, \welch ),
        Impulse.kr(100),
        timeScale: 0.1
    ) * SinOsc.ar(440, 0, 0.3)
}.play;
)
::

]
@section{subsection}
  Modulating the levelScale

@racketblock[
// no, it doesn't take a ugen in ...
(
{
    EnvGen.kr(
        Env.asr( 0.1, 1.0, 0.5, \welch ),
        1.0,
        FSinOsc.ar(1.0).range(0.0, 1.0),
        timeScale: 0.1
    ) * SinOsc.ar(440, 0, 0.3)
}.play;
)

// ...but an .ir rate input, a float or an ir rate ugen like Rand would work
(
{
    EnvGen.kr(
        Env.asr( 0.1, 1.0, 0.5, \welch ),
        1.0,
        Rand(0.1, 1.0),
        timeScale: 0.1
    ) * SinOsc.ar(440, 0, 0.3)
}.play;
)
::

]
@section{subsection}
 More examples

For more information about the emphasis::control bus mapping:: used in the line 
@racketblock[a = Synth(\sine, [freq: f.asMap]);::, see link::Classes/Node#-map:: and link::Classes/Bus#-asMap::.

]

@racketblock[

// Changing an Env while playing
(
SynthDef(\env, { arg i_outbus=0;
	var env, envctl;

	// make a dummy 8 segment envelope
	env = Env.newClear(8);

	// create a control argument array
	envctl = \env.kr( env.asArray );

	ReplaceOut.kr(i_outbus, EnvGen.kr(envctl, doneAction: Done.freeSelf));
}).add;
)

(
SynthDef(\sine, { |freq = 0|
	Out.ar(0, SinOsc.ar(freq, 0, 0.2));
}).add;
)

f = Bus.control(s, 1);
f.set(800);

// use f's control bus value for frequency
// i.e. *map* the control to read from the bus
a = Synth(\sine, [freq: f.asMap]);

Synth(\env, [i_outbus: f, env: Env([700, 900, 900, 800], [1, 1, 1]*0.4, \exp)]);

Synth(\env, [i_outbus: f, env: Env([1000, 1000, 800, 1000, 900, 1000], [1, 1, 1, 1, 1]*0.3, \step)]);

a.free;
f.free;
::
]


