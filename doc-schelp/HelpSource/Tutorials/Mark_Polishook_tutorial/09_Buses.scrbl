#lang scribble/manual
@(require (for-label racket))

@title{09_Buses}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial


By default, SuperCollider has 1024 buses for audio signals and 16,384 for control signals. The buses, which are items in an array, are what SuperCollider uses to represent audio and control rate data.

////////////////////////////////////////////////////////////////////////////////////////////////////


@racketblock[
// the array of audio buses (channels)
[ channel0, channel1, channel2, channel3, channel4, ... , ..., ..., etc., ... channel127 ]

// the array of control buses (channels)
[ channel0, channel1, channel2, channel3, channel4, ... , ..., ..., etc., ... channel4095 ]
::

]
@section{section}
 Placing audio into a bus

Use an Out ugen at the audio rate to put data into an audio bus.


@racketblock[
(
SynthDef("dataForABus", {
	Out.ar(
		0,		// write 1 channel of audio into bus 0
		Saw.ar(100, 0.1)
	)
}).add;
)

Synth("dataForABus");
::

A SynthDef browser

]

@racketblock[
(
SynthDescLib.global.read;
SynthDescLib.global.browse;
)
::

shows 1 channel of output on channel 0.

]
@section{section}
 Getting audio from a bus

Send an .ar message to an In ugen to get data from an audio bus.


@racketblock[
(
SynthDef("dataFromABus", {
	Out.ar(
		0,
		[	// the left channel gets input from an audio bus
			In.ar(0, 1),
			SinOsc.ar(440, 0.2)
		]
	)
}).add;
)

(
Synth("dataForABus");	// synthesize a sawtooth wave on channel 0
Synth("dataFromABus");	// pair it with a sine wave on channel 1
)
::

]
@section{section}
 Control rate buses

Use 
@racketblock[In.kr:: and ]

@racketblock[Out.kr:: to read from or write to control buses.

////////////////////////////////////////////////////////////////////////////////////////////////////

For additional information, see the link::Classes/Out::, link::Classes/In::, and link::Classes/Bus:: files in the SuperCollider help system.

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/10_Controls::
]

