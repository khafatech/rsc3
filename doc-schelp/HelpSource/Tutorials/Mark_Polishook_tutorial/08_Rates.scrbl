#lang scribble/manual
@(require (for-label racket))

@title{08_Rates}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Audio rate

Ugens to which an .ar message is sent run at the audio rate, by default, at 44,100 samples per second. Send the .ar message to unit generators when they're part of the audio chain that will be heard.


@racketblock[
SinOsc.ar(440, 0, 1);
::

]
@section{section}
 Control rate

Ugens to which a .kr message is appended run at the control rate.


@racketblock[
SinOsc.kr(440, 0, 1);
::

By default, control rate ugens generate one sample value for every sixty-four sample values made by an audio rate ugen. Control rate ugens thus use fewer resources and are less computationally expensive than their audio rate counterparts.

Use control rate ugens as modulators, that is, as signals that shape an audio signal.

////////////////////////////////////////////////////////////////////////////////////////////////////

Here, a control rate SinOsc modulates the frequency of the audio rate Pulse wave.

]

@racketblock[
(
SynthDef("anExample", {
	Out.ar(
		0,
		Pulse.ar(
			[220, 221.5] + SinOsc.kr([7, 8], 0, 7), // the control rate conserves CPU cycles
			0.35,
			0.02
		)
	)
}).add;
)

Synth("anExample")
::

Type command-period (cmd-.) to stop synthesis.

////////////////////////////////////////////////////////////////////////////////////////////////////

Go to link::Tutorials/Mark_Polishook_tutorial/09_Buses::

]


